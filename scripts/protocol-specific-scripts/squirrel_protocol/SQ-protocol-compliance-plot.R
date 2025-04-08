# ===========================
# ===========================
# This code can be used to create a plot that shows:
#     - How often the squirrel protocol has been followed re: three surveys completed within the same 30-day period
# ===========================
# ===========================


# ===========================
# Install (if necessary) & load required packages
# ===========================
# Install the required packages if not already installed & load all required packages

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}
if (!require(lubridate)) {
  install.packages("lubridate")
  library(lubridate)
}


# ===========================
# Settings based on the user-specific changes made in 'config_file.R'
# ===========================

source(config_file)  # Load user-specific settings
setwd(working_dir)  # Set working directory


# ===========================
# Organize data for plotting
# ===========================

# Isolate required columns
SQ.data.timing = SQ.data.comp %>%
  # Create new column for Survey Date
  mutate(`Date` = ymd(paste(Year, Month, Day, sep = "-"))) %>%
  # Select required columns
  select(Study.Area, Date, 
         Year, Month, Day) %>%
  distinct(.)

# Create presence/absence for each year and month combination
SQ.df.month.presence = SQ.data.timing %>%
  group_by(Study.Area, Year, Month) %>%
  summarise(Present = 1, .groups = "drop")  # 1 for Present where survey was done

# Generate full grid of locations and years
SQ.df.missing.years1 = expand.grid(
  Study.Area = unique(SQ.df.month.presence$Study.Area), 
  Year = SQ.all.years)

# Identify years with no survey data per location
SQ.df.missing.years = SQ.df.missing.years1 %>%
  left_join(SQ.data.timing %>% select(Study.Area, Year, everything()), 
            by = c("Study.Area", "Year")) %>%
  mutate(is.surveyed = ifelse(rowSums(select(., -Study.Area, -Year, -Month) > 0) > 0, 1, 0)) %>%
  mutate(across(-c(Study.Area, Year, Month, Day, Date), ~ replace_na(., 0))) %>%
  mutate(surv.status = case_when(is.surveyed == 0 ~ "No Survey Conducted",
                                 is.surveyed == 1 ~ "Survey Conducted"))

# Create Function for assigning compliance groups
assign_compliance_groups = function(data) {
  
  data = data %>%
    arrange(Study.Area, Year, Date) %>%
    group_by(Study.Area, Year) %>%
    mutate(
      ComplianceGroupID = NA_integer_,
      ComplianceGroupSize = NA_integer_,
      Compliant = NA
    )
  
  # Create an empty vector to track assigned rows
  assigned = rep(FALSE, nrow(data))
  
  group.counter = 1
  
  for (i in seq_len(nrow(data))) {
    
    # Skip if already assigned to a group
    if (assigned[i]) next
    
    # Current location and year
    current.location = data$Study.Area[i]
    current.year = data$Year[i]
    
    #Get dates for current group search
    current.date = data$Date[i]
    
    # Look forward from the current date to find surveys within 30 days
    window.indices = which(
      data$Study.Area == current.location &
        data$Year == current.year &
        !assigned &
        data$Date >= current.date &
        data$Date <= current.date + days(30)
    )
    
    # Number of surveys in the window
    group.size = length(window.indices)
    
    # Assign group info to these rows
    data$ComplianceGroupID[window.indices] = group.counter
    data$ComplianceGroupSize[window.indices] = group.size
    data$Compliant[window.indices] = group.size >= 3
    
    # Mark them as assigned to avoid overlapping groups
    assigned[window.indices] = TRUE
    
    # Increment group counter
    group.counter = group.counter + 1
  }
  
  return(data)
}

# Apply function
SQ.data.compliance = SQ.data.timing %>%
  group_modify(~assign_compliance_groups(.)) %>%
  mutate(DOY = yday(Date)) %>% # Day of year from 1 to 365/366
  mutate(ComplianceGroupSize = factor(ComplianceGroupSize)) %>%
  # column for plotting 1 point per group
  group_by(Study.Area, Year, ComplianceGroupID) %>%
  # Only compute for actual groups, skip NA
  mutate(
    GroupStartDate = min(Date, na.rm = TRUE),
    GroupEndDate = max(Date, na.rm = TRUE),
    # Calculate the midpoint date
    GroupMidDate = GroupStartDate + as.integer(GroupEndDate - GroupStartDate) / 2) %>%
  ungroup() %>%
  mutate(MidDateDOY = yday(GroupMidDate)) %>%
  mutate(FillGroup = ComplianceGroupSize)


# ===========================
# Set up for plotting
# ===========================

# Create dataframe for red tile visual guide
red.data = SQ.df.missing.years %>%
  select(Study.Area, Year, is.surveyed, surv.status) %>%
  group_by(Study.Area, Year) %>%
  distinct(.) %>%
  mutate(
    `y_mid` = case_when(is.surveyed == 1 ~ median(c(213, 243))),
    `FillGroup` = case_when(is.surveyed == 1 ~ "Visual Guide for August")
  ) 

# Define labels
comp.lab = c(str_wrap("Number of Surveys in Group", width = 20))
# month.labs = c(`167` = "Jun", `197.5` = "Jul", `228.5` = "Aug", `259` = "Sep", `289.5` = "Oct")
redline.lab = c(str_wrap("Visual Guide for August", width = 16), str_wrap("No Survey Conducted", width = 16))
subtitle.lab = c(str_wrap("Survey groups of 1 or more surveys completed within a 30-day period. Points represent one group plotted on the median survey date of that group. Compliance is determined by number of surveys per group, with 3 or more completed surveys indicating a 'Compliant' group.", width = 154))

# Define file name for plot png
plot.filename = paste0("SQ-timing-compliance-plot-",
                       thedate,
                       ".png")

# Define folder to save plot png in
plot_file = file.path(output_folder_plots, plot.filename) 


# ===========================
# Plot the data
# ===========================

plot = ggplot(SQ.data.compliance, aes(x = Year, y = MidDateDOY)) +
  # Add horizontal lines for month borders
  geom_hline(
    yintercept = c(152, 182, 213, 244, 274, 305),
    color = "gray60"
  ) +
  # Add red tiles as visual guide for August
  geom_tile(
    data = red.data,
    aes(x = Year, 
        y = y_mid,
        fill = FillGroup),
    inherit.aes = FALSE,
    width = 1,
    height = 29.99,
    alpha = 0.4
  ) +
  # Add grey tiles for years with no surveys
  geom_tile(
    data = SQ.df.missing.years %>% filter(is.surveyed == 0),
    aes(x = Year, 
        y = (max(SQ.data.compliance$DOY, na.rm = TRUE) / 2), 
        fill = surv.status,),
    width = 0.85,
    height = Inf
  ) +
  geom_point(
    data = SQ.data.compliance,
    aes(shape = Compliant,
        colour = FillGroup,
        fill = factor(ComplianceGroupSize)),
    size = 3.5,
    alpha = 0.8
  ) +
  scale_fill_manual(
    values = c("1" = "#CC6677", "2" = "#0072B2",
               "3" = "#117733", "4" = "#FDB462",
               "5" = "#FDB462", "8" = "#FDB462",
               "10" = "#FDB462",
               "Visual Guide for August" = "#CC6677",
               "No Survey Conducted" = "gray90"),
    name = "Legend",
    breaks = c("Visual Guide for August", "No Survey Conducted"),
    labels = redline.lab
  ) +
  scale_colour_manual(
    values = c("1" = "#CC6677", "2" = "#0072B2",
               "3" = "#117733", "4" = "#FDB462",
               "5" = "#FDB462", "8" = "#FDB462",
               "10" = "#FDB462"),
    breaks = c("1", "2", "3", "4"),
    labels = c("1", "2", "3", "4 +"),
    name = comp.lab
  ) +
  scale_shape_manual(
    values = c("FALSE" = 13, "TRUE" = 16),
    labels = c("FALSE" = "Not Compliant", "TRUE" = "Compliant"),
    name = "Protocol Compliance"
  ) +
  guides(
    fill = guide_legend(order = 1,
                        title.theme = element_text(size = 13, face = "bold",
                                                   hjust = 0.6, vjust = 5)),
    colour = guide_legend(order = 2,
                          title.theme = element_text(size = 9, face = "bold")),
    shape = guide_legend(order = 3,
                         title.theme = element_text(size = 9, face = "bold"))
  ) +
  # Facet by Study.Area
  facet_wrap(~Study.Area, ncol = 3) +
  # Customize scales and themes
  scale_y_continuous(
    name = "Day of Year",
    limits = c(152, 305),
    breaks = c(167, 197.5, 228.5, 259, 289.5),
    labels = c("Jun", "Jul", "Aug", "Sep", "Oct"),
    expand = expansion(mult = c(0,0))
  ) +
  scale_x_continuous(
    name = "Year", 
    breaks = SQ.all.years, 
    expand = c(0,0)
    ) +
  # Additional aesthetics
  theme_minimal() +
  labs(
    title = "Squirrel Survey Protocol Compliance by Study Area (2012-2024)",
    subtitle = subtitle.lab,
    linetype = str_wrap("Visual Guide for August", width = 16)
    ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.key.height = unit(15, "pt"),
    legend.spacing.y = unit(1, "pt"),
    legend.box.spacing = unit(1.5, "pt"),
    legend.key.spacing.y = unit(1.5, "pt"),
    plot.title = element_text(size = 11.5),
    plot.subtitle = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8, margin = unit(c(0,1,0,0), "pt")),
    axis.title.y = element_text(margin = unit(c(0,6.5,0,0), "pt")),
    strip.text = element_text(size = 9, face = "bold"), 
    panel.grid.major.x = element_line(color = "transparent", linewidth = 0),
    panel.grid.minor.x = element_line(color = "gray70", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "transparent", linewidth = 0),
    panel.grid.minor.y = element_line(color = "transparent", linewidth = 0),
    plot.margin = unit(c(7.5,5.5,5.5,7.5), "pt"),
    panel.spacing.x = unit(10, "pt"),
    panel.spacing.y = unit(2, "pt"),
    panel.border = element_rect(colour = "gray70", fill=NA, linewidth=0.6)
  )

# Save the plot as a PNG
ggsave(plot_file, 
       plot = plot, 
       width = 12.75, 
       height = 10.5, 
       dpi = 300, 
       bg = "white")


