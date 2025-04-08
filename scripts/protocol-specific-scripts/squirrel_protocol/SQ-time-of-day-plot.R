# ===========================
# ===========================
# This code can be used to create a plot that shows:
#     - The squirrel detection rate (# of detections per survey) for each hour, using the Visit.Start.Time hour values for each site
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
if (!require(patchwork)) {
  install.packages("patchwork")
  library(patchwork)
}


# ===========================
# Settings based on the user-specific changes made in 'config_file.R'
# ===========================

source(config_file)  # Load user-specific settings
setwd(working_dir)  # Set working directory


# ===========================
# Organize data for plotting
# ===========================

# Define the data to use
SQ.TOD.hourly = SQ.data.TOD %>%
  select(Study.Area, Visit.Start.Date, Year, Month, Day, Hour, Species.Code, Count, Det.Type) %>%
  # Count detection for each hour and species (detections by hour)
  group_by(Study.Area, Year, Hour, Det.Type) %>%
  summarise(Detection.Count = n(), .groups = "drop")

# Generate a full grid of all possible combinations of Study.Area and Hour
SQ.TOD.expand = expand.grid(
  Study.Area = unique(SQ.TOD.hourly$Study.Area),
  Hour = SQ.all.hours)

# Identify hours with no survey data per location
SQ.TOD.merge = SQ.TOD.expand %>%
  left_join(SQ.TOD.hourly %>% select(Study.Area, Hour, Det.Type, 
                                     Detection.Count), 
            by = c("Study.Area", "Hour")) %>%
  # Ensure columns are filled with 0 where NA is present
  mutate(across(-c(Study.Area, Hour, Det.Type), ~ replace_na(., 0))) %>%
  # Create 'is.surveyed' column for plotting
  rowwise()%>%
  mutate(is.surveyed = case_when(Detection.Count >0 && Det.Type != "NODET" ~ 1,
                                 Detection.Count == 0 && Det.Type == "NODET" ~ 0,
                                 Detection.Count == 0 ~ 0,
                                 TRUE ~ NA)) %>%
  # Create 'Survey.Status' column for plotting
  mutate(`Survey.Status` = case_when(is.surveyed == "0" ~ "No Survey",
                                     is.surveyed == "1" ~ "Survey",
                                     TRUE ~ "Unknown")) %>%
  # Factor for legend
  mutate(Survey.Status = factor(Survey.Status, 
                                levels = c("No Survey", "Survey")))


# Calculate 'Detection Count' (sum of the Count for each combination of Study.Area & Hour)
SQ.TOD.DetCount = SQ.data.TOD %>%
  group_by(Study.Area, Hour) %>%
  summarise(Detection.Count = sum(Count, na.rm = TRUE), .groups = "drop")

# Calculate 'Survey Count' (# of unique Visit.Start.Date at each Study.Area at each hour)
SQ.TOD.SurvCount = SQ.data.TOD %>%
  group_by(Study.Area, Hour) %>%
  summarise(Survey.Count = n_distinct(Visit.Start.Date), .groups = "drop")

# Create data frame of merged detection counts and survey counts
SQ.DetSurv.merge = SQ.TOD.DetCount %>%
  left_join(SQ.TOD.SurvCount,
            by = c("Study.Area", "Hour"))

# Calculate Detection Rate (Detection.Count / Survey.Count)
SQ.TOD.plot = SQ.DetSurv.merge %>%
  mutate(Detection.Count = replace_na(Detection.Count, 0),  # Replace NAs in Detection.Count with 0
         Survey.Count = replace_na(Survey.Count, 0),  # Replace NAs in Survey.Count with 0
         # Avoid dividing by 0
         Detection.Rate = ifelse(Survey.Count == 0, 0, Detection.Count / Survey.Count))

# Generate a full grid of all possible combinations of Study.Area and Hour (including sites with no recorded Visit.Start.Time values)
SQ.TOD.tile1 = expand.grid(
  Study.Area = unique(SQ.data$Study.Area),
  Hour = as.numeric(SQ.all.hours))

#Identify hours with no surveys by subtracting the existing survey hours from the full grid of possible hours
SQ.TOD.tile = SQ.TOD.tile1 %>%
  anti_join(SQ.TOD.SurvCount, by = c("Study.Area", "Hour")) %>%
  filter(Hour %in% c(6:20)) %>%
  left_join(SQ.TOD.merge %>% select(Study.Area, Hour, is.surveyed, Survey.Status),
            by = c("Study.Area", "Hour")) %>%
  mutate(is.surveyed = replace_na(is.surveyed, 0),  # Replace NAs with 0
         Survey.Status = replace_na(Survey.Status, "No Survey")) %>%  # Replace NAs with "No Survey"
  # Rename 'Study.Area' value of Tarahne Park for * legend annotation
  mutate(Study.Area = case_when(Study.Area == "Tarahne Park" ~ "Tarahne Park *",
                                Study.Area != "Tarahne Park" ~ as.character(Study.Area)))


# ===========================
# Set up for plotting
# ===========================

# Define legend labels
SQ.TOD.labs = c("Detection Rate" = "Detection Rate",
                "No Survey" = "No Survey Conducted")

# Define plot colours
SQ.TOD.cols = c("Detection Rate" = "#377EB8", 
                "No Survey" = "gray90")

# Define x-axis labels
SQ.TOD.xlabs = c("06","07","08","09",
                 "10","11","12","13",
                 "14","15","16","17",
                 "18","19","20")

# Define x-axis breaks
SQ.xbreaks = c(6:20)

# Define x-axis limits
SQ.xlimit = c(5.5, 20.5)

# Define file name for plot png
plot.filename = paste0("SQ-timing-timeofday-plot-",
                       thedate,
                       ".png")

# Define folder to save plot png in
plot_file = file.path(output_folder_plots, plot.filename) 


# ===========================
# Plot the data
# ===========================

plot.v1 = ggplot() +
  # Add grey bars for hours with no surveys
  geom_tile(
    aes(x = Hour, y = 0, fill = "No Survey"), 
    data = SQ.TOD.tile, 
    width = 0.8, 
    height = Inf
  ) +
  # Plot the detection rates for each hour with stacked bars
  geom_bar(
    aes(x = Hour, y = Detection.Rate, fill = "Detection Rate"), 
    stat = "identity", position = "stack",
    data = SQ.TOD.plot 
  ) +
  scale_fill_manual(
    values = SQ.TOD.cols,
    labels = str_wrap(SQ.TOD.labs, width = 10),
    name = "Legend"
  ) +
  # Facet by Study.Area
  facet_wrap(~ Study.Area, ncol = 3) + 
  # Customize scales and themes
  scale_x_continuous(
    expand = c(0,0.05),
    breaks = SQ.xbreaks, 
    limits = SQ.xlimit,
    labels = SQ.TOD.xlabs
    ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 3),
    expand = c(0,0)
    ) +
  # Additional aesthetics
  theme_minimal() +
  labs(
    title = "Squirrel Detection Rate per Hour by Study Area (2012-2024)",
    x = "Hour of Day",
    y = "Detection Rate (Detections per Survey)",
    fill = "Legend"
  ) +
  theme(
    legend.position = "right",
    legend.key.size = unit(20, 'pt'),
    legend.key.spacing.y = unit(1.5, "pt"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, margin = unit(c(0.25,0,0,0), "pt")),
    axis.text.y = element_text(size = 9, margin = unit(c(0,1,0,0), "pt")),
    axis.title.y = element_text(margin = unit(c(0,6.5,0,0), "pt")),
    strip.text = element_text(size = 9, face = "bold"), 
    panel.grid.major.x = element_line(color = "transparent", size = 0),
    panel.grid.minor.x = element_line(color = "gray70", size = 0.3),
    panel.grid.major.y = element_line(color = "gray70", size = 0.3),
    panel.grid.minor.y = element_line(color = "gray80", size = 0.2),
    panel.border = element_rect(colour = "gray70", fill=NA, linewidth=0.6),
    plot.margin = unit(c(6.5,5.5,5.5,6.5), "pt"),
    panel.spacing.x = unit(10, "pt"),
    panel.spacing.y = unit(2, "pt")
  )

# Create annotation for explanation for why Tarahne Park is plotted with no detection rates
legend_annotation = grid::textGrob("* No visit start times were recorded for\n    surveys completed at Tarahne Park",
                                   x = 0.81, y = 0.03, 
                                   hjust = 0, vjust = 0.5, 
                                   gp = grid::gpar(fontsize = 9))
annotation_layout = c(area(t = 1, b = 1, l = 1, r = 1),
                      area(t = 1, b = 1, l = 1, r = 1))
plot = plot.v1 + wrap_elements(full = legend_annotation) + plot_layout(design = annotation_layout)


# Save the plot as a PNG
ggsave(plot_file, 
       plot = plot, 
       width = 12.75, 
       height = 10.5, 
       dpi = 300, 
       bg = "white")
