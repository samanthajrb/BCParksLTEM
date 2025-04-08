# ===========================
# ===========================
# This code can be used to create a plot that shows:
#     - Amphibian survey timing each year at all sites
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

# Define the data to use & isolate required columns
AMPH.data.timing = AMPH.data.EC %>%
  # Create column for day of year (from 1 to 365/366)
  mutate(DOY = yday(Visit.Start.Date)) %>%
  select(Study.Area, Year, Month, Day, Visit.Start.Date, DOY) %>%
  mutate(
    `y_mid` = case_when(Month == 1 ~ median(c(1, 32)),
                        Month == 2 ~ median(c(32, 60)),
                        Month == 3 ~ median(c(60, 91)),
                        Month == 4 ~ median(c(91, 121)),
                        Month == 5 ~ median(c(121, 152)),
                        Month == 6 ~ median(c(152, 182)),
                        Month == 7 ~ median(c(182, 213)),
                        Month == 8 ~ median(c(213, 244)),
                        Month == 9 ~ median(c(244, 274)),
                        Month == 10 ~ median(c(274, 305)),
                        Month == 11 ~ median(c(305, 335)),
                        Month == 12 ~ median(c(335, 365)))) 

# Create presence/absence for each combination of Study.Area, Year & Month
AMPH.timing.months = AMPH.data %>%
  group_by(Study.Area, Year, Month) %>%
  summarise(Present = 1, .groups = "drop") %>%
  complete(Study.Area, Year, Month = 1:12, fill = list(Present = 0)) %>%  # fill missing months with 0
  spread(Month, Present, fill = 0)  # Converts the data into wide format, ensuring all 12 months are columns

# Generate full grid of locations and years                                       
AMPH.months.expand = expand.grid(
  Study.Area = unique(AMPH.timing.months$Study.Area), 
  Year = AMPH.all.years)

# Identify years with no survey data per location                               
AMPH.months = AMPH.months.expand %>%
  left_join(AMPH.timing.months %>% select(Study.Area, Year, everything()), 
            by = c("Study.Area", "Year")) %>%
  mutate(is.surveyed = ifelse(rowSums(select(., -Study.Area, -Year) > 0) >= 1, 1, 0)) %>%
  # Ensure species columns are filled with 0 where NA is present
  mutate(across(-c(Study.Area, Year), ~ replace_na(., 0))) %>%
  mutate(Survey.Status = case_when(is.surveyed == 0 ~ "No Survey Conducted",
                                   is.surveyed == 1 ~ "Survey Conducted")) 

# Reshape the data back into long format for ggplot                       
AMPH.months.long = AMPH.months %>%
  pivot_longer(
    cols = -c(Study.Area, Year, is.surveyed, Survey.Status),
    names_to = "Month",
    values_to = "Presence") %>%
  filter(Presence > 0) %>%
  mutate(Month = as.numeric(Month)) %>%
  mutate(
    `y_mid` = case_when(Month == 1 ~ median(c(1, 32)),
                        Month == 2 ~ median(c(32, 60)),
                        Month == 3 ~ median(c(60, 91)),
                        Month == 4 ~ median(c(91, 121)),
                        Month == 5 ~ median(c(121, 152)),
                        Month == 6 ~ median(c(152, 182)),
                        Month == 7 ~ median(c(182, 213)),
                        Month == 8 ~ median(c(213, 244)),
                        Month == 9 ~ median(c(244, 274)),
                        Month == 10 ~ median(c(274, 305)),
                        Month == 11 ~ median(c(305, 335)),
                        Month == 12 ~ median(c(335, 365)))) %>%
  mutate(DOY = y_mid)


# ===========================
# Set up for plotting
# ===========================

# Create dataframe for red tile visual guide
red.data = AMPH.months %>%
  select(Study.Area, Year, is.surveyed, Survey.Status) %>%
  group_by(Study.Area, Year) %>%
  distinct(.) %>%
  mutate(
    `y_mid` = case_when(is.surveyed == 1 ~ 136.5),
    `FillGroup` = case_when(y_mid == 136.5 ~ "Visual Guide for May")
  ) %>%
  filter(Survey.Status != "No Survey Conducted")

# Define shapes & shape legend labels
AMPH.timing.shapes = c("Circle" = 1, "Square" = 15)
AMPH.shp.labs = c("Circle" = str_wrap("Survey Completed", width = 12), "Square" = str_wrap("Representative Survey **", width = 12))


# Define legend labels
AMPH.timing.fill = c("Visual Guide for May" = "#CC6677",
                     "No Survey Conducted" = "gray90")
redline.lab = c(str_wrap("Visual Guide for May *", width = 12),
                str_wrap("No Survey Conducted", width = 16))

# Define file name for plot png
plot.filename = paste0("AMPH-EC-timing-plot-",
                       thedate,
                       ".png")

# Define folder to save plot png in
plot_file = file.path(output_folder_plots, plot.filename) 


# ===========================
# Plot the data
# ===========================

plot.v1 = ggplot(AMPH.data.timing,
                 aes(x = Year, y = y_mid)) +
#  Add red tiles as visual guide for May
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
  # Add horizontal lines for month borders
  geom_hline(
    yintercept = c(32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    color = "gray80"
  ) +
  # Plot all months with surveys as circles
  geom_point(
    data = AMPH.months.long,
    aes(shape = "Circle"),
    size = 1.5,
    colour = "black"
  ) +
  # Plot selected (EC) surveys as squares
  geom_point(
    data = AMPH.data.timing,
    aes(shape = "Square"),
    size = 2,  
    colour = "black"
  ) +
  # Add grey tiles for years with no surveys
  geom_tile(
    data = AMPH.months %>% filter(is.surveyed == 0),
    aes(x = Year, 
        y = 0, 
        fill = Survey.Status),  # Map to legend
    width = 0.8,
    height = Inf
  ) +
  # Customize legends for fill and color
  scale_fill_manual(
    values = AMPH.timing.fill,
    name = "Legend",
    breaks = c("Visual Guide for May", 
               "No Survey Conducted"),
    labels = redline.lab
  ) +
  scale_shape_manual(
    values = AMPH.timing.shapes,
    labels = AMPH.shp.labs,
    name = NULL
  ) +
  guides(
    fill = guide_legend(order = 1, title.theme = element_text(size = 13, face = "bold",
                                                              hjust = 0.6, vjust = 5)),
    shape = guide_legend(order = 2, title.theme = element_blank())
  ) +
  # Facet by Study.Area
  facet_wrap(~Study.Area, ncol = 3) +
  # Additional aesthetics
  theme_minimal() +
  labs(
    title = "Amphibian Survey Timing (2013-2024)",
    x = "Year",
  ) +
  scale_x_continuous(
    breaks = AMPH.all.years, 
    expand = c(0,0)
  ) +
  scale_y_continuous(
    name = "Calendar Month",
    limits = c(0, 365),
    breaks = c(16.5, 46, 75.5, 106, 136.5, 167, 
               197.5, 228.5, 259, 289.5, 320, 350),
    labels = c("Jan", "", "Mar", "", "May", "", 
               "Jul", "", "Sep", "", "Nov", ""),
    expand = c(0,0)
  ) +
  theme(
    legend.position = "right",
    legend.key.height = unit(15, "pt"),
    legend.spacing.y = unit(1, "pt"),
    legend.box.spacing = unit(1.5, "pt"),
    legend.key.spacing.y = unit(3, "pt"),
    axis.title.x = element_text(margin = unit(c(6.5,0,0,0), "pt")),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, margin = unit(c(1,0,0,0), "pt")),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(margin = unit(c(0,6.5,0,0), "pt")),
    strip.text = element_text(size = 10, face = "bold"), 
    panel.grid.major.x = element_line(color = "transparent", linewidth = 0),
    panel.grid.minor.x = element_line(color = "gray70", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "transparent", linewidth = 0),
    panel.grid.minor.y = element_line(color = "transparent", linewidth = 0),
    axis.ticks.y = element_line(color = "gray70", size = 0.5),  # Adds black tick marks
    axis.ticks.length = unit(0.1, "cm"),  # Adjust the length of the tick marks
    plot.margin = unit(c(7.5,5.5,5.5,7.5), "pt"),
    panel.spacing.x = unit(10, "pt"),
    panel.spacing.y = unit(2, "pt"),
    panel.border = element_rect(colour = "gray70", fill=NA, linewidth=0.6)
  )

# Create annotation for further explanation
legend_annotation = grid::textGrob("  * Median of preferred\n     survey months\n** Survey selected for\n    effort standardization",
                                   x = 0.87, y = 0.035, 
                                   hjust = 0, vjust = 0.5, 
                                   gp = grid::gpar(fontsize = 8))
annotation_layout = c(area(t = 1, b = 1, l = 1, r = 1),
                      area(t = 1, b = 1, l = 1, r = 1))
plot = plot.v1 + wrap_elements(full = legend_annotation) + plot_layout(design = annotation_layout)

# Save the plot as a PNG
ggsave(plot_file, 
       plot = plot, 
       width = 10, 
       height = 10.5, 
       dpi = 300, 
       bg = "white")

