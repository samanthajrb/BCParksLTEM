# ===========================
# ===========================
# This code can be used to create a plot that shows:
#     - The squirrel species present/not detected at each site 
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

# Reorganize data frame & create a presence/not detected column for each of the species codes
SQ.data.species = SQ.data %>%
  group_by(Study.Area, Species.Code, Year) %>%
  summarise(Present = 1, .groups = "rowwise") %>%
  spread(Species.Code, Present, fill = 0) # Converts the data into wide format

# Generate full grid of locations and years
SQ.species.expand = expand.grid(
  Study.Area = unique(SQ.data.species$Study.Area), 
  Year = SQ.all.years)

# Identify years with no survey data per location
SQ.species = SQ.species.expand %>%
  left_join(SQ.data.species %>% select(Study.Area, Year, everything()), 
            by = c("Study.Area", "Year")) %>%
  mutate(is.surveyed = ifelse(rowSums(select(., -Study.Area, -Year) > 0) > 0, 1, 0)) %>%
  # Ensure species columns are correctly filled with 0 where NA
  mutate(across(-c(Study.Area, Year), ~ replace_na(., 0))) %>%
  # Create 'Survey.Status' column
  mutate(
    `Survey.Status` = case_when(is.surveyed == "0" ~ "No Survey",
                                is.surveyed == "1" ~ "Survey",
                                TRUE ~ "Unknown")) %>%  # If none of the conditions match, fill with "Unknown"
  mutate(Survey.Status = factor(Survey.Status, levels = c("No Survey", "Survey")))

# Reshape the data back into a long format for ggplot
SQ.species.long = SQ.species %>%
  pivot_longer(
    cols = -c(Study.Area, Year, is.surveyed, Survey.Status),
    names_to = "Species.Code",
    values_to = "Presence") %>%
  filter(Presence > 0) %>%  # Keep only rows where species are present
  # Filter out NULL detections
  filter(Species.Code != "NULL")


# ===========================
# Set up for plotting
# ===========================

# Define plot colours
SQ.species.cols = c("M-SCCA" = "#88CCEE",
                    "M-TADO" = "#117733",
                    "M-TAHU" = "#CC6677",
                    "DOUG-OR-RED" = "#DDCC77",
                    "No Survey" = "grey90")

# Define plot labels
SQ.species.labs = c("M-SCCA" = "Eastern Grey Squirrel",
                    "M-TADO" = "Douglas Squirrel",
                    "M-TAHU" = "Red Squirrel",
                    "DOUG-OR-RED" = "Douglas Squirrel or Red Squirrel *",
                    "No Survey" = "No Survey Conducted")

# Define the data to be plotted
plot.data = SQ.species.long
tile.data = SQ.species %>% filter(is.surveyed == 0)

# Define file name for plot png
plot.filename = paste0("SQ-species-plot-",
                       thedate,
                       ".png")

# Define folder to save plot png in
plot_file = file.path(output_folder_plots, plot.filename) 


# ===========================
# Plot the data
# ===========================

plot.v1 = ggplot() +
  # Plot species presence as stacked bars
  geom_bar(
    data = plot.data,
    aes(x = Year, fill = Species.Code),
    stat = "count",
    position = "stack"
  ) +
  # Add grey tiles for years with no surveys
  geom_tile(
    data = tile.data,
    aes(x = Year, y = 0, fill = Survey.Status),
    width = 0.8,
    height = Inf
  ) +
  # Apply custom colours and labels
  scale_fill_manual(
    values = SQ.species.cols, 
    labels = str_wrap(SQ.species.labs, width = 17),
    name = "Species Observed"
  ) +     
  # Facet by location
  facet_wrap(~Study.Area, ncol = 3) +
  # Additional aesthetics
  theme_minimal() +
  labs(
    title = paste0("Total Squirrel Species Present/Not Detected in Squirrel Surveys (2012-2024)"),
    x = "Year",
    y = "Number of Species Present",
    fill = "Species"
  ) +
  scale_x_continuous(breaks = SQ.all.years, expand = c(0,0.05)) +  # Show all values on the x-axis
  scale_y_continuous(breaks = scales::breaks_pretty(n = 2),
                     expand = c(0,0)) +  
  theme(
    legend.position = "right",
    legend.key.size = unit(20, 'pt'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = unit(c(0,6.5,0,0), "pt")),
    strip.text = element_text(size = 8.6, face = "bold"), 
    panel.grid.major.x = element_line(color = "transparent", size = 0),
    panel.grid.minor.x = element_line(color = "gray70", size = 0.3),
    panel.grid.major.y = element_line(color = "gray70", size = 0.2),
    panel.grid.minor.y = element_line(color = "transparent", size = 0),
    plot.margin = unit(c(6.5,5.5,5.5,6.5), "pt"),
    panel.border = element_rect(colour = "gray70", fill=NA, linewidth=0.6),
    panel.spacing.x = unit(15, "pt"),
    panel.spacing.y = unit(5, "pt")
  )

# Create annotation for Douglas or Red Squirrel species overlap range explanation
  legend_annotation = grid::textGrob("* Detected at sites located in the overlap range\n   of Douglas Squirrels and Red Squirrels",
                                     x = 0.81, y = 0.03, 
                                     hjust = 0, vjust = 0.5, 
                                     gp = grid::gpar(fontsize = 8))
  annotation_layout = c(area(t = 1, b = 1, l = 1, r = 1),
                        area(t = 1, b = 1, l = 1, r = 1))
  plot = plot.v1 + wrap_elements(full = legend_annotation) + plot_layout(design = annotation_layout)

# Save the plot as a PNG
ggsave(plot_file, 
       plot = plot, 
       width = 12.5, 
       height = 8, 
       dpi = 300, 
       bg = "white")

