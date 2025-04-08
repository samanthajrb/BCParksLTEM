# ===========================
# ===========================
# This code can be used to create a grid of plots for all sites that show:
#     - The detection types (call or visual) used in surveys at least once in each year, at each site
#     - The years with no surveys at each site
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
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}
if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}


# ===========================
# Settings based on the user-specific changes made in 'config_file.R'
# ===========================

source(config_file)  # Load user-specific settings
setwd(working_dir)  # Set working directory


# ===========================
# Organize data for plotting
# ===========================

# Reorganize data frame & create presence column for each detection type code
AMPH.data.detection = AMPH.data %>%
  select(Study.Area, Year, 
         Species.Code, Det.Type) %>%
  group_by(Study.Area, Year, Det.Type) %>%
  summarise(Present = 1, .groups = "rowwise") %>%
  spread(Det.Type, Present, fill = 0) %>%  # Converts the data into wide format
  # Convert 'Year' column to numeric, if not already
  mutate(Year = as.numeric(Year))

# Define all years
AMPH.all.years = 2013:2024

# Generate full grid of locations and years
AMPH.detection.expand = expand.grid(
  Study.Area = unique(AMPH.data.detection$Study.Area), 
  Year = AMPH.all.years)

# Identify years with no survey data per location
AMPH.detection = AMPH.detection.expand %>%
  left_join(AMPH.data.detection %>% select(Study.Area, Year, everything()), 
            by = c("Study.Area", "Year")) %>%
  mutate(is.surveyed = ifelse(rowSums(select(., -Study.Area, -Year) > 0) > 0, 1, 0)) %>%
  # Ensure columns are filled with 0 where NA is present
  mutate(across(-c(Study.Area, Year), ~ replace_na(., 0))) %>%
  # Create 'Survey.Status' column
  mutate(`Survey.Status` = case_when(is.surveyed == "0" ~ "No Survey",
                                     is.surveyed == "1" ~ "Survey",
                                     TRUE ~ "Unknown")) %>%  # If none of the conditions match, fill with "Unknown"
  mutate(Survey.Status = factor(Survey.Status, levels = c("No Survey", "Survey")))

# Reshape the data back into long format for ggplot
AMPH.detection.long = AMPH.detection %>%
  pivot_longer(
    cols = -c(Study.Area, Year, is.surveyed, Survey.Status),
    names_to = "Det.Type",
    values_to = "Presence") %>%
  filter(Presence > 0)  # Keep only rows where entries are present


# ===========================
# Set up for plotting
# ===========================

# Define colours
AMPH.det.colours = c("CA" = "#377EB8", 
                    "VI" = "#984EA3",
                    "No Survey" = "gray90")

# Define labels
AMPH.det.labs = c("No Survey" = "No Survey Conducted",
                  "VI" = "Visual",
                  "CA" = "Auditory")

# Define y-axis values
AMPH.det.ylabs = c("Visual", "Auditory")

# Define the data to be plotted
tile.data = AMPH.detection %>% filter(is.surveyed == 0)
call.data = AMPH.detection.long %>% filter(Det.Type == "CA")
visual.data = AMPH.detection.long %>% filter(Det.Type == "VI")
n.facet.row = length(unique(AMPH.detection.long$Study.Area))/3

# Define file name for plot png
plot.filename = paste0("AMPH-detectiontype-plot-", 
                       thedate, 
                       ".png")

# Define folder to save plot png in
plot_file = file.path(output_folder_plots, plot.filename) 


# ===========================
# Plot the data
# ===========================

plot = ggplot() +
  # Blank plot for set up
  geom_bar(
    data = AMPH.detection.long,
    aes(x = Year, y = Presence, fill = "white"),
    stat = "identity",
    fill = NA,
    position = "stack"
  ) +
  # Add grey tiles for years with no surveys
  geom_tile(
    data = tile.data,
    aes(x = Year, y = 0, fill = Survey.Status), 
    width = 0.8,
    height = Inf
  ) +
  # Add tiles for years with visual surveys
  geom_tile(
    data = visual.data,
    aes(x = Year, y = 0.5, fill = Det.Type), 
    width = 0.8,
    height = 0.8
  ) +
  # Add tiles for years with call surveys
  geom_tile(
    data = call.data,
    aes(x = Year, y = 1.5, fill = Det.Type), 
    width = 0.8,
    height = 0.8
  ) +
  # Apply custom colours
  scale_fill_manual(
    values = AMPH.det.colours, 
    labels = str_wrap(AMPH.det.labs, width = 15), 
    name = "Detection Type"
  ) +
  # Facet by Study.Area
  facet_wrap(~Study.Area, nrow = n.facet.row) +
  # Additional aesthetics
  theme_minimal() +
  labs(
    title = paste0("Detection Types Recorded in Amphibian Surveys (2013-2024)"),
    x = "Year",
    y = NULL,
    fill = "Detection Types"
  ) +
  scale_x_continuous(breaks = AMPH.all.years, expand = c(0,0.05)) +
  scale_y_continuous(breaks = 0.5:1.5,                      # Numeric positions
                     labels = AMPH.det.ylabs,             # Custom labels for each position
                     expand = c(0,0)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    legend.position = "right",
    legend.key.size = unit(20, 'pt'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10, face = "bold"), 
    panel.grid.major.x = element_line(color = "transparent", linewidth = 0),
    panel.grid.minor.x = element_line(color = "gray70", linewidth =  0.3),
    panel.grid.major.y = element_line(color = "transparent", linewidth =  0),
    panel.grid.minor.y = element_line(color = "gray70", linewidth =  0.3),
    panel.border = element_rect(colour = "gray70", fill=NA, linewidth=0.6),
    plot.margin = unit(c(6.5,5.5,5.5,6.5), "pt"),
    panel.margin.x = unit(15, "pt"),
    panel.margin.y = unit(5, "pt")
  )

# Save the plot as a PNG
ggsave(plot_file, 
       plot = plot, 
       width = 12.5, 
       height = 6.25, 
       dpi = 300, 
       bg = "white")

