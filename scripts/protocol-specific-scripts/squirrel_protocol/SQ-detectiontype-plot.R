# ===========================
# ===========================
# This code can be used to create a plot that shows:
#     - The detection types used during squirrel surveys at all sites
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

# Define the data to use
SQ.data.dettype = SQ.data.STDT %>%
  filter(!(Species.Code %in% c("NULL")))

# Create a presence/absence column for each of the detection type codes
SQ.dettype.presence = SQ.data.dettype %>%
  select(Study.Area, Year, Det.Type) %>%
  group_by(Study.Area, Year, Det.Type) %>%
  summarise(Present = 1, .groups = "rowwise") %>%
  spread(Det.Type, Present, fill = 0)  # Converts the data into wide format

# Generate full grid of locations and years
SQ.dettype.expand = expand.grid(
  Study.Area = unique(SQ.data.dettype$Study.Area), 
  Year = SQ.all.years)

# Identify years with no survey data per location
SQ.dettype = SQ.dettype.expand %>%
  left_join(SQ.dettype.presence %>% select(Study.Area, Year, everything()), 
            by = c("Study.Area", "Year")) %>%
  mutate(is.surveyed = ifelse(rowSums(select(., -Study.Area, -Year) > 0) > 0, 1, 0)) %>%
  # Ensure columns are filled with 0 where NA is present
  mutate(across(-c(Study.Area, Year), ~ replace_na(., 0))) %>%
  # Create 'Survey.Status' column
  mutate(
    `Survey.Status` = case_when(
      is.surveyed == "0" ~ "No Survey",
      is.surveyed == "1" ~ "Survey",
      TRUE ~ "Unknown"  # If none of the conditions match, fill with "Unknown"
    )) %>%
  mutate(Survey.Status = factor(Survey.Status, 
                                levels = c("No Survey", "Survey")))

# Reshape the data back into long format for ggplot
SQ.dettype.long = SQ.dettype %>%
  pivot_longer(
    cols = -c(Study.Area, Year, is.surveyed, Survey.Status),
    names_to = "Det.Type",
    values_to = "Presence") %>%
  # Make factor for legend
  mutate(Det.Type = factor(Det.Type,
                           levels = c("VI", "CA", "SI", "UNK"))) %>%
  filter(Presence > 0)


# ===========================
# Set up for plotting
# ===========================

# Define plot colours
SQ.dettype.cols = c("CA" = "#377EB8", 
                    "VI" = "#984EA3",
                    "SI" = "#FDB462",
                    "UNK" = "#E41A1C",
                    "No Survey" = "gray90")

# Define plot labels
SQ.dettype.labs = c("No Survey" = "No Survey Conducted",
                    "VI" = "Visual",
                    "CA" = "Auditory",
                    "SI" = "Sign",
                    "UNK" = "Unknown")

# Define y-axis labels
SQ.dettype.ylabs = c("Visual", "Auditory", "Sign", "Unknown")

# Define data to be plotted
SQ.tile.data = SQ.dettype %>% filter(is.surveyed == 0)
SQ.call.data = SQ.dettype.long %>% filter(Det.Type == "CA")
SQ.visual.data = SQ.dettype.long %>% filter(Det.Type == "VI")
SQ.sign.data = SQ.dettype.long %>% filter(Det.Type == "SI")
SQ.unknown.data = SQ.dettype.long %>% filter(Det.Type == "UNK")

# Define file name for plot png
plot.filename = paste0("SQ-detectiontype-plot-", 
                       thedate, 
                       ".png")

# Define folder to save plot png in
plot_file = file.path(output_folder_plots, plot.filename)


# ===========================
# Plot the data
# ===========================

plot = 
  ggplot() +
  # Plot species presence as stacked bars
  geom_bar(data = SQ.dettype.long,
           aes(x = Year, y = Presence, fill = "white"),
           stat = "identity",
           fill = NA,
           position = "stack"
  ) +
  # Add grey tiles for years with no surveys
  geom_tile(
    data = SQ.tile.data,
    aes(x = Year, y = 0, fill = Survey.Status), 
    width = 0.8,
    height = Inf
  ) +
  # Add tiles for years with visual surveys
  geom_tile(
    data = SQ.visual.data,
    aes(x = Year, y = 0.5, fill = Det.Type), 
    width = 0.8,
    height = 0.8
  ) +
  # Add tiles for years with call surveys
  geom_tile(
    data = SQ.call.data,
    aes(x = Year, y = 1.5, fill = Det.Type), 
    width = 0.8,
    height = 0.8
  ) +
  # Add tiles for years with sign surveys
  geom_tile(
    data = SQ.sign.data,
    aes(x = Year, y = 2.5, fill = Det.Type), 
    width = 0.8,
    height = 0.8
  ) +
  # Add tiles for years with unknown surveys
  geom_tile(
    data = SQ.unknown.data,
    aes(x = Year, y = 3.5, fill = Det.Type), 
    width = 0.8,
    height = 0.8
  ) +
  # Apply custom colours
  scale_fill_manual(values = SQ.dettype.cols, 
                    labels = str_wrap(SQ.dettype.labs, width = 15), 
                    name = "Detection Type"
  ) +
  # Facet by Study.Area
  facet_wrap(~Study.Area, 
             ncol = 3
  ) +
  # Additional aesthetics
  theme_minimal() +
  labs(
    title = paste0("Detection Types Recorded in Squirrel Surveys (2012-2024)"),
    x = "Year",
    y = NULL,
    fill = "Detection Types"
  ) +
  scale_x_continuous(breaks = SQ.all.years, expand = c(0,0.05)
  ) +
  scale_y_continuous(
    breaks = 0.5:3.5,                      # Numeric positions
    labels = SQ.dettype.ylabs,             # Custom labels for each position
    expand = c(0,0)
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    legend.position = "right",
    legend.key.size = unit(20, 'pt'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8.75, face = "bold"), 
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
       height = 11.75, 
       dpi = 300, 
       bg = "white")

