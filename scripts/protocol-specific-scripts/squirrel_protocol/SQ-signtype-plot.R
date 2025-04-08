# ===========================
# ===========================
# This code can be used to create a plot that shows:
#     - The sign types recorded during squirrel surveys at all sites
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


# ===========================
# Settings based on the user-specific changes made in 'config_file.R'
# ===========================

source(config_file)  # Load user-specific settings
setwd(working_dir)  # Set working directory


# ===========================
# Organize data for plotting
# ===========================

# Define the data to use
SQ.data.signtype = SQ.data.STDT %>%
  select(Study.Area, Year, Det.Type, Sign.Type, Det.Sign) %>%
  distinct(.)

# Create a presence/absence column for each of the sign type codes
SQ.signtype.presence = SQ.data.signtype %>%
  group_by(Study.Area, Year, Det.Sign) %>%
  summarise(Present = 1, .groups = "rowwise") %>%
  spread(Det.Sign, Present, fill = 0)  # Converts the data into wide format

# Generate full grid of locations and years
SQ.signtype.expand = expand.grid(
  Study.Area = unique(SQ.data.signtype$Study.Area), 
  Year = SQ.all.years) 

# Identify years with no survey data per location
SQ.signtype = SQ.signtype.expand %>%
  left_join(SQ.signtype.presence %>% select(Study.Area, Year, everything()), 
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
SQ.signtype.long = SQ.signtype %>%
  pivot_longer(
    cols = -c(Study.Area, Year, is.surveyed, Survey.Status),
    names_to = "Type.Det.Sign",
    values_to = "Presence") %>%
  filter(Type.Det.Sign %in% c("SI-FD", "SI-UNK")) %>%
  # Make factor for legend
  mutate(Type.Det.Sign = factor(Type.Det.Sign,
                                levels = c("SI-FD",
                                           "SI-UNK"))) 


# ===========================
# Set up for plotting
# ===========================

# Define colours
SQ.signtype.cols = c("SI-FD" = "#0072B2",
                     "SI-UNK" = "#E41A1C",
                     "No Survey" = "gray90",
                     "Survey" = "transparent",
                     "Not Detected" = "transparent")

# Define labels
SQ.signtype.labs = c("No Survey" = "No Survey Conducted",
                     "SI-FD" = "Feeding",
                     "SI-UNK" = "Unknown")

# Define y-axis values
SQ.signtype.ylabs = c("Feeding", 
                      "Unknown")

# Define y-axis limits
SQ.signtype.limits = c("No Survey",
                       "SI-FD",
                       "SI-UNK")

# Define the data to be plotted
SQ.signtype.tile = SQ.signtype %>% filter(is.surveyed == "0")
SQ.FD.tile = SQ.signtype.long %>% 
  filter(Presence == "1") %>% 
  filter(Type.Det.Sign == "SI-FD")
SQ.unk.tile = SQ.signtype.long %>% 
  filter(Presence == "1")  %>% 
  filter(Type.Det.Sign == "SI-UNK")

# Define file name for plot png
plot.filename = paste0("SQ-signtype-plot-", 
                       thedate, 
                       ".png")

# Define folder to save plot png in
plot_file = file.path(output_folder_plots, plot.filename)


# ===========================
# Plot the data
# ===========================

plot = 
  ggplot(  ) +
  # Add horizonal border lines for each species row
  geom_hline(
    yintercept = c(seq(1, 2, by = 1)), 
    color = "gray70",
    linewidth = 0.3
  ) +
  # Add grey tiles for years with no surveys
  geom_tile(
    data = SQ.signtype.tile,
    aes(x = Year, y = 0, 
      fill = Survey.Status), 
    width = 0.8,
    height = Inf
  ) +
  # Add tiles for years with FD signs
  geom_tile(
    data = SQ.FD.tile,
    aes(x = Year, y = 0.5, fill = Type.Det.Sign), 
    width = 0.8,
    height = 0.8
  ) +
  # Add tiles for years with unknown signs
  geom_tile(
    data = SQ.unk.tile,
    aes(x = Year, y = 1.5, fill = Type.Det.Sign),
    width = 0.8,
    height = 0.8
  ) +
  # Apply custom colours
  scale_fill_manual(values = SQ.signtype.cols, 
                    labels = str_wrap(SQ.signtype.labs, width = 15),
                    name = "Sign Type") +
  # Facet by Study.Area
  facet_wrap(~Study.Area, 
             ncol = 3
  ) +
  # Additional aesthetics
  theme_minimal() +
  labs(
    title = paste0("Sign Types Recorded in Squirrel Sign Surveys (2012-2024)"),
    x = "Year",
    y = NULL,
    fill = "Sign Types"
  ) +
  scale_x_continuous(breaks = SQ.all.years, expand = c(0,0.1)) + 
  scale_y_continuous(
    breaks = 0.5:1.5,                      # Numeric positions
    labels = SQ.signtype.ylabs,             # Custom labels for each position
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
    panel.grid.minor.y = element_line(color = "transparent", linewidth =  0),
    panel.border = element_rect(colour = "gray70", fill=NA, linewidth=0.6),
    plot.margin = unit(c(6.5,5.5,5.5,6.5), "pt"),
    panel.margin.x = unit(15, "pt"),
    panel.margin.y = unit(5, "pt"),
  )

# Save the plot as a PNG
ggsave(plot_file, 
       plot = plot, 
       width = 12.5, 
       height = 8, 
       dpi = 300, 
       bg = "white")

