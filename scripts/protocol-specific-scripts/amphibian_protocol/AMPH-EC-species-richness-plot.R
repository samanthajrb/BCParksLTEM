# ===========================
# ===========================
# This code can be used to create a grid of plots for all sites that show:
#     - The amphibian species richness at each site (using the effort corrected dataset)
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
AMPH.data.richness = AMPH.data.EC %>%
  select(Study.Area, Species.Code, Year, Det.Type)

# Create presence/absence for each combination of Study.Area, Year & Month
AMPH.richness = AMPH.data.richness %>%
  group_by(Study.Area, Species.Code, Year) %>%
  summarise(Present = 1, .groups = "rowwise") %>%
  spread(Species.Code, Present, fill = 0) # Converts the data into wide format

# Generate full grid of locations and years
AMPH.richness.expand = expand.grid(
  Study.Area = unique(AMPH.richness$Study.Area),
  Year = AMPH.all.years)

# Identify years with no survey data per location
AMPH.richness.my = AMPH.richness.expand %>%
  left_join(AMPH.richness %>% select(Study.Area, Year, everything()), 
            by = c("Study.Area", "Year")) %>%
  mutate(is.surveyed = ifelse(rowSums(select(., -Study.Area, -Year) > 0) > 0, 1, 0)) %>%
  # Ensure species columns are correctly filled with 0 where NA
  mutate(across(-c(Study.Area, Year), ~ replace_na(., 0))) %>%
  mutate(
    `Survey.Status` = case_when(
      is.surveyed == "0" ~ "No Survey",
      is.surveyed == "1" ~ "Survey",
      TRUE ~ "Unknown")) %>%  # If none of the conditions match, fill with "Unknown"
  mutate(Survey.Status = factor(Survey.Status, levels = c("No Survey", "Survey")))

# Reshape the data back into long format for ggplot                       
AMPH.richness.long = AMPH.richness.my %>%
  pivot_longer(
    cols = -c(Study.Area, Year, is.surveyed, Survey.Status),
    names_to = "Species.Code",
    values_to = "Presence") %>%
  filter(Presence > 0) %>%
  filter(Species.Code != "AMPHIBIA") %>%
  filter(Species.Code != "NULL") %>%
  # Factor for legend order
  mutate(
    Species.Code = factor(Species.Code, 
                          levels = c("A-AMGR", "A-AMMA", "A-TAGR", 
                                     "A-ANBO", "A-SPIN", "A-LICA",
                                     "A-LISY", "A-PSRE", "A-RAAU",
                                     "A-RALU")))

# ===========================
# Set up for plotting
# ===========================

# Define colours for each species code
AMPH.species.colours = c("A-AMGR" = "#CC6677",
                         "A-AMMA" = "#0072B2",
                         "A-ANBO" = "#BEBADA",
                         "A-LICA" = "#88CCEE",
                         "A-LISY" = "#FDB462",
                         "A-PSRE" = "#44AA99",
                         "A-RAAU" = "#999933",
                         "A-RALU" = "#FCCDE5",
                         "A-SPIN" = "#117733",
                         "A-TAGR" = "#DDCC77",
                         "AMPHIBIA" = "#CCEBC5",
                         "CAUDATA" = "grey60",
                         "No Survey" = "grey90")


#Define labels for each species code
AMPH.species.labs = c("A-LICA" = "Bullfrog",
                      "A-RALU" = "Columbia Spotted Frog",
                      "A-SPIN" = "Great Basin Spadefoot **",
                      "A-RAAU" = "Northern Red-Legged Frog ***",
                      "A-PSRE" = "Pacific Chorus Frog",
                      "A-ANBO" = "Western Toad *",
                      "A-LISY" = "Wood Frog",
                      "A-AMMA" = "Long-Toed Salamander",
                      "A-AMGR" = "Northwestern Salamander",
                      "A-TAGR" = "Roughskin Newt",
                      "No Survey" = "No Survey Conducted")

# Define the data to be plotted
plot.data = AMPH.richness.long
tile.data = AMPH.richness.my %>% filter(is.surveyed == 0)

# Define file name for plot png
plot.filename = paste0("AMPH-EC-species-richness-plot-", 
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
  # Apply custom colours & labels
  scale_fill_manual(
    values = AMPH.species.colours, 
    labels = AMPH.species.labs, 
    name = "Species Detected"
  ) +    
  # Facet by location
  facet_wrap(~Study.Area, ncol = 3) +
  # Additional aesthetics
  theme_minimal() +
  labs(
    title = paste0("Amphibian Species Richness (2013-2024)"),
    x = "Year",
    y = "Number of Species Detected",
    fill = "Species"
  ) +
  scale_x_continuous(
    breaks = AMPH.all.years, 
    expand = c(0,0.05)
  ) +  
  scale_y_continuous(
    breaks = 0:6,
    expand = c(0,0)
  ) +  
  theme(
    legend.position = "right",
    legend.key.height = unit(15, "pt"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = unit(c(0,6.5,0,0), "pt")),
    strip.text = element_text(size = 10, face = "bold"),  # Make facet labels clearer
    panel.grid.major.x = element_line(color = "transparent", linewidth = 0),
    panel.grid.minor.x = element_line(color = "gray70", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "gray70", linewidth = 0.2),
    panel.grid.minor.y = element_line(color = "transparent", linewidth = 0),
    plot.margin = unit(c(6.5,5.5,5.5,6.5), "pt"),
    panel.spacing.x = unit(15, "pt"),
    panel.spacing.y = unit(5, "pt"),
    panel.border = element_rect(colour = "gray70", fill=NA, linewidth=0.6)
  )

# Create annotation for species status legend
legend_annotation = grid::textGrob("   * Federally listed Species of Special\n                                               Concern\n  ** Provincially Blue-listed & Federally\n                            listed as Threatened\n*** Provincially Blue-listed & Federally\n       listed Species of Special Concern",
                                   x = 0.815, y = 0.065, 
                                   hjust = 0, vjust = 0.5, 
                                   gp = grid::gpar(fontsize = 8.9))
annotation_layout = c(area(t = 1, b = 1, l = 1, r = 1),
                      area(t = 1, b = 1, l = 1, r = 1)
)
plot = plot.v1 + wrap_elements(full = legend_annotation) + plot_layout(design = annotation_layout)

# Save the plot as a PNG
ggsave(plot_file, 
       plot = plot, 
       width = 12, 
       height = 10, 
       dpi = 300, 
       bg = "white")

