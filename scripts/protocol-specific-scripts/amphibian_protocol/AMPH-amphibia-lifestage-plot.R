# ===========================
# ===========================
# This code can be used to create a plot that shows:
#     - The number of observation entries identified to the class 'Amphibia' at each site
#     - The recorded life stage for each observation entry
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
if (!require(RColorBrewer)) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}


# ===========================
# Settings based on the user-specific changes made in 'config_file.R'
# ===========================

source(config_file)  # Load user-specific settings
setwd(working_dir)  # Set working directory


# ===========================
# Organize data for plotting
# ===========================

AMPH.data.amphibia = AMPH.data %>%
  arrange(Study.Area, Year) %>%       # Sort by Study.Area column values
  replace(is.na(.), 0) %>%        # Fill NA values in each life stage column with "0"
  # Group life stage columns into 1 column per life stage (ADULT, EGG, LARV)
  mutate(UNK.LS = ifelse(rowSums(select(.,Adult.Males, Adult.Females, Adults...Unclassified.Sex, Eggs, Egg.Masses, Larvae) >0) ==0, 1, 0)) %>%
  mutate(ADULT = ifelse(rowSums(select(., Adult.Males, Adult.Females, Adults...Unclassified.Sex) > 0) > 0, 1, 0)) %>%
  mutate(EGG = ifelse(rowSums(select(., Eggs, Egg.Masses) >0) >0, 1, 0)) %>%
  mutate(LARV = ifelse(rowSums(select(., Larvae) >0) >0, 1, 0)) %>%
  # Select only the necessary columns
  select(c(Study.Area, Species.Code, 
           ADULT, EGG, 
           LARV, UNK.LS)) %>%
  group_by(Study.Area, Species.Code) %>% 
  summarise(across(c(EGG, LARV, ADULT, UNK.LS), sum)) %>%
  filter(Species.Code == "AMPHIBIA") %>%
  pivot_longer(
    cols = -c(1:2),
    names_to = "Life.Stage",
    values_to = "n") %>%
  filter(n > 0)

# Generate full grid of locations and life stages
AMPH.amphibia.expand = expand.grid(
  Study.Area = unique(AMPH.data$Study.Area),
  Life.Stage = unique(AMPH.data.amphibia$Life.Stage)) %>%
  left_join(AMPH.data.amphibia %>% select(Study.Area, Life.Stage, 
                                          Species.Code, everything()),
            by = c("Study.Area", "Life.Stage")) %>%
  mutate(across(c(Species.Code), ~ replace_na(.,"No AMPHIBIA ID"))) %>%
  mutate(across(-c(Study.Area, Life.Stage, Species.Code), ~ replace_na(., 0))) %>%
  mutate(Life.Stage = factor(Life.Stage, levels = c("EGG", "LARV", "ADULT", "UNK.LS")))

# Identify sites with no AMPHIBIA identifications
AMPH.amphibia.tile = AMPH.amphibia.expand %>%
  select(Study.Area, Species.Code, n) %>%
  mutate(
    `No.AMPH.Pres` = case_when(
      AMPH.amphibia.expand$Species.Code == "AMPHIBIA" ~ "0",
      AMPH.amphibia.expand$Species.Code == "No AMPHIBIA ID" ~ "1",
      TRUE ~ "0"  # If none of the conditions match, fill with "Unknown"
    )) %>%
  mutate(No.AMPH.Pres = as.numeric(No.AMPH.Pres)) %>%
  group_by(Study.Area, Species.Code, n) %>% 
  summarise(across(c(No.AMPH.Pres), sum)) %>%
  ungroup(Study.Area, Species.Code, n) %>%
  mutate(`Tile.Fill` = case_when(
    No.AMPH.Pres == 4 ~ "Fill",
    No.AMPH.Pres < 4 ~ "No Fill"
  ))


# ===========================
# Set up for plotting
# ===========================

AMPH.amphibia.labs = c("ADULT" = "Adult",
                       "LARV" = "Larvae (Tadpoles)",
                       "EGG" = "Egg or Egg Mass",
                       "UNK.LS" = "No Life Stage Recorded",
                       "Fill" = "No Observations Identified as 'AMPHIBIA'"
                       )

AMPH.amphibia.colours = c("ADULT" = "#0072B2", 
                          "LARV" = "#CC6677", 
                          "EGG" = "#BEBADA",
                          "UNK.LS" = "#FCCDE5",
                          "Fill" = "gray90",
                          "No Fill" = "transparent"
)

# Define file name for plot png
plot.filename = paste0("AMPH-amphibiaID-lifestage-plot-", 
                       thedate, 
                       ".png")

# Define folder to save plot png in
plot_file = file.path(output_folder_plots, plot.filename) 


# ===========================
# Plot the data
# ===========================

plot = ggplot() +
  # Plot species presence as stacked bars
  geom_bar(data = AMPH.amphibia.expand,
           aes(x = Study.Area, 
               y = n, 
               fill = Life.Stage
           ),
           stat = "identity",
           position = "dodge"
  ) +
  # Add tiles over sites with no AMPHIBIA IDs
  geom_tile(data = AMPH.amphibia.tile,
            aes(x = Study.Area,
                y = n,
                fill = Tile.Fill),
            width = 0.9,
            height = Inf
  ) +
  # Add vertical border lines 
  geom_vline(
    xintercept = c(seq(-0.5, 19, by = 1)), 
    color = "gray70",
    size = 0.4
  ) +
  scale_fill_manual(
    values = AMPH.amphibia.colours, 
    labels = ~ stringr::str_wrap(AMPH.amphibia.labs, width = 25),
    breaks = c("ADULT", "LARV", "EGG", "UNK.LS", "Fill"
               ),
    name = "Legend",
    guide = guide_legend(reverse = TRUE)
  ) + 
  theme_minimal() +
  labs(
    title = "Life Stages of Observation Entries Identified as Class Amphibia (2013-2024)",
    x = "Study Area",
    y = "Number of Observation Entries",
    fill = "Life Stage"
  ) +
  scale_y_continuous(limits = c(0, 120), 
                     breaks = seq(0, 120, by = 5), 
                     expand = c(0,0)) +  
  scale_x_discrete(expand = c(0,0)) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", hjust = 0.5),
    legend.title.position = "top",
    legend.key.size = unit(25, 'pt'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_line(color = "transparent", size = 0),
    panel.grid.minor.y = element_line(color = "transparent", size = 0),
    plot.margin = unit(c(7.5,5.5,5.5,7.5), "pt"),
    panel.border = element_rect(color = "gray70", size = 0.6, fill = "transparent")
  )

# Save the plot as a PNG
ggsave(plot_file, 
       plot = plot, 
       width = 10, 
       height = 8.5, 
       dpi = 300, 
       bg = "white")
