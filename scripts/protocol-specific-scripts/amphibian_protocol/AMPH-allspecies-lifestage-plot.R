# ===========================
# ===========================
# This code can be used to create a grid of plots for all sites that show:
#     - The amphibian species present/not detected at each site
#     - The life stage that each species was recorded at for each site
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
if (!require(data.table)) {
  install.packages("data.table")
  library(data.table)
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

# Reorganize data frame & group common life stage columns
AMPH.data.lifestage = AMPH.data %>%
  arrange(Study.Area, Year) %>%       # Sort by Study.Area column values
  replace(is.na(.), 0) %>%        # Fill NA values in each life stage column with "0"
  # Group life stage columns into 1 column per life stage (ADULT, EGG, LARV)
  mutate(UNK.LS = ifelse(rowSums(select(.,Adult.Males, Adult.Females, Adults...Unclassified.Sex, Eggs, Egg.Masses, Larvae) >0) ==0, 1, 0)) %>%
  mutate(ADULT = ifelse(rowSums(select(., Adult.Males, Adult.Females, Adults...Unclassified.Sex) > 0) > 0, 1, 0)) %>%
  mutate(EGG = ifelse(rowSums(select(., Eggs, Egg.Masses) >0) >0, 1, 0)) %>%
  mutate(LARV = ifelse(rowSums(select(., Larvae) >0) >0, 1, 0)) %>%
  # Select only the necessary columns
  select(c(Study.Area, Year, 
           Species.Code, ADULT, 
           EGG, LARV, UNK.LS)) %>%
  # Convert 'Year' column to numeric, if not already
  mutate(Year = as.numeric(Year)) %>%
  distinct(.)
  
# Define all years
AMPH.all.years = 2013:2024

# Generate full grid of locations and years
AMPH.lifestage.expand = expand.grid(
  Study.Area = unique(AMPH.data.lifestage$Study.Area), 
  Year = AMPH.all.years)

# Identify years with no survey data per location
AMPH.lifestage = AMPH.lifestage.expand %>%
  left_join(AMPH.data.lifestage %>% select(Study.Area, Year, everything()), by = c("Study.Area", "Year")) %>%
  mutate(is.surveyed = ifelse(rowSums(select(., -Study.Area, -Year, -Species.Code) > 0) > 0, 1, 0)) %>%
  # Ensure life stage columns are correctly filled with 0 where NA
  mutate(across(-c(Study.Area, Year, Species.Code), ~ replace_na(., 0))) %>%
  # Create 'Survey.Status' column
  mutate(`Survey.Status` = case_when(is.surveyed == "0" ~ "No Survey",
                                     is.surveyed == "1" ~ "Survey",
                                     TRUE ~ "Unknown")) %>%  # If none of the conditions match, fill with "Unknown"
  mutate(Survey.Status = factor(Survey.Status, levels = c("No Survey", "Survey"))) %>%
  distinct(.)

# Reshape the data back into a long format for ggplot
AMPH.lifestage.long = AMPH.lifestage %>%
  pivot_longer(
    cols = -c(Study.Area, Year, Species.Code, is.surveyed, Survey.Status),
    names_to = "Life.Stage",
    values_to = "Presence") %>%
  filter(Presence > 0) %>%  # Keep only rows where species are present
  mutate(Species.Code = factor(Species.Code,           # Factor for legend order
                               levels = c("A-AMGR","A-AMMA","A-TAGR", 
                                          "A-ANBO","A-SPIN","A-LICA",
                                          "A-LISY","A-PSRE","A-RAAU",
                                          "A-RALU","AMPHIBIA","CAUDATA"))) %>%
  filter(Species.Code != "NULL") %>%
  distinct(.)

#### PLOTTING ALL SPECIES AND LIFE STAGES ON ONE PLOT USING SHAPES AND COLOURS TO DECIPHER LIFE STAGES ####
AMPH.lifestage.plot = AMPH.lifestage.long %>%
  group_by(Study.Area, Year, Species.Code, is.surveyed, Survey.Status, Life.Stage) %>%
  summarise(Present = 1, .groups = "rowwise") %>%
  spread(Life.Stage, Present, fill = 0) %>%
  mutate(`LS.Code` = case_when(
    EGG == "1" & LARV == "1" & ADULT == "1" ~ "ELA",
    EGG == "1" & LARV == "1" & ADULT == "1" & UNK.LS == "1" ~ "ELA",
    EGG == "1" & LARV == "1" ~ "EL",
    EGG == "1" & LARV == "1" & UNK.LS == "1" ~ "EL",
    EGG == "1" & ADULT == "1" ~ "EA",
    EGG == "1" & ADULT == "1" & UNK.LS == "1" ~ "EA",
    LARV == "1" & ADULT == "1" ~ "LA",
    LARV == "1" & ADULT == "1" & UNK.LS == "1" ~ "LA",
    EGG == "1" ~ "E",
    EGG == "1" & UNK.LS == "1" ~ "E",
    LARV == "1" ~ "L",
    LARV == "1" & UNK.LS == "1" ~ "L",
    ADULT == "1" ~ "A",
    ADULT == "1" & UNK.LS == "1" ~ "A",
    UNK.LS == "1" ~ "UNK",
    TRUE ~ "Unknown"  # If none of the conditions match, fill with "Unknown"
  )) %>% 
  mutate(LS.Code = factor(LS.Code,
                          levels = c("E", "L", "A", "EL", "EA", "LA", "ELA", "UNK")))


# ===========================
# Set up for plotting
# ===========================

# Determine positions of white shapes on darker coloured boxes
AMPH.lifestage.shco = seq(1, nrow(AMPH.lifestage.plot), by = 1)
tempdf = AMPH.lifestage.plot %>% arrange(Study.Area, Year, Species.Code)
tempdf_sub = c(which(tempdf$Species.Code == "A-SPIN"), 
          which(tempdf$Species.Code == "A-AMMA"),
          which(tempdf$Species.Code == "A-AMGR"),
          which(tempdf$Species.Code == "CAUDATA"))

white = "white"
black = "black"

# Define shape colours
AMPH.lifestage.shco[tempdf_sub] = white
AMPH.lifestage.shco[AMPH.lifestage.shco != white] = black

# Create temporary data frame for shape positions
tempdf1 = tempdf %>%
  select(c(Study.Area, Year, n.row = is.surveyed)) %>%
  group_by(Study.Area, Year) %>%
  summarize(n.row = sum(n.row))

# Determine & define y-axis positions for shape positions
all.y = c(seq(0.5, 5.5, by = 1))
y.ints = unlist(lapply(tempdf1$n.row, function(i) rev(all.y[1:i])))

# Define colours for each species code
AMPH.species.colours = c(
  "A-AMGR" = "#CC6677",
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
  "No Survey" = "grey90"
  )

# Define shapes for each life stage
AMPH.lifestage.sh = c(
  "E" = 1,
  "L" = 3,
  "A" = 4,
  "EL" = 10,
  "EA" = 13,
  "LA" = 8,
  "ELA" = 15,
  "UNK" = 17)

#Define labels for each species code
AMPH.lifestage.labs = c("A-LICA" = "Bullfrog",
                        "A-RALU" = "Columbia Spotted Frog",
                        "A-SPIN" = "Great Basin Spadefoot **",
                        "A-RAAU" = "Northern Red-Legged Frog ***",
                        "A-PSRE" = "Pacific Chorus Frog",
                        "A-ANBO" = "Western Toad *",
                        "A-LISY" = "Wood Frog",
                        "A-AMMA" = "Long-Toed Salamander",
                        "A-AMGR" = "Northwestern Salamander",
                        "A-TAGR" = "Roughskin Newt",
                        "AMPHIBIA" = "Class: Amphibia",
                        "CAUDATA" = "Class: Caudata",
                        "No Survey" = "No Survey Conducted",
                        "E" = "Egg",
                        "L" = "Larvae",
                        "A" = "Adult",
                        "EL" = "Egg & Larvae",
                        "EA" = "Egg & Adult",
                        "LA" = "Larvae & Adult",
                        "ELA" = "Egg, Larvae & Adult",
                        "UNK" = "No Life Stage Recorded"
                        )

# Define the data to be plotted
plot.data = AMPH.lifestage.plot
tile.data = AMPH.lifestage %>% filter(is.surveyed == 0)
max.y = max(tempdf1$n.row)
n.facet.row = length(unique(AMPH.lifestage.plot$Study.Area))/3

# Define file name for plot png
plot.filename = paste0("AMPH-species-lifestage-plot-", 
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
  geom_point(
    data = plot.data,
    aes(x = Year, y = y.ints, shape = LS.Code),
    size = 3.5,
    colour = AMPH.lifestage.shco
  ) +
  # Add grey tiles for years with no surveys
  geom_tile(
    data = tile.data,
    aes(x = Year, y = 0, fill = Survey.Status),
    width = 0.85,
    height = Inf
  ) +
  # Apply custom colours & labels
  scale_fill_manual(
    values = AMPH.species.colours, 
    labels = AMPH.lifestage.labs, 
    name = "Species Detected"
    ) +    
  # Apply custom shapes and labels
  scale_shape_manual(
    values = AMPH.lifestage.sh,
    labels = AMPH.lifestage.labs,
    name = "Life Stage(s) Observed"
  ) +
  # Facet by location 
  facet_wrap(~Study.Area, nrow = n.facet.row) +
  # Additional aesthetics
  theme_minimal() +
  labs(
    title = paste0("Amphibian Species Present/Not Detected by Life Stage (2013-2024)"),
    x = "Year",
    y = paste0("Number of Species Detected"),
    fill = "Species"
  ) +
  scale_x_continuous(breaks = AMPH.all.years, expand = c(0,0.05)) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = max.y),
                     expand = c(0,0)) +
  theme(
    legend.position = "right",
    legend.key.size = unit(15, 'pt'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y = element_text(margin = unit(c(0,6.5,0,0), "pt")),
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.major.x = element_line(color = "transparent", size = 0),
    panel.grid.minor.x = element_line(color = "gray70", size = 0.3),
    panel.grid.major.y = element_line(color = "gray70", size = 0.2),
    panel.grid.minor.y = element_line(color = "transparent", size = 0),
    plot.margin = unit(c(6.5,5.5,5.5,6.5), "pt"),
    panel.border = element_rect(colour = "gray70", fill=NA, linewidth=0.6),
    panel.margin.x = unit(15, "pt"),
    panel.margin.y = unit(5, "pt")
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
