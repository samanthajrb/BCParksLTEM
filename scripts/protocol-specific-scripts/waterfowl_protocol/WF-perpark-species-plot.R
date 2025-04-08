# ===========================
# ===========================
# This code can be used to create a plot for each site that shows:
#     - The waterfowl species present/not detected at the selected site
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

# Define the site name
park.name = c(unique(park.plot.data$Study.Area))

# Create a presence/absence column for each new species code
WF.data.presence = park.plot.data %>%
  group_by(Study.Area, Year, Species.Code) %>%
  summarise(Present = 1, .groups = "rowwise") %>%
  spread(Species.Code, Present, fill = 0) %>%  # Converts the data into wide format
  # Convert 'Year' column to numeric, if not already
  mutate(Year = as.numeric(Year)) 

# Generate full grid of locations and years
WF.presence.expand = expand.grid(
  Study.Area = unique(WF.data.presence$Study.Area), 
  Year = WF.all.years)

# Identify years with no survey data per location
WF.presence = WF.presence.expand %>%
  left_join(WF.data.presence %>% select(Study.Area, Year, everything()), by = c("Study.Area", "Year")) %>%
  mutate(is.surveyed = ifelse(rowSums(select(., -Study.Area, -Year) > 0) > 0, 1, 0)) %>%
  # Ensure columns are filled with 0 where NA is present
  mutate(across(-c(Study.Area, Year), ~ replace_na(., 0))) %>%
  # Create 'Survey.Status' column
  mutate(
    `Survey.Status` = case_when(is.surveyed == "0" ~ "No Survey",
                                is.surveyed == "1" ~ "Survey",
                                TRUE ~ "Unknown")) %>% # If none of the conditions match, fill with "Unknown"
  mutate(Survey.Status = factor(Survey.Status, levels = c("No Survey", "Survey")))

# Reshape the data back into long format for ggplot
WF.presence.long = WF.presence %>%
  pivot_longer(
    cols = -c(Study.Area, Year, is.surveyed, Survey.Status),
    names_to = "Species.Code",
    values_to = "Presence") %>%
  filter(Species.Code != "NULL") %>%
  # Rename higher level ID groups
  mutate(
      Species.Code = case_when(Species.Code == "OR-ANSERIFORMES" ~ "ID-WATFWL", # Order
                               Species.Code == "FA-ANATIDAE" ~ "ID-WATFWL", # Family
                               Species.Code == "GE-AYTHYA" ~ "ID-WATFWL", # Genus
                               Species.Code == "GE-BUCEPHALA" ~ "G-GOEY", # Genus
                               Species.Code == "GE-MERGUS" ~ "G-MERG", # Genus
                               .default = as.character(Species.Code))) %>%
  mutate(
    Species.Code = factor(Species.Code,
                          levels = c("B-TRSW", "B-NOWA", "B-RWBL", "B-BAEA",
                                     "B-OSPR", "ID-WATFWL", "B-SPSA", "B-WISN", 
                                     "B-AMCO", "B-BEKI", "B-COLO", "B-TRUS", 
                                     "B-CAGO", "OR-PODICIPEDIDAE", "B-EAGR", "B-PBGR", 
                                     "B-RNGR", "B-BWTE", "B-CITE", "B-GWTE", 
                                     "B-HOME", "G-MERG", "B-REDH", "B-RNDU", 
                                     "G-SCAP", "B-WODU", "B-RUDU", "B-NOPI", 
                                     "B-MALL", "B-BUFF", "G-GOEY", "B-AWPE", 
                                     "B-GBHE", "B-SACR", "NULL"))) %>%
  # Create column for Presence Code (to be plotted)
  mutate(
    `Pres.Code`= case_when(
      Presence == 1 ~ paste(Species.Code, "Present"),
      Presence == 0 & is.surveyed == 1 ~ "Not Detected",
      is.surveyed == 0 ~ "No Survey", # Distinguish years with no surveys
      TRUE ~ NA_character_))

# Define park name based on entry in config_file
prk = str_replace_all(park.name, " ", "")

# Define data to be plotted
plot.data = WF.presence.long %>%
  filter(Presence == "1")


# ===========================
# Set up for plotting
# ===========================

WF.colours.bank = data.frame(Colour.Codes = c("#CC6677", "#CC6677",
                                              "#0072B2", "#0072B2", "#0072B2", 
                                              "#DDCC77", "#DDCC77", "#DDCC77", "#DDCC77", 
                                              "#BEBADA", "#BEBADA",     
                                              "#88CCEE", "#88CCEE", "#88CCEE", "#88CCEE", 
                                              "#117733", "#117733",  
                                              "#44AA99", "#44AA99", "#44AA99",     
                                              "#999933", "#999933", "#999933", "#999933", "#999933", "#999933",
                                              "#CCEBC5", "#CCEBC5", "#CCEBC5",
                                              "#FDB462", "#FDB462", "#FDB462",  
                                              "#FCCDE5", "#FCCDE5",     
                                              "gray90", "transparent", "transparent"),
                             Sp.Pres.Codes = c("B-BAEA Present", "B-OSPR Present", 
                                               "B-NOWA Present", "B-RWBL Present", "B-TRSW Present", 
                                               "OR-PODICIPEDIDAE Present", "B-EAGR Present", "B-PBGR Present", "B-RNGR Present",
                                               "G-MERG Present", "B-HOME Present", 
                                               "B-MALL Present", "B-NOPI Present", "B-RUDU Present", "B-WODU Present",
                                               "G-GOEY Present", "B-BUFF Present", 
                                               "B-BWTE Present", "B-CITE Present", "B-GWTE Present", 
                                               "ID-WATFWL Present", "B-COLO Present", "B-AMCO Present", "B-BEKI Present", "B-WISN Present", "B-SPSA Present",
                                               "G-SCAP Present", "B-REDH Present", "B-RNDU Present", 
                                               "B-GBHE Present", "B-AWPE Present", "B-SACR Present", 
                                               "B-CAGO Present", "B-TRUS Present", 
                                               "No Survey", "Not Detected",  "Survey"))

# Define overlay shapes
WF.shapes.bank = data.frame(Shape.Codes = c(21, 1,
                                            24, 2, 6, 
                                            12, 22, 7, 0,
                                            6, 25, 
                                            0, 7, 22, 12, 
                                            1, 21, 
                                            9, 23, 5, 
                                            14, 0, 22, 7, 12, 5,
                                            5, 9, 23, 
                                            8, 11, 10, 
                                            2, 24,
                                            NA, NA),
                            Sp.Pres.Codes = c("B-BAEA Present", "B-OSPR Present", 
                                              "B-NOWA Present", "B-RWBL Present", "B-TRSW Present", 
                                              "OR-PODICIPEDIDAE Present", "B-EAGR Present", "B-PBGR Present", "B-RNGR Present",
                                              "G-MERG Present", "B-HOME Present", 
                                              "B-MALL Present", "B-NOPI Present", "B-RUDU Present", "B-WODU Present",
                                              "G-GOEY Present", "B-BUFF Present", 
                                              "B-BWTE Present", "B-CITE Present", "B-GWTE Present", 
                                              "ID-WATFWL Present", "B-COLO Present", "B-AMCO Present", "B-BEKI Present", "B-WISN Present", "B-SPSA Present",
                                              "G-SCAP Present", "B-REDH Present", "B-RNDU Present", 
                                              "B-GBHE Present", "B-AWPE Present", "B-SACR Present", 
                                              "B-CAGO Present", "B-TRUS Present", 
                                              "No Survey", "Not Detected"))

# Define legend labels
WF.labels.bank = data.frame(Labels = c("Bald Eagle", "Osprey", 
                                       "Northern Waterthrush", "Red-winged Blackbird", "Tree Swallow", 
                                       "Unidentified Grebe Species", "Eared Grebe", "Pied-billed Grebe", "Red-necked Grebe", 
                                       "Mergansers (Common & Red-breasted)", "Hooded Merganser", 
                                       "Mallard", "Northern Pintail", "Ruddy Duck", "Wood Duck", 
                                       "Goldeneyes (Common & Barrow's)", "Bufflehead",
                                       "Blue-winged Teal", "Cinnamon Teal", "Green-winged Teal",
                                       "Unidentified Waterfowl Species", "Common Loon", "American Coot", "Belted Kingfisher", "Wilson's Snipe", "Spotted Sandpiper",
                                       "Scaups (Lesser & Greater)", "Redhead", "Ring-necked Duck", 
                                       "Great Blue Heron **", "American White Pelican ***", "Sandhill Crane *", 
                                       "Canada Goose", "Trumpeter Swan", 
                                       "No Survey Conducted"),
                            Sp.Pres.Codes = c("B-BAEA Present", "B-OSPR Present", 
                                              "B-NOWA Present", "B-RWBL Present", "B-TRSW Present", 
                                              "OR-PODICIPEDIDAE Present", "B-EAGR Present", "B-PBGR Present", "B-RNGR Present",
                                              "G-MERG Present", "B-HOME Present", 
                                              "B-MALL Present", "B-NOPI Present", "B-RUDU Present", "B-WODU Present",
                                              "G-GOEY Present", "B-BUFF Present", 
                                              "B-BWTE Present", "B-CITE Present", "B-GWTE Present", 
                                              "ID-WATFWL Present", "B-COLO Present", "B-AMCO Present", "B-BEKI Present", "B-WISN Present", "B-SPSA Present",
                                              "G-SCAP Present", "B-REDH Present", "B-RNDU Present", 
                                              "B-GBHE Present", "B-AWPE Present", "B-SACR Present", 
                                              "B-CAGO Present", "B-TRUS Present", 
                                              "No Survey"))

# Define parks with ** species
WF.2.star.sp = c("Burges James Gadsden Park", "Yellow Point Bog Ecological Reserve")

# Define parks with ** species that need different spacing
WF.2.star.sp2 = c("Champion Lakes Park", "White Lake Park")

# Define parks with * species
WF.1.star.sp = c("Lac du Bois Protected Area", "Naikoon Park")

# Define park with * & *** species
WF.1.3.star.sp = c("Nazko Lake Park")

# Define unique species
unique.sp = unique(plot.data$Species.Code)
unique.sp.pres = unique(plot.data$Pres.Code)

# Determine number of species observed (= number of rows) for use with horizontal line positions
n.horizontal.lines = n_distinct(WF.presence.long$Species.Code)

## Define the codes with consistent colours, labels & shapes
WF.constant.colours = c("No Survey", "Not Detected", "Survey")
WF.constant.labels = c("No Survey")
WF.constant.shapes = c("No Survey", "Not Detected")

## Determine the unique species codes observed at the park being plotted
unique.cols = c(unique.sp.pres, WF.constant.colours)
unique.labs = c(unique.sp.pres, WF.constant.labels)
unique.shapes = c(unique.sp.pres, WF.constant.shapes)

# Determine the colour codes required for the unique species codes
WF.prk.colours = WF.colours.bank %>%
  filter(Sp.Pres.Codes %in% unique.cols)

# Define the colours to be used in the plot
WF.colours = setNames(WF.prk.colours$Colour.Codes, WF.prk.colours$Sp.Pres.Codes)

# Determine the shape codes required for the unique species codes
WF.prk.shapes = WF.shapes.bank %>%
  filter(Sp.Pres.Codes %in% unique.shapes)

# Define the shapes to be used in the plot
WF.shapes = setNames(WF.prk.shapes$Shape.Codes, WF.prk.shapes$Sp.Pres.Codes)

# Define colours for shapes
white = "white"
black = "black"

# Define function that does the opposite of "%in%"
`%ni%` = Negate(`%in%`)

# Define codes with white vs. black shapes
WF.white.shapes = which(WF.presence.long$Pres.Code %in% c("B-BAEA Present", "B-OSPR Present","B-NOWA Present", "B-RWBL Present", "B-TRSW Present","G-GOEY Present", "B-BUFF Present"))
WF.black.shapes = which(WF.presence.long$Pres.Code %ni% c("B-BAEA Present", "B-OSPR Present","B-NOWA Present", "B-RWBL Present", "B-TRSW Present","G-GOEY Present", "B-BUFF Present"))

# Determine shape colour on boxes
WF.presence.shco = seq(1, nrow(WF.presence.long), by = 1)
for (i in WF.white.shapes){
  WF.presence.shco[i] = white
}
for (i in WF.black.shapes){
  WF.presence.shco[i] = black
}

# Determine the species labels required for the unique species codes
WF.prk.labs = WF.labels.bank %>%
  filter(Sp.Pres.Codes %in% unique.labs) %>%
  mutate(
    `ylab.Codes` = case_when(substr(Sp.Pres.Codes, 1, 1) == "B" ~ substr(Sp.Pres.Codes, start = 1, stop = 6),
                             substr(Sp.Pres.Codes, 1, 2) == "ID" ~ substr(Sp.Pres.Codes, start = 0, stop = nchar(Sp.Pres.Codes)-8),
                             substr(Sp.Pres.Codes, 1, 1) == "G" ~ substr(Sp.Pres.Codes, start = 0, stop = nchar(Sp.Pres.Codes)-8),
                             substr(Sp.Pres.Codes, 1, 2) == "OR" ~ substr(Sp.Pres.Codes, start = 0, stop = nchar(Sp.Pres.Codes)-8)))

# Define the species labels to be used in the plot
WF.labels = setNames(WF.prk.labs$Labels, WF.prk.labs$Sp.Pres.Codes)
WF.labels = WF.labels[c(n.horizontal.lines+1)]

# Define the legend limits (which entries to show in the legend)
WF.limits = c("No Survey")

# Define the y-axis labels to use in the plot (and the width/max # of characters that trigger text wrapping)
WF.ylabs = setNames(str_wrap(WF.prk.labs$Labels, width = 20), WF.prk.labs$ylab.Codes)

# Define plot width
plot.width = c(unique(park.plot.data$Plot.Width))
# Define plot height
plot.height = c(unique(park.plot.data$Plot.Height))

# Define file name for plot png
plot.filename = paste0("WF-perpark-species-plot-", 
                       prk, 
                       "-",
                       thedate, 
                       ".png")

# Define folder to save plot png in
plot_file = file.path(output_folder_plots, plot.filename)


# ===========================
# Plot the data
# ===========================

plot.v1 = 
  ggplot(
    WF.presence.long, 
    aes(x = Year, y = Species.Code)
  ) +
  # Add species presence tiles
  geom_tile(
    aes(fill = Pres.Code),
    width = 0.88,
    height = 0.88
  ) +
  # Add horizonal border lines for each species row
  geom_hline(
    yintercept = c(seq(0.5, n.horizontal.lines + 0.5, by = 1)), 
    color = "gray70"
  ) +
  # Add tiles over years with no survey
  geom_tile(
    aes(fill = Survey.Status),
    width = 0.9,
    height = 1
  ) +
  # Add points (shapes) on top of the presence tiles
  geom_point(
    aes(shape = Pres.Code),
    size = 4,
    # colour = "black"
    colour = WF.presence.shco,
    fill = WF.presence.shco
  ) +
  # Define manual scale for the 'fill' variables (coloured tiles)
  scale_fill_manual(
    values = WF.colours,
    labels =  ~ stringr::str_wrap(WF.labels, width = 10),
    name = "Legend",
    breaks = WF.limits
  ) +
  # Define manual scale for the shape variables (point shapes)
  scale_shape_manual(
    values = WF.shapes,
    labels =  ~ stringr::str_wrap(WF.labels, width = 10),
    name = "Legend",
    breaks = WF.limits
  ) +
  # Additional aesthetics
  theme_minimal() +
  # Define plot title & titles for both axes
  labs(
    title = paste0("Waterfowl Species Present/Not Detected at ", park.name, " (2013-2024)"),
    x = "Year",
    y = "Waterfowl Species Observed"
  ) +
  # Define x-axis scale (shows all years)
  scale_x_continuous(
    breaks = WF.all.years,
    expand = c(0,0)    # Removes empty plotting area on right and left sides of x-axis
  ) +
  # Define y-axis scale (list of observed species)
  scale_y_discrete(
    labels = WF.ylabs,
    expand = c(0,0)       # Removes empty plotting area on top and bottom of y-axis
  ) +
  # More plot aesthetics:
  theme(
    legend.position = "right",
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    plot.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.title.x = element_text(margin = unit(c(7.5,0,0,0), "pt"),size = 8),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_text(margin = unit(c(0,7.5,0,0), "pt"), size = 7),
    panel.grid.major.x = element_line(color = "transparent", linewidth = 0),
    panel.grid.minor.x = element_line(color = "gray70", linewidth =  0.3),
    panel.grid.major.y = element_line(color = "transparent", linewidth = 0),
    plot.margin = unit(c(7.5,5.5,5.5,7.5), "pt"),
    panel.border = element_rect(colour = "gray70", fill=NA, linewidth=0.6)
  )

# Determine whether park requires additional legend text to explain *, **, & *** species
if (unique(park.plot.data$Study.Area) %in% WF.1.star.sp){
  legend_annotation = grid::textGrob("  * Regionally Significant",
                                     x = 0.005, y = 0.025, 
                                    hjust = 0, vjust = 0.5, 
                                    gp = grid::gpar(fontsize = 5.75))
  annotation_layout = c(area(t = 1, b = 1, l = 1, r = 1),
                        area(t = 1, b = 1, l = 1, r = 1))
  plot = plot.v1 + wrap_elements(full = legend_annotation) + plot_layout(design = annotation_layout)
} else if (unique(park.plot.data$Study.Area) %in% WF.2.star.sp){
  legend_annotation = grid::textGrob(" ** Provincially Blue-listed\n     & Species of Special Concern",
                                     x = 0.005, y = 0.025, 
                                     hjust = 0, vjust = 0.5, 
                                     gp = grid::gpar(fontsize = 5.75))
  annotation_layout = c(area(t = 1, b = 1, l = 1, r = 1),
                        area(t = 1, b = 1, l = 1, r = 1))
  plot = plot.v1 + wrap_elements(full = legend_annotation) + plot_layout(design = annotation_layout)
} else if (unique(park.plot.data$Study.Area) %in% WF.2.star.sp2){
  legend_annotation = grid::textGrob(" ** Provincially Blue-listed\n     & Species of Special Concern",
                                     x = 0.005, y = 0.045, 
                                     hjust = 0, vjust = 0.5, 
                                     gp = grid::gpar(fontsize = 5.75))
  annotation_layout = c(area(t = 1, b = 1, l = 1, r = 1),
                        area(t = 1, b = 1, l = 1, r = 1))
  plot = plot.v1 + wrap_elements(full = legend_annotation) + plot_layout(design = annotation_layout)
} else if (unique(park.plot.data$Study.Area) %in% WF.1.3.star.sp){
  legend_annotation = grid::textGrob("   * Regionally Significant\n*** Provincially Red-listed",
                                     x = 0.0025, y = 0.0225, 
                                     hjust = 0, vjust = 0.5, 
                                     gp = grid::gpar(fontsize = 5.75))
  annotation_layout = c(area(t = 1, b = 1, l = 1, r = 1),
                        area(t = 1, b = 1, l = 1, r = 1))
  plot = plot.v1 + wrap_elements(full = legend_annotation) + plot_layout(design = annotation_layout)
} else {plot = plot.v1}

# Save the plot as a PNG with pre-defined dimensions based on the # of species being plotted (as of Feb 6th, 2025)
ggsave(plot_file, 
       plot = plot, 
       width = plot.width, 
       height = plot.height, 
       dpi = 300, 
       bg = "white")

