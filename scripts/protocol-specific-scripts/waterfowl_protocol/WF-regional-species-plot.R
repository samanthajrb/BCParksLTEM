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
region.name = c(unique(region.plot.data$Site.Region))

# Create a presence/absence column for each new species code
WF.data.presence = region.plot.data %>%
  group_by(Study.Area, Site.Region, Year, Species.Code) %>%
  summarise(Present = 1, .groups = "rowwise") %>%
  spread(Species.Code, Present, fill = 0) %>%  # Converts the data into wide format
  # Convert 'Year' column to numeric, if not already
  mutate(Year = as.numeric(Year)) 

# Generate full grid of locations and years
WF.presence.expand = expand.grid(
  Study.Area = unique(WF.data.presence$Study.Area),
  Site.Region = unique(WF.data.presence$Site.Region), 
  Year = WF.all.years)

# Identify years with no survey data per location
WF.presence = WF.presence.expand %>%
  left_join(WF.data.presence %>% select(Study.Area, Site.Region, Year, everything()), by = c("Study.Area", "Site.Region", "Year")) %>%
  mutate(is.surveyed = ifelse(rowSums(select(., -Study.Area, -Site.Region, -Year) > 0) > 0, 1, 0)) %>%
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
    cols = -c(Study.Area, Site.Region, Year, is.surveyed, Survey.Status),
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
reg = str_replace_all(region.name, " ", "")

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
WF.2.star.reg = c("Kootenay Okanagan")

# Define parks with * species
WF.1.2.star.reg = c("West Coast")

# Define park with *, ** & *** species
WF.1.2.3.star.reg = c("Thompson Cariboo")

# Define the number of column required in the final plot
n.col.facet = c(3,2,2,4,2)

# Define panel margin presets for the final plot
panel.margin.presets = c(35,35,35,30,35)

# Define unique species
unique.sp.reg = unique(plot.data$Species.Code)
unique.sp.pres.reg = unique(plot.data$Pres.Code)

# Determine number of species observed (= number of rows) for use with horizontal line positions
n.horizontal.lines = n_distinct(WF.presence.long$Species.Code)

## Define the codes with consistent colours, labels & shapes
WF.constant.colours = c("No Survey", "Not Detected", "Survey")
WF.constant.labels = c("No Survey")
WF.constant.shapes = c("No Survey", "Not Detected")

## Determine the unique species codes observed at the park being plotted
unique.cols = c(unique.sp.pres.reg, WF.constant.colours)
unique.labs = c(unique.sp.pres.reg, WF.constant.labels)
unique.shapes = c(unique.sp.pres.reg, WF.constant.shapes)

# Determine the colour codes required for the unique species codes
WF.reg.colours = WF.colours.bank %>%
  filter(Sp.Pres.Codes %in% unique.cols)

# Define the colours to be used in the plot
WF.colours = setNames(WF.reg.colours$Colour.Codes, WF.reg.colours$Sp.Pres.Codes)

# Determine the shape codes required for the unique species codes
WF.reg.shapes = WF.shapes.bank %>%
  filter(Sp.Pres.Codes %in% unique.shapes)

# Define the shapes to be used in the plot
WF.shapes = setNames(WF.reg.shapes$Shape.Codes, WF.reg.shapes$Sp.Pres.Codes)

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
WF.reg.labs = WF.labels.bank %>%
  filter(Sp.Pres.Codes %in% unique.labs) %>%
  mutate(
    `ylab.Codes` = case_when(substr(Sp.Pres.Codes, 1, 1) == "B" ~ substr(Sp.Pres.Codes, start = 1, stop = 6),
                             substr(Sp.Pres.Codes, 1, 2) == "ID" ~ substr(Sp.Pres.Codes, start = 0, stop = nchar(Sp.Pres.Codes)-8),
                             substr(Sp.Pres.Codes, 1, 1) == "G" ~ substr(Sp.Pres.Codes, start = 0, stop = nchar(Sp.Pres.Codes)-8),
                             substr(Sp.Pres.Codes, 1, 2) == "OR" ~ substr(Sp.Pres.Codes, start = 0, stop = nchar(Sp.Pres.Codes)-8)))

# Define the species labels to be used in the plot
WF.labels = setNames(WF.reg.labs$Labels, WF.reg.labs$Sp.Pres.Codes)
WF.labels = WF.labels[c(n.horizontal.lines+1)]

# Define the legend limits (which entries to show in the legend)
WF.limits = c("No Survey")

# Define the y-axis labels to use in the plot (and the width/max # of characters that trigger text wrapping)
WF.ylabs = setNames(str_wrap(WF.reg.labs$Labels, width = 35), WF.reg.labs$ylab.Codes)

# Define plot width
plot.width = c(unique(region.plot.data$Reg.Plot.Width))
# Define plot height
plot.height = c(unique(region.plot.data$Reg.Plot.Height))

# Define file name for plot png
plot.filename = paste0("WF-regional-species-plot-", 
                       reg,
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
  # Add horizonal border lines for each species row
  geom_hline(
    yintercept = c(seq(0.5, n.horizontal.lines + 0.5, by = 1)),
    color = "gray70"
  ) +
  # Add tiles over years with no survey
  geom_tile(
    aes(fill = Survey.Status),
    width = 0.88,
    height = Inf
  ) +
  # Add species presence tiles
  geom_tile(
    aes(fill = Pres.Code),
    width = 0.88,
    height = 0.88
  ) +
  # Add points (shapes) on top of the presence tiles
  geom_point(
    aes(shape = Pres.Code),
    size = 3,
    colour = WF.presence.shco,
    fill = WF.presence.shco
  ) +
  # Define manual scale for the 'fill' variables (coloured tiles)
  scale_fill_manual(
    values = WF.colours,
    labels =  ~ stringr::str_wrap(WF.labels, width = 20),
    name = "Legend",
    breaks = WF.limits,
#    str_wrap(x, width = 5)
  ) +
  # Define manual scale for the shape variables (point shapes)
  scale_shape_manual(
    values = WF.shapes,
    labels =  ~ stringr::str_wrap(WF.labels, width = 20),
    name = "Legend",
    breaks = WF.limits
  ) +
  facet_wrap(~Study.Area,
             ncol = n.col.facet[region.to.plot]
  ) +
  # Additional aesthetics
  theme_minimal() +
  # Define plot title & titles for both axes
  labs(
    title = paste0("Waterfowl Species Present/Not Detected in the ", region.name, " Park Region (2013-2024)"),
    x = "Year",
    y = "Waterfowl Species Observed"
  ) +
  # Define x-axis scale (shows all years)
  scale_x_continuous(
    breaks = WF.all.years,
    expand = c(0,0.05)    # Removes empty plotting area on right and left sides of x-axis
  ) +
  # Define y-axis scale (list of observed species)
  scale_y_discrete(
    labels = WF.ylabs,
    expand = c(0,0)       # Removes empty plotting area on top and bottom of y-axis
  ) +
  # More plot aesthetics:
  theme(
    plot.title = element_text(size = 18),
    legend.position = "bottom",
    legend.key.size = unit(10, 'pt'),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 15, face = "bold"), 
    panel.grid.major.x = element_line(color = "transparent", linewidth = 0),
    panel.grid.minor.x = element_line(color = "gray70", linewidth =  0.3),
    panel.grid.major.y = element_line(color = "transparent", linewidth = 0),
    plot.margin = unit(c(7.5, 5.5, 5.5, 7.5), "pt"),
    panel.margin.x = unit(panel.margin.presets[region.to.plot], "pt"),
    panel.margin.y = unit(5, "pt"),
    axis.ticks.x.bottom = element_line(colour = "black", linewidth = 0.3),
    axis.ticks.length.x.bottom = unit(2.5, 'pt'),
    axis.ticks.y.left = element_line(colour = "black", linewidth = 0.3),
    axis.ticks.length.y.left = unit(2.5, 'pt'),
    panel.border = element_rect(colour = "gray70", fill=NA, linewidth=0.6)
  )

# Determine whether park requires additional legend text to explain *, **, & *** species
if (unique(region.plot.data$Site.Region) %in% WF.1.2.star.reg){
  legend_annotation = grid::textGrob("   * Regionally Significant\n ** Provincially Blue-listed\n     & Species of Special Concern",
                                     x = 0.0025, y = 0.0225, 
                                     hjust = 0, vjust = 0.5, 
                                     gp = grid::gpar(fontsize = 7))
  annotation_layout = c(area(t = 1, b = 1, l = 1, r = 1),
                        area(t = 1, b = 1, l = 1, r = 1))
  plot = plot.v1 + wrap_elements(full = legend_annotation) + plot_layout(design = annotation_layout)
} else if (unique(region.plot.data$Site.Region) %in% WF.2.star.reg){
  legend_annotation = grid::textGrob(" ** Provincially Blue-listed\n     & Species of Special Concern",
                                     x = 0.005, y = 0.025, 
                                      hjust = 0, vjust = 0.5, 
                                     gp = grid::gpar(fontsize = 7))
  annotation_layout = c(area(t = 1, b = 1, l = 1, r = 1),
                        area(t = 1, b = 1, l = 1, r = 1))
  plot = plot.v1 + wrap_elements(full = legend_annotation) + plot_layout(design = annotation_layout)
} else if (unique(region.plot.data$Site.Region) %in% WF.1.2.3.star.reg){
  legend_annotation = grid::textGrob("   * Regionally Significant\n  ** Provincially Blue-listed\n      & Species of Special Concern\n*** Provincially Red-listed",
                                     x = 0.005, y = 0.035, 
                                     hjust = 0, vjust = 0.5, 
                                     gp = grid::gpar(fontsize = 7))
  annotation_layout = c(area(t = 1, b = 1, l = 1, r = 1),
                        area(t = 1, b = 1, l = 1, r = 1))
  plot = plot.v1 + wrap_elements(full = legend_annotation) + plot_layout(design = annotation_layout)
} else {plot = plot.v1}

# Save the plot as a PNG with pre-defined dimensions
ggsave(plot_file, 
       plot = plot, 
       width = plot.width, 
       height = plot.height, 
       dpi = 300, 
       bg = "white")

