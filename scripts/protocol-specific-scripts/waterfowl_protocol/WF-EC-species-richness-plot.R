# ===========================
# ===========================
# This code can be used to create one plot that shows:
#     - The waterfowl species richness at each site across all years (using the effort corrected dataset)
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

# Define the data to use & isolate required columns
WF.data.richness = WF.data.EC %>%
  select(Study.Area, Year, Month, Day, Species.Code)

# Create a presence/absence column for each new species code
WF.richness.presence = WF.data.richness %>%
  group_by(Study.Area, Year, Month, Day, Species.Code) %>%
  summarise(Present = 1, .groups = "rowwise") %>%
  spread(Species.Code, Present, fill = 0)  # Converts the data into wide format

# Generate full grid of locations and years
WF.richness.expand = expand.grid(
  Study.Area = unique(WF.richness.presence$Study.Area), 
  Year = WF.all.years)

# Identify years with no survey data per location
WF.richness = WF.richness.expand %>%
  left_join(WF.richness.presence %>% select(Study.Area, Year, everything()), 
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
    )
  ) %>%
  mutate(Survey.Status = factor(Survey.Status, levels = c("No Survey", "Survey")))

# Reshape the data back into long format for ggplot
WF.richness.long = WF.richness %>%
  pivot_longer(
    cols = -c(Study.Area, Year, Month, Day, is.surveyed, Survey.Status),
    names_to = "Species.Code",
    values_to = "Presence") %>%
  filter(Species.Code != "NULL")%>%
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
                                     "B-GBHE", "B-SACR", "NULL")))

# Define base data frame for plotting
WF.richness.plot = WF.richness.long %>%
  select(-Year, -Month, -Day, -is.surveyed, -Survey.Status) %>%
  group_by(Study.Area, Species.Code) %>% 
  summarise(across(c(Presence), sum)) %>%
  mutate(`Combined.Pres` = case_when(
    Presence > 0 ~ 1,
    Presence == 0 ~ 0,
    TRUE ~ NA
  )) %>%
  mutate(
    `Pres.Code` = case_when(
      Combined.Pres == 1 ~ paste(Species.Code, "Present"),
      Combined.Pres == 0 ~ "Not Detected",
      TRUE ~ NA_character_))

# Create expanded data frame
WF.richness.plot.expand = expand.grid(
  Study.Area = unique(WF.richness.plot$Study.Area),
  Species.Code = unique(WF.richness.plot$Species.Code)) 

# Create tile data frame to be plotted
WF.richness.plot.tile = WF.richness.plot.expand %>%
  left_join(WF.richness.plot %>% select(Study.Area, Species.Code, Combined.Pres, Pres.Code),
            by = c("Study.Area", "Species.Code"))


# ===========================
# Set up for plotting
# ===========================

# Define colours
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
                             Pres.Codes = c("B-BAEA Present", "B-OSPR Present", 
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
                            Pres.Codes = c("B-BAEA Present", "B-OSPR Present", 
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
                            Pres.Codes = c("B-BAEA Present", "B-OSPR Present", 
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


# Define unique species
unique.sp = unique(WF.richness.plot.tile$Species.Code)
unique.sp.pres = unique(WF.richness.plot.tile$Pres.Code)

# Determine number of species observed (= number of rows) for use with horizontal line positions
n.horizontal.lines = n_distinct(WF.richness.plot.tile$Species.Code)

# Define the colour codes that are constant
WF.constant.colours = c("No Survey", "Not Detected", "Survey")
WF.constant.labels = c("No Survey")
WF.constant.shapes = c("No Survey", "Not Detected")

# Determine the unique species codes being plotted
unique.cols = c(unique.sp.pres, WF.constant.colours)
unique.shapes = c(unique.sp.pres, WF.constant.shapes)
unique.labs = c(unique.sp.pres, WF.constant.labels)

# Determine the colour codes required for the unique species codes
WF.all.colours = WF.colours.bank %>%
  filter(Pres.Codes %in% unique.cols)

# Define the colours to be used in the plot
WF.colours = setNames(WF.all.colours$Colour.Codes, WF.all.colours$Pres.Codes)

# Determine the shape codes required for the unique species codes
WF.all.shapes = WF.shapes.bank %>%
  filter(Pres.Codes %in% unique.shapes) 

# Define the shapes to be used in the plot
WF.shapes = setNames(WF.all.shapes$Shape.Codes, WF.all.shapes$Pres.Codes)

# Define colours for shapes
white = "white"
black = "black"

# Define function that does the opposite of "%in%"
`%ni%` = Negate(`%in%`)

# Define codes with white vs. black shapes
WF.white.shapes = which(WF.richness.plot.tile$Pres.Code %in% c("B-BAEA Present", "B-OSPR Present",
                                                         "B-NOWA Present", "B-RWBL Present", 
                                                         "B-TRSW Present","G-GOEY Present", 
                                                         "B-BUFF Present"))
WF.black.shapes = which(WF.richness.plot.tile$Pres.Code %ni% c("B-BAEA Present", "B-OSPR Present",
                                                         "B-NOWA Present", "B-RWBL Present", 
                                                         "B-TRSW Present","G-GOEY Present", 
                                                         "B-BUFF Present"))

# Determine shape colour on boxes
WF.all.shco = seq(1, nrow(WF.richness.plot.tile), by = 1)
for (i in WF.white.shapes){
  WF.all.shco[i] = white
}
for (i in WF.black.shapes){
  WF.all.shco[i] = black
}

# Determine the species labels required for the unique species codes
WF.all.labs = WF.labels.bank %>%
  filter(Pres.Codes %in% unique.labs)  %>%
  mutate(
    `xlab.Codes` = case_when(substr(Pres.Codes, 1, 1) == "B" ~ substr(Pres.Codes, start = 1, stop = 6),
                             substr(Pres.Codes, 1, 2) == "ID" ~ substr(Pres.Codes, start = 0, stop = nchar(Pres.Codes)-8),
                             substr(Pres.Codes, 1, 1) == "G" ~ substr(Pres.Codes, start = 0, stop = nchar(Pres.Codes)-8),
                             substr(Pres.Codes, 1, 2) == "OR" ~ substr(Pres.Codes, start = 0, stop = nchar(Pres.Codes)-8)))

# Define the species labels to be used in the plot
WF.labels = setNames(WF.all.labs$Labels, WF.all.labs$Pres.Codes)
WF.labels = WF.labels[c(n.horizontal.lines+1)]

# Define the legend limits (which entries to show in the legend)
WF.limits = c("No Survey")

# Define the x-axis labels to use in the plot (and the width/max # of characters that trigger text wrapping)
WF.xlabs = setNames(str_wrap(WF.all.labs$Labels, width = 35), WF.all.labs$xlab.Codes)

# Define file name for plot png
plot.filename = paste0("WF-EC-species-richness-plot-",
                       thedate, 
                       ".png")

# Define folder to save plot png in
plot_file = file.path(output_folder_plots, plot.filename)


# ===========================
# Plot the data
# ===========================

plot.v1 = 
  ggplot(
    WF.richness.plot.tile, 
    aes(x = reorder(Species.Code, desc(Species.Code)), y = reorder(Study.Area, desc(Study.Area)))
  ) +
  # Add species presence tiles
  geom_tile(
    aes(fill = Pres.Code),
    width = 1,
    height = 1
  ) +
  # Add horizonal border lines for each park row
  geom_hline(
    yintercept = c(seq(0.5, 18.5, by = 1)),
    color = "gray30",
    size = 0.3
  ) +
  # Add vertical border lines for each species row
  geom_vline(
    xintercept = c(seq(0.5, 34.5, by = 1)),
    color = "gray30",
    size = 0.3
  ) +
  # Add points (shapes) on top of the presence tiles
  geom_point(
    aes(shape = Pres.Code),
    size = 3.5,
    colour = WF.all.shco,
    fill = WF.all.shco
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
    title = paste0("Waterfowl Species Richness (2013-2024)"),
    x = "Waterfowl Species & Species Groups",
    y = "Study Site"
  ) +
  # Define x-axis scale (shows all years)
  scale_x_discrete(
    labels = WF.xlabs,
    expand = c(0,0)    
  ) +
  # Define y-axis scale (list of observed species)
  scale_y_discrete(
    expand = c(0,0)      
  ) +
  # More plot aesthetics:
  theme(
    legend.position = "bottom",
    axis.ticks.x.bottom = element_line(colour = "black", linewidth = 0.3),
    axis.ticks.length.x.bottom = unit(5, 'pt'),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.ticks.y.left = element_line(colour = "black", linewidth = 0.3),
    axis.ticks.length.y.left = unit(5, 'pt'),
    axis.title.y = element_text(margin = unit(c(0,7.5,0,0), "pt")),
    panel.background = element_rect(fill = "gray95"),
    panel.grid.major.x = element_line(color = "transparent", linewidth = 0),
    panel.grid.minor.x = element_line(color = "gray70", linewidth =  0.3),
    panel.grid.major.y = element_line(color = "transparent", linewidth = 0),
    plot.margin = unit(c(7.5,5.5,5.5,7.5), "pt"),
    panel.border = element_rect(colour = "gray70", fill=NA, linewidth=0.6)
  )

# Additional legend text to explain the *, ** & *** species
legend_annotation = grid::textGrob("   * Regionally Significant\n  ** Provincially Blue-listed\n      & Species of Special Concern\n*** Provincially Red-listed",
                                   x = 0.005, y = 0.045, 
                                   hjust = 0, vjust = 0.5, 
                                   gp = grid::gpar(fontsize = 8))
annotation_layout = c(area(t = 1, b = 1, l = 1, r = 1),
                      area(t = 1, b = 1, l = 1, r = 1))
plot = plot.v1 + wrap_elements(full = legend_annotation) + plot_layout(design = annotation_layout)

# Save the plot as a PNG
ggsave(plot_file, 
       plot = plot, 
       width = 12, 
       height = 8.5, 
       dpi = 300, 
       bg = "white")
