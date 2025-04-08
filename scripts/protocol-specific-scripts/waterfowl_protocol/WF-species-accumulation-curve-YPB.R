# ===========================
# ===========================
# This code can be used to create a Waterfowl species accumulation curve for Yellow Point Bog Ecological Reserve
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
if (!require(purrr)) {
  install.packages("purrr")
  library(purrr)
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
WF.data.YPB = WF.data %>%
  # Filter out non species-level observations
  filter(startsWith(Species.Code, "B-") | Species.Code == "NULL") %>%
  # Filter only observations from Yellow Point Bog ER
  filter(Study.Area == "Yellow Point Bog Ecological Reserve") %>%
  # Remove 2 observations from 2020
  filter(!(Year == 2020)) %>%
  # Isolate required columns
  select(Study.Area, Year, Month, Visit.Start.Date, Visit.Start.Time, Species.Code) %>%
  # Convert Visit.Start.Date to Date format (if not already)
  mutate(Visit.Start.Date = as.Date(Visit.Start.Date)) %>%
  arrange(Visit.Start.Date) %>%
  # Create column for day of year
  mutate( `DOY` = yday(Visit.Start.Date)) %>%
  # Replace "NULL" entries with 0 in Species.Code column
  mutate(Species.Code = ifelse(Species.Code == "NULL", 0, Species.Code))

# Filter for unique species per survey (to avoid counting duplicates within the same survey)
WF.YPB.uniquesp = WF.data.YPB %>%
  filter(Species.Code != 0) %>%  # Exclude '0' entries
  distinct(Year, DOY, Month, Visit.Start.Date, 
           Species.Code, .keep_all = TRUE)

# Group by Year and day_of_year, and summarize the data
WF.YPB.grouped = WF.YPB.uniquesp %>%
  group_by(Year, DOY, Month, Visit.Start.Date) %>%
  summarise(
    species.count = n_distinct(Species.Code),  # Count unique species observed in each day
    species.list = paste(unique(Species.Code), collapse = ", "),  # List unique species observed
    .groups = 'drop') %>%
  # Create a list to accumulate species seen up to each survey day for each year
  group_by(Year) %>%
  # Track & count the cumulative unique species across days (only add when a new species is observed)
  mutate(
    cumulative.species.list = purrr::accumulate(species.list, ~ paste(unique(c(.x, .y)), collapse = ", ")),
    cumulative.species.count = sapply(cumulative.species.list, function(x) length(strsplit(x, ", ")[[1]]))) %>%
  ungroup()

# Separate the rows where species.count == 0 (surveys with no species detected)
WF.YPB.nonzero = WF.YPB.grouped %>%
  filter(species.count > 0) %>%
  # Calculate the cumulative species count for surveys with species detected
  group_by(Year) %>%
  mutate(
    cumulative.species.list = purrr::accumulate(species.list, ~ paste(unique(c(strsplit(.x, ", ")[[1]], strsplit(.y, ", ")[[1]])), collapse = ", ")),
    cumulative.species.count = sapply(cumulative.species.list, function(x) {
      length(unique(strsplit(x, ", ")[[1]]))
    })) %>%
  ungroup()

# Now, merge back the 0-species rows (preserve these rows, and copy the previous cumulative count)
WF.YPB.zero = WF.YPB.grouped %>%
  filter(species.count == 0) %>%
  mutate(
    cumulative.species.count = NA,  # Set initial cumulative count as NA for 0 species rows
    cumulative.species.list = ""    # Empty list for these rows
  )

# Merge the data with and without zero species rows
WF.YPB.final = bind_rows(WF.YPB.nonzero, WF.YPB.zero) %>%
  arrange(Year, DOY) %>%
  group_by(Year) %>%
  # Fill the cumulative.species.count for the 0-species rows with the last non-zero count
  mutate(
    cumulative.species.count = ifelse(is.na(cumulative.species.count), 
                                      lag(cumulative.species.count), 
                                      cumulative.species.count)) %>%
  ungroup() %>%
  mutate(cumulative.species.count = ifelse(is.na(cumulative.species.count), 0, cumulative.species.count)) %>%
  # Create the cumulative species count with correct logic
  group_by(Year) %>%
  mutate(
    # Create an accumulated list of species for each day
    cumulative.species.list = purrr::accumulate(
      species.list, 
      ~ paste(unique(c(strsplit(.x, ", ")[[1]], strsplit(.y, ", ")[[1]])), collapse = ", ")),
    # Calculate the cumulative species count by counting the unique species in the list
    cumulative.species.count = sapply(cumulative.species.list, function(x) length(unique(strsplit(x, ", ")[[1]])))) %>%
  ungroup()


# ===========================
# Set up for plotting
# ===========================

# Define file name for plot png
plot.filename = paste0("WF-species-accumulation-curve-YPB-",
                       thedate, 
                       ".png")

# Define folder to save plot png in
plot_file = file.path(output_folder_plots, plot.filename)


# ===========================
# Plot the data
# ===========================

plot = ggplot(WF.YPB.final, aes(x = DOY, y = cumulative.species.count)) + 
  geom_vline(
    xintercept = c(31.5,59.5,90.5,120.5,151.5,181.5,212.5,243.5,273.5,304.5,334.5), 
    colour = "gray70",
    size = 0.3
  ) +
  geom_hline(
    yintercept = c(2,4,6,8,10,12),
    colour = "gray70",
    size = 0.4
  ) +
  geom_hline(
    yintercept = c(1,3,5,7,9,11,13),
    colour = "gray80",
    size = 0.3
  ) +
  geom_line(
    colour = "black",
    size = 0.75
  ) + 
  geom_point(
    colour = "black",
    size = 1.5
  ) +  
  facet_wrap(~ Year, scales = "free_y") +  # Subplots by year
  scale_y_continuous(
    limits = c(0,14),
    breaks = c(seq(0, 14, by = 2)),
    expand = c(0,0)
  ) +
  scale_x_continuous(
    limits = c(0, 365),
    breaks = c(16.5,46,75.5,106,136.5,167,197.5,228.5,259,289.5,320,350), 
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    ,
    expand = c(0,0)
  ) +
  labs(
    title = paste0("Waterfowl Species Accumulation Curve at Yellow Point Bog Ecological Reserve (2013-2017, 2019, 2021-2023)"),
    x = "Month of Year",
    y = "Cumulative Number of Species Detected"
  ) +
  theme_minimal() +  # Apply minimal theme
  theme(
    axis.title.x = element_text(margin = unit(c(6.5,0,0,0), "pt")),
    axis.text.x = element_text(size = 9.5, margin = unit(c(1.5,0,0,0), "pt")),
    axis.text.y = element_text(size = 9.5),
    axis.title.y = element_text(margin = unit(c(0,6.5,0,0), "pt")),
    strip.text = element_text(size = 11, face = "bold"), 
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.grid.major.x = element_line(color = "transparent", linewidth = 0),
    panel.grid.minor.x = element_line(color = "transparent", linewidth = 0),
    panel.grid.major.y = element_line(color = "transparent", linewidth = 0),
    panel.grid.minor.y = element_line(color = "transparent", linewidth = 0),
    axis.ticks.y = element_line(color = "gray70", linewidth = 0.5),  # Adds black tick marks
    axis.ticks.length = unit(0.1, "cm"),  # Adjust the length of the tick marks
    axis.ticks.x = element_line(colour = "gray70", linewidth = 0.5),
    plot.margin = unit(c(7.5,5.5,5.5,7.5), "pt"),
    panel.spacing.x = unit(10, "pt"),
    panel.spacing.y = unit(2, "pt"),
    panel.background = element_rect(fill = "gray98"),
    panel.border = element_rect(colour = "gray70", fill=NA, linewidth=0.6)
  ) 

# Save the plot as a PNG
ggsave(plot_file, 
       plot = plot, 
       width = 11.5, 
       height = 8, 
       dpi = 300, 
       bg = "white")

