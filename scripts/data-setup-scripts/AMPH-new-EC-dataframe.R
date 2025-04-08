# ===========================
# ===========================
# This code was used to:
#     - Create the 'effort corrected' dataframe that included one randomly selected survey per site per year (with preference for surveys from April, May, or June).
#         - Years with no surveys conducted in the preferred months were manually selected based on the survey time of other years at the same site.
#         - As this process involves random selection of surveys, this code is unlikely to produce the same data frame that was created and plotted in the report.
#             - For the exact data frame created and plotted for the report, please see the 'AMPH-data-cleaning.R' file where the original randomized version has been specifically recreated.
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


# ===========================
# Settings based on the user-specific changes made in 'config_file.R'
# ===========================

source(config_file)  # Load user-specific settings
setwd(working_dir)  # Set working directory


# ===========================
# BELOW THIS IS HOW YOU WOULD CREATE THE DATA FRAME WITH THE RANDOMLY SELECTED SURVEYS WHEN MAKING IT FOR THE FIRST TIME
# ===========================

# Define the data to use
AMPH.EC.grouped.data = AMPH.data %>%
  select(Study.Area, Year, Month, Day, Visit.Start.Date, Det.Type, Species.Code) %>%
  # Group rows by study area & survey date
  group_by(Study.Area, Year, Month, Day, Visit.Start.Date) %>%
  # Create columns for the number of unique Species.Codes per group & a list of those unique codes
  summarise(
    species.count = n_distinct(Species.Code), 
    species.list = paste(unique(Species.Code), collapse = ", "), 
    .groups = "drop") %>%
  # Randomly select one survey per study area, year, & month combination
  group_by(Study.Area, Year, Month) %>%
  # Randomly select survey
  slice_sample(n = 1) %>%
  ungroup()

# Favour survey selection to April, May, June over other months
AMPH.EC.AMJ = AMPH.EC.grouped.data %>%
  filter(Month %in% c(4:6)) %>%
  # Get distinct study area and year combinations
  distinct(Study.Area, Year) %>%
  # Create column for rows where survey was completed in May, June or July
  mutate('AMJ' = TRUE)

# Filter out surveys in other months if there's at least 1 survey in April, May, or June
AMPH.EC.filtered.data = AMPH.EC.grouped.data %>%
  left_join(AMPH.EC.AMJ, by = c("Study.Area", "Year")) %>%
  # If no survey in April, May, or June, keep all surveys
  mutate(Month.to.Keep = if_else(is.na(AMJ), TRUE, Month %in% c(4:6))) %>%
  filter(Month.to.Keep) %>%
  # Drop helper columns
  select(-c(AMJ, Month.to.Keep)) 

# Randomly select survey from rows with April, May, or June survey date
AMPH.EC.random.surveys = AMPH.EC.filtered.data %>%
  filter(Month %in% c(4:6)) %>%
  group_by(Study.Area, Year) %>%
  slice_sample(n = 1) %>% # Randomly select 1 survey per year
  ungroup()

# Keep the rest of the data (outside of April, May, June)
AMPH.EC.remaining.months = AMPH.EC.filtered.data %>%
  filter(!(Month %in% c(4:6)))

# Combine the randomly selected surveys with the rest of the data to create the collapsed final, effort corrected data frame
AMPH.EC.final.collapse = AMPH.EC.random.surveys %>%
  bind_rows(AMPH.EC.remaining.months) %>%
  # Manually filter surveys where multiple surveys occurred outside the preferred (May, June, July) months
  filter(!(Study.Area == "Naikoon Park" & Year == 2017 & Month == 7), 
         !(Study.Area == "Wells Gray Park" & Year == 2018 & Month == 8),
         !(Study.Area == "Wells Gray Park" & Year == 2022 & Month == 8)
  ) %>%
  arrange(Study.Area, Year, Month, Day)

# Extract unique combinations of Study.Area and Visit.Start.Date from the final, effort corrected data frame
AMPH.EC.final.unique = AMPH.EC.final.collapse %>%
  select(Study.Area, Visit.Start.Date) %>%
  distinct()

# Filter the original data frame (with all columns) to only include the unique combinations in the final, effort corrected data frame
AMPH.EC.final.data = AMPH.data %>%
  semi_join(AMPH.EC.final.unique, by = c("Study.Area", "Visit.Start.Date"))

# Write the effort corrected data to a .csv file
write.csv(AMPH.EC.final.data, output_csv_newEC, row.names = FALSE)

# Confirmation message
message(paste("Data successfully written to", output_csv_newEC))
