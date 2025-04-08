# ===========================
# ===========================
# This code was used to:
#     - Create the 'effort corrected' dataframe that included one randomly selected survey per site per year (with preference for surveys from May, June or July).
#         - Years with no surveys conducted in the preferred months were manually selected based on the survey time of other years at the same site.
#         - As this process involves random selection of surveys, this code is unlikely to produce the same data frame that was created and plotted in the report.
#             - For the exact data frame created and plotted for the report, please see the 'WF-data-cleaning.R' file where the original randomized version has been specifically recreated.
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
WF.EC.grouped.data = WF.data %>%
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

# Favour survey selection to May, June, July over other months
WF.EC.MJJ = WF.EC.grouped.data %>%
  filter(Month %in% c(5:7)) %>%
  # Get distinct study area and year combinations
  distinct(Study.Area, Year) %>%
  # Create column for rows where survey was completed in May, June or July
  mutate('MJJ' = TRUE)

# Filter out surveys in other months if there's at least 1 survey in May, June or July
WF.EC.filtered.data = WF.EC.grouped.data %>%
  left_join(WF.EC.MJJ, by = c("Study.Area", "Year")) %>%
  # If no survey in May, June, or July, keep all surveys
  mutate(Month.to.Keep = if_else(is.na(MJJ), TRUE, Month %in% c(5:7))) %>%
  filter(Month.to.Keep) %>%
  # Drop helper columns
  select(-c(MJJ, Month.to.Keep)) 

# Randomly select survey from rows with May, June or July survey date
WF.EC.random.surveys = WF.EC.filtered.data %>%
  filter(Month %in% c(5:7)) %>%
  group_by(Study.Area, Year) %>%
  slice_sample(n = 1) %>% # Randomly select 1 survey per year
  ungroup()

# Keep the rest of the data (outside of May, June, July)
WF.EC.remaining.months = WF.EC.filtered.data %>%
  filter(!(Month %in% c(5:7)))

# Combine the randomly selected surveys with the rest of the data to create the final, effort corrected data frame
WF.EC.final.collapse = WF.EC.random.surveys %>%
  bind_rows(WF.EC.remaining.months) %>%
  # Manually filter surveys where multiple surveys occurred outside the preferred (May, June, July) months
  filter(!(Study.Area == "Call Lake Park" & Year == 2022 & Month == 9), # Removing September survey, choosing August survey
         !(Study.Area == "Misty Lake Ecological Reserve" & Year %in% c(2015, 2018) & Month == 4), # Removing April survey, choosing March survey
         !(Study.Area == "Misty Lake Ecological Reserve" & Month %in% c(1, 11, 12)), # Removing all January, November & December surveys
         !(Study.Area == "Yellow Point Bog Ecological Reserve" & Month %in% c(1, 2, 8, 9, 10, 11, 12)), # Removing all January, February, August, September, October, November & December surveys
         !(Study.Area == "Yellow Point Bog Ecological Reserve" & Year == 2016 & Month == 3) # Removing March survey, choosing April survey
  ) %>%
  arrange(Study.Area, Year, Month, Day)

# Extract unique combinations of Study.Area and Visit.Start.Date from the final, effort corrected data frame
WF.EC.final.unique = WF.EC.final.collapse %>%
  select(Study.Area, Visit.Start.Date) %>%
  distinct()

# Filter the original data frame (with all columns) to only include the unique combinations in the final, effort corrected data frame
WF.EC.final.data = WF.data %>%
  semi_join(WF.EC.final.unique, by = c("Study.Area", "Visit.Start.Date"))

# Write the effort corrected data to a .csv file
write.csv(WF.EC.final.data, output_csv_newEC, row.names = FALSE)

# Confirmation message
message(paste("Data successfully written to", output_csv_newEC))
