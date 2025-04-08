# ===========================
# ===========================
# This code can be used to:
#     - Remove columns with only NA values (empty columns)
#     - Filter out the unnecessary Species.Code observation entries ("REPTILIA", "THAMNOPHIS", and "ODONATA")
#     - Create unique data frames for subsets of the entire dataset
# ===========================
# ===========================
# Run this file BEFORE any of the AMPHIBIAN protocol plot scripts
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
if (!require(lubridate)) {
  install.packages("lubridate")
  library(lubridate)
}


# ===========================
# Settings based on the user-specific changes made in 'config_file.R'
# ===========================

source(config_file)  # Load user-specific settings
setwd(working_dir)  # Set working directory


# ===========================
# Remove unnecessary columns and rows
# ===========================

# Read input csv file
AMPH.data.raw = read.csv(input_csv)

# Remove empty & unnecessary columns
AMPH.data = AMPH.data.raw %>% 
  select(-c(Project.ID, Survey.ID,
            Study.Area.ID, Study.Area.Visit.ID, 
            Design.Component.ID, Design.Component.Label, 
            Design.Component.Visit.ID, Nights.Deployed, 
            Design.Component.Visit.Note, Taxonomic.Unit.ID, 
            Animal.ID, Survey.Observation.ID, 
            Detection.Direction..deg., Detection.Distance..m., 
            Temporary.Animal.ID, Sex.Code, 
            Feature.Label, Feature.Count, 
            Sign.or.Sample.Age.Code, Group.Label, 
            Juvenile.Males, Juvenile.Females, 
            Juveniles...Unclassified.Sex, Males...Unclassified.Life.Stage, 
            Females...Unclassified.Life.Stage, Unclassified.Life.Stage.and.Sex,
            Pupae, Hatchlings,
            Fledglings)) %>%
  # Rename 'Detection.Type.Code' column for brevity
  rename(Det.Type = Detection.Type.Code) %>%
  # Remove unnecessary Species.Code rows
  filter(!(Species.Code %in% c("REPTILIA", "THAMNOPHIS", "ODONATA"))) %>%
  # Create column for Month
  mutate(`Month` = substr(Visit.Start.Date, 6, 7)) %>%
  # Create column for Day
  mutate(`Day` = substr(Visit.Start.Date, 9, 10)) %>%
  # Rearrange order of columns
  relocate(Study.Area, Year, Month, Day) %>%
  # Make date-related columns numeric, if not already
  mutate(Month = as.numeric(Month),
         Year = as.numeric(Year),
         Day = as.numeric(Day)) %>%
  # Rename & reformat column for Survey Date
  mutate(Visit.Start.Date = substr(Visit.Start.Date, 1, 10))


# ===========================
# Create alternate data frames for various plotting purposes
# ===========================

# Create data frame of ONLY higher level ID Species.Code rows ("AMPHIBIA", "CAUDATA")
AMPH.data.highID = AMPH.data %>%
  filter(Species.Code %in% c("AMPHIBIA", "CAUDATA"))


# Create a data frame of all Species.Code rows EXCEPT higher level ID rows
AMPH.data.speciesID = AMPH.data %>%
  filter(!(Species.Code %in% c("AMPHIBIA", "CAUDATA")))


# Create a data frame of all Species.Code rows EXCEPT "NULL" Species.Code rows
AMPH.data.NO.nulls = AMPH.data %>%
  filter(!(Species.Code %in% c("NULL")))


# Create data frame to recreate the original effort corrected dataset used in report plots
AMPH.data.EC = read.csv(original_EC_csv)

