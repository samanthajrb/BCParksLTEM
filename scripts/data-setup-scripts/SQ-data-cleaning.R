# ===========================
# ===========================
# This code can be used to:
#     - Remove columns with only NA values (empty columns)
#     - Create unique data frames for subsets of the entire dataset
# ===========================
# ===========================
# Run this file BEFORE any of the SQUIRREL protocol plot scripts
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
data = read.csv(input_csv)

# Remove empty & unnecessary columns
SQ.data = data %>% 
  select(-c(Project.ID, Survey.ID,
            Study.Area.ID, Study.Area.Visit.ID, 
            Design.Component.ID, Design.Component.Label, 
            Design.Component.Visit.ID, Nights.Deployed, 
            Design.Component.Visit.Note, Taxonomic.Unit.ID, 
            Animal.ID, Survey.Observation.ID, 
            Temporary.Animal.ID, Life.Stage.Code, 
            Sex.Code, Activity.Code, 
            Feature.Type, Feature.Label, 
            Feature.Count, Sign.or.Sample.Age.Code, 
            Group.Label, Adult.Males, 
            Adult.Females, Juvenile.Males, 
            Juvenile.Females, Juveniles...Unclassified.Sex, 
            Males...Unclassified.Life.Stage, Females...Unclassified.Life.Stage, 
            Unclassified.Life.Stage.and.Sex, Eggs, 
            Egg.Masses, Larvae, 
            Pupae, Hatchlings, 
            Fledglings)) %>%
  # Remove unnecessary Species.Codes (as per Appendix A in the Final Report)
  filter(Species.Code != "M-LYCA") %>%
  # Make year numeric, if not already
  mutate(Year = as.numeric(Year)) %>%
  # Rename 'Detection.Type.Code' and 'Sign.Type.Code' columns for brevity
  rename(Det.Type = Detection.Type.Code,
         Sign.Type = Sign.Type.Code) %>%
  # Clean up the Detection Type Codes for consistency
  mutate(
    Det.Type = case_when(
      Det.Type == "VI" ~ "VI",
      Det.Type == "Vi" ~ "VI",
      Det.Type == "CA" ~ "CA",
      Det.Type == "Ca" ~ "CA",
      Det.Type == "SI" ~ "SI",
      Species.Code == "NULL" ~ "NODET",
      TRUE ~ "UNK"  # If none of the conditions match, fill with "Unknown"
    )) %>%
  mutate(Det.Type = factor(Det.Type, levels = c("VI", "CA", "SI", "NODET","UNK"))) %>%
  # Clean up Sign Type Codes where possible
  mutate(
    Sign.Type = case_when(
      Sign.Type == "FD" ~ "FD",
      Det.Type != "SI" ~ "NOSIGN",
      Sign.Type == ""  ~ "UNK",
      TRUE ~ "UNK"  # If none of the conditions match, fill with "Unknown"
    )) %>%
  # Make new species group for parks where Red and Douglas squirrel ranges overlap
  mutate(Species.Code = ifelse(Study.Area %in% c("E.C. Manning Park", "Skagit Valley Park"),
                               "DOUG-OR-RED", 
                               Species.Code)) %>%
  # Factor for legend order
  mutate(Species.Code = factor(Species.Code,
                               levels = c("M-SCCA","M-TADO","M-TAHU",
                                          "DOUG-OR-RED", "NULL"))) %>%
  # Manually edit 2 start time entries from Eskers Park
  mutate(Visit.Start.Time = case_when(Study.Area == "Eskers Park" & Visit.Start.Date == "2015-08-26T00:00:00Z" ~ 1500,
                                      TRUE ~ Visit.Start.Time))


# ===========================
# Create alternate data frames for various plotting purposes
# ===========================

# Create a data frame for use with Sign Type & Det Type plots
SQ.data.STDT = SQ.data %>%
    # Create column to display the combination of Det Type & Sign Type for each row
    mutate(
      Det.Sign = case_when(
        Sign.Type == "FD" ~ paste0(as.character(Det.Type), "-", as.character(Sign.Type)),
        Sign.Type == "NOSIGN" ~ paste0(as.character(Det.Type), "-", as.character(Sign.Type)),
        Sign.Type == "UNK"  ~ paste0(as.character(Det.Type), "-", as.character(Sign.Type)),
        TRUE ~ "UNK"  # If none of the conditions match, fill with "Unknown"
      ))


# Create a data frame for use with the protocol compliance plot
SQ.data.comp = SQ.data %>%
  # Create columns for survey month and day
  mutate(`Month` = case_when(
    substr(Visit.Start.Date, 6, 7) == "01" ~ "January",
    substr(Visit.Start.Date, 6, 7) == "02" ~ "February",
    substr(Visit.Start.Date, 6, 7) == "03" ~ "March",
    substr(Visit.Start.Date, 6, 7) == "04" ~ "April",
    substr(Visit.Start.Date, 6, 7) == "05" ~ "May",
    substr(Visit.Start.Date, 6, 7) == "06" ~ "June",
    substr(Visit.Start.Date, 6, 7) == "07" ~ "July",
    substr(Visit.Start.Date, 6, 7) == "08" ~ "August",
    substr(Visit.Start.Date, 6, 7) == "09" ~ "September",
    substr(Visit.Start.Date, 6, 7) == "10" ~ "October",
    substr(Visit.Start.Date, 6, 7) == "11" ~ "November",
    substr(Visit.Start.Date, 6, 7) == "12" ~ "December")) %>%
  mutate(Month = factor(Month, levels = c("January", "February",
                                          "March", "April",
                                          "May", "June", "July", "August",
                                          "September", "October",
                                          "November", "December"))) %>%
  mutate(`Day` = substr(Visit.Start.Date, 9, 10))


# Create a data frame for use with the time of day plots
SQ.data.TOD = SQ.data %>%
  rowwise() %>%
  # Rename some Detection Type Codes
  mutate(
    Det.Type = case_when(
      Det.Type == "VI" ~ "VI",
      Det.Type == "CA" ~ "CA",
      Det.Type == "SI" ~ "SI",
      Species.Code == "NULL" ~ "NODET",
      Species.Code != "NULL" && Count == 0 ~ "NODET",
      Count == 1 ~ "BLANK",
      TRUE ~ "UNK"  # If none of the conditions match, fill with "Unknown"
    )) %>%
  # Remove rows with 'SI' sign type
  filter(!(Det.Type %in% c("SI"))) %>%
  # Make factor for legend
  mutate(Det.Type = factor(Det.Type, levels = c("VI", "CA", "NODET", "BLANK", "UNK"))) %>%
  # Create columns for survey month and day
  mutate(`Day` = substr(Visit.Start.Date, 9, 10)) %>%
  mutate(`Month` = substr(Visit.Start.Date, 6, 7)) %>%
  # Rearrange order of columns
  relocate(Study.Area, Year, Month, Day) %>%
  # Rename & reformat column for Survey Date
  mutate(Visit.Start.Date = substr(Visit.Start.Date, 1, 10)) %>%
  # Remove rows with 0' in the Visit.Start.Time column (no Visit.Start.Time entered)
  filter(!(Visit.Start.Time %in% c(0))) %>%
  # Convert the integer time format (e.g., 1300 -> "13:00") to a string
  mutate(Visit.Start.Time = sprintf("%04d", Visit.Start.Time)) %>%
  # Extract the hour from the Visit.Start.Time
  mutate(`Hour` = as.numeric(substr(Visit.Start.Time, 1, 2))) %>%
  # Remove rows with 'BLANK' Det.Type (no Det.Type entered but a detection was recorded)
  filter(!(Det.Type %in% c("BLANK")))


# Create a data frame of all Species.Code rows EXCEPT "NULL" Species.Code rows
SQ.data.NO.nulls = SQ.data %>%
  filter(!(Species.Code %in% c("NULL")))

