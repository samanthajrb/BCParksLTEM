# ===========================
# ===========================
# This code can be used to:
#     - Remove columns with only NA values (empty columns)
#     - Create unique data frames for subsets of the entire dataset
# ===========================
# ===========================
# Run this file BEFORE any of the WATERFOWL protocol plot scripts
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


# ===========================
# Settings based on the user-specific changes made in 'config_file.R'
# ===========================

source(config_file)  # Load user-specific settings
setwd(working_dir)  # Set working directory


# ===========================
# Remove unnecessary columns and rows
# ===========================

# Read input csv file
WF.data.raw = read.csv(input_csv)

# Remove empty & unnecessary columns
WF.data = WF.data.raw %>% 
  select(-c(Project.ID, Survey.ID,
            Study.Area.ID, Study.Area.Visit.ID, 
            Design.Component.ID, Design.Component.Label, 
            Design.Component.Visit.ID, Nights.Deployed, 
            Design.Component.Visit.Note, Taxonomic.Unit.ID, 
            Survey.Observation.ID, Spatial.Accuracy..m....SMT,
            Detection.Direction..deg., Detection.Distance..m., 
            Temporary.Animal.ID, Sex.Code, 
            Feature.Label, Feature.Count, 
            Sign.or.Sample.Age.Code, Sign.Count, 
            Group.Label, Juvenile.Males, 
            Juvenile.Females, Egg.Masses, 
            Larvae, Pupae)) %>%
  # Rename 'Detection.Type.Code' column for brevity
  rename(Det.Type = Detection.Type.Code) %>%
  # Clean up the Detection Type Codes for consistency
  mutate(
    Det.Type = case_when(
      Det.Type == "VI" ~ paste0(as.character(Det.Type)),
      Det.Type == "CA" ~ paste0(as.character(Det.Type)),
      TRUE ~ "UNK"  # If none of the conditions match, fill with "Unknown"
    )) %>%
  # Remove 'Shadow Lake' single entry row from the 'Wells Gray Park' site & rename other entries
  filter(Study.Area != "Wells Gray Park - Shadow Lake") %>%
  mutate(
    `Study.Area` = case_when(
      Study.Area == "Wells Gray Park - Alice Lake" ~ "Wells Gray Park",
      TRUE ~ as.character(Study.Area))) %>%
  # Remove unnecessary Species.Codes (as per Appendix A in the Final Report)
  filter(!(Species.Code %in% c("AVES", "PASSERIFORMES", "CHARADRIIDAE",
                               "BOMBYCILLIDAE", "LARUS", "B-ALFL",
                               "B-AMCR", "B-AMRO", "B-ANHU",
                               "B-BCCH", "B-CEWA", "B-CORA",
                               "B-EAKI", "B-GRCA", "B-HOFI",
                               "B-KILL", "B-LISP", "B-OCWA",
                               "B-OSFL", "B-PIGR", "B-REVI",
                               "B-ROPI", "B-SAVS", "B-SOGR",
                               "B-TOSO", "B-TUVU",
                               "B-VATH", "B-WAVI", "B-WIWA",
                               "B-WIWR", "B-WWPE", "B-YEWA"))) %>%
  # Re-name the Species.Codes for grouped species (those difficult to distinguish for ID) & higher taxonomic level groups
  mutate(
    Species.Code = case_when(Species.Code == "B-COME" ~ "G-MERG", # Grouped mergansers
                             Species.Code == "B-RBME" ~ "G-MERG", # Grouped mergansers
                             Species.Code == "B-COGO" ~ "G-GOEY", # Grouped goldeneyes
                             Species.Code == "B-BAGO" ~ "G-GOEY", # Grouped goldeneyes
                             Species.Code == "B-GRSC" ~ "G-SCAP", # Grouped scaups
                             Species.Code == "B-LESC" ~ "G-SCAP", # Grouped scaups
                             Species.Code == "ANSERIFORMES" ~ "OR-ANSERIFORMES", # Order
                             Species.Code == "ANATIDAE" ~ "FA-ANATIDAE", # Family
                             Species.Code == "AYTHYA" ~ "GE-AYTHYA", # Genus
                             Species.Code == "BUCEPHALA" ~ "GE-BUCEPHALA", # Genus
                             Species.Code == "MERGUS" ~ "GE-MERGUS", # Genus
                             Species.Code == "PODICIPEDIDAE" ~ "OR-PODICIPEDIDAE", # Order
                             TRUE ~ as.character(Species.Code))) %>%
  # Create column for month
  mutate(`Day` = substr(Visit.Start.Date, 9, 10)) %>%
  # Create column for day
  mutate(`Month` = substr(Visit.Start.Date, 6, 7)) %>%
  # Rearrange order of columns
  relocate(Study.Area, Year, Month, Day) %>%
  # Make date-related columns numeric, if not already
  mutate(Month = as.numeric(Month),
         Year = as.numeric(Year),
         Day = as.numeric(Day)) %>%
  # Rename & reformat column for Survey Date
  mutate(Visit.Start.Date = substr(Visit.Start.Date, 1, 10)) %>%
  # Create column for plot width value (individual park)
  mutate(`Plot.Width` = 8) %>%
  # Create column for plot height value (based on Study.Area for individual park plots)
  mutate(
    `Plot.Height` = case_when(Study.Area == "Alice Lake Park" ~ 2.5,
                              Study.Area == "Big Bar Lake Park" ~ 5,                                           
                              Study.Area == "Bonaparte Park" ~ 2,                             
                              Study.Area == "Bowron Lakes Park" ~ 3.5,                                           
                              Study.Area == "Brandywine Falls Park" ~ 5,                      
                              Study.Area == "Burges James Gadsden Park" ~ 7,                           
                              Study.Area == "Call Lake Park" ~ 6,                             
                              Study.Area == "Champion Lakes Park" ~ 4,                                     
                              Study.Area == "Hai Lake-Mount Herman Park" ~ 3.5,                 
                              Study.Area == "Lac du Bois Protected Area" ~ 3.5,                       
                              Study.Area == "Mahoney Lake Ecological Reserve" ~ 6,           
                              Study.Area == "Misty Lake Ecological Reserve" ~ 4,                  
                              Study.Area == "Naikoon Park" ~ 2,                               
                              Study.Area == "Nazko Lake Park" ~ 5,                                             
                              Study.Area == "Sutton Pass Ecological Reserve" ~ 2,             
                              Study.Area == "Wells Gray Park" ~ 2.75,                               
                              Study.Area == "White Lake Park" ~ 3.5,                            
                              Study.Area == "Yellow Point Bog Ecological Reserve" ~ 8)) %>%
  # Create column for site region
  mutate(
    `Site.Region` = case_when(Study.Area %in% c("Burges James Gadsden Park", "Champion Lakes Park", "Mahoney Lake Ecological Reserve") ~ "Kootenay Okanagan",
                              Study.Area %in% c("Call Lake Park", "Hai Lake-Mount Herman Park") ~ "North Coast Skeena",
                              Study.Area %in% c("Alice Lake Park", "Brandywine Falls Park") ~ "South Coast",
                              Study.Area %in% c("Big Bar Lake Park", "Bonaparte Park", "Bowron Lakes Park", "Lac du Bois Protected Area", "Nazko Lake Park", "Wells Gray Park", "White Lake Park") ~ "Thompson Cariboo",
                              Study.Area %in% c("Misty Lake Ecological Reserve", "Naikoon Park", "Sutton Pass Ecological Reserve", "Yellow Point Bog Ecological Reserve") ~ "West Coast")) %>%
  # Create column for regional plot width value
  mutate(
    `Reg.Plot.Width` = case_when(Site.Region == "Kootenay Okanagan" ~ 14,
                                 Site.Region == "North Coast Skeena" ~ 14,
                                 Site.Region == "South Coast" ~ 13,
                                 Site.Region == "Thompson Cariboo" ~ 15,
                                 Site.Region == "West Coast" ~ 13)) %>%
  # Create columns for regional plot height value
  mutate(
    `Reg.Plot.Height` = case_when(Site.Region == "Kootenay Okanagan" ~ 7,
                                 Site.Region == "North Coast Skeena" ~ 5,
                                 Site.Region == "South Coast" ~ 4,
                                 Site.Region == "Thompson Cariboo" ~ 9.5,
                                 Site.Region == "West Coast" ~ 13))


# ===========================
# Create alternate data frames for various plotting purposes
# ===========================

# Create data frame of ONLY higher level ID Species.Code rows
WF.data.highID = WF.data %>%
  filter(Species.Code %in% c("OR-ANSERIFORMES", "FA-ANATIDAE", "GE-AYTHYA", 
                             "GE-BUCEPHALA", "GE-MERGUS", "OR-PODICIPEDIDAE",
                             "G-MERG", "G-GOEY", "G-SCAP"))


# Create a data frame of all Species.Code rows EXCEPT higher level ID rows
WF.data.speciesID = WF.data %>%
  filter(!(Species.Code %in% c("OR-ANSERIFORMES", "FA-ANATIDAE", "GE-AYTHYA", 
                               "GE-BUCEPHALA", "GE-MERGUS", "OR-PODICIPEDIDAE",
                               "G-MERG", "G-GOEY", "G-SCAP")))


# Create a data frame of all Species.Code rows EXCEPT "NULL" Species.Code rows
WF.data.NO.nulls = WF.data %>%
  filter(!(Species.Code %in% c("NULL")))


# Create a data frame for ONLY the target park (when plotting 1 park at a time as per config_file)
park.plot.data = WF.data %>%
  filter(Study.Area == study.area.list[park.to.plot])


# Create a data frame for ONLY the target park region (as per config_file)
region.plot.data = WF.data %>%
  filter(Site.Region == region.group.list[region.to.plot])


# Create data frame to recreate the original effort corrected dataset used in report plots
WF.data.EC = read.csv(original_EC_csv) 

