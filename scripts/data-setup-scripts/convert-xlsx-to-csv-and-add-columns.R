# ===========================
# ===========================
# This code can be used to:
#     - Convert .xlsx file to .csv file format
#     - Add 'Study.Area' column to the new .csv file
#     - Add 'Year' column to the new .csv file
# ===========================
# ===========================

# -------------------------------------------------------------------------



# ===========================
# Install (if necessary) & load required packages
# ===========================
# Install the required packages if not already installed & load all required packages
if (!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}
if (!require(data.table)) {
  install.packages("data.table")
  library(data.table)
}
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
# Convert .xlsx file to .csv
# ===========================

# Ensure the output_folder exists, create one if it doesn't
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# Read the .xlsx file
data = read_excel(input_xlsx, sheet = sheetname)

# Write to .csv
fwrite(data, output_csv, row.names = FALSE, col.names = TRUE, quote = TRUE)

# Confirmation message
message(paste("Data successfully written to", output_csv))


# ===========================
# Add park name ("Study.Area") column to csv created above
# ===========================

# Define input file
input_csv = output_csv

# Define the function to add the new park name column
add_column = function(data, column_name) {
  
  # Ensure the column is numeric
  data[[column_name]] = as.numeric(data[[column_name]])
  
  # Print unique values in the column for debugging
  print(unique(data[[column_name]]))  # Check the unique values of the column
  
  # Add the new column "Study.Area" based on the value of another column
  data = data %>%
    mutate(
      `Study.Area` = case_when(
        # Amphibian Study.Area codes:
        data[[column_name]] == 32522 ~ "Alice Lake Park",
        data[[column_name]] == 32523 ~ "Big Bar Lake Park",
        data[[column_name]] == 43341 ~ "Bonaparte Park",
        data[[column_name]] == 32542 ~ "Bowron Lakes Park",
        data[[column_name]] == 32524 ~ "Brandywine Falls Park",
        data[[column_name]] == 32464 ~ "Burges James Gadsden Park",
        data[[column_name]] == 32461 ~ "Call Lake Park",
        data[[column_name]] == 32465 ~ "Champion Lakes Park",
        data[[column_name]] == 32466 ~ "Hai Lake-Mount Herman Park",
        data[[column_name]] == 32521 ~ "Lac du Bois Protected Area",
        data[[column_name]] == 32467 ~ "Mahoney Lake Ecological Reserve",
        data[[column_name]] == 32460 ~ "Misty Lake Ecological Reserve",
        data[[column_name]] == 32462 ~ "Naikoon Park",
        data[[column_name]] == 32463 ~ "Nazko Lake Park",
        data[[column_name]] == 32468 ~ "Sutton Pass Ecological Reserve",
        data[[column_name]] == 36401 ~ "Wells Gray Park",
        data[[column_name]] == 36400 ~ "White Lake Park",
        data[[column_name]] == 36840 ~ "Yellow Point Bog Ecological Reserve",
        # Waterfowl Study.Area codes:
        data[[column_name]] == 32841 ~ "Alice Lake Park",
        data[[column_name]] == 32842 ~ "Big Bar Lake Park",
        data[[column_name]] == 44980 ~ "Bonaparte Park",
        data[[column_name]] == 32843 ~ "Bowron Lakes Park",
        data[[column_name]] == 32844 ~ "Brandywine Falls Park",
        data[[column_name]] == 32845 ~ "Burges James Gadsden Park",
        data[[column_name]] == 32846 ~ "Call Lake Park",
        data[[column_name]] == 32847 ~ "Champion Lakes Park",
        data[[column_name]] == 32848 ~ "Hai Lake-Mount Herman Park",
        data[[column_name]] == 32840 ~ "Lac du Bois Protected Area",
        data[[column_name]] == 32849 ~ "Mahoney Lake Ecological Reserve",
        data[[column_name]] == 32850 ~ "Misty Lake Ecological Reserve",
        data[[column_name]] == 32880 ~ "Naikoon Park",
        data[[column_name]] == 32881 ~ "Nazko Lake Park",
        data[[column_name]] == 43321 ~ "Sutton Pass Ecological Reserve",
        data[[column_name]] == 36961 ~ "Wells Gray Park - Alice Lake",
        data[[column_name]] == 41760 ~ "Wells Gray Park - Shadow Lake",
        data[[column_name]] == 36960 ~ "White Lake Park",
        data[[column_name]] == 30494 ~ "Yellow Point Bog Ecological Reserve",
        # Squirrel Study.Area codes:
        data[[column_name]] == 36342 ~ "Atlin School",
        data[[column_name]] == 32700 ~ "Beatton Park",
        data[[column_name]] == 32701 ~ "Bowser Ecological Reserve",
        data[[column_name]] == 32721 ~ "Call Lake Park",
        data[[column_name]] == 32702 ~ "Crooked River Park",
        data[[column_name]] == 45620 ~ "Dune Za Keyih Park",
        data[[column_name]] == 32741 ~ "E.C. Manning Park",
        data[[column_name]] == 41601 ~ "Elk Falls Park",
        data[[column_name]] == 32703 ~ "Eskers Park",
        data[[column_name]] == 32722 ~ "Goldstream Park",
        data[[column_name]] == 32720 ~ "John Dean Park",
        data[[column_name]] == 32742 ~ "Kinaskin Lake Park",
        data[[column_name]] == 32704 ~ "Lakelse Lake Park",
        data[[column_name]] == 32740 ~ "Liard River Corridor Park and Protected Area",
        data[[column_name]] == 36341 ~ "Mount Robson Park",
        data[[column_name]] == 36340 ~ "Mount Seymour Park",
        data[[column_name]] == 32705 ~ "Muncho Lake Park",
        data[[column_name]] == 32780 ~ "Okanagan Mountain Park",
        data[[column_name]] == 32783 ~ "Purcell Wilderness Conservancy Park and Protected Area",
        data[[column_name]] == 43540 ~ "Sargeant Bay Park",
        data[[column_name]] == 41640 ~ "Schoen Lake Park",
        data[[column_name]] == 32900 ~ "Skagit Valley Park",
        data[[column_name]] == 32781 ~ "Strathcona Park",
        data[[column_name]] == 46560 ~ "Tarahne Park",
        data[[column_name]] == 32782 ~ "West Arm Park",
        data[[column_name]] == 32707 ~ "Yaaguun Gandlaay Conservancy",
        TRUE ~ "Unknown"  # If none of the conditions match, fill with "Unknown"
      )
    )
  # Identify the position of the "Study.Area.ID" column
  StudyAreaID_position = which(names(data) == "Study.Area.ID")
  
  # Move "Study.Area" right after the "Study.Area.ID" column
  data = data %>%
    select(
      c(1:StudyAreaID_position, 
        `Study.Area`, 
        (StudyAreaID_position+1):ncol(data))
    )
  
  # Return the modified data with the new column
  return(data)
}

# Assuming your data is stored in 'data'
data = read.csv(input_csv)

# Add the "Study.Area" column based on the values in 'column_name'
data = add_column(data, "Study.Area.ID")

# Write the updated data back to a CSV file
write.csv(data, output_csv, row.names = FALSE)

# Confirmation message
message(paste("Data successfully written to", output_csv))


# ===========================
# Add year ("Year") column to csv created above
# ===========================

# Read csv file
data = read.csv(input_csv)

# Define function to add new column with the extracted year
add_column = function(data, column_name) {
  data = data %>%
    mutate(
      `Year` = substr(Visit.Start.Date, 1, 4)
    )
  # Move column to desired position
  # Identify the position of the "Design.Component.Visit.ID" column
  DCL_position = which(names(data) == "Design.Component.Visit.ID")
  # Move "Year" right after the "Design.Component.Visit.ID" column
  data = data %>%
    select(
      c(1:DCL_position, `Year`, (DCL_position+1):ncol(data))
    )
  # Return the modified data with the new column
  return(data)
}

# Call function
data = add_column(data, "Year")

# Write the updated data back to a CSV file
write.csv(data, output_csv, row.names = FALSE)

# Confirmation message
message(paste("Data successfully written to", output_csv))

