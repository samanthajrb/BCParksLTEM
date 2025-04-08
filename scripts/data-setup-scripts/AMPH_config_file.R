# ==========================
# AMPHIBIAN DATASET
# ==========================
# User-Specific Configuration
# Run this file BEFORE running other AMPHIBIAN data set up or plot scripts.
# ==========================

# replace with your input file path
config_file = "/your/file/path/to/this/file.R"   # Change to your config_file path

# Set working directory & output folder
working_dir = "/your/file/path/for/working/directory/"   # Change to your working directory folder path
output_folder = "/your/file/path/for/output/folder/"   # Change to your output folder path
output_folder_plots = "/your/file/path/for/plot/output/folder/"   # Change to your plot output folder path

# Define 'thedate' as today's date for file naming purposes
thedate = strftime(Sys.Date(),"%Y%m%d")

# Set input & output file naming & saving structure
input_xlsx = file.path(working_dir, 
                       "name-of-your-input-file.xlsx")   # Change to the name of your input .xlsx file
output_csv = file.path(output_folder, 
                       paste0("name-of-your-output-file",   # Change to the desired name of your output .csv file
                              thedate, 
                              ".csv"))


# Define file path to the original randomized effort corrected dataset csv file (data used for EC plots in report)
original_EC_csv = file.path(output_folder, "AMPH-original-EC-data-20250402.csv")   # Change to the name of the original EC data file

# Define output csv file for new randomized effort corrected dataset
output_csv_newEC = file.path(output_folder, 
                             paste0("AMPH-new-EC-data-",   # Change to the desired name of your effort corrected data output .csv file
                                    thedate, 
                                    ".csv"))

# Define input csv file for use in data cleaning file
input_csv = output_csv   # Leave as is or change to the name of your input .csv file (if different than the output_csv defined above)

# Define sheet name to be read from input .xlsx file
sheetname = "Survey Observations"

# Define all years
AMPH.all.years = 2013:2024

