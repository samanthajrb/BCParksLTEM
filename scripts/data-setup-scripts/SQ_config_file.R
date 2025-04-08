# ==========================
# SQUIRREL DATASET
# ==========================
# User-Specific Configuration
# Run this file BEFORE running other SQUIRREL data set up or plot scripts.
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

# Define input csv file for use in data cleaning file
input_csv = output_csv   # Leave as is or change to the name of your input .csv file (if different than the output_csv defined above)

# Define sheet name to be read from input .xlsx file
sheetname = "SOs (Srv -1)"

# Define all years
SQ.all.years = 2012:2024

# Define all hours
SQ.all.hours = 0:23
