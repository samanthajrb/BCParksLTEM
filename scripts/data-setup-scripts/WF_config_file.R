# ==========================
# WATERFOWL DATASET
# ==========================
# User-Specific Configuration
# Run this file BEFORE running other WATERFOWL data set up or plot scripts.
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

# Define file path to the original randomized effort corrected dataset csv file (data used for EC plots in report)
original_EC_csv = file.path(output_folder, "WF-original-EC-data-20250403.csv")   # Change to the name of the original EC data file

# Define output csv file for new randomized effort corrected dataset
output_csv_newEC = file.path(output_folder, 
                             paste0("WF-new-EC-data-",   # Change to the desired name of your effort corrected data output .csv file
                                    thedate, 
                                    ".csv"))

# Define sheet name to be read from input .xlsx file
sheetname = "SOs (Srv -1)"

# Define all years
WF.all.years = 2013:2024

# Define list of study area names (corresponding numbers written beside)
study.area.list = c("Alice Lake Park",  #                     1
                    "Big Bar Lake Park",  #                   2
                    "Bonaparte Park",  #                      3
                    "Bowron Lakes Park",  #                   4
                    "Brandywine Falls Park", #                5
                    "Burges James Gadsden Park", #            6
                    "Call Lake Park", #                       7
                    "Champion Lakes Park", #                  8
                    "Hai Lake-Mount Herman Park", #           9
                    "Lac du Bois Protected Area", #           10
                    "Mahoney Lake Ecological Reserve", #      11
                    "Misty Lake Ecological Reserve", #        12
                    "Naikoon Park", #                         13
                    "Nazko Lake Park", #                      14
                    "Sutton Pass Ecological Reserve", #       15
                    "Wells Gray Park", #                      16
                    "White Lake Park", #                      17
                    "Yellow Point Bog Ecological Reserve") #  18

# Define study area that you want to plot
park.to.plot = 1   # Change to the corresponding number for the site (as listed above)

# Define list of region group names (corresponding numbers written beside)
region.group.list = c("Kootenay Okanagan", #    1
                      "North Coast Skeena", #   2
                      "South Coast", #          3
                      "Thompson Cariboo", #     4
                      "West Coast") #           5


# Define region that you want to plot
region.to.plot = 1   # Change to the corresponding number for the region (as listed above)

