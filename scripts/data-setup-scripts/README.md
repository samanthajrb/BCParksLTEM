# Data Set Up Scripts

This directory contains the R code scripts that are used to set up and prepare the data for plotting. The script files are organized by protocol:

-   AMPH = Amphibian protocol (Wetland biome)

-   SQ = Squirrel protocol (Forest biome)

-   WF = Waterfowl protocol (Wetland biome)

------------------------------------------------------------------------

**Script files should be run in the following order:**

1.  `config_file.R` (for the target protocol)

    -   This file should be the **only** file that requires changes/manipulation in order to reproduce the plots included in the Final Report. File paths in this script should be changed to direct to the local folders and files where the LTEM .xlsx or .csv files from the repository are located.

    -   **NOTE:** To produce WF species plots for a specific site or region, see the site and region numbers at the end of the [`WF_config_file.R`](/scripts/data-setup-scripts/WF_config_file.R) script for the corresponding numbers to enter in the 'park.to.plot' or 'region.to.plot' fields of this WF configuration file.

2.  `data-cleaning.R` file (for the target protocol)

3.  After these files have been run successfully, any of the plot script files found in [`protocol-specific-scripts`](/scripts/protocol-specific-scripts) can be run for the target protocol.

------------------------------------------------------------------------

[`convert-xlsx-to-csv-and-add-columns.R`](/scripts/data-setup-scripts/convert-xlsx-to-csv-and-add-columns.R)\` can be used to convert a raw data .xlsx (Excel) file to a CSV file, and to create columns populated with the corresponding Study.Area (name of site) and Year (based on Visit.Start.Date) columns. This file can be used for any protocol.

`new-EC-dataframe.R` files for [`AMPH`](/scripts/data-setup-scripts/AMPH-new-EC-dataframe.R) & [`WF`](/scripts/data-setup-scripts/WF-new-EC-dataframe.R) protocols can be used to create new randomized effort corrected dataframes, with one survey selected per year at each site, and selection preference within the preferred survey months for each protocol.
