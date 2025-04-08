# Protocol-Specific Scripts

This directory contains the R code scripts that are used to create and produce the plots and non-map figures from the FInal Report. The script files are organized into folders by protocol.

------------------------------------------------------------------------

**BEFORE RUNNING THESE PLOT SCRIPT FILES:**

Please ensure that you have run the following files for the corresponding protocol:

1.  `config_file.R` (for the target protocol)

    -   This file should be the **only** file that requires changes/manipulation in order to reproduce the plots included in the Final Report. File paths in this script should be changed to direct to the local folders and files where the LTEM .xlsx or .csv files from the repository are located.

    -   **NOTE:** To produce WF species plots for a specific site or region, see the site and region numbers at the end of the [`WF_config_file.R`](/scripts/data-setup-scripts/WF_config_file.R) script for the corresponding numbers to enter in the 'park.to.plot' or 'region.to.plot' fields of this WF configuration file.

2.  `data-cleaning.R` file (for the target protocol)

3.  After these files have been run successfully, any of the plot script files found in [`protocol-specific-scripts`](/scripts/protocol-specific-scripts) can be run for the target protocol.
