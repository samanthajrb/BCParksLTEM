# BC Parks LTEM

This repository contains all R code used in the data analysis project for the BC Parks Long Term Ecological Monitoring (LTEM) program.

The folders contain the following:

1.  [`all-protocol-inputs`](/all-protocol-inputs) — Contains the Excel (.xlsx) data files for each protocol as they were extracted from the BC Parks SPI and provided by BC Parks for analysis.
2.  [`all-protocol-outputs`](/all-protocol-outputs) — Contains all outputs produced in the process of analysis including plots and figures. This folder contains three subfolders:
    -   [`dataset-csv-files`](/all-protocol-outputs/dataset-csv-files) — Contains the dataset CSV files produced by converting the Excel input files. These CSV files were then used to produce plots in R for each protocol. This folder also includes CSV files for the original randomized datasets that were used in the plots and analyses included in the Final Report.
    -   [`plots`](/all-protocol-outputs/plots) — Contains the PNG files for all plots included in the Final Report.
    -   [`maps`](/all-protocol-outputs/maps) — Contains the PNG files for all maps included in the Final Report.
3.  [`scripts`](/scripts) — Contains R code used to set up data and create plots and figures for each of the protocol datasets. This folder contains 2 subfolders, one containing the scripts for general data set up, called [`data-setup-scripts`](/scripts/data-setup-scripts), and the other containing the scripts to create individual plots, called [`protocol-specific scripts`](/scripts/protocol-specific-scripts).
