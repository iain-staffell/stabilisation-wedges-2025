# Stablisiation wedges 2025 - Replication package
This package contains the code and data needed to replicate the results from Johnson & Staffell, *"Democratising climate change mitigation pathways using modernised Stabilisation Wedges"*

Pre-requisites:  [R](https://www.r-project.org/) and [Microsoft Excel](https://www.microsoft.com/en-us/microsoft-365/excel).

R requires the following packages: `mgcv`, `reshape2`, `dplyr` and `tidyverse`.

To ensure these are installed, you could run:
```
if (!require(mgcv)) { install.packages('mgcv'); require(mgcv) }
if (!require(reshape2)) { install.packages('reshape2'); require(reshape2) }
if (!require(dplyr)) { install.packages('dplyr'); require(dplyr) }
if (!require(tidyverse)) { install.packages('tidyverse'); require(tidyverse) }
```

<br>
<br>



# A Wedge Approach to Mitigation

The first section of the paper provides some general context on global GHG emissions and their breakdown between sectors.

## Figure 1

`/Fig 1/Figure 1.xlsx` gives the historical GHG emissions and stylised pathways for future emissions with different numbers of wedges.  The main figure is assembled in `Sheet 1`, and the calculation of emissions in 2050 is given in `Sheet 2`.

The temperature ranges that are related to different levels of mitigation are derived from the IAMC AR6 database, described later in the sub-section on Figure S2.



## Figure 2

`/Fig 2/Figure 2.xlsx` gives the sectoral breakdown historical emissions (in 2019) from IPCC (2022) AR6 WG3 and projected emissions in 2050 under each baseline scenario: IEA 6DS, RCP 8.5, IEA 4DS, RCP 6.0.

<br>
<br>


# Options That Can Achieve a Wedge

The second section of the paper quantifies 36 options that can acheive a 'wedge' of mitigation.

## Figure 3

The code and spreadsheets in the `/Fig 3/` sub-folder calculate the scale at which each strategy must be deployed to achieve one wedge.  All results are collated in `/Fig 3/Figure 3 Main workbook.xlsx`.  This takes in data from other files to compile sector specific results.


### Electricity sector

Absolute values are calculated in `/Fig 3/Power.r`.  Running the code prints out summaries for each wedge.  Results from the following data frames are then compiled in `/Fig 3/Figure 3 Main workbook.xlsx`:

- **RES and nuclear wedges:** `wedge_res`
- **Coal-to-gas fuel switching:** `wedge_c2g`
- **Coal power with CCS:** `wedge_CCS_coal`
- **Gas power with CCS:** `wedge_CCS_gas`
- **Bioenergy power with CCS:** `wedge_BECCS`
- **Direct air capture:** `wedge_DAC`


### Transport sector

Absolute values are calculated in `/Fig 3/Transport.r`.  Running the code prints out summaries for each wedge.  Results from the following data frames are then compiled in `/Fig 3/Figure 3 Main workbook.xlsx`:

- **Vehicle efficiency:** `wedge_v_eff`
- **Avoided or active travel:** `wedge_avoid`
- **Electric vehicles:** `wedge_EVs`
- **Public transport:** `wedge_public`
- **Avoided air travel:** `wedge_air`
- **Biofuels:** `wedge_bio`
- **Freight decarbonisation:** `wedge_freight`


### Buildings sector

Absolute values are calculated in `/Fig 3/Buildings.r`.  Running the code prints out summaries for each wedge.  Results from the following data frames are then compiled in `/Fig 3/Figure 3 Main workbook.xlsx`:

- **Building heat transfer:** `wedge_fabric`
- **Heat pumps:** `wedge_hp`  
- **Clean cookstoves:** `wedge_stoves`  

### Land use and food production:

Absolute values are calculated in `/Fig 3/Buildings.r`.  Running the code prints out summaries for each wedge.  Results from the following data frames are then compiled in `/Fig 3/Figure 3 Main workbook.xlsx`:

- **Reduced deforestation:** `wedge_def`
- **Temperate reforestation:** `wedge_temp`
- **Tropical reforestation:** `wedge_trop`
- **Trees in tropical cropland:** `wedge_trop_incrop`
- **Trees in temperate cropland:** `wedge_temp_incrop`
- **Trees in tropical pastures:** `wedge_trop_silvo`
- **Trees in temperate pastures:** `wedge_temp_silvo`
- **Rewetting peatlands (half-wedge):** `h_wedge_rewet`
- **Preventing peatland drainage (half-wedge):** `h_wedge_drain`

<br>
The remaining land and food wedges are calculated in separate spreadsheets and then compiled in `/Fig 3/Figure 3 Main workbook.xlsx`.  

- **Soil carbon sequestration:** Calculated in `Soil carbon sequestration.xlsx` and cells P43:R43 (highlighted in pink) show the area of cropland required in 2050 for a wedge.

- **Enhanced weathering:** Calculated in `Enhanced weathering.xlsx` and cell D18 (highlighted in pink) shows the area of cropland required in 2050 for a wedge.

- **Reduced meat consumption:** Calculated in `Dietary change.xlsb` on the `Scenarios 2050` worksheet. Cells Y2:Y3 to AB2:AB13 which are highlighted in pink, show the reduction in calories from meat in 2050 required for a wedge.

- **Avoid food loss and waste:** Calculated in `Food loss and waste.xlsb` on the  `Model PivotTables` sheet. Select the value “2050 baseline” in cell B1. Cells G3:G7 to I3:I7 which are highlighted in pink, show the reduction in the mass of food loss and waste required for a wedge.


### Industry sector

Absolute values are calculated in `Industry.xlsx` which are compiled in `/Fig 3/Figure 3 Main workbook.xlsx`.

- **Produce clean hydrogen:** Calculated in the `Clean hydrogen` worksheet. The mass of hydrogen required to acheive a wedge is given in cells E28:H28 (highlighted in pink). 

- **Decarbonising steel:** Calculated in the `Steel and cement` worksheet as the average effort across steel produced with CCS and steel produced via hydrogen-DRI. The amount of steel that must be produced with CCS and via hydrogen DRI to achieve a wedge are given in cells E33:H33 and and E37:H37 respecively (all highlighted in pink). 

- **CCS at cement plants:** Calculated in the `Steel and cement` worksheet. The amount of cement that must be produced with CCS to achieve a wedge is given in E24:H24 (highlighted in pink).

- **Methane in oil and gas:** Calculated in the `Methane in oil and gas` worksheet. The cumulative methane emissions are used to calculate the effort required for a wedge, and are given in cells AI50:AL50 (highlighted in pink).

<br>
<br>

# IAM Results in the Language of Wedges

The third section of the paper takes mitigation pathways from the IAMC AR6 database and translates these into units of wedges.

The code for this section requires a copy of the AR6 database to be downloaded and pre-processed.  Redistribution of the database is not permitted by IIASA, so we instead provide instructions on how to recreate the necessary input files.

First, download the the [AR6 Scenarios Database](https://data.ece.iiasa.ac.at/ar6/#/downloads).  From that link, press the `Guest login` button, then select `Downloads` from the top menu, and finally scroll down to the link for `AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1`.

Extract the ZIP file to the root folder (where this `readme.md` file exists).  Then in `ar6_library.r` run the `PREREQUISITE` code block starting on line 25.  This will generate a minimally-processed version of the AR6 databse, where emissions in 2050 and warming in 2100 are appended to each result, to allow for faster searching and filtering.  After running this code block successfully, you will have a 750 MB file .rds file (R's internal binary format, for faster reading), with MD5 checksum of `4a922b73c0ac53be693b8f975edf7ff2`.

The subsequent scripts in this section then call `ar6_library.r` which will load this .rds file and correct some obvious errors in it (e.g. population being reported as 10 trillion rather than 10 billion, or CH₄ emissions being 200 Gt rather than 200 Mt).



## Figure 4

First, run `/Fig 4/Figure 4 Part 1.r` which reads the AR6 database, an extracts values for variables representing our wedge strategies under 'current policy' and 'decarbonisation' scenarios.  This saves a set of interim files and graphs in `/Fig 4/iamc_data/`.

Second, run `/Fig 4/Figure 4 Part 2.r` which reads in these interim files, calculates the difference in effort between the two scenarios using Monte Carlo, and then translates these into units of wedges.  

This second part uses `/Fig 4/AR6_targets.csv` to normalise 'effort' (i.e. TWh, MtCO₂) into wedges. These targets are derived from those reported in `/Fig 3/Figure 3 Main workbook.xlsx`. Unit conversions and adjustments applied to targets (to make them compatible with values from the AR6 database) are specified in the 'Translating IAM results into wedges" section of the supplementary material. 



## Figure S2

The code in the `/Fig S2/` sub-folder reads the IAMC AR6 database, and generates a relationship between emissions from 2020 to 2050 and temperature in 2100.  This relationship is also used in Figure 1.



## Figure S5

The code in the `/Fig S5/` sub-folder reads the IAMC AR6 database, extracts scenarios matching our definitions, and summarises the emissions and temperature time series.



## Figures S6–S10

To generate these figures, run `/Fig 4/Figure 4 Part 1.r` and they will be produced in the `/Fig 4/iamc_data/` subfolder.

<br>
<br>

# Other files

`ar6_library.r` – Code to process and work with the IAMC AR6 database.

`blam_library.r` – Background data handling functions.

`/Fig 3/Inputs/` – Data sets from the literature to specify individual wedges (detailed in the Figure 3 code files).

`/Fig 4/iamc_data/` – A folder to store the interim results and figures from the Figure 4 code.

