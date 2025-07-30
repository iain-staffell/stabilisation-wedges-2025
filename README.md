# Stabilisation wedges 2025 - Replication package
This package contains the code and data needed to replicate the results from Johnson & Staffell, *"Democratising climate change mitigation pathways using modernised Stabilisation Wedges"*.

Citation: ... *(DOI to be added when known)* ...

Contact: [Iain Staffell](mailto:i.staffell@imperial.ac.uk?subject=Stabilisation%20Wedges%20Github) and [Nathan Johnson](mailto:nathan.johnson17@imperial.ac.uk?subject=Stabilisation%20Wedges%20Github).

Licence: MIT



<br><br>

## Pre-requisites
* [Microsoft Excel](https://www.microsoft.com/en-us/microsoft-365/excel) for viewing the main results
* [R 4.4](https://www.r-project.org/) and [Microsoft Excel](https://www.microsoft.com/en-us/microsoft-365/excel) for replicating the main analysis
* [GDAL 3.7](https://gdal.org/en/stable/) and [R 4.4](https://www.r-project.org/) for replicating the supplementary analysis

R requires the following packages: `mgcv`, `reshape2`, `terra`, `dplyr` and `tidyverse`.

To ensure these are installed, you could run:
```
if (!require(mgcv))      install.packages('mgcv')
if (!require(reshape2))  install.packages('reshape2')
if (!require(terra))     install.packages('terra')
if (!require(dplyr))     install.packages('dplyr')
if (!require(tidyverse)) install.packages('tidyverse')
```

<br>
<br>


## Main data

`Main Data - Figures 1 to 4.xlsx` gives the data presented in the four figures of the main paper.  Data are given in tabular form, so they can be read by Python, R, and other languages easily.

The **Meta** sheet gives definitions of all columns in the other worksheets.

The **Fig1** sheet gives the stylised pathways for future emissions with different numbers of wedges and their resulting temperature rise, as shown in Figure 1.  The temperature ranges that are related to different levels of mitigation are derived from the IAMC AR6 database, described in the section on Figure S2.

The **Fig2Fig3** sheet gives the definitions of how much effort is needed to achieve a 'wedge' of mitigation from each of our 36 strategies, both in relative shares (as shown in Figure 2) and absolute units (as shown in Figure 3).  The code for calculating effort are described in the section on [Options That Can Achieve a Wedge](#options-that-can-achieve-a-wedge).

The **Fig4** sheet gives the translation of mitigation pathways from the IAMC AR6 database into units of wedges, as shown in Figure 4.  The code for calculating these results are describe in the section on [IAM Results in the Language of Wedges](#iam-results-in-the-language-of-wedges)

<br>
<br>


## Options That Can Achieve a Wedge

The code and spreadsheets in the `effort_calc/` sub-folder calculate the scale at which each strategy must be deployed to achieve one wedge.  All results are collated in the Fig2Fig3 sheet of `Main Data - Figures 1 to 4.xlsx`.  

Data inputs to these scripts are contained in `effort_calc/inputs/`


#### Electricity sector

Absolute values are calculated in `effort_calc/Power.r`.  Running the code calculates the effort required for six strategies, stored in the following variables:
- RES and nuclear wedges: `wedge_res`
- Coal-to-gas fuel switching: `wedge_c2g`
- Coal power with CCS: `wedge_CCS_coal`
- Gas power with CCS: `wedge_CCS_gas`
- Bioenergy power with CCS: `wedge_BECCS`
- Direct air capture: `wedge_DAC`


#### Transport sector

Absolute values are calculated in `effort_calc/Transport.r`.  Running the code calculates the effort required for seven strategies, stored in the following variables:

- Vehicle efficiency: `wedge_v_eff`
- Avoided or active travel: `wedge_avoid`
- Electric vehicles: `wedge_EVs`
- Public transport: `wedge_public`
- Avoided air travel: `wedge_air`
- Biofuels: `wedge_bio`
- Freight decarbonisation: `wedge_freight`


#### Buildings sector

Absolute values are calculated in `effort_calc/Buildings.r`.  Running the code calculates the effort required for three strategies, stored in the following variables:

- Building heat transfer: `wedge_fabric`
- Heat pumps: `wedge_hp`  
- Clean cookstoves: `wedge_stoves`  

#### Land use and food production:

Absolute values are calculated in `effort_calc/Land.r`.  Running the code calculates the effort required for nine strategies, stored in the following variables:

- Reduced deforestation: `wedge_def`
- Temperate reforestation: `wedge_temp`
- Tropical reforestation: `wedge_trop`
- Trees in tropical cropland: `wedge_trop_incrop`
- Trees in temperate cropland: `wedge_temp_incrop`
- Trees in tropical pastures: `wedge_trop_silvo`
- Trees in temperate pastures: `wedge_temp_silvo`
- Rewetting peatlands (half-wedge): `h_wedge_rewet`
- Preventing peatland drainage (half-wedge): `h_wedge_drain`

<br>
The remaining land and food wedges are calculated in separate spreadsheets:

- Soil carbon sequestration: Calculated in `effort_calc/Soil carbon sequestration.xlsx`.  Cells P43:R43 (highlighted in pink) show the area of cropland required in 2050 for a wedge.

- Enhanced weathering: Calculated in `effort_calc/Enhanced weathering.xlsx`.  Cell D18 (highlighted in pink) shows the area of cropland required in 2050 for a wedge.

- Reduced meat consumption: Calculated in `effort_calc/Dietary change.xlsb` on the `Scenarios 2050` worksheet.  Cells Y2:Y3 to AB2:AB13 (highlighted in pink), show the reduction in calories from meat in 2050 required for a wedge.

- Avoid food loss and waste: Calculated in `effort_calc/Food loss and waste.xlsb` on the `Model PivotTables` sheet.  With the value "2050 baseline" selected in cell B1, cells G3:G7 to I3:I7 (highlighted in pink) show the reduction in the mass of food loss and waste required for a wedge.

<br>

For the six 'add trees' strategies, the relative share of available land area is computed using GDAL and R, as described in [Contextualising global land use areas](#contextualising-global-land-use-areas)


#### Industry sector

Absolute values are calculated in `effort_calc/Industry.xlsx`.

- Produce clean hydrogen: Calculated in the `Clean hydrogen` worksheet. The mass of hydrogen required to achieve a wedge is given in cells E28:H28 (highlighted in pink). 

- Decarbonising steel: Calculated in the `Steel and cement` worksheet as the average effort across steel produced with CCS and steel produced via hydrogen-DRI. The amount of steel that must be produced with CCS and via hydrogen DRI to achieve a wedge are given in cells E33:H33 and and E37:H37 respectively (all highlighted in pink). 

- CCS at cement plants: Calculated in the `Steel and cement` worksheet. The amount of cement that must be produced with CCS to achieve a wedge is given in E24:H24 (highlighted in pink).

- Methane in oil and gas: Calculated in the `Methane in oil and gas` worksheet. The cumulative methane emissions are used to calculate the effort required for a wedge, and are given in cells AI50:AL50 (highlighted in pink).

<br>
<br>


## IAM Results in the Language of Wedges

#### Initial setup
Our analysis of mitigation effort in IAMs requires a copy of the AR6 database to be downloaded and pre-processed. 
 
1. Download the the [AR6 Scenarios Database](https://data.ece.iiasa.ac.at/ar6/#/downloads).  From that link, press the *Guest login* button,  select *Downloads* from the top menu, and finally scroll down to the link for *'AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1'*.
2. Extract the zip file to give the csv database and xlsx metadata.
3. Run `iams_calc/ar6_first_run.r`, following the instructions to set the paths where you saved the files in step 2  

These three steps will generate a minimally-processed version of the AR6 database, featuring only the 1,202 vetted scenarios, with emissions in 2050 and warming in 2100 appended to each result to allow for faster searching and filtering.  After running this code block successfully, you will have a 533 MB file in R's binary format, with MD5 checksum of `266f7c8348ea821b3077ed1e9b9305e4`.

<br>

#### Paired scenarios

`iams_calc/AR6_Paired_Scenarios.csv` ← Gives the 959 pairs of 'baseline' and 'decarbonisation' scenarios from the AR6 database that we grouped together.  These pairs use the same model family and experiment family, but have different emissions budgets.  The data file gives the identifier for each scenario along with metadata on their GHG emissions in 2050.


#### Figure 4

All the code needed to produce the translation from IAM outputs into wedges is given in `iams_calc/iams_to_wedges.r`.  This includes blocks of code to reproduce Figure S6, Figure S7, Figure 4, and Figures S8 to S29.  

The final output from running this code is provided in `iams_calc/wedge_iam_results.csv` to verify results you produce, or to save you having to set up that code. These results are then reshaped (wide to long), given nicer column names, and presented in the Fig4 sheet of `Main Data - Figures 1 to 4.xlsx`.

<br>
<br>


## Other supporting analysis

#### Contextualising global land use areas

The analysis of land usage within the temperate and tropical biomes relies on external data from ESA, NASA and Beck et al.  Download instructions are included within the first file below.

We first use GDAL to align all three datasets onto a common grid, then use R to combine land usage and biomes, compute areas, and draw maps.

- `others/global_land_area_1.bat` ← Code to pre-process GIS data from ESA-CCI, NASA MODIS and Beck's Koppen-Geiger projections.

- `others/global_land_area_2.r` ← Code to analyse the ESA-CCI data, generating the results in Table S22-S23 and Figure S31-S32.

- `others/global_land_area_3.r` ← Code to analyse the NASA MODIS data, generating the results in Table S22-S23 and Figure S31-S32.

<br>


#### 6 trillion ways to decarbonise

- `others/wedge_combinations.py` ← Calculates the total number of combinations for deploying strategies to achieve 20 wedges, shown in Table S4.

<br>


#### Helper functions

- `others/ar6_library.r` ← A helper file which is called by other scripts.  It loads the AR6 database produced in the initial setup stage, removes some obvious errors from it (e.g. scenarios where people eat 5,000 calories of meat per day), and defines several background functions.

- `others/blam_library.r` ← Background data handling functions
