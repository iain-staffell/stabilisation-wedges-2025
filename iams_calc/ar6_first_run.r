#########################################################################
##  
##  This code produces a minimally pre-processed version of the 
##  IAMC AR6 database
##
##  Instructions:
##    first, download the AR6 Scenarios Database from IIASA from
##    https://data.ece.iiasa.ac.at/ar6/#/downloads
##    access via https://iiasa.ac.at/models-tools-data/ar6-scenario-explorer-and-database
##    
##    you need to files:
##     * AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1.csv
##     * AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx
##
##    next, change line 25 to give the path where these two files are saved
##    and change the path to this code package on line 34
##
##    finally, run the code below to generate a processed version which
##    features only vetted scenarios, ties in climate indicators,
##    and saves in R's binary format for blazing speed
##    it will require ~30 minutes and ~10 GB of free RAM to complete >_<
##

	# path to your downloaded AR6 files (e.g. 'C:/Users/Iain/' or './data/' etc.)
	PATH = 'M:/'




#####
## ##  INITIALISE AND LOAD DATA
#####

	source('others/blam_library.r')
	library(openxlsx)

	# read in the AR6 database
	data = read.csv(PATH %&% 'AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1.csv')

	# read in the AR6 metadata
	meta = read.xlsx(PATH %&% 'AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx', sheet=2)



#####
## ##  FILTER OUT NON-VETTED SCENARIOS
#####

	# tie in a unique id for each row in the form of 'Model|Scenario|Region'
	colnames(data) = gsub('X', '', colnames(data))
	data$id = paste(data$Model, data$Scenario, data$Region, sep='|')

	# build the same for the metadata
	meta$id = paste(meta$Model, meta$Scenario, 'World', sep='|')

	# identify and remove non-vetted scenarios from the database
	is_vetted = data$id %in% meta$id

	flush('Keeping', sum(is_vetted), 'out of', length(is_vetted), 'database rows.\n')
	data = data[is_vetted, ]
	


#####
## ##  TIE META-DATA INTO THE MAIN DATA
#####

	# attribute the 2100 warming to each row
	warming = (data$Variable == 'AR6 climate diagnostics|Surface Temperature (GSAT)|MAGICCv7.5.3|50.0th Percentile')
	warming = data.frame(id=data$id[warming], value=data$`2100`[warming])
	m = match(data$id, warming$id)
	data$warming = warming$value[m]

	# attribute the 2050 emissions to each row
	emissions = (data$Variable == 'AR6 climate diagnostics|Infilled|Emissions|Kyoto Gases (AR6-GWP100)')
	emissions = data.frame(id=data$id[emissions], value=data$`2050`[emissions])
	m = match(data$id, emissions$id)
	data$emissions = emissions$value[m] / 1000

	# save as rds for speed
	saveRDS(data, PATH %&% 'AR6_Scenarios_Database_World_ALL_CLIMATE_v1.1_vetted.rds')
