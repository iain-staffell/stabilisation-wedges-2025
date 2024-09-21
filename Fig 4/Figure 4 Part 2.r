#########################################################################
##  
##  Assess the extent to which each wedge strategy is deployed in the
##  IAMC AR6 database by comparing the scenarios calculated in Part 1
##
##  Instructions:
##    change the path on line 16 to be the base of this replication package
##    (where readme.md exists)
##  
##  Outputs:
##    Produces /Fig 4/results.csv which summarises the number of wedges
##  
##  

	# set working directory
	setwd('C:/stabilisation-wedges-2025/')

  ### get AR6 data ready ###

  # set directory
  directory = "Fig 4/iamc_data/"
  
  # Get a list of all CSV files in the specified directory
  csv_files = list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Create an empty list to store data frames
  csv_list = list()
  
  # Read each CSV file into a data frame and store in the list
  for (csv_file in csv_files) {
    # Extract the file name without extension
    file_name = tools::file_path_sans_ext(basename(csv_file))
    
    # Read CSV into a data frame and use file name as the list element name
    csv_list[[file_name]] = read.csv(csv_file)
  }
  
  # remove mid and historical column from all dataframes
  data_list = lapply(csv_list, function(df) df[, -which(names(df) %in% c("mid", "historical"))])
  
  ### get wedge targets loaded ###
  targets = t(read.csv("Fig 4/AR6_targets.csv", row.names = 1))
  
  
  
  ### get our samples and calculate difference ###
  
  set.seed(42)
  
  # create an empty list to store sampled dataframes
  samples = list()
  
  # specify the number of samples you want
  num_samples = 10000
  
  # iterate through data_list
  samples = lapply(data_list, function(df) {

    # extract non-NA values
    high_values = na.omit(df$high)
    lo_values = na.omit(df$lo)
    
    # sample with replacement
    sampled_high = sample(high_values, size = num_samples, replace = TRUE)
    sampled_lo = sample(lo_values, size = num_samples, replace = TRUE)
    
    # assemble the sampled values
    temp_df = data.frame(high = sampled_high, lo = sampled_lo)
    names(temp_df) = names(df)
    
    return(temp_df)
  })
  
  # calculate the difference across samples
  list_diff = lapply(samples, function(df) {
    df$lo - df$high
  })
 
  # convert to a dataframe
  differences = data.frame(do.call(cbind, list_diff))
  
  calculate_percentiles = function(column) {
    medians = median(column)
    percentile_25 = quantile(column, probs = 0.25)
    percentile_75 = quantile(column, probs = 0.75)
    means = mean(column)
    
    result = c(median = medians, percentile_25 = percentile_25, percentile_75 = percentile_75, mean = means)
    return(result)
  }
  
  # calculate the p25, p50, p75 for each wedge
  diff_matrix = t(apply(differences, 2, calculate_percentiles))
  
  # convert the result to a dataframe
  diff_dataframe = as.data.frame(diff_matrix)
  names(diff_dataframe) = c("median", "percentile_25", "percentile_75", "mean")
  
  # reorder columns in both dataframes
  ordered_column_names = sort(colnames(targets))
  differences = differences[, ordered_column_names]
  targets = targets[, ordered_column_names]
  

  # create all possible combinations of row indices
  combinations = expand.grid(row_targets = seq_len(nrow(targets)), row_differences = seq_len(nrow(differences)))
  
  # initialise an empty dataframe to store the results
  result_df = data.frame(matrix(NA, nrow = nrow(combinations), ncol = ncol(targets)))
  
  # perform element-wise division and store the results in the new dataframe
  for (i in 1:ncol(result_df))
  {
    result_df[ , i] = differences[combinations$row_differences, i] / targets[combinations$row_targets, i]
  }


  
  # set column names for the new dataframe
  colnames(result_df) = colnames(targets)
  
  # summarise all the results
  medians = apply(result_df, 2, median)
  percentile_25 = apply(result_df, 2, quantile, probs = 0.25)
  percentile_75 = apply(result_df, 2, quantile, probs = 0.75)
  mean = apply(result_df, 2, mean)
  
  summary = data.frame(
    Median = medians,
    Percentile_25 = percentile_25,
    Percentile_75 = percentile_75,
    Mean = mean
  )
  
  # save our results
  write.csv(summary, "/Fig 4/results.csv")
