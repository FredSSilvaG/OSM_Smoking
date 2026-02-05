filter_species_data <- function(data_species,
                                metadata_smokers_general,
                                metadata_smokers_with_Neg,
                                metadata_smokers) {
  
  # Identify columns to keep
  cols_general <- c(colnames(data_species)[1], metadata_smokers_general$sample)
  cols_with_Neg <- c(colnames(data_species)[1], metadata_smokers_with_Neg$sample)
  cols_smokers <- c(colnames(data_species)[1], metadata_smokers$sample)
  
  # Subset abundance tables
  data_general <- data_species[, cols_general, drop = FALSE]
  data_with_Neg <- data_species[, cols_with_Neg, drop = FALSE]
  data_smokers <- data_species[, cols_smokers, drop = FALSE]
  
  # Rename columns using sample_name
  colnames(data_general)[-1] <-
    metadata_smokers_general$sample_name[
      match(colnames(data_general)[-1], metadata_smokers_general$sample)
    ]
  
  colnames(data_with_Neg)[-1] <-
    metadata_smokers_with_Neg$sample_name[
      match(colnames(data_with_Neg)[-1], metadata_smokers_with_Neg$sample)
    ]
  
  colnames(data_smokers)[-1] <-
    metadata_smokers$sample_name[
      match(colnames(data_smokers)[-1], metadata_smokers$sample)
    ]
  
  # Remove zero-only species and samples
  data_general <- data_general[
    rowSums(data_general[, -1]) > 0,
    colSums(data_general != 0) > 0
  ]
  
  data_with_Neg <- data_with_Neg[
    rowSums(data_with_Neg[, -1]) > 0,
    colSums(data_with_Neg != 0) > 0
  ]
  
  data_smokers <- data_smokers[
    rowSums(data_smokers[, -1]) > 0,
    colSums(data_smokers != 0) > 0
  ]
  
  # Refilter metadata
  metadata_general <- metadata_smokers_general[
    metadata_smokers_general$sample_name %in% colnames(data_general),
  ]
  
  metadata_with_Neg <- metadata_smokers_with_Neg[
    metadata_smokers_with_Neg$sample_name %in% colnames(data_with_Neg),
  ]
  
  metadata_smokers <- metadata_smokers[
    metadata_smokers$sample_name %in% colnames(data_smokers),
  ]
  
  return(list(
    data_general = data_general,
    data_with_Neg = data_with_Neg,
    data_smokers = data_smokers,
    metadata_general = metadata_general,
    metadata_with_Neg = metadata_with_Neg,
    metadata_smokers = metadata_smokers
  ))
}