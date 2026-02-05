
#Modular Functions

#Preparation of stats_table
prepare_stats_table <- function(stats_table, metadata_general, metadata_target) {
  # Match Sample ID with metadata general
  stats_table$Sample <- metadata_general$sample_name[
    match(stats_table$Sample, metadata_general$sample)
  ]
  
  stats_subset <- stats_table[stats_table$Sample %in% metadata_target$sample_name, ]
  stats_subset_general <- stats_table[stats_table$Sample %in% metadata_general$sample_name, ]
  
  # Ensure order match for pseudo count alignment
  stats_subset <- stats_subset[match(metadata_target$sample_name, stats_subset$Sample), ]
  
  stopifnot(all(metadata_target$sample_name == stats_subset$Sample))
  
  return(list(stats_subset=stats_subset, stats_subset_general=stats_subset_general))
}



#Adjust reads and normalization

adjust_reads_and_normalize <- function(data_species, stats_subset, read_column_suffix = "metaphlan") {
  read_col <- paste0("Reads_after_dedup_", read_column_suffix)
  
  if (!(read_col %in% colnames(stats_subset))) {
    stop(paste("Column", read_col, "not found in stats_subset."))
  }
  
  # Step 1: Adjust selected read column
  sample_names <- colnames(data_species)[-1]  # Exclude clade_name
  relative_sums <- colSums(data_species[, sample_names])
  
  stats_subset$Reads_after_dedup_selected <- stats_subset[[read_col]]
  
  matched_samples <- match(sample_names, stats_subset$Sample)
  stats_subset$Reads_after_dedup_selected[matched_samples[!is.na(matched_samples)]] <-
    round(stats_subset[[read_col]][matched_samples[!is.na(matched_samples)]] *
            (relative_sums[!is.na(matched_samples)] / 100))
  
  # Step 2: Normalize to 100
  abundance_matrix <- data_species[, sample_names]
  column_sums <- colSums(abundance_matrix)
  abundance_renormalized <- sweep(abundance_matrix, 2, column_sums, `/`) * 100
  
  data_renormalized <- cbind(clade_name = data_species$clade_name, abundance_renormalized)
  
  return(list(
    renormalized_data = data_renormalized,
    updated_stats = stats_subset
  ))
}




#Calculate pseudo_counts

calculate_pseudo_counts <- function(renormalized_data, stats_subset) {
  abundance_matrix <- renormalized_data
  abundance_matrix$clade_name <- sapply(strsplit(abundance_matrix$clade_name, "\\|"), tail, 1)
  abundance_matrix$clade_name <- gsub("s__", "", abundance_matrix$clade_name)
  abundance_matrix$clade_name <- gsub(" ", "_", abundance_matrix$clade_name)
  
  abundance_only <- abundance_matrix[, -1]
  reads_vector <- stats_subset$Reads_after_dedup_selected
  names(reads_vector) <- stats_subset$Sample
  
  reads_vector <- reads_vector[colnames(abundance_only)]
  
  absolute_counts <- round(sweep(abundance_only, 2, reads_vector / 100, `*`))
  absolute_counts <- cbind(clade_name = abundance_matrix$clade_name, absolute_counts)
  rownames(absolute_counts) <- absolute_counts$clade_name
  
  absolute_counts_tp <- t(absolute_counts[, -1])
  
  return(absolute_counts_tp)
}
