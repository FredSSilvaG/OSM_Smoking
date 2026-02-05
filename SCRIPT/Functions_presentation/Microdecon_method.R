run_microdecon <- function(data_species, metadata) {
  library(microDecon)
  
  # Identify relevant columns
  blank_cols <- grep("Neg", colnames(data_species), value = TRUE)
  taxa_col <- "clade_name"
  all_cols <- colnames(data_species)
  sample_cols <- setdiff(all_cols, c(taxa_col, blank_cols))
  
  # Default to input (used if skipping microDecon)
  cleaned_data <- data_species
  clade_names_to_remove <- NULL
  
  # Conditionally run microDecon if more than 20 species
  if (nrow(data_species) > 20) {
    message("Running microDecon (more than 20 species)...")
    
    # Reorder: clade_name, blanks, samples
    data_decon <- as.data.frame(data_species[, c(taxa_col, blank_cols, sample_cols)])
    
    # Sample column names (excluding clade_name and Neg)
    sample_cols_filtered <- colnames(data_decon)[!grepl("clade_name|Neg", colnames(data_decon))]
    
    # Count samples per group (adjust these if you use other suffixes)
    n_CN <- sum(grepl("_CN$", sample_cols_filtered))
    n_CR <- sum(grepl("_CR$", sample_cols_filtered))
    
    # Run decontamination
    decon_result <- decon(
      data = data_decon,
      numb.blanks = length(blank_cols),
      numb.ind = c(n_CN, n_CR),
      taxa = FALSE,
      runs = 2,
      thresh = 1,
      prop.thresh = 0,
      regression = 0
    )
    
    # Remove contaminants
    clade_names_to_remove <- decon_result$OTUs.removed$clade_name
    cleaned_data <- data_species[!data_species$clade_name %in% clade_names_to_remove, ]
  } else {
    message("Skipping microDecon: 20 or fewer species present.")
  }
  
  ### ✅ ALWAYS DO THE FOLLOWING (even if microDeCon skipped):
  
  # 1. Remove negative control columns
  cleaned_data <- cleaned_data[, !colnames(cleaned_data) %in% blank_cols]
  
  # 2. Remove species (rows) with no presence
  cleaned_data <- cleaned_data[rowSums(cleaned_data[, -1]) > 0, ]
  
  # 3. Remove samples (columns) with no species
  cleaned_data <- cleaned_data[, c(taxa_col, colnames(cleaned_data)[-1][colSums(cleaned_data[, -1] != 0) > 0])]
  
  # 4. Re-filter metadata
  remaining_samples <- colnames(cleaned_data)[-1]
  filtered_metadata <- metadata[metadata$sample_name %in% remaining_samples, ]
  
  return(list(
    data = cleaned_data,
    metadata = filtered_metadata,
    removed_otus = clade_names_to_remove
  ))
}
