renormalize_if_needed <- function(data, stats_subset, read_column_suffix) {
  
  if (nrow(data) > 20) {
    norm_result <- adjust_reads_and_normalize(
      data,
      stats_subset,
      read_column_suffix = read_column_suffix
    )
    
    list(
      data_renormalized = norm_result$renormalized_data,
      stats_subset = norm_result$updated_stats
    )
    
  } else {
    read_col <- paste0("Reads_after_dedup_", read_column_suffix)
    if (!(read_col %in% colnames(stats_subset))) {
      stop(paste("Column", read_col, "not found in stats_subset."))
    }
    
    stats_subset$Reads_after_dedup_selected <- stats_subset[[read_col]]
    
    list(
      data_renormalized = data,
      stats_subset = stats_subset
    )
  }
}