plot_species_abundance <- function(species_name,
                                   maaslin_result_file = NULL,
                                   abundance_data,
                                   metadata,
                                   treatment_col = NULL,
                                   colors = NULL,
                                   metadata_filter = NULL,
                                   tag_label = NULL) {
  
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(ggtext)
  
  # Normalize species name: replace dots with underscores
  species_name <- gsub("\\.", "_", species_name)
  
  # Load MaAsLin2 results
  maaslin_results <- read_tsv(maaslin_result_file, show_col_types = FALSE)
  
  # Normalize feature names in MaAsLin2 results
  maaslin_results <- maaslin_results %>%
    mutate(feature = gsub("\\.", "_", feature))
  
  # Filter results by species_name and optionally by metadata_filter
  filtered_results <- maaslin_results %>%
    filter(feature == species_name)
  
  if (!is.null(metadata_filter)) {
    filtered_results <- filtered_results %>% filter(metadata == metadata_filter)
  }
  
  # Extract p-value and q-value for the filtered results
  p_value <- filtered_results %>% pull(pval)
  q_value <- filtered_results %>% pull(qval)
  
  # Check if species and metadata combination was found
  if (length(p_value) == 0 || length(q_value) == 0) {
    stop(paste("Species", species_name, "with metadata", 
               ifelse(is.null(metadata_filter), "ANY", metadata_filter), 
               "not found in the MaAsLin2 results."))
  }
  
  # Simplify abundance_data column names if needed
  original_colnames <- colnames(abundance_data)
  simplified_colnames <- original_colnames
  
  # Find the column corresponding to the species_name
  matched_index <- which(grepl(species_name, simplified_colnames, ignore.case = FALSE))
  
  if (length(matched_index) == 0) {
    similar <- grep(species_name, simplified_colnames, value = TRUE, ignore.case = TRUE)
    msg <- paste0("Species '", species_name, "' not found in abundance_data columns.\n")
    if (length(similar) > 0) {
      msg <- paste0(msg, "Did you mean one of these?\n  - ", paste(similar, collapse = "\n  - "))
    }
    stop(msg)
  }
  
  matched_colname <- original_colnames[matched_index]
  
  if (!is.data.frame(abundance_data)) {
    abundance_data <- as.data.frame(abundance_data)
  }
  
  # Combine metadata and abundance
  plot_data <- metadata %>%
    mutate(species_abundance = abundance_data[[matched_colname]])
  
  # Handle grouping column
  if (is.null(treatment_col)) {
    plot_data$treatment_group <- "All"
    treatment_col <- "treatment_group"
  } else if (!treatment_col %in% colnames(plot_data)) {
    stop(paste("Column", treatment_col, "not found in metadata."))
  }
  
  # Set default colors if not provided
  if (is.null(colors)) {
    unique_groups <- unique(plot_data[[treatment_col]])
    default_palette <- scales::hue_pal()(length(unique_groups))
    colors <- setNames(default_palette, unique_groups)
  }
  
  clean_species_name <- gsub("_", " ", species_name)
  
  # Build the plot
  plot <- ggplot(plot_data, aes_string(x = treatment_col, y = "species_abundance")) +
    geom_dotplot(
      aes_string(fill = treatment_col),
      binaxis = "y",
      stackdir = "center",
      dotsize = 2,
      alpha = 0.7,
      position = position_jitter(width = 0.1)
    ) +
    stat_summary(
      fun = mean,
      geom = "point",
      size = 2,
      color = "black",
      position = position_dodge(0.8)
    ) +
    stat_summary(
      fun.data = function(x) data.frame(ymin = min(x), ymax = max(x)),
      geom = "errorbar",
      width = 0.1,
      color = "black",
      position = position_dodge(0.8)
    ) +
    labs(
      x = ifelse(treatment_col == "treatment_group", NULL, treatment_col),
      y = "Pseudo-raw counts",
      title = paste0("_", clean_species_name, "_"),
      subtitle = paste0(
        "p-value: ", round(p_value, 3), "\n",
        "q-value: ", round(q_value, 3)
      )
    ) +
    theme_bw() +
    scale_fill_manual(values = colors) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = ggtext::element_markdown(size = 24, hjust = 0.5),
      plot.subtitle = element_text(size = 22),
      #plot.subtitle = ggtext::element_markdown(size = 22, hjust = 0.5, margin = margin(t = 10, b = 20)),
      strip.background = element_blank(),
      axis.text.x = element_text(size = 22),
      axis.text.y = element_text(size = 22),
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = 22),
      legend.text = element_blank(),
      legend.title = element_blank(),legend.position = "none"
    ) 
  
  if (!is.null(tag_label)) {
    plot <- plot +
      labs(tag = tag_label) +
      theme(
        plot.tag = element_text(face = "bold", size = 22),
        plot.tag.position = c(0, 1.05),
        plot.margin = ggplot2::margin(t = 40, r = 20, b = 20, l = 40)
      )
  }
  
  return(list(
    plot = plot,
    pval = p_value,
    qval = q_value
  ))
}
