plot_relative_abundance <- function(data_species, metadata, level = NULL, 
                                    low_abundance_threshold = 1, colors_list, 
                                    genus_colors = NULL, family_colors = NULL, phyla_colors = NULL,
                                    kingdom_label = "Bacterial",
                                    group_var = NULL, group_levels = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Separate taxonomy
  data_species <- data_species %>%
    separate(clade_name, into = c("Kingdom", "Group", "Order", "Family","SubFamily", "Genus", "Species"),
             sep = "\\|", fill = "right")
  
  # Remove prefixes like k__, p__, etc.
  data_species[] <- lapply(data_species, function(x) if (is.character(x)) gsub("^[kpcfogs]__", "", x) else x)
  
  # Long format abundance
  abundance_data <- data_species[, setdiff(names(data_species), c("Kingdom", "Group","Family","SubFamily"))]
  abundance_data <- gather(abundance_data, samples, RA, -c("Order", "Genus", "Species"))
  
  # Merge with metadata
  metadata_subset <- metadata %>%
    dplyr::select(sample_name, all_of(group_var))
  
  # Conditionally recode only if group_var is 'treatment' or 'treatment_group'
  if (group_var %in% c("treatment", "treatment_group")) {
    if (!is.null(metadata_subset[[group_var]])) {
      metadata_subset[[group_var]] <- dplyr::recode(metadata_subset[[group_var]],
                                                    control = "non-smoker",
                                                    smoker = "smoker")
    } else {
      warning(paste("Warning: group_var", group_var, "not found in metadata. Skipping recode."))
    }
    
    if (!is.null(group_levels)) {
      group_levels <- dplyr::recode(group_levels,
                                    control = "non-smoker",
                                    smoker = "smoker")
    }
  }
  
  # Add group label with sample count
  group_counts <- metadata_subset %>%
    dplyr::count(.data[[group_var]]) %>%
    dplyr::mutate(label = paste(.data[[group_var]], paste0("(n = ", n, ")"), sep = "\n"))
  
  metadata_subset <- metadata_subset %>%
    left_join(group_counts, by = group_var)
  
  abundance_data <- merge(abundance_data, metadata_subset,
                          by.x = "samples", by.y = "sample_name", all.x = TRUE)
  
  # Set factor levels if provided
  if (!is.null(group_levels)) {
    abundance_data[[group_var]] <- factor(abundance_data[[group_var]], levels = group_levels)
    abundance_data$label <- factor(abundance_data$label,
                                   levels = group_counts$label[match(group_levels, group_counts[[group_var]])])
  }
  
  # Assign colors if provided
  if (!is.null(genus_colors)) {
    abundance_data <- abundance_data %>% mutate(Color_Genus = genus_colors[Genus])
  }
  if (!is.null(family_colors)) {
    abundance_data <- abundance_data %>% mutate(Color_Family = family_colors[Family])
  }
  if (!is.null(phyla_colors)) {
    abundance_data <- abundance_data %>% mutate(Color_Phyla = phyla_colors[Phyla])
  }
  
  # Drop unused levels
  drop_levels <- setdiff(c("Phyla", "Species", "Genus"), level)
  color_columns <- paste0("Color_", drop_levels)
  abundance_data <- abundance_data[, !(names(abundance_data) %in% c(drop_levels, color_columns))]
  
  # Collapse abundance at selected level
  abundance_data$RA <- with(abundance_data, ave(RA, get(level), label, samples, FUN = sum))
  abundance_data <- unique(abundance_data)
  
  # Define colors
  color_col <- paste0("Color_", level)
  abundance_data[[color_col]] <- ifelse(abundance_data$RA > low_abundance_threshold,
                                        colors_list[abundance_data[[level]]], "gray")
  
  abundance_data$Legend_Taxon <- ifelse(abundance_data$RA < low_abundance_threshold,
                                        paste0(level, " < ", low_abundance_threshold),
                                        abundance_data[[level]])
  
  legend_colors <- setNames(abundance_data[[color_col]], abundance_data$Legend_Taxon)
  legend_colors[paste0(level, " < ", low_abundance_threshold)] <- "gray"
  legend_colors <- legend_colors[!duplicated(names(legend_colors))]
  
  # Conditionally add 'Contamination' only if total RA < 100 (by more than small tolerance)
  total_RA <- abundance_data %>%
    group_by(samples, label) %>%
    summarise(total_RA = sum(RA), .groups = "drop")
  
  tolerance <- 1e-3  # ~0.001%
  if (any(total_RA$total_RA < (100 - tolerance))) {
    missing_RA <- total_RA %>%
      filter(total_RA < (100 - tolerance)) %>%
      mutate(RA = 100 - total_RA,
             !!level := "Contamination",
             Legend_Taxon = "Contamination",
             !!color_col := "ivory")
    
    others_rows <- missing_RA %>%
      dplyr::select(samples, label, RA, all_of(level), Legend_Taxon, all_of(color_col))
    
    colnames(others_rows) <- colnames(abundance_data)[match(colnames(others_rows), colnames(abundance_data))]
    
    abundance_data <- bind_rows(abundance_data, others_rows)
    legend_colors["Contamination"] <- "ivory"
    legend_colors <- legend_colors[!duplicated(names(legend_colors))]
  }
  
  # Plot
  p <- ggplot(abundance_data, aes(x = samples, y = RA)) +
    geom_bar(aes(fill = Legend_Taxon), stat = "identity", width = 0.9, colour = "black") +
    ylab(paste(kingdom_label, "Relative Abundance [%]")) +
    xlab("") +
    scale_fill_manual(paste(level, "clade"),
                      values = legend_colors,
                      drop = FALSE,
                      guide = guide_legend(reverse = TRUE)) +
    facet_grid(~label, scales = "free", switch = "x") +
    theme_classic() +
    theme(
      strip.placement = "outside",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text.x = element_text(size = 20),
      axis.text = element_text(size = 20),
      axis.title = element_text(size = 20, face = "bold"), 
      legend.text = element_text(size = 20),
      legend.title = element_text(size = 20, face = "bold"), 
      axis.title.y = element_text(size = 20)
    )
  
  return(list(plot = p, abundance_data = abundance_data))
}
