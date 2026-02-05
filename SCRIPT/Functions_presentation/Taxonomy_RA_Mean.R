#New for smoking
plot_relative_abundance_with_means <- function(data_species,
                                               metadata,
                                               level = NULL,
                                               low_abundance_threshold = 1,
                                               colors_list,
                                               genus_colors = NULL,
                                               family_colors = NULL,
                                               phyla_colors = NULL,
                                               kingdom_label = "Bacterial",
                                               group_var = NULL,
                                               group_levels = NULL,
                                               tag_label = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Step 1: Run the base relative abundance function
  result <- plot_relative_abundance(
    data_species = data_species,
    metadata = metadata,
    level = level,
    low_abundance_threshold = low_abundance_threshold,
    colors_list = colors_list,
    genus_colors = genus_colors,
    family_colors = family_colors,
    phyla_colors = phyla_colors,
    kingdom_label = kingdom_label,
    group_var = group_var,
    group_levels = group_levels
  )
  
  phylum_abundance_data <- result$abundance_data
  
  # Step 2: Remove "Contamination" rows
  phylum_abundance_data <- phylum_abundance_data %>%
    filter(.data[[level]] != "Contamination", Legend_Taxon != "Contamination")
  
  # Step 3: Summarize means per group
  phyla_summary <- phylum_abundance_data %>%
    select(-label, -starts_with("Color_"), -Legend_Taxon) %>%
    group_by(samples, .data[[group_var]], .data[[level]]) %>%
    summarise(RA = round(sum(RA, na.rm = TRUE), 1), .groups = "drop") %>%
    mutate(Legend_Taxon = ifelse(RA < low_abundance_threshold,
                                 paste0(level, " < ", low_abundance_threshold),
                                 .data[[level]]))
  
  mean_df_summary <- phyla_summary %>%
    group_by(.data[[group_var]], Legend_Taxon) %>%
    summarise(mean_RA = mean(RA), .groups = "drop") %>%
    group_by(.data[[group_var]]) %>%
    mutate(norm_RA = 100 * mean_RA / sum(mean_RA)) %>%
    ungroup()
  
  # Step 4: Format both datasets for plotting
  p_data <- phylum_abundance_data %>%
    mutate(plot_type = "Detailed samples") %>%
    mutate(samples = factor(samples, levels = unique(samples)))
  
  q_data <- mean_df_summary %>%
    rename(RA = norm_RA) %>%
    mutate(samples = .data[[group_var]],
           plot_type = "Mean by group",
           label = paste("Mean", .data[[group_var]], sep = "\n"))
  
  # Step 5: Combine data for plotting
  combined_data <- bind_rows(
    p_data %>% select(samples, RA, Legend_Taxon, plot_type, label),
    q_data %>% select(samples, RA, Legend_Taxon, plot_type, label)
  )
  
  # ---- Build legend colors (including low abundance + contamination) ----
  legend_colors <- colors_list
  
  low_label <- paste0(level, " < ", low_abundance_threshold)
  
  if (!low_label %in% names(legend_colors)) {
    legend_colors[low_label] <- "gray"
  }
  
  if ("Contamination" %in% combined_data$Legend_Taxon) {
    legend_colors["Contamination"] <- "ivory"
  }
  
  legend_colors <- legend_colors[names(legend_colors) %in% combined_data$Legend_Taxon]
  
  # Set factor levels for consistent facet ordering
  unique_labels <- unique(c(
    unique(p_data$label),
    unique(q_data$label)
  ))
  combined_data$label <- factor(combined_data$label, levels = unique_labels)
  combined_data$samples <- factor(combined_data$samples, levels = unique(combined_data$samples))
  
  # Step 6: Plot
  taxonomy_plot <- ggplot(combined_data, aes(x = samples, y = RA, fill = Legend_Taxon)) +
    geom_bar(stat = "identity", width = 0.9, colour = "black") +
    facet_grid(~label, scales = "free_x", space = "fixed", switch = "x") +
    ylab("Relative Abundance [%]") +
    xlab("") +
    scale_fill_manual(
      paste(level, " clade"),
      values = legend_colors,
      drop = FALSE,
      guide = guide_legend(reverse = TRUE)
    ) +
    theme_classic() +
    theme(
      strip.placement = "outside",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text.x = element_text(size = 24),
      axis.text = element_text(size = 24),
      axis.title = element_text(size = 24, face = "bold"),
      legend.text = element_text(size = 28, face = ifelse(level %in% c("Species", "Genus", "Phyla"), "italic", "plain")
      ),
      legend.title = element_text(size = 30, face = "bold"),
      axis.title.y = element_text(size = 23)
    )
  
  if (!is.null(tag_label)) {
    taxonomy_plot <- taxonomy_plot +
      labs(tag = tag_label) +
      theme(
        plot.tag = element_text(face = "bold", size = 30),
        plot.tag.position = c(0, 1.05),
        plot.margin = ggplot2::margin(t = 40, r = 20, b = 20, l = 40)
      )
  }
  
  return(list(plot = taxonomy_plot,
              combined_data = combined_data,
              abundance_data = phylum_abundance_data))
}
