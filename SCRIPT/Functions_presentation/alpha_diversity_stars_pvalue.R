#new for smoking group
alpha_diversity_analysis <- function(species_df, metadata_df, metric = c("richness", "diversity", "evenness", "simpson", "invsimpson"), 
                                     group_var, group_levels, color_values, y_expand = 1.3, tag_label = NULL) {
  library(vegan)
  library(ggplot2)
  library(dplyr)
  library(ggsignif)
  library(FSA)  # For Dunn test if Kruskal
  library(multcompView)
  
  metric <- match.arg(metric)
  
  # Compute alpha diversity metrics
  richness_mat <- specnumber(species_df, MARGIN = 2)
  shannon <- vegan::diversity(species_df, index = "shannon", MARGIN = 2)
  d <- exp(shannon)
  j <- d / richness_mat
  simpson <- vegan::diversity(species_df, index = "simpson", MARGIN = 2)
  si <- exp(simpson)
  invsi <- 1/exp(simpson)
  
  metric_vec <- switch(metric,
                       richness = as.numeric(richness_mat),
                       diversity = as.numeric(d),
                       evenness = as.numeric(j),
                       simpson = as.numeric(si),
                       invsimpson = as.numeric(invsi))
  
  # Prepare data
  data_for_analysis <- data.frame(Sample = colnames(species_df),
                                  Metric = metric_vec)
  data_for_analysis <- merge(data_for_analysis, metadata_df, 
                             by.x = "Sample", by.y = "sample_name", all.x = TRUE)
  data_for_analysis[[group_var]] <- factor(data_for_analysis[[group_var]], levels = group_levels)
  
  # Normality test
  normality_tests <- lapply(levels(data_for_analysis[[group_var]]), function(g) {
    vals <- data_for_analysis$Metric[data_for_analysis[[group_var]] == g]
    if(length(unique(vals)) >= 3 && length(vals) >= 3) {
      shapiro <- shapiro.test(vals)
      return(data.frame(Group = g, W = shapiro$statistic, p_value = shapiro$p.value))
    } else {
      return(data.frame(Group = g, W = NA, p_value = NA))
    }
  })
  normality_df <- do.call(rbind, normality_tests)
  all_normal <- all(normality_df$p_value > 0.05, na.rm = TRUE)
  n_groups <- length(levels(data_for_analysis[[group_var]]))
  
  comparisons <- NULL
  signif_labels <- NULL
  y_positions <- NULL
  
  if (n_groups == 2) {
    if (all_normal) {
      test_result <- t.test(Metric ~ get(group_var), data = data_for_analysis)
      test_name <- "t-test"
    } else {
      test_result <- wilcox.test(Metric ~ get(group_var), data = data_for_analysis)
      test_name <- "Wilcoxon test"
    }
    p_value <- test_result$p.value
    comparisons <- list(levels(data_for_analysis[[group_var]]))  # e.g., list(c("control", "smoker"))
    signif_labels <- cut(p_value, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns"))
    y_positions <- max(data_for_analysis$Metric, na.rm = TRUE) * 1.1
  } else if (n_groups > 2) {
    if (all_normal) {
      test_result <- aov(Metric ~ get(group_var), data = data_for_analysis)
      test_name <- "ANOVA"
      posthoc <- TukeyHSD(test_result)
      tukey_df <- as.data.frame(posthoc[[1]])
      tukey_df$comparison <- rownames(tukey_df)
      tukey_df$p_adj <- tukey_df$`p adj`
      
      tukey_df$group1 <- sub(" - .*", "", tukey_df$comparison)
      tukey_df$group2 <- sub(".* - ", "", tukey_df$comparison)
      comparisons <- Map(c, tukey_df$group1, tukey_df$group2)
      signif_labels <- cut(tukey_df$p_adj, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns"))
      y_positions <- max(data_for_analysis$Metric, na.rm = TRUE) * 1.05 + seq_along(comparisons) * 0.05 * max(data_for_analysis$Metric, na.rm = TRUE)
      
    } else {
      test_result <- kruskal.test(Metric ~ get(group_var), data = data_for_analysis)
      test_name <- "Kruskal-Wallis"
      posthoc <- dunnTest(Metric ~ get(group_var), data = data_for_analysis, method = "bh")
      dunn_df <- posthoc$res
      dunn_df$group1 <- sub(" - .*", "", dunn_df$Comparison)
      dunn_df$group2 <- sub(".* - ", "", dunn_df$Comparison)
      comparisons <- Map(c, dunn_df$group1, dunn_df$group2)
      signif_labels <- cut(dunn_df$P.adj, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), labels = c("***", "**", "*", "ns"))
      y_positions <- max(data_for_analysis$Metric, na.rm = TRUE) * 1.05 + seq_along(comparisons) * 0.05 * max(data_for_analysis$Metric, na.rm = TRUE)
    }
  } else {
    stop("Grouping variable must have at least 2 levels")
  }
  
  y_label <- switch(metric,
                    richness = "Species Richness",
                    diversity = "Shannon Diversity",
                    evenness = "Evenness (Hill's Ratio)",
                    simpson = "Simpson Diversity",
                    invsimpson = "Inverse Simpson Diversity")
  
  # Format the p-value for annotation
  p_val <- ifelse(test_result$p.value < 0.001, "< 0.001", signif(test_result$p.value, digits = 3))
  p_text <- paste0(test_name, " p = ", p_val)
  
  plot <- ggplot(data_for_analysis, aes_string(x = group_var, y = "Metric")) +
    geom_boxplot(aes_string(fill = group_var), outlier.shape = NA) +
    geom_jitter(width = 0.1, size = 4, shape = 16, color = "black", alpha = 0.7) +
    scale_fill_manual(values = color_values) +
    scale_y_continuous(
      limits = c(0, max(data_for_analysis$Metric, na.rm = TRUE) * y_expand),  # make taller
      expand = expansion(mult = c(0, 0.05))  # small space at bottom
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      strip.background = element_blank(),
      axis.text.x = element_text(size = 35),
      axis.title.y = element_text(size = 35)
    ) +
    labs(x = NULL, y = y_label) +
    annotate("text", x = Inf, y = Inf, 
             label = p_text, hjust = 1.1, vjust = 1.5, 
             size = 9, fontface = "italic")
  
  # Add significance annotations
  #if (!is.null(comparisons) && !is.null(signif_labels)) {
  # plot <- plot +
  #  ggsignif::geom_signif(
  #   comparisons = comparisons,
  #  annotations = as.character(signif_labels),
  # y_position = y_positions,
  #tip_length = 0.01,
  #textsize = 12
  #)
  #}
  if (!is.null(tag_label)) {
    plot <- plot +
      labs(tag = tag_label) +
      theme(
        plot.tag = element_text(face = "bold", size = 30),
        plot.tag.position = c(0, 1.05),
        plot.margin = ggplot2::margin(t = 40, r = 20, b = 20, l = 40)
      )
  }
  
  return(list(
    plot = plot,
    test = test_result,
    test_name = test_name,
    normality = normality_df, 
    pval = p_val
  ))
}
