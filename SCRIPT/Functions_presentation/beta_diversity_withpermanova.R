#new for smoking group
library(vegan)
library(ggplot2)
library(dplyr)

beta_diversity_analysis <- function(species_df,
                                    metadata_df,
                                    method = c("PCA", "PCoA", "CAP", "NMDS"),
                                    color_var = NULL,
                                    shape_var = NULL,
                                    distance_method = "bray",
                                    ellipse = TRUE,
                                    formula_vars = c("treatment"),
                                    color_values = NULL,
                                    fill_label = NULL, 
                                    tag_label=NULL) {
  method <- match.arg(method)
  
  # Transpose data for vegan functions
  data_transposed <- t(species_df)
  assign("data_transposed", data_transposed, envir = parent.frame())
  
  # Bray-Curtis dissimilarity
  dist_matrix <- vegdist(data_transposed, method = distance_method)
  
  # Construct PERMANOVA formula
  formula_text <- paste("dist_matrix ~", paste(formula_vars, collapse = " + "))
  formula_obj <- as.formula(formula_text)
  
  adonis_result <- adonis2(formula_obj, data = metadata_df, method = distance_method)
  p_value <- adonis_result$`Pr(>F)`[1]
  r_squared <- adonis_result$R2[1]
  stat_label <- paste0("PERMANOVA: p = ", round(p_value, 3), ", R² = ", round(r_squared, 3))
  
  # Ordination methods
  if (method == "PCA") {
    pca_res <- prcomp(data_transposed, scale. = FALSE)
    scores <- as.data.frame(pca_res$x)
    var_exp <- round((pca_res$sdev^2 / sum(pca_res$sdev^2)) * 100, 1)
    xlab <- paste0("PC1 (", var_exp[1], "%)")
    ylab <- paste0("PC2 (", var_exp[2], "%)")
    plot_df <- cbind(scores, metadata_df)
    xcol <- "PC1"
    ycol <- "PC2"
    
  } else if (method == "PCoA") {
    pcoa_res <- cmdscale(dist_matrix, k = 2, eig = TRUE)
    eigen <- pcoa_res$eig
    xlab <- paste0("PCoA 1 (", round((eigen[1] / sum(eigen)) * 100, 1), "%)")
    ylab <- paste0("PCoA 2 (", round((eigen[2] / sum(eigen)) * 100, 1), "%)")
    scores <- as.data.frame(pcoa_res$points)
    colnames(scores) <- c("Axis1", "Axis2")
    plot_df <- cbind(scores, metadata_df)
    xcol <- "Axis1"
    ycol <- "Axis2"
    
  } else if (method == "CAP") {
    cap_formula <- as.formula(paste("dist_matrix ~", paste(formula_vars, collapse = " + ")))
    cap_res <- capscale(cap_formula, data = metadata_df, distance = distance_method)
    scores <- scores(cap_res, display = "sites")
    scores_df <- as.data.frame(scores)
    colnames(scores_df)[1:2] <- c("CAP1", "CAP2")
    plot_df <- cbind(scores_df, metadata_df)
    xcol <- "CAP1"
    ycol <- "CAP2"
    xlab <- "CAP Axis 1"
    ylab <- "CAP Axis 2"
    
  } else if (method == "NMDS") {
    nmds_res <- metaMDS(dist_matrix, k = 2, trymax = 100)
    scores_df <- as.data.frame(nmds_res$points)
    colnames(scores_df) <- c("NMDS1", "NMDS2")
    plot_df <- cbind(scores_df, metadata_df)
    xcol <- "NMDS1"
    ycol <- "NMDS2"
    xlab <- "NMDS Axis 1"
    ylab <- "NMDS Axis 2"
  }
  
  # Build the plot
  p <- ggplot(plot_df, aes_string(x = xcol, y = ycol)) +
    geom_point(aes_string(fill = color_var, shape = shape_var), 
               shape = 21,size = 8, color = "black", stroke = 1.2)  
  #geom_point(aes_string(color = color_var, shape = shape_var), 
  #size = 7, stroke = 1.2)
  
  if (ellipse && !is.null(color_var)) {
    p <- p + stat_ellipse(aes_string(group = color_var, color = color_var), 
                          level = 0.95, linetype = 2, segments = 51, show.legend = FALSE, linewidth = 1.5)
  } 
  p <- p + labs(x = xlab, y = ylab, title = stat_label, fill = fill_label, shape = shape_var) +
    
    #p <- p + labs(x = xlab, y = ylab, title = stat_label, fill = fill_label, shape = shape_var) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_blank(),
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 30),
          legend.text = element_text(size = 35),   # you can adjust
          legend.title = element_text(size = 35),
          legend.position = "bottom")
  
  if (!is.null(color_values)) {
    p <- p + 
      scale_fill_manual(values = color_values) +
      scale_color_manual(values = color_values)
  }
  
  # Fix legends so only fill legend is shown (ellipse lines will use color but no legend)
  p <- p + guides(color = "none") 
  
  p <- p +
    labs(tag = tag_label) +   # use the "tag" feature for panel labels
    theme(
      plot.tag = element_text(size = 20, face = "bold")
    )
  
  p <- p + theme(
    plot.tag = element_text(face = "bold", size = 30),
    plot.tag.position = c(0, 1.05),
    plot.margin = ggplot2::margin(t = 40, r = 20, b = 20, l = 40)  # top, right, bottom, left
  )
  
  return(list(
    plot = p,
    permanova = adonis_result,
    distance_matrix = dist_matrix 
  ))
  
}
