#!/usr/bin/env Rscript
# ggcomplex demo — run this script to verify the package works end to end.

library(ggcomplex)

# --- 1. Generate simulated data ----
set.seed(2026)
n_genes   <- 30
n_samples <- 12

mat <- matrix(rnorm(n_genes * n_samples), nrow = n_genes,
              dimnames = list(paste0("Gene", seq_len(n_genes)),
                              paste0("Sample", seq_len(n_samples))))

# Two sample groups
col_meta <- data.frame(
  Sample = colnames(mat),
  Group  = rep(c("Control", "Treatment"), each = n_samples / 2)
)

# A per-gene statistic
row_stat <- data.frame(
  Gene  = rownames(mat),
  LogFC = rnorm(n_genes, mean = 0, sd = 1.5)
)


# --- 2. Build the composite plot ----
p <- gg_heatmap(mat,
                fill_palette = c("#2166AC", "white", "#B2182B"),
                fill_label   = "Expression") %>%
  add_row_tree(hclust_method = "ward.D2") %>%
  add_col_tree(hclust_method = "ward.D2") %>%
  add_col_tile(col_meta,
               ggplot2::aes(x = Sample, y = 1, fill = Group),
               height = 0.04) %>%
  add_row_bar(row_stat,
              ggplot2::aes(x = Gene, y = LogFC, fill = LogFC > 0),
              width = 0.2)


# --- 3. Print ----
print(p)

# --- 4. Inspect structure ----
summary(p)
