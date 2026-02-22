# ggcomplex

Complex composite plots built on ggplot2 + ggtree + aplot.

## Installation

```r
# Install dependencies from Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("ggtree")

# Install from GitHub
devtools::install_github("YOUR_GITHUB_USERNAME/ggcomplex")
```

## Quick Start

```r
library(ggcomplex)

# Prepare a numeric matrix
mat <- matrix(rnorm(200), nrow = 20,
              dimnames = list(paste0("Gene", 1:20), paste0("Sample", 1:10)))

# Build a composite heatmap with row/column dendrograms
p <- gg_heatmap(mat) %>%
  add_row_tree() %>%
  add_col_tree()

# Add column annotation
col_meta <- data.frame(
  Sample = colnames(mat),
  Group  = rep(c("Control", "Treatment"), 5)
)
p <- p %>%
  add_col_tile(col_meta, ggplot2::aes(x = Sample, y = 1, fill = Group))

print(p)
```

## Architecture

ggcomplex follows an **Anchor + Satellites** design:

1. **Anchor**: A main heatmap rendered via `geom_tile()`.
2. **Satellites**: Dendrograms (`ggtree`) and annotation tracks attached to the sides.
3. **Assembly**: All sub-plots are aligned and composed via `aplot`.

The pipeline API (`%>%`) lets you build complex figures step by step while the
package handles axis alignment automatically.

## Key Functions

| Function | Description |
|---|---|
| `gg_heatmap()` | Create the main heatmap from a matrix |
| `add_row_tree()` | Cluster rows and attach a left-side dendrogram |
| `add_col_tree()` | Cluster columns and attach a top dendrogram |
| `add_row_annotation()` | Add an arbitrary ggplot to the right side |
| `add_col_annotation()` | Add an arbitrary ggplot to the top |
| `add_row_bar()` / `add_col_bar()` | Shortcut for bar-chart annotations |
| `add_row_tile()` / `add_col_tile()` | Shortcut for colour-strip annotations |
| `theme_clean_side()` | Minimal theme for annotation panels |

## License

MIT
