# ggcomplex <img src="man/figures/logo.png" align="right" height="139" />

> Complex composite plots built on **ggplot2** + **ggtree** + **aplot**

[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen)](#)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R ≥ 4.1.0](https://img.shields.io/badge/R-%E2%89%A5%204.1.0-blue)](#)

**ggcomplex** is an R package for building complex composite figures —
heatmaps with dendrograms, side annotations, bubble charts, circular heatmaps
with chord diagrams, and more — using a pipeline-style API built on top of
the ggplot2 grammar of graphics.

---

## Features

- **Three main plot types**: tile heatmap, dot/bubble plot, and circular heatmap
- **Hierarchical clustering** with dendrogram rendering via ggtree
- **8 annotation geometries**: bar, tile, dot, boxplot, violin, line, text, histogram
- **Annotations on all 4 sides**: left, right, top, bottom
- **Circular heatmaps**: sector splits, inner chord-diagram links, outer annotation rings
- **26 categorical + 17 continuous built-in palettes** (including ggsci journal palettes)
- **Pipeline API** (`%>%`): build complex figures step by step
- **Automatic axis alignment** across all sub-plots via aplot

---

## Installation

```r
# 1. Install ggtree from Bioconductor (required dependency)
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("ggtree")

# 2. Install ggcomplex from GitHub
devtools::install_github("YRD666/ggcomplex")
```

---

## Quick Start

### Heatmap with dendrograms and annotations

```r
library(ggcomplex)
library(ggplot2)

mat <- matrix(rnorm(200), nrow = 20,
              dimnames = list(paste0("Gene", 1:20), paste0("Sample", 1:10)))

col_meta <- data.frame(
  Sample = colnames(mat),
  Group  = rep(c("Control", "Treatment"), 5)
)

row_meta <- data.frame(
  Gene  = rownames(mat),
  Score = runif(20, 0, 10)
)

gg_heatmap(mat, fill_palette = gc_palette("RdBu")) %>%
  add_row_tree(dist_method = "correlation", k = 3) %>%
  add_col_tree() %>%
  add_col_tile(col_meta, aes(x = Sample, y = 1, fill = Group),
               palette = gc_cat_palette("lancet", 2)) %>%
  add_row_bar(row_meta, aes(x = Gene, y = Score),
              fill = "steelblue", anno_title = "Score")
```

### Dot / bubble plot

```r
pmat <- matrix(runif(200, 0.001, 0.1), nrow = 20, dimnames = dimnames(mat))

gg_dotplot(mat, size_mat = -log10(pmat),
           color_palette = gc_palette("PuOr"),
           color_label = "Z-score",
           size_label  = "-log10(p)") %>%
  add_row_tree() %>%
  add_col_tree()
```

### Circular heatmap with chord links

```r
circ_mat <- matrix(rnorm(8 * 24), 8, 24,
                   dimnames = list(paste0("Gene", 1:8), paste0("S", 1:24)))

groups <- setNames(rep(c("A", "B", "C"), c(8, 8, 8)), colnames(circ_mat))

gg_circular(circ_mat,
            fill_palette = gc_palette("RdBu"),
            col_split = groups, split_gap = 1.5,
            heatmap_border_color = "grey20",
            gap_degree = 40, inner_radius = 4) %>%
  add_circular_tree(which = "col") %>%
  add_outer_ring(data.frame(sample = colnames(circ_mat),
                            group = groups),
                 col_var = "sample", fill_var = "group",
                 geom = "tile", height = 0.6)
```

---

## Main Plot Constructors

| Function | Description |
|:---|:---|
| `gg_heatmap()` | Tile heatmap from a numeric matrix (supports circle cells, value overlay, split groups) |
| `gg_dotplot()` | Bubble chart encoding colour + size from two matrices |
| `gg_matrix()` | Fully custom main plot — bring your own geom and mapping |
| `gg_circular()` | Circular (polar) heatmap with sector splits and gap openings |

## Dendrograms

| Function | Description |
|:---|:---|
| `add_row_tree()` | Cluster rows and attach a left-side dendrogram |
| `add_col_tree()` | Cluster columns and attach a top dendrogram |
| `add_circular_tree()` | Add a circular dendrogram inside a polar heatmap |

Supports all `stats::dist()` methods plus `"correlation"`, `"spearman"`, `"kendall"`, and custom distance functions. Branch colouring by `k` clusters is built in.

## Annotation Tracks (8 geometries × 4 sides)

| Row-side | Column-side | Geometry |
|:---|:---|:---|
| `add_row_bar()` | `add_col_bar()` | Bar chart |
| `add_row_tile()` | `add_col_tile()` | Colour strip |
| `add_row_dot()` | `add_col_dot()` | Dot / point plot |
| `add_row_box()` | `add_col_box()` | Boxplot |
| `add_row_violin()` | `add_col_violin()` | Violin plot |
| `add_row_line()` | `add_col_line()` | Line chart |
| `add_row_text()` | `add_col_text()` | Text labels |
| `add_row_histogram()` | `add_col_histogram()` | Histogram |

Or use the unified interface `geom_add()` to attach any annotation type to any side with a single function.

## Circular-Specific

| Function | Description |
|:---|:---|
| `add_outer_ring()` | Add tile / bar / point / text / line / histogram annotation rings |
| `add_inner_links()` | Draw chord-diagram Bézier curves inside the inner circle |

## Colour Palettes

| Function | Count | Description |
|:---|:---|:---|
| `gc_palette()` | 17 | Continuous palettes (7 diverging + 10 sequential) |
| `gc_cat_palette()` | 26 | Categorical palettes (Brewer, ggsci journal, genomics, sci-fi) |
| `gc_palette_names()` | — | Print all available palette names |

```r
gc_palette("RdBu")          # diverging: blue-white-red
gc_palette("viridis")        # sequential: viridis
gc_cat_palette("npg")        # Nature Publishing Group
gc_cat_palette("jco", n = 5) # Journal of Clinical Oncology, 5 colours
```

## Utilities

| Function | Description |
|:---|:---|
| `scale_matrix()` | Pre-process matrix (Z-score, log2, minmax, center, etc.) |
| `set_title()` | Add title / subtitle / caption to the composite figure |
| `subset_rows()` / `subset_cols()` | Subset and rebuild the plot |
| `assemble_plot()` | Get the raw aplot object for manual tweaking |
| `gc_save()` | Save to PDF / PNG / TIFF via `ggplot2::ggsave()` |
| `summary()` | Print a structural overview of the ggcomplex object |

---

## Architecture

ggcomplex follows an **Anchor + Satellites** design pattern:

```
                    ┌─────────────┐
                    │  col tree   │
                    ├─────────────┤
                    │  col tile   │
         ┌──────┬──┼─────────────┼──┬──────────┐
         │      │  │             │  │          │
         │ row  │  │   MAIN      │  │ row bar  │
         │ tree │  │  HEATMAP    │  │          │
         │      │  │             │  │          │
         └──────┴──┼─────────────┼──┴──────────┘
                    │  col box   │
                    └─────────────┘
```

1. **Anchor**: A main plot (heatmap / dotplot / circular) rendered with ggplot2
2. **Satellites**: Dendrograms and annotation tracks attached to any of the 4 sides
3. **Assembly**: All sub-plots are automatically aligned via aplot
4. **Pipeline**: The `%>%` operator lets you build the figure step by step

---

## Vignette

A comprehensive guide with 40+ runnable examples is available:

- **[Online HTML version](https://htmlpreview.github.io/?https://github.com/YRD666/ggcomplex/blob/main/doc/ggcomplex-guide.html)** — click to view directly in browser
- **Source**: [`vignettes/ggcomplex-guide.Rmd`](vignettes/ggcomplex-guide.Rmd)

After installation, view it in R:

```r
vignette("ggcomplex-guide", package = "ggcomplex")
```

---

## Dependencies

| Package | Source | Role |
|:---|:---|:---|
| [ggplot2](https://ggplot2.tidyverse.org/) | CRAN | Core plotting engine |
| [ggtree](https://bioconductor.org/packages/ggtree/) | Bioconductor | Dendrogram rendering |
| [aplot](https://github.com/YuLab-SMU/aplot) | CRAN | Sub-plot alignment and composition |
| [dplyr](https://dplyr.tidyverse.org/) | CRAN | Data manipulation |
| [tidyr](https://tidyr.tidyverse.org/) | CRAN | Matrix → tidy conversion |
| [rlang](https://rlang.r-lib.org/) | CRAN | Tidy evaluation |
| [scales](https://scales.r-lib.org/) | CRAN | Colour scale utilities |

---

## License

MIT © 2026 Ruidong Yang
