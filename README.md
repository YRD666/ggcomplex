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

## Quick Start

### 1. Installation

```r
# Install ggtree from Bioconductor (required dependency)
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("ggtree")

# Install ggcomplex from GitHub
devtools::install_github("YRD666/ggcomplex")
```

### 2. Usage

```r
library(ggcomplex)
library(ggplot2)

mat <- matrix(rnorm(200), nrow = 20,
              dimnames = list(paste0("Gene", 1:20), paste0("Sample", 1:10)))

gg_heatmap(mat, fill_palette = gc_palette("RdBu")) %>%
  add_row_tree(k = 3) %>%
  add_col_tree()
```

For **40+ complete runnable examples** — heatmaps, dot plots, circular heatmaps, annotations, chord diagrams, and more — see the full guide:

**[>>> ggcomplex Guide (HTML) <<<](https://htmlpreview.github.io/?https://github.com/YRD666/ggcomplex/blob/main/doc/ggcomplex-guide.html)**

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

### `geom_add()` — Unified Interface

Instead of remembering 16+ individual `add_row_*` / `add_col_*` functions, you can use the single entry-point `geom_add()` to attach **any** annotation or dendrogram to **any** side:

```r
geom_add(obj, geom, side, data, mapping, ...)
```

| Argument | Description |
|:---|:---|
| `geom` | `"bar"`, `"tile"`, `"dot"`, `"boxplot"`, `"violin"`, `"line"`, `"text"`, `"histogram"`, or `"tree"` |
| `side` | `"left"`, `"right"`, `"top"`, or `"bottom"` |
| `data` | A data frame (ignored for `"tree"`) |
| `mapping` | `aes()` mapping (ignored for `"tree"`) |
| `...` | Passed to the underlying `add_*` function (e.g. `k`, `fill`, `palette`, `show_legend`, `title`, etc.) |

**Example — build a full composite figure with `geom_add()` only:**

```r
mat <- matrix(rnorm(100), 10, 10,
              dimnames = list(paste0("G", 1:10), paste0("S", 1:10)))
meta_r <- data.frame(gene = rownames(mat), score = runif(10))
meta_c <- data.frame(sample = colnames(mat), quality = runif(10, 70, 100))

gg_heatmap(mat) %>%
  geom_add("tree", side = "left", k = 3) %>%
  geom_add("tree", side = "top") %>%
  geom_add("bar", side = "right", data = meta_r,
           mapping = aes(x = gene, y = score),
           fill = "steelblue") %>%
  geom_add("dot", side = "top", data = meta_c,
           mapping = aes(x = sample, y = quality),
           color = "red")
```

`geom_add()` internally dispatches to the correct `add_row_*` or `add_col_*` function based on `geom` and `side`, so you get the same result with less mental overhead.

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

## Dependencies

| Package | Source | Role |
|:---|:---|:---|
| [ggplot2](https://ggplot2.tidyverse.org/) | CRAN | Core plotting engine |
| [ggtree](https://bioconductor.org/packages/ggtree/) | Bioconductor | Dendrogram rendering |
| [aplot](https://github.com/YuLab-SMU/aplot) | CRAN | Sub-plot alignment and composition |
| [tidyr](https://tidyr.tidyverse.org/) | CRAN | Matrix → tidy conversion |
| [rlang](https://rlang.r-lib.org/) | CRAN | Tidy evaluation |
| [scales](https://scales.r-lib.org/) | CRAN | Colour scale utilities |

---

## License

MIT © 2026 Ruidong Yang
