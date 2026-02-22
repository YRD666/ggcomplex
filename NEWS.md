# ggcomplex 0.1.0

## New features

* Three main plot constructors: `gg_heatmap()`, `gg_dotplot()`, `gg_matrix()`.
* Circular (polar) heatmap via `gg_circular()` with sector splits, inner chord
  links (`add_inner_links()`), and outer annotation rings (`add_outer_ring()`).
* Hierarchical clustering with dendrogram rendering:
  `add_row_tree()`, `add_col_tree()`, `add_circular_tree()`.
* Eight annotation geometries for all four sides: bar, tile, dot, boxplot,
  violin, line, text, histogram.
* Unified annotation interface: `geom_add()`.
* 17 continuous + 26 categorical built-in colour palettes
  (`gc_palette()`, `gc_cat_palette()`), including ggsci journal palettes.
* Matrix pre-processing: `scale_matrix()` with 9 methods
  (Z-score, log2, minmax, centering, etc.).
* Utilities: `set_title()`, `subset_rows()`, `subset_cols()`, `gc_save()`,
  `assemble_plot()`.
