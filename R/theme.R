# ===========================================================================
#  Side-panel themes
# ===========================================================================

#' Minimal theme for side-panel plots
#'
#' Strips axis text, titles, ticks, and background from a ggplot so that it
#' can serve as a clean annotation track next to the main heatmap.
#'
#' @param base_size Base font size.
#' @param keep_legend Keep the legend?
#'
#' @return A [ggplot2::theme] object.
#' @export
theme_clean_side <- function(base_size = 8, keep_legend = TRUE,
                             keep_axis = FALSE, axis_side = "x") {
  th <- ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      plot.margin  = ggplot2::margin(0, 0, 0, 0),
      axis.title   = ggplot2::element_blank(),
      axis.text    = ggplot2::element_blank(),
      axis.ticks   = ggplot2::element_blank(),
      panel.grid   = ggplot2::element_blank()
    )

  if (keep_axis) {
    if ("x" %in% axis_side) {
      th <- th + ggplot2::theme(
        axis.text.x  = ggplot2::element_text(size = base_size - 1,
                                                angle = 45, hjust = 1, vjust = 1),
        axis.ticks.x = ggplot2::element_line(),
        axis.ticks.length.x = ggplot2::unit(1.5, "pt")
      )
    }
    if ("y" %in% axis_side) {
      th <- th + ggplot2::theme(
        axis.text.y         = ggplot2::element_text(size = base_size - 1),
        axis.text.y.right   = ggplot2::element_text(size = base_size - 1,
                                                     margin = ggplot2::margin(l = 1)),
        axis.ticks.y        = ggplot2::element_line(),
        axis.ticks.length.y = ggplot2::unit(1.5, "pt")
      )
    }
  }

  if (!keep_legend) {
    th <- th + ggplot2::theme(legend.position = "none")
  }
  th
}


# ===========================================================================
#  Built-in continuous colour palettes (for heatmaps / dotplots)
# ===========================================================================

#' Built-in colour palettes for heatmaps
#'
#' Returns a colour vector (length 2 or 3) suitable for `fill_palette` /
#' `color_palette` arguments in [gg_heatmap()] and [gg_dotplot()].
#'
#' @param name Palette name. One of:
#'   - **Diverging (3-colour)**: `"RdBu"`, `"RdYlBu"`, `"PiYG"`, `"BrBG"`,
#'     `"PuOr"`, `"coolwarm"`, `"spectral"`
#'   - **Sequential (2-colour)**: `"viridis"`, `"magma"`, `"inferno"`,
#'     `"plasma"`, `"YlOrRd"`, `"Blues"`, `"Greens"`, `"Reds"`, `"Greys"`,
#'     `"heat"`
#'
#' @return A character vector of colours.
#'
#' @examples
#' gc_palette("RdBu")
#' gc_palette("viridis")
#'
#' @export
gc_palette <- function(name = "RdBu") {
  pals <- list(
    RdBu     = c("#2166AC", "#F7F7F7", "#B2182B"),
    RdYlBu   = c("#4575B4", "#FFFFBF", "#D73027"),
    PiYG     = c("#4DAC26", "#F7F7F7", "#D01C8B"),
    BrBG     = c("#01665E", "#F5F5F5", "#8C510A"),
    PuOr     = c("#2D004B", "#F7F7F7", "#7F3B08"),
    coolwarm = c("#3B4CC0", "#F7F7F7", "#B40426"),
    spectral = c("#2B83BA", "#FFFFBF", "#D7191C"),

    viridis  = c("#440154", "#FDE725"),
    magma    = c("#000004", "#FCFDBF"),
    inferno  = c("#000004", "#FCFFA4"),
    plasma   = c("#0D0887", "#F0F921"),
    YlOrRd   = c("#FFFFCC", "#BD0026"),
    Blues    = c("#F7FBFF", "#08306B"),
    Greens   = c("#F7FCF5", "#00441B"),
    Reds     = c("#FFF5F0", "#67000D"),
    Greys    = c("#FFFFFF", "#000000"),
    heat     = c("#FFEDA0", "#F03B20")
  )

  if (!name %in% names(pals)) {
    available <- paste(names(pals), collapse = ", ")
    rlang::abort(paste0("Unknown palette '", name, "'. Available: ", available))
  }
  pals[[name]]
}


# ===========================================================================
#  Built-in categorical colour palettes (for annotations & trees)
# ===========================================================================

#' Built-in categorical colour palettes
#'
#' Returns a character vector of discrete colours for annotations, cluster
#' colouring, and grouping. Includes classic RColorBrewer sets **and all
#' ggsci journal / sci-fi palettes** (embedded, no ggsci dependency needed).
#'
#' @param name Palette name (case-sensitive). See **Available palettes**
#'   below.
#' @param n Number of colours to return. If `NULL`, returns the full palette.
#'   If `n` exceeds the palette length, colours are interpolated.
#'
#' @section Available palettes:
#'
#' **RColorBrewer-style:**
#' `"set1"`, `"set2"`, `"set3"`, `"pastel"`, `"dark"`, `"paired"`
#'
#' **ggsci — journal palettes:**
#' `"npg"` (Nature Publishing Group),
#' `"aaas"` (Science / AAAS),
#' `"nejm"` (New England Journal of Medicine),
#' `"lancet"` (The Lancet),
#' `"jama"` (JAMA),
#' `"jco"` (Journal of Clinical Oncology),
#' `"bmj"` (British Medical Journal),
#' `"frontiers"` (Frontiers)
#'
#' **ggsci — database / genomics palettes:**
#' `"d3"` (D3.js),
#' `"igv"` (Integrative Genomics Viewer),
#' `"ucscgb"` (UCSC Genome Browser),
#' `"locuszoom"` (LocusZoom),
#' `"cosmic"` (COSMIC)
#'
#' **ggsci — university palettes:**
#' `"uchicago"` (University of Chicago)
#'
#' **ggsci — sci-fi / pop-culture palettes:**
#' `"startrek"` (Star Trek),
#' `"tron"` (Tron Legacy),
#' `"futurama"` (Futurama),
#' `"rickandmorty"` (Rick and Morty),
#' `"simpsons"` (The Simpsons),
#' `"flatui"` (Flat UI)
#'
#' @return A character vector of hex colour strings.
#'
#' @examples
#' gc_cat_palette("npg")
#' gc_cat_palette("jco", n = 5)
#' gc_cat_palette("simpsons", n = 3)
#'
#' @export
gc_cat_palette <- function(name = "npg", n = NULL) {
  pals <- list(
    # --- RColorBrewer-style ---
    set1   = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
               "#FFFF33", "#A65628", "#F781BF", "#999999"),
    set2   = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854",
               "#FFD92F", "#E5C494", "#B3B3B3"),
    set3   = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3",
               "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9"),
    pastel = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6",
               "#FFFFCC", "#E5D8BD", "#FDDAEC"),
    dark   = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
               "#E6AB02", "#A6761D", "#666666"),
    paired = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
               "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A"),

    # --- ggsci: journal palettes ---
    npg    = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F",
               "#8491B4", "#91D1C2", "#DC0000", "#7E6148", "#B09C85"),
    aaas   = c("#3B4992", "#EE0000", "#008B45", "#631879", "#008280",
               "#BB0021", "#5F559B", "#A20056", "#808180", "#1B1919"),
    nejm   = c("#BC3C29", "#0072B5", "#E18727", "#20854E", "#7876B1",
               "#6F99AD", "#FFDC91", "#EE4C97"),
    lancet = c("#00468B", "#ED0000", "#42B540", "#0099B4", "#925E9F",
               "#FDAF91", "#AD002A", "#ADB6B6", "#1B1919"),
    jama   = c("#374E55", "#DF8F44", "#00A1D5", "#B24745", "#79AF97",
               "#6A6599", "#80796B"),
    jco    = c("#0073C2", "#EFC000", "#868686", "#CD534C", "#7AA6DC",
               "#003C67", "#8F7700", "#3B3B3B", "#A73030", "#4A6990"),
    bmj    = c("#2A6EBB", "#F0AB00", "#C50084", "#7D5CC6", "#E37222",
               "#69BE28", "#00B2A9", "#CD202C", "#747678"),
    frontiers = c("#D51317", "#F39200", "#EFD500", "#95C11F", "#007B3D",
                  "#31B7BC", "#0094CD"),

    # --- ggsci: database / genomics palettes ---
    d3     = c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
               "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF"),
    igv    = c("#5050FF", "#CE3D32", "#749B58", "#F0E685", "#466983",
               "#BA6338", "#5DB1DD", "#802268", "#6BD76B", "#D595A7"),
    ucscgb = c("#FF0000", "#FF9900", "#FFCC00", "#00FF00", "#6699FF",
               "#CC33FF", "#99991E", "#999999", "#FF00CC", "#CC0000"),
    locuszoom = c("#D43F3A", "#EEA236", "#5CB85C", "#46B8DA", "#357EBD",
                  "#9632B8", "#B8B8B8"),
    cosmic = c("#2E2A2B", "#CF4E9C", "#8C57A2", "#358DB9", "#82581F",
               "#2F509E", "#E5614C", "#97A1A7", "#3DA873", "#DC9445"),

    # --- ggsci: university palettes ---
    uchicago = c("#800000", "#767676", "#FFA319", "#8A9045", "#155F83",
                 "#C16622", "#8F3931", "#58593F", "#350E20"),

    # --- ggsci: sci-fi / pop-culture palettes ---
    startrek = c("#CC0C00", "#5C88DA", "#84BD00", "#FFCD00", "#7C878E",
                 "#00B5E2", "#00AF66"),
    tron   = c("#FF410D", "#6EE2FF", "#F7C530", "#95CC5E", "#D0DFE6",
               "#F79D1E", "#748AA6"),
    futurama = c("#FF6F00", "#C71000", "#008EA0", "#8A4198", "#5A9599",
                 "#FF6348", "#84D7E1", "#FF95A8", "#3D3B25", "#ADE2D0"),
    rickandmorty = c("#FAFD7C", "#82491E", "#24325F", "#B7E4F9", "#FB6467",
                     "#526E2D", "#E762D7", "#E89242", "#FAE48B", "#A6EEE6"),
    simpsons = c("#FED439", "#709AE1", "#8A9197", "#D2AF81", "#FD7446",
                 "#D5E4A2", "#197EC0", "#F05C3B", "#46732E", "#71D0F5"),
    flatui = c("#C0392B", "#D35400", "#F39C12", "#27AE60", "#16A085",
               "#2980B9", "#8E44AD", "#2C3E50", "#7F8C8D", "#BDC3C7")
  )

  if (!name %in% names(pals)) {
    available <- paste(sort(names(pals)), collapse = ", ")
    rlang::abort(paste0(
      "Unknown palette '", name, "'.\nAvailable: ", available
    ))
  }

  colors <- pals[[name]]
  if (!is.null(n)) {
    if (n > length(colors)) {
      colors <- grDevices::colorRampPalette(colors)(n)
    } else {
      colors <- colors[seq_len(n)]
    }
  }
  colors
}


#' Default categorical colour palette (internal)
#'
#' Returns `n` distinct colours for tree cluster colouring.
#'
#' @param n Number of colours needed.
#' @return Character vector of hex colours.
#' @keywords internal
default_cat_palette <- function(n) {
  base <- gc_cat_palette("npg")
  if (n <= length(base)) {
    return(base[seq_len(n)])
  }
  grDevices::colorRampPalette(base)(n)
}


#' List all available palette names
#'
#' Prints a categorised overview of every palette accessible via
#' [gc_palette()] (continuous) and [gc_cat_palette()] (categorical).
#'
#' @return Invisibly returns a list with elements `continuous` and
#'   `categorical`, each a character vector of names.
#' @export
gc_palette_names <- function() {
  cont <- c("RdBu", "RdYlBu", "PiYG", "BrBG", "PuOr", "coolwarm",
            "spectral", "viridis", "magma", "inferno", "plasma", "YlOrRd",
            "Blues", "Greens", "Reds", "Greys", "heat")

  categ <- c("set1", "set2", "set3", "pastel", "dark", "paired",
             "npg", "aaas", "nejm", "lancet", "jama", "jco", "bmj",
             "frontiers", "d3", "igv", "ucscgb", "locuszoom", "cosmic",
             "uchicago", "startrek", "tron", "futurama", "rickandmorty",
             "simpsons", "flatui")

  cat("Continuous palettes (gc_palette):\n")
  cat("  Diverging  :", paste(cont[1:7], collapse = ", "), "\n")
  cat("  Sequential :", paste(cont[8:17], collapse = ", "), "\n")
  cat("\nCategorical palettes (gc_cat_palette):\n")
  cat("  Brewer     :", paste(categ[1:6], collapse = ", "), "\n")
  cat("  Journal    :", paste(categ[7:14], collapse = ", "), "\n")
  cat("  Genomics   :", paste(categ[15:19], collapse = ", "), "\n")
  cat("  University :", categ[20], "\n")
  cat("  Sci-fi     :", paste(categ[21:26], collapse = ", "), "\n")

  invisible(list(continuous = cont, categorical = categ))
}
