#' Create a new ggcomplex object
#'
#' Internal constructor for the S3 class `ggcomplex`. This object carries all
#' state needed to assemble a composite plot: the original matrix, tidy data,
#' individual ggplot layers, and layout metadata.
#'
#' @param mat Numeric matrix (rows = features, cols = samples).
#' @param tidy Tidy (long-format) data frame derived from `mat`.
#' @return A `ggcomplex` S3 object.
#' @keywords internal
new_ggcomplex <- function(mat, tidy) {
  structure(
    list(
      data = list(
        original = mat,
        tidy     = tidy,
        row_meta = NULL,
        col_meta = NULL
      ),
      plots = list(
        main   = NULL,
        top    = list(),
        bottom = list(),
        left   = list(),
        right  = list()
      ),
      layout = list(
        row_tree  = NULL,
        col_tree  = NULL,
        row_order = rownames(mat),
        col_order = colnames(mat)
      ),
      params = list(
        side_widths  = list(left = 0.15, right = 0.25),
        side_heights = list(top = 0.08, bottom = 0.08)
      )
    ),
    class = "ggcomplex"
  )
}


#' Test if an object is a ggcomplex
#'
#' @param x An R object.
#' @return Logical.
#' @keywords internal
is_ggcomplex <- function(x) {
  inherits(x, "ggcomplex")
}


#' Validate a ggcomplex object
#'
#' Checks structural integrity before assembly.
#'
#' @param x A `ggcomplex` object.
#' @return `x` (invisibly), or raises an error.
#' @keywords internal
validate_ggcomplex <- function(x) {
  if (!is_ggcomplex(x)) {
    rlang::abort("`x` must be a <ggcomplex> object.")
  }
  if (is.null(x$data$original)) {
    rlang::abort("The ggcomplex object does not contain a data matrix.")
  }
  if (is.null(x$plots$main)) {
    rlang::abort("No main plot has been generated yet. Call `gg_heatmap()` first.")
  }
  invisible(x)
}
