#' Scale / transform a numeric matrix
#'
#' Pre-processes a matrix before visualisation. This is typically the first
#' step in a ggcomplex pipeline when your raw data needs standardisation.
#'
#' @param mat A numeric matrix.
#' @param method Scaling method:
#'   \describe{
#'     \item{`"none"`}{No transformation (default).}
#'     \item{`"row_zscore"`}{Row-wise Z-score: \eqn{(x - \mu) / \sigma}.}
#'     \item{`"col_zscore"`}{Column-wise Z-score.}
#'     \item{`"log2"`}{\eqn{\log_2(x + 1)}.}
#'     \item{`"log10"`}{\eqn{\log_{10}(x + 1)}.}
#'     \item{`"minmax"`}{Row-wise min-max scaling to [0, 1].}
#'     \item{`"col_minmax"`}{Column-wise min-max scaling to [0, 1].}
#'     \item{`"center"`}{Row-wise centering (subtract the row mean).}
#'     \item{`"col_center"`}{Column-wise centering.}
#'   }
#' @param pseudocount Pseudocount added before log transforms.
#'   Defaults to 1.
#'
#' @return A transformed numeric matrix (same dimensions, same names).
#'
#' @examples
#' mat <- matrix(rpois(200, 50), 20, 10,
#'               dimnames = list(paste0("G", 1:20), paste0("S", 1:10)))
#' scale_matrix(mat, "row_zscore")
#' scale_matrix(mat, "log2")
#'
#' @export
scale_matrix <- function(mat,
                         method      = c("none", "row_zscore", "col_zscore",
                                         "log2", "log10",
                                         "minmax", "col_minmax",
                                         "center", "col_center"),
                         pseudocount = 1) {
  method <- match.arg(method)
  if (method == "none") return(mat)

  switch(method,
    row_zscore = {
      rm <- rowMeans(mat, na.rm = TRUE)
      rs <- apply(mat, 1, stats::sd, na.rm = TRUE)
      rs[rs == 0] <- 1
      (mat - rm) / rs
    },
    col_zscore = {
      cm <- colMeans(mat, na.rm = TRUE)
      cs <- apply(mat, 2, stats::sd, na.rm = TRUE)
      cs[cs == 0] <- 1
      t((t(mat) - cm) / cs)
    },
    log2  = log2(mat + pseudocount),
    log10 = log10(mat + pseudocount),
    minmax = {
      rmin <- apply(mat, 1, min, na.rm = TRUE)
      rmax <- apply(mat, 1, max, na.rm = TRUE)
      rng  <- rmax - rmin
      rng[rng == 0] <- 1
      (mat - rmin) / rng
    },
    col_minmax = {
      cmin <- apply(mat, 2, min, na.rm = TRUE)
      cmax <- apply(mat, 2, max, na.rm = TRUE)
      rng  <- cmax - cmin
      rng[rng == 0] <- 1
      t((t(mat) - cmin) / rng)
    },
    center = {
      mat - rowMeans(mat, na.rm = TRUE)
    },
    col_center = {
      t(t(mat) - colMeans(mat, na.rm = TRUE))
    }
  )
}


#' Convert a matrix to tidy (long) format
#'
#' Transforms a numeric matrix into a three-column tibble suitable for
#' ggplot2 mapping. Row and column names are preserved as ordered factors
#' whose levels follow the original matrix order (or a custom order if
#' clustering has been applied).
#'
#' @param mat A numeric matrix with row names and column names.
#' @param row_order Character vector giving the desired row order.
#'   Defaults to `rownames(mat)`.
#' @param col_order Character vector giving the desired column order.
#'   Defaults to `colnames(mat)`.
#' @return A tibble with columns `row_id`, `col_id`, and `value`.
#' @keywords internal
matrix_to_tidy <- function(mat,
                           row_order = rownames(mat),
                           col_order = colnames(mat)) {
  if (is.null(rownames(mat))) {
    rownames(mat) <- paste0("row_", seq_len(nrow(mat)))
  }
  if (is.null(colnames(mat))) {
    colnames(mat) <- paste0("col_", seq_len(ncol(mat)))
  }

  df <- tibble::as_tibble(mat, rownames = "row_id")
  df <- tidyr::pivot_longer(
    df,
    cols      = -"row_id",
    names_to  = "col_id",
    values_to = "value"
  )

  df$row_id <- factor(df$row_id, levels = row_order)
  df$col_id <- factor(df$col_id, levels = col_order)

  df
}


#' Re-level tidy data after clustering
#'
#' Updates the factor levels in `tidy` to match a new ordering vector.
#'
#' @param tidy A tibble produced by [matrix_to_tidy()].
#' @param new_row_order New row order (character), or `NULL` to keep current.
#' @param new_col_order New column order (character), or `NULL` to keep current.
#' @return Updated tibble.
#' @keywords internal
relevel_tidy <- function(tidy,
                         new_row_order = NULL,
                         new_col_order = NULL) {
  if (!is.null(new_row_order)) {
    tidy$row_id <- factor(tidy$row_id, levels = new_row_order)
  }
  if (!is.null(new_col_order)) {
    tidy$col_id <- factor(tidy$col_id, levels = new_col_order)
  }
  tidy
}
