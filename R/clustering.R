#' Compute hierarchical clustering
#'
#' Thin wrapper around [stats::hclust()] that returns both the tree and the
#' leaf order. Supports all [stats::dist()] methods plus correlation-based
#' distances and fully custom distance functions.
#'
#' @param mat Numeric matrix.
#' @param direction `"row"` or `"col"`.
#' @param dist_method Distance method passed to [stats::dist()].
#'   Additionally accepts `"correlation"` (Pearson), `"spearman"`, and
#'   `"kendall"` for \eqn{1 - |r|} correlation-based distances.
#'   Defaults to `"euclidean"`.
#' @param hclust_method Agglomeration method passed to [stats::hclust()].
#'   Defaults to `"complete"`.
#' @param dist_fn Optional custom distance function. Should accept a matrix
#'   (rows = observations) and return a [stats::dist] object. When supplied
#'   this overrides `dist_method`.
#' @return A list with components `tree` (hclust object) and `order`
#'   (character vector of names in dendrogram order).
#' @keywords internal
compute_cluster <- function(mat,
                            direction     = c("row", "col"),
                            dist_method   = "euclidean",
                            hclust_method = "complete",
                            dist_fn       = NULL) {
  direction <- match.arg(direction)
  target <- if (direction == "row") mat else t(mat)

  if (!is.null(dist_fn)) {
    d <- dist_fn(target)
    if (!inherits(d, "dist")) {
      d <- stats::as.dist(d)
    }
  } else {
    cor_methods <- c("correlation", "pearson", "spearman", "kendall")
    if (tolower(dist_method) %in% cor_methods) {
      cor_type <- if (tolower(dist_method) == "correlation") "pearson"
                  else tolower(dist_method)
      cor_mat <- stats::cor(t(target), method = cor_type, use = "pairwise.complete.obs")
      d <- stats::as.dist(1 - cor_mat)
    } else {
      d <- stats::dist(target, method = dist_method)
    }
  }

  hc  <- stats::hclust(d, method = hclust_method)

  nms <- if (direction == "row") rownames(mat) else colnames(mat)
  ordered_names <- nms[hc$order]

  list(tree = hc, order = ordered_names)
}
