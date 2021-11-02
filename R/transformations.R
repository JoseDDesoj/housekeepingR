#' Scale rows
#'
#' Scales the rows of a numeric matrix.
#'
#' @param mat matrix 
#'
#' @return matrix
#' @export
#'
#' @examples
#' x <- rnorm(100, 10, 10)
#' scale_rows(x)
scale_rows <- function(mat) {
  return(t(scale(t(mat))))
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' Min max normalization
#'
#' @param x numeric vector
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' min_max_normalize(rnorm(10))
min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' Sigmoid transform
#'
#' @param x numeric vector
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' sigmoid_transform(rnorm(10))
sigmoid_transform <- function(x) {
  return(1/(1+exp(-x)))
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' Binarize numeric vector
#' 
#' Binarize a numeric vector using kmeans clustering (k=2)
#'
#' @param vec numeric
#'
#' @return vec numeric
#' @export
#'
#' @examples
#' binarize(rnorm(100))
binarize <- function(vec) {
  X <- stats::kmeans(vec, 2)
  out <- X$cluster*0
  out[X$cluster==which.max(as.numeric(X$centers))] <- 1
  return(out)
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::