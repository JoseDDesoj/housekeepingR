#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' Extract character part
#'
#' Extract the subset of characters at position inx after splitting at pattern patt.
#' 
#' @param charvec character
#' @param patt character
#' @param inx integer
#' @param ... arguments for strsplit
#' 
#' @return character
#' @export
#'
#' @examples
#' str_extract_pos("split_this_at_patt", "_", 2)
str_extract_pos <- function(charvec, patt, inx, ...) {
  return(as.character(sapply(charvec, function(i) unlist(strsplit(i, patt, ...))[inx])))
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' Open file
#'
#' Open a file in mac OS.
#'
#' @param file character 
#'
#' @return none
#' @export
#'
#' @examples
#' write("test", "test.txt")
#' open_file_mac("test.txt")
open_file_mac <- function(file) {
  system(paste("open", file))
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' Map character vector to colors
#'
#' @param vec character vector
#' @param cpall character
#'
#' @return character vector
#' @export
#'
#' @examples
#' vector_to_colors(c("Yes", "Yes", "No", "No"))
vector_to_colors <- function(vec, cpall="npg") {
  inVec <- as.character(vec)
  out <- ggpubr::get_palette(cpall, length(unique(inVec)))[factor(inVec)]
  names(out) <- inVec
  return(out)
}
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

