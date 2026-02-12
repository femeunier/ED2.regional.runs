#' Read individual css, pss, and site files
#'
#' Read files into objects usable by other PEcAn.ED2 utilities, and optionally check for errors.
#' @param filepath Full path to css, pss, or site file
#' @param check Logical. If `TRUE` (default), [check][check_css] that file is valid.
#' @param ... Additional arguments to [check functions][check_css].
#' @return `data.frame` containing
#' @export
read_css <- function(filepath, check = TRUE, ...) {
  css <- utils::read.table(filepath, header = TRUE)
  if (check) {
    check_css(css, ...)
  }
  css
}

#' @rdname read_css
#' @export
read_pss <- function(filepath, check = TRUE) {
  pss <- utils::read.table(filepath, header = TRUE)
  if (check) {
    check_pss(pss)
  }
  pss
}
