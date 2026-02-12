check_ed_metheader <- function (ed_metheader, check_files = TRUE)
{
  testthat::test_that("ED met header object is a nested list",
                      {
                        testthat::expect_true(!is.null(names(ed_metheader[[1]])))
                      })
  .z <- lapply(ed_metheader, check_ed_metheader_format, check_files = check_files)
  invisible(TRUE)
}
