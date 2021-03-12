#' is_correct
#'
#' Wrapper for \code{is.submodel} that changes the handling of \code{NA}s and \code{NULL}s
#'
#' @param a a CNA solution
#' @param b a CNA solution
#'
#' @details A wrapper of \code{is.submodel} for use in checking the correctness
#'   of solutions created and stored programmatically (such that empty solutions
#'   may get converted to \code{NA}s or \code{NULL}s). Returns \code{TRUE} if first
#'   argument is \code{NA} or \code{NULL}.
#'
#' @returns Logical

#' @export
is_correct <- function(a,b){
  if (is.na(a) | is.null(a)){TRUE} else {
    is.submodel(a,b)
  }
}
