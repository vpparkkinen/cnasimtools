#' @export
is_correct <- function(a,b){
  if (is.na(a) | is.null(a)){TRUE} else {
    is.submodel(a,b)
  }
}
