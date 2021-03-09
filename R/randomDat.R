#' randomDat
#'
#' @description Simulate a data set from a randomly generated CNA solution in
#'   one go
#'
#' @param x Positive integer determining the number of factors in the created
#'   data
#' @param condtype Character vector determining if the data-generating structure
#'   (DGS) can be a csf or not, defaults to csf. If "csf", \code{randomCsf} is
#'   used to generate the DGS, otherwise \code{randomAsf}
#' @param ... Additional arguments passed to \code{randomConds} and
#'   \code{selectCases}
#'
#' @details Primarily a replacement for a snippet
#'   \code{ct2df(selectCases(randomConds(x)))}. By default produces a data set
#'   over five crisp-set factors from a randomly generated csf. Abuses
#'   \code{...} to pass additional arguments to randomConds and selectCases.
#'   Only valid arguments are passed, will not brake if given unused arguments.
#'

#' @import cna
#' @export
randomDat <- function(x = 5, condtype = c("csf", "asf"), ...){
  call <- match.call()
  ctype <- match.arg(condtype)
  margs <- list(...)

  if (ctype == "csf"){
    randomCondsArgs <- margs[which(names(margs) %in% c("outcome", "n.asf", "compl"))]
    if("how" %in% names(margs)){warning("'condtype' set to '", ctype, "' ignoring incompatible argument 'how'")}
  }

  if (ctype == "asf"){
    randomCondsArgs <- margs[which(names(margs) %in% c("outcome", "how", "compl"))]
    if("n.asf" %in% names(margs)){warning("'condtype' set to '", ctype, "' ignoring incompatible argument 'n.asf")}
  }

  selectCasesArgs <- margs[which(names(margs) %in% c("type", "cutoff",
            "rm.dup.factors", "rm.const.factors"))]
  x <- if(any(class(x) %in% c("data.frame", "configTable"))){x} else {full.ct(x)}
  x <- list(x = x)
  rargs <- if(length(randomCondsArgs) < 1){x} else {c(x, randomCondsArgs)}
  mod <- if(ctype == "csf"){do.call(cna::randomCsf, rargs)}else{
    do.call(cna::randomAsf, rargs)
    }

  sargs <- if(length(selectCasesArgs) < 1){c(x, list(mod))} else {c(x, mod, selectCasesArgs)}
  ct <- do.call(cna::selectCases, sargs)
  df <- cna::ct2df(ct)
  if (ncol(df) < ncol(x[[1]])){df <- eval.parent(call)}
  attributes(df)$target <- mod
  return(df)
}
