#' randomDat
#'
#' @description Simulate a data set from a randomly generated CNA solution in
#'   one go
#'
#' @param x Positive scalar integer, data.frame of binary crisp set data, or a
#'   configTable object, determining the number of factors in the created data
#'   as well as their type
#' @param condtype Character vector determining if the data-generating structure
#'   (DGS) can be a csf or not, defaults to csf. If "csf", \code{randomCsf} is
#'   used to generate the DGS, otherwise \code{randomAsf}
#' @param fuzzy Logical; if \code{TRUE}, fuzzy-set data will be generated
#'
#' @param ... Additional arguments passed to \code{randomConds},
#'   \code{selectCases}, and \code{makefuzzy}
#'
#' @details Primarily a replacement for a snippet
#'   \code{ct2df(selectCases(randomConds(x)))}. By default produces a data set
#'   over five crisp-set factors from a randomly generated csf. Abuses
#'   \code{...} to pass additional arguments to randomConds, selectCases, and
#'   makeFuzzy (if \code{fuzzy = TRUE}). Only valid arguments are passed, will
#'   not brake if given unused arguments. The user may control the creation of
#'   fuzzy set data by passing an explicit \code{fuzzvalues} argument in
#'   \code{...}, otherwise makeFuzzy default values are used.
#'

#' @import cna
#' @export
randomDat <- function(x = 5, condtype = c("csf", "asf"), fuzzy = FALSE, ...){
  call <- match.call()
  ctype <- match.arg(condtype)
  margs <- list(...)

  if (ctype == "csf"){
    randomCondsArgs <- margs[which(names(margs) %in% c("outcome", "n.asf", "compl"))]
    if("how" %in% names(margs)){
      warning("'condtype' set to '", ctype, "' ignoring incompatible argument 'how'")
      }
  }

  if (ctype == "asf"){
    randomCondsArgs <- margs[which(names(margs) %in% c("outcome", "how", "compl"))]
    if("n.asf" %in% names(margs)){
      warning("'condtype' set to '", ctype, "' ignoring incompatible argument 'n.asf")
      }
  }

  selectCasesArgs <- margs[which(names(margs) %in% c("type", "cutoff",
            "rm.dup.factors", "rm.const.factors"))]
  x <- if(any(class(x) %in% c("data.frame", "configTable"))){x} else {full.ct(x)}
  if(any(names(attributes(x)) == "type") && attr(x, "type") == "fs"){
    x <- full.ct(names(x))
  }
  x <- list(x = x)
  rargs <- if(length(randomCondsArgs) < 1){x} else {c(x, randomCondsArgs)}
  mod <- if(ctype == "csf"){do.call(cna::randomCsf, rargs)}else{
    do.call(cna::randomAsf, rargs)
    }

  sargs <- if(length(selectCasesArgs) < 1){c(x, list(mod))} else {c(x, mod, selectCasesArgs)}
  ct <- do.call(cna::selectCases, sargs)
  df <- cna::ct2df(ct)
  if(fuzzy){fuzargs <- if(any(names(margs) == "fuzzvalues")) {
    c(list(x = df), list(fuzzvalues = margs$fuzzvalues))
    } else {
    c(list(x = df), list(fuzzvalues = formals(makeFuzzy)$fuzzvalues))
    }
  df <- ct2df(do.call(makeFuzzy, fuzargs))
  }



  if (ncol(df) < ncol(x[[1]])){df <- eval.parent(call)}
  attributes(df)$target <- mod
  return(df)
}
