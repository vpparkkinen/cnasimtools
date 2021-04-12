#' randomDat
#'
#' @description Simulate a data set from a randomly generated CNA solution in
#'   one go
#'
#' @param x A \code{configTable}, a data frame, a matrix, an integer, a list
#'   specifying the factors' value ranges, or a character vector featuring all
#'   admissible factor values
#' @param condtype Character vector determining if the data-generating structure
#'   (DGS) can be a csf or not, defaults to csf.
#' @param type Character vector; determines the type of data generated
#' @param mvlevels Integer vector of length two. Determines the range of
#'   permissible factor values when creating multi-value data. Only used if
#'   \code{type = "mv"} and admissible values are not implicitly determined by
#'   \code{x}
#' @param ... Additional arguments passed to \code{randomConds},
#'   \code{selectCases}, and \code{makefuzzy}
#'
#' @details Primarily a more flexible replacement for a snippet
#'   \code{ct2df(selectCases(randomConds(x)))}. By default produces a data set
#'   over five crisp-set factors from a randomly generated csf. Other types of
#'   data can be produced by changing the value of \code{type} to "mv" or "fs".
#'   Abuses \code{...} to pass additional arguments to randomConds, selectCases,
#'   and makeFuzzy (if \code{type = "fs"}). Only valid arguments are passed,
#'   will not break if given unused arguments in \code{...}. The user may
#'   control the creation of fuzzy set data by passing an explicit
#'   \code{fuzzvalues} argument in \code{...}, otherwise makeFuzzy default
#'   values are used. Note that argument \code{x} accepts any type of value that
#'   can be passed to \code{full.ct}, but the underlying procedure is the same
#'   regardless. The only purpose of assigning other than integer value to
#'   \code{x} is to control the column names in the created data and admissible
#'   factor values in mv data (otherwise controlled by the argument
#'   \code{mvlevels}), if \code{x} is given a multi-value data set.
#'
#'
#'
#' @returns A data frame
#' @examples
#' randomDat(5, type = "mv")

#' @import cna
#' @export
randomDat <- function(x = 5,
                      condtype = c("csf", "asf"),
                      type = c("cs", "mv", "fs"),
                      mvlevels = if(type == "mv") c(0L:3L) else NULL,
                      samplesize = NULL,
                      ...){
  call <- match.call()
  ctype <- match.arg(condtype)
  dtype <- match.arg(type)
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
  # if(any(class(x) %in% c("data.frame", "configTable"))){
  #   x <- x
  #   }
  # if(any(names(attributes(x)) == "type") && attr(x, "type") == "fs"){
  #     x <- full.ct(names(x))
  #   } else
  if (any(class(x) %in% c("integer", "numeric")) & dtype == "mv"){
      x <- replicate(x, c(list(mvlevels)))
      names(x) <- LETTERS[1:length(x)]
    }
  x <- full.ct(x, type = dtype)


  x <- list(x = x)
  rargs <- if(length(randomCondsArgs) < 1){x} else {c(x, randomCondsArgs)}
  mod <- if(ctype == "csf"){do.call(cna::randomCsf, rargs)}else{
    do.call(cna::randomAsf, rargs)
    }

  #xsc <- list(x = x)
  sargs <- if(length(selectCasesArgs) < 1){c(x, list(mod))} else {c(x, mod, selectCasesArgs)}
  ct <- do.call(cna::selectCases, sargs)
  df <- cna::ct2df(ct)
  if(dtype == "fs"){fuzargs <- if(any(names(margs) == "fuzzvalues")) {
    c(list(x = df), list(fuzzvalues = margs$fuzzvalues))
    } else {
    c(list(x = df), list(fuzzvalues = formals(makeFuzzy)$fuzzvalues))
    }
  df <- ct2df(do.call(makeFuzzy, fuzargs))
  }


  if (ncol(df) < ncol(x[[1]])){df <- eval.parent(call)}

  fragmented <- 0L
  duplicaterows <- 0L
  attributes(df)$target <- mod

  if(!is.null(samplesize)){
    gap <- samplesize - nrow(df)
    if (gap == 0){df <- df} else if(gap < 0){
      fragmented <- gap
      df <- df[sample(1:nrow(df), nrow(df) + gap),]
    } else {
      n <- samplesize %/% nrow(df)
      nmod <- samplesize - (n*nrow(df))
      duplicaterows <- nmod
      if (n == 1) {predf <- df} else {
        predf <- do.call(rbind, rep(list(df), n))
      }
     df <- rbind(predf, df[sample(1:nrow(df), nmod),])
    }
  }
  attr(df, "fragmented") <- fragmented
  attr(df, "duplicaterows") <- duplicaterows
  return(df)
}
