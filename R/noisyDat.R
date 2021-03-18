#' noisyDat
#'
#' Create a noisy data set
#'
#' @param x A data frame, configTable, or an integer. If data frame or
#'   configTable, this will be used as the data to which noise id added. If an
#'   integer, a data set is created by \code{randomDat}.
#' @param noiselevel A numeric less than one, determines the proportion of noise
#'   in the generated data
#' @param add Logical; if \code{TRUE}, noise is added to \code{x}, otherwise
#'   rows in \code{x} are replaced by noise
#' @param cleanbias Either NULL, or a vector of probability weights to use in
#'   sampling rows from \code{x}. Only used if \code{add = FALSE}
#' @param noisebias Either NULL, or a vector of probability weights to use in
#'   sampling noise rows to add to or replace clean rows with
#' @param rep.noise A numeric less than one; proportion of noise
#'   rows that are forced to be identical, provided that the number of noise rows
#'   (which depends on \code{x} and \code{noiselevel}) is large enough
#' @param rep.clean A numeric less than one; proportion of rows in clean data
#'   that are forced to be identical, given \code{add = FALSE} and the clean data set
#'   is large enough
#' @param type Character string; determines the type of data used
#' @param ... Other arguments; passed to randomDat in case \code{x} is given an
#'   integer value, ignored otherwise
#'
#' @details Takes as input a data frame or a configTable, or (by default) creates one with
#'   \code{randomDat}, and adds or imputes a desired amount of noise rows, where
#'   noise row is defined as a row featuring a configuration not found in the
#'   input data. If \code{x} is created by \code{randomDat}, the target used to
#'   create the data is stored as attribute \code{target} in the returned data
#'   frame.
#'
#' @returns A data frame



#' @export
noisyDat <- function(x = 5,
                     set_N = 20,
                     noisefraction = 0.2,
                     add = TRUE,
                     cleanbias = NULL,
                     noisebias = NULL,
                     rep.noise = 0L,
                     rep.clean = 0L,
                     type = c("cs", "mv", "fs"),
                     ...){

  type <- match.arg(type)
  xarg <- substitute(x)
  dots <- list(...)

  if(any(class(x) %in% c("configTable", "data.frame"))){
    x <- if(any(class(x) == "configTable")) ct2df(x) else x

    no.replace <- if(add) {
      (noisefraction * nrow(x)) / (1 - noisefraction)
      } else {
        nrow(x) * noisefraction
      }

  }




  # if ("type" %in% names(xarg) && class(xarg$type) == "character"){
  #   type <- xarg$type
  #   }
  if(any(class(x) %in% c("numeric", "integer"))) {
    if(!is.null(set_N) && set_N %% (set_N * noisefraction) != 0){
      stop("noisefraction must represent a fraction of set_N")
    }

    ssize <- if(add) {
      (1 - noisefraction) * set_N
      } else {set_N}

    x <- randomDat(x,
                   type = type,
                   samplesize = ssize,
                   ...)

    no.replace <- set_N * noisefraction
    }

  if(!any(class(x) %in% c("data.frame", "configTable"))){
    stop("Invalid argument x = ", deparse(xarg))
  }
  allconfs <- ct2df(full.ct(x, type = type))
  pnoise <- dplyr::setdiff(allconfs, x)
  #dots <- substitute(list(...))
  # no.replace <- if(add) {
  #   (noiselevel * nrow(x)) / (1 - noiselevel)
  #   } else {
  #     nrow(x) * noiselevel
  #     }

  # if (add){
  #   b <- makedat(pnoise, bias = bias, rep.rows = rep.noise)
  # } else {
  if(!add){
    x <- makedat(some(x, nrow(x)-no.replace, replace = F),
                 bias = cleanbias, rep.rows = rep.clean)
  }

  #nmod <-
  # }
  if (nrow(pnoise) < no.replace) {
    pnoise <- rbind(pnoise, some(pnoise, no.replace - nrow(pnoise)))
  }
  b <- makedat(some(pnoise, no.replace, replace = FALSE), bias = noisebias, rep.rows = rep.noise)

  out <- rbind(x, b)
    if("target" %in% attributes(x)) {
    attr(out, "target") <- attr(x, "target")
    }
  class(out) <- c("noisyDat", "data.frame")
  return(out)
}

makedat <- function(x, bias = NULL, rep.rows = 0L){
  if (!is.null(bias)){
    return(x[sample(nrow(x), nrow(x), prob = bias, replace = TRUE),])
  }
  if (!identical(rep.rows, 0L)){
    rep.rows <- rep.rows*nrow(x)
    duprow <- rep(sample(1:nrow(x), 1), rep.rows)
    if (length(duprow) >= nrow(x)){
      out <- x[duprow,]
    } else {
      tempout <- dplyr::setdiff(x, x[duprow,])
      out <- rbind(tempout[1:(nrow(tempout)-rep.rows+1),], x[duprow,])
    }
  } else {
    out <- x
  }
  return(out)
}


#' @export
print.noisyDat <- function(x){
  ats <- attributes(x)
  if (any(names(ats) == "target")) {
    target <- ats[names(ats) == "target"][[1]]
    print.data.frame(x)
    cat("\n Data created from randomd target", target, "\n\n")
  } else {
    print.data.frame(x)
  }

}




