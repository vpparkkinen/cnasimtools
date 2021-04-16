#' noisyDat
#'
#' Create a noisy data set
#'
#' @param x A data frame, configTable, or an integer. If data frame or
#'   configTable, this will be used as the data that is manipulated. If an
#'   integer, a data set over \code{x} factors is created by \code{randomDat}.
#' @param set_N Integer determining the number of cases in the created data set.
#' @param add Logical; if \code{TRUE}, noise will be added to a data set, else
#'   noise rows will replace clean rows.
#' @param noisefraction A numeric less than one that can be expressed as a
#'   vulgar fraction of \code{samplesize}, determining the proportion of noise
#'   in the generated data.
#' @param cleanbias Either NULL, or a vector of probability weights to use in
#'   sampling rows from \code{x}. Only used if \code{add = FALSE}. This argument
#'   is very impractical to use and will probably be removed.
#' @param noisebias Either NULL, or a vector of probability weights to use in
#'   sampling noise rows to add to or replace clean rows with. This argument is
#'   very impractical to use and will probably be removed.
#' @param rep.noise A numeric less than one; proportion of noise rows that are
#'   forced to be identical, provided that the number of noise rows (which
#'   depends on \code{x} and \code{noisefraction}) is large enough. This
#'   argument is very impractical to use and will probably be removed.
#' @param rep.clean A numeric less than one; proportion of rows in clean data
#'   that are forced to be identical, given \code{add = FALSE} and the clean
#'   data set is large enough. This argument is very impractical to use and will
#'   probably be removed.
#' @param type Character string; determines the type of data used
#' @param ... Other arguments; passed to randomDat in case \code{x} is given an
#'   integer value, ignored otherwise.
#'
#' @details Takes as input a data frame or a configTable, or (by default)
#'   creates one with \code{randomDat}, and adds rows or replaces rows with
#'   noise, where noise row is defined as a row featuring a configuration not
#'   found in the input data or the data set created by \code{randomDat} if
#'   \code{x} is integer value. In the latter case the noise rows are always
#'   incompatible with the data-generating structure. If \code{x} is created by
#'   \code{randomDat}, the target structure that was used to create the data is
#'   stored as attribute \code{target} in the returned data frame. When creating
#'   an ideal data set from a randomly generated target and manipulating that to
#'   produce a noisy data set, the ideal data itself may be end up being
#'   fragmented even before adding noise, or may feature multiple identical
#'   rows, depending on the the number of asfs in the structure that is
#'   used to generate the ideal data, and the combination of values of
#'   \code{set_N} and \code{noisefraction}. To alleviate this, when the data
#'   type is "cs", the function will automatically choose a target
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
                     verbose = FALSE,
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
    if(!is.null(set_N) && (set_N * noisefraction) %% 1 != 0){
      stop("noisefraction must represent a fraction of set_N")
    }

    ssize <- if(add) {
      (1 - noisefraction) * set_N
      } else {set_N}
    
    if(type == "cs"){
      if(any(names(dots) == "n.asf")){
      nasf <- dots$n.asf
      } else {
        all_n <- 2**x
        perm_nasfs <- 1:(x-2)
        nn_rown <- sapply(perm_nasfs, function(x) all_n / 2^x)
        diffs <- abs(ssize - nn_rown)
        #min(abs(set_N - nn_rown))
        nasfs <- which(diffs == min(diffs))

        # maxrow <- all_n / 2 #maximum no. of clean rows w/ n.asf=1
        # maxmod <- all_n / maxrow
        # f <- function(x, ss, maxs) abs(ss - (maxs / (2*x)))
        # optimize(f, c(1,x-2), ss = set_N, maxs = all_n)
        # nasffactor <- all_n / 2
        # 
        # nasf <- all_n %/% 2*set_N
        }
    }
    x <- randomDat(x,
                   type = type,
                   samplesize = ssize,
                   n.asf = max(nasfs)
                   )

    no.replace <- set_N * noisefraction
    }

  if(!any(class(x) %in% c("data.frame", "configTable"))){
    stop("Invalid argument x = ", deparse(xarg))
  }
  allconfs <- ct2df(full.ct(x, type = type))
  pnoise <- dplyr::setdiff(allconfs, x)
  #dots <- substitute(list(...))
 # no.replace <- if(add) {
    #(noisefraction * nrow(x)) / (1 - noisefraction)
  #  (noisefraction * samplesize) / (1 - noisefraction)
   # } else {
      #nrow(x) * noisefraction
    #  samplesize * noisefraction
     # }

  # no.replace <- if(add) {
  #   (noiselevel * nrow(x)) / (1 - noiselevel)
  #   } else {
  #     nrow(x) * noiselevel
  #     }


 # no.replace <- samplesize * noisefraction
  # if (add){
  #   b <- makedat(pnoise, bias = bias, rep.rows = rep.noise)
  # } else {
  # if(!add){
  #   x <- makedat(some(x, nrow(x)-no.replace, replace = F),
  #                bias = cleanbias, rep.rows = rep.clean)
  # }
  
  dif <- set_N - no.replace
 # ro <- 1:nrow(x)
  x <- makedat(x, size = dif, bias = cleanbias, rep.rows = rep.clean)
    

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
  attr(out, "noise") <- b
  attr(out, "verbose") <- verbose
  class(out) <- c("noisyDat", "data.frame")
  return(out)
}

makedat <- function(x, size = NULL, bias = NULL, rep.rows = 0L){
  #ro <- 1:nrow(x)
  #rs <- sample(ro, size = size, prob = bias, replace = TRUE)
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
print.noisyDat <- function(x, verbose = attr(x, "verbose"),...){
  ats <- attributes(x)
  print.data.frame(x)
  cat("\n")
  if (any(names(ats) == "target")) {
    target <- ats[names(ats) == "target"][[1]]
    #print.data.frame(x)
    cat("Data created from random target", target, "\n\n")
  }
  if (verbose){
    if (any(names(ats) == "fragmented")){
      cat("Fragmentation in clean data: ", 
          ats[["fragmented"]], "rows", "\n")
    }
    if (any(names(ats) == "duplicaterows")){
      cat("Duplicated rows in clean data: ", 
          ats[["duplicaterows"]], "\n\n")
    }
    cat("Noise rows: \n\n")
    print(ats[["noise"]])
  }
  invisible(x)
}




