#' noisyDat
#'
#' Create a noisy data set by imputing noise to ideal data that conforms to a
#' randomly generated causal structure, or by imputing noise to user-provided
#' data set.
#'
#' @param x A data frame, configTable, or an integer. If data frame or
#'   configTable, \code{x} will be treated as clean data that is manipulated by
#'   adding to it or replacing observations with noise. If an integer, a data
#'   set over \code{x} alphabetically named factors is created by
#'   \code{randomDat}, and this is treated as the clean data.
#' @param set_N \code{NULL}, or an integer determining the number of cases in
#'   the created data set, if \code{x} is given an integer value.
#' @param add Logical; if \code{TRUE}, noise will be added to a data set, else
#'   noise rows will replace clean rows.
#' @param noisefraction A numeric less than one determining the proportion of
#'   noise in the generated data. If \code{set_N} is other than \code{NULL}, the
#'   value must be expressible as a vulgar fraction of \code{set_N}.
#' @param cleanbias Either \code{NULL}, or a vector of probability weights to
#'   use in sampling rows from clean data. This argument is a bit impractical to
#'   use and will probably be removed.
#' @param noisebias Either \code{NULL}, or a vector of probability weights to
#'   use in sampling noise rows to add to or replace clean rows with. This
#'   argument is a bit impractical to use and will probably be removed.
#' @param rep.noise A positive numeric less than one; proportion of noise rows
#'   that are forced to be identical, provided that the number of noise rows
#'   (which depends on \code{x} and \code{noisefraction}) is large enough. This
#'   argument is very impractical to use and will probably be removed.
#' @param rep.clean A positive numeric less than one; proportion of rows in
#'   clean data that are forced to be identical, given \code{add = FALSE} and
#'   the clean data set is large enough. This argument is very impractical to
#'   use and will probably be removed.
#' @param type Character string; determines the type of data used.
#' @param verbose Logical. If \code{TRUE}, additional information about the
#'   added noise, and possible fragmentation / duplicate rows in the clean data
#'   (before adding noise) is printed.
#' @param ... Other arguments. Passed to \code{randomDat} in case \code{x} is
#'   given an integer value, ignored otherwise.
#'
#' @details \code{noisyDat} simplifies the process of simulating noisy data with
#'   the \code{\link[cna]{cna}} package. It provides two functionalities based
#'   on the value of the argument \code{x}. If \code{x} is given an integer
#'   value, an exhaustive, ideal data set that conforms to a randomly generated
#'   target structure is first created with \code{\link{randomDat}}. This data
#'   set is used as the clean data that is manipulated by adding to it, or
#'   replacing rows with noise. If \code{x} is a data set  given as a data frame
#'   or a configTable object, that data set is used as the clean data that is
#'   manipulated with noise. In either case, noise is defined as configurations
#'   not found in the clean data.
#'
#'   Additionally, the argument \code{cleanbias} can be used to introduce the
#'   chance of randomly fragmenting the clean data even before making it noisy.
#'   This is done by sampling rows from the initial clean data with replacement,
#'   using the probability weights given in \code{cleanbias}. This results in a
#'   data set of the same size as the initial clean data, but with some rows
#'   possibly replaced by duplicates of other clean rows. This data set is then
#'   used as the clean data that is manipulated to make it noisy. Similarly,
#'   some noise configurations can be made more likely to occur than others by
#'   using the \code{noisebias} argument. In the case where \code{x} is an
#'   integer and the clean data is created within the function by
#'   \code{randomDat}, the noise rows are always incompatible with structure
#'   that created the clean data. In this case, the target structure that was
#'   used to create the clean data is stored as attribute \code{target} of the
#'   returned data frame.
#'
#'   Note that when creating a data set from a randomly generated target, i.e.
#'   when \code{x} is given an integer value, the value of the \code{add}
#'   argument does _not_ guarantee that either every configuration compatible
#'   with the target is present or that some configurations are absent in the
#'   resulting noisy data set. Depending on the combination of values of
#'   \code{x}, \code{set_N}, \code{noisefraction}, and the number of asfs in the
#'   target structure used to generate the data, some rows of ideal data may
#'   have to be either duplicated or removed even before imputing noise, to meet
#'   the desired combination of number of rows and proportion of noise. This may
#'   lead to a situation where the effect of the \code{add} argument is
#'   effectively overriden in the sense that every ideal configuration is
#'   present even though \code{add = FALSE}, or some are missing even though
#'   \code{add = TRUE}. For example, in crisp-set data, a structure with two
#'   asfs over five factors will return an ideal data set of eight rows, with
#'   each unique configuration compatible with the target exhibited once. If the
#'   desired sample size is set to 16 rows by \code{set_N = 16}, and in addition
#'   \code{add = FALSE} and \code{noisefraction = 0.25}, meaning that 4 out of
#'   the 16 rows should be noise (0.25 = 4/16), this means that four rows of
#'   ideal data will have to be duplicated in order to meet the constraints
#'   defined by \code{set_N} and \code{noisefraction}, i.e. 16 rows with 4 rows
#'   incompatible with the target. It is thus possible that every configuration
#'   compatible with the data generating structure is present in the resulting
#'   noisy data, even though \code{add = FALSE}. To alleviate this issue, when
#'   the data type is "cs" and no explicit \code{n.asf} argument is given in
#'   \code{...}, the function will automatically choose a target with a number
#'   of asfs such that the possibility of fragmentation or duplicating ideal
#'   rows before imputing noise is minimized. If no value is provided for
#'   \code{set_N}, the function will try to find a suitable combination of
#'   sample size and number of asfs in the target to minimize fragmentation or
#'   duplicate ideal rows relative to the value of \code{x} alone. Ultimately,
#'   however, it is up to the user to choose reasonable values for these
#'   arguments, depending on one's use case.
#'
#'
#'
#' @returns A data frame



#' @export

noisyDat <- function(x = 5,
                     set_N = NULL,
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
  
  if(noisefraction == 0){stop("noisefraction has value 0. For generating noise-free data from a randomly drawn target, use randomDat() instead.")}
  if(any(class(x) %in% c("configTable", "data.frame"))){
    x <- if(any(class(x) == "configTable")) ct2df(x) else x

    no.replace <- if(add) {
      #(noisefraction * nrow(x)) / (1 - noisefraction)
      (nrow(x) / (1-noisefraction)) - nrow(x)
      } else {
        nrow(x) * noisefraction
      }
    x <- makedat(x, 
                 size = if (add) nrow(x) else nrow(x) - no.replace, 
                 bias = cleanbias, 
                 rep.rows = rep.clean)
  }




  # if ("type" %in% names(xarg) && class(xarg$type) == "character"){
  #   type <- xarg$type
  #   }
  if(any(class(x) %in% c("numeric", "integer"))) {
    if(!is.null(set_N) && (set_N * noisefraction) %% 1 != 0){
      stop("noisefraction must represent a fraction of set_N")
    }
    
    # if(type == "mv" & is.null(set_N)){
    #   stop("For multi-value data, desired sample size must be set manually with set_N")
    # }
    if("n.asf" %in% names(dots)){
      nasfs <- dots$n.asf
    } else {
      nasfs <- NULL
    }
    
    if (!is.null(set_N)){ 
      if(add) {
        ssize <- (1 - noisefraction) * set_N
        no.replace <- set_N - ssize
        } else {
          ssize <- set_N
          no.replace <- set_N * noisefraction
        }
    } else {
      ssize <- set_N
    }
    
    if(type == "mv"){
      if(any(names(dots) == "mvlevels")){
        mvlevels <- mvlevels
        } else {
          mvlevels <- eval(formals(randomDat)$mvlevels)
        }
      
      dat_norep <- mvdat(x = x, 
                         ssize = ssize, 
                         noisefraction = noisefraction, 
                         nasfs = nasfs, 
                         add = add, 
                         type = type, 
                         mvlevels = mvlevels)
      x <- dat_norep[[1]]
      no.replace <- dat_norep[[2]]
      # if(is.null(ssize)){
      #   fc <- MASS::fractions(noisefraction)
      #   dn <- as.integer(gsub("^\\d*/", "", fc))
      #   nu <- as.integer(gsub("/\\d*$", "", fc))
      #   # if(any(names(dots) == "mvlevels")){
      #   #   mvlevels <- mvlevels
      #   #   } else {
      #   #     mvlevels <- formals(randomDat)$mvlevels
      #   #   }
      #   x <- randomDat(x, type=type, mvlevels = mvlevels, n.asf = nasfs)
      #   rem <- nrow(x) %% dn
      #   if(!add){
      # 
      #     #div <- nrow(x) %/% dn
      #     x <- resize(x, nrow(x) - rem)
      #     no.replace <- nrow(x) * noisefraction
      #   } else {
      #     # req_size <- nrow(x) / (1 - noisefraction)
      #     # req_size %/% dn
      #     req_size <- reqsize(nrow(x), noisefraction = noisefraction, dn = dn)
      #     xsize <- (1 - noisefraction) * req_size
      #     x <- resize(x, xsize)
      #     no.replace <- req_size - xsize
      #   }
      # } else {
      #   x <- randomDat(x, samplesize = ssize, type=type, mvlevels = mvlevels, n.asf = nasfs)
      # }

    }
    
    if(type == "cs"){
      dat_norep <- csdat(x = x, 
                 ssize = ssize, 
                 noisefraction = noisefraction, 
                 nasfs = nasfs, 
                 add = add,
                 type = type,
                 no.replace = no.replace)
      x <- dat_norep[[1]]
      no.replace <- dat_norep[[2]]
    }
    # if(type == "cs"){
    #   all_n <- 2**x
    #   perm_nasfs <- 1:(x-2)
    #   nn_rown <- sapply(perm_nasfs, function(x) all_n / 2^x)
    #   
    #   # if(!is.null(nasfs)){
    #   #   base_rown <- nn_rown[nasfs]
    #   # } else {
    #     
    # 
    #     if (is.null(ssize)){
    #       #if(!is.null)
    #       if(!add){
    #         fc <- MASS::fractions(noisefraction)
    #         dn <- as.integer(gsub("^\\d*/", "", fc))
    #         nu <- as.integer(gsub("/\\d*$", "", fc))
    #         divs <- nn_rown %/% dn
    #         rems <- nn_rown %% dn
    #         if(!is.null(nasfs)){
    #           if(divs[nasfs] == 0){
    #             ssize <- dn
    #           } else {
    #               pick <- divs[nasfs]
    #               ssize <- pick * dn
    #               }
    #         } else {
    #           if(max(divs) == 0){ssize <- dn} else {
    #           dividx <- which(divs > 0)
    #           pick <- sample(dividx, 1)
    #           #ssize <- nn_rown[pick] + rems[pick]
    #           ssize <- pick * dn
    #           }
    #         }
    # 
    #         no.replace <- ssize * noisefraction
    #         diffs <- abs(ssize - nn_rown)
    #         nasfs <- if(is.null(nasfs)) which(diffs == min(diffs)) else nasfs
    #       } else {
    #           fc <- MASS::fractions(1 - noisefraction)
    #           dn <- as.integer(gsub("^\\d*/", "", fc))
    #           nu <- as.integer(gsub("/\\d*$", "", fc))
    #           
    #           divs <- nn_rown %/% nu
    #           rems <- nn_rown %% nu
    #           if(!is.null(nasfs)){
    #             if(divs[nasfs] == 0){
    #               ssize <- nu
    #               no.replace <- dn - nu
    #             } else {
    #               mplier <- divs[nasfs]  
    #               }
    #           } else {
    #               if(max(divs) == 0){
    #               ssize <- nu
    #               no.replace <- dn - nu
    #               } else {
    #                 dividx <- which(divs >= 1)
    #                 pick <- sample(dividx, 1)
    #                 mplier <- divs[pick]
    # 
    #               }
    #           }
    #         if(nu * mplier < min(nn_rown)){
    #           mplier <- mplier + 1
    #         }
    #         ssize <- nu * mplier
    #         # if (ssize < min(nn_rown)){
    #         #   ssize <- ssize * 2
    #         #   }
    #         no.replace <- (dn * mplier) - ssize
    #         #diffs <- nn_rown %% ssize
    #         #diffs <- abs(nn_rown - ssize)
    #         diffs <- nn_rown - ssize
    #         if(any(diffs == 0)){
    #           nasfs <- if(is.null(nasfs)) which(diffs == 0) else nasfs
    #         } else if (!any(diffs < 0)){
    #           nasfs <- if(is.null(nasfs)) which(diffs == min(diffs)) else nasfs  
    #         } else {
    #           tempdiffs <- diffs[diffs < 0]
    #           #tempdiffs <- c(0, tempdiffs)
    #           tidx <- which(min(abs(tempdiffs)) == abs(tempdiffs))
    #           nasfs <- if(is.null(nasfs)) which(diffs == tempdiffs[tidx]) else nasfs              
    #         }
    # 
    #         
    #         #diffs <- unique(diffs)
    #         #nasfs <- min(which(diffs == min(diffs)))
    #         #nasfs <- if(is.null(nasfs)) which(diffs == min(diffs)) else nasfs
    #       }
    #     } else {
    #       diffs <- abs(ssize - nn_rown)
    #       #min(abs(set_N - nn_rown))
    #       nasfs <- if (is.null(nasfs)) which(diffs == min(diffs)) else nasfs
    #       }
    #   #}
    #   x <- randomDat(x,
    #              type = type,
    #              samplesize = ssize,
    #              n.asf = if (type == "cs") max(nasfs) else NULL
    #   )
    # }
    # 
    
    x <- makedat(x, 
                 size = if (add) nrow(x) else nrow(x) - no.replace, 
                 bias = cleanbias, 
                 rep.rows = rep.clean)
    
    }
    
    #no.replace <- nrow(x) * noisefraction
    

  if(!any(class(x) %in% c("data.frame", "configTable"))){
    stop("Invalid argument x = ", deparse(xarg))
  }
  allconfs <- ct2df(full.ct(x, type = type))
  pnoise <- dplyr::setdiff(allconfs, ct2df(selectCases(attributes(x)$target)))
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
  
  #dif <- set_N - no.replace
 # ro <- 1:nrow(x)
  
    

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


mvdat <- function(x, ssize, noisefraction, nasfs, add, type, mvlevels){
  if(is.null(ssize)){
    fc <- MASS::fractions(noisefraction)
    dn <- as.integer(gsub("^\\d*/", "", fc))
    nu <- as.integer(gsub("/\\d*$", "", fc))
    # if(any(names(dots) == "mvlevels")){
    #   mvlevels <- mvlevels
    #   } else {
    #     mvlevels <- formals(randomDat)$mvlevels
    #   }
    x <- randomDat(x, type=type, mvlevels = mvlevels, n.asf = nasfs)
    rem <- nrow(x) %% dn
    if(!add){

      #div <- nrow(x) %/% dn
      x <- resize(x, nrow(x) - rem)
      no.replace <- nrow(x) * noisefraction
    } else {
      # req_size <- nrow(x) / (1 - noisefraction)
      # req_size %/% dn
      req_size <- reqsize(nrow(x), noisefraction = noisefraction, dn = dn)
      xsize <- (1 - noisefraction) * req_size
      x <- resize(x, xsize)
      no.replace <- req_size - xsize
    }
  } else {
    x <- randomDat(x, samplesize = ssize, type=type, mvlevels = mvlevels, n.asf = nasfs)
  }
  return(list(x, no.replace))
}





csdat <- function(x, ssize, noisefraction, nasfs, add, type, no.replace){  
  all_n <- 2**x
  perm_nasfs <- 1:(x-2)
  nn_rown <- sapply(perm_nasfs, function(x) all_n / 2^x)

  # if(!is.null(nasfs)){
  #   base_rown <- nn_rown[nasfs]
  # } else {


  if (is.null(ssize)){
    #if(!is.null)
    if(!add){
      fc <- MASS::fractions(noisefraction)
      dn <- as.integer(gsub("^\\d*/", "", fc))
      nu <- as.integer(gsub("/\\d*$", "", fc))
      divs <- nn_rown %/% dn
      rems <- nn_rown %% dn
      if(!is.null(nasfs)){
        if(divs[nasfs] == 0){
          ssize <- dn
        } else {
            pick <- divs[nasfs]
            ssize <- pick * dn
            }
      } else {
        if(max(divs) == 0){ssize <- dn} else {
        dividx <- which(divs > 0)
        pick <- sample(dividx, 1)
        #ssize <- nn_rown[pick] + rems[pick]
        ssize <- pick * dn
        }
      }

      no.replace <- ssize * noisefraction
      diffs <- abs(ssize - nn_rown)
      nasfs <- if(is.null(nasfs)) which(diffs == min(diffs)) else nasfs
    } else {
        fc <- MASS::fractions(1 - noisefraction)
        dn <- as.integer(gsub("^\\d*/", "", fc))
        nu <- as.integer(gsub("/\\d*$", "", fc))

        divs <- nn_rown %/% nu
        rems <- nn_rown %% nu
        if(!is.null(nasfs)){
          if(divs[nasfs] == 0){
            ssize <- nu
            no.replace <- dn - nu
            mplier <- 1
          } else {
            mplier <- divs[nasfs]
            }
        } else {
            if(max(divs) == 0){
            ssize <- nu
            no.replace <- dn - nu
            mplier <- 1
            } else {
              dividx <- which(divs >= 1)
              pick <- sample(dividx, 1)
              mplier <- divs[pick]

            }
        }
      if(nu * mplier < min(nn_rown)){
        mplier <- mplier + 1
      }
      ssize <- nu * mplier
      # if (ssize < min(nn_rown)){
      #   ssize <- ssize * 2
      #   }
      no.replace <- (dn * mplier) - ssize
      #diffs <- nn_rown %% ssize
      #diffs <- abs(nn_rown - ssize)
      diffs <- nn_rown - ssize
      if(any(diffs == 0)){
        nasfs <- if(is.null(nasfs)) which(diffs == 0) else nasfs
      } else if (!any(diffs < 0)){
        nasfs <- if(is.null(nasfs)) which(diffs == min(diffs)) else nasfs
      } else {
        tempdiffs <- diffs[diffs < 0]
        #tempdiffs <- c(0, tempdiffs)
        tidx <- which(min(abs(tempdiffs)) == abs(tempdiffs))
        nasfs <- if(is.null(nasfs)) which(diffs == tempdiffs[tidx]) else nasfs
      }


      #diffs <- unique(diffs)
      #nasfs <- min(which(diffs == min(diffs)))
      #nasfs <- if(is.null(nasfs)) which(diffs == min(diffs)) else nasfs
    }
  } else {
    diffs <- abs(ssize - nn_rown)
    #min(abs(set_N - nn_rown))
    nasfs <- if (is.null(nasfs)) which(diffs == min(diffs)) else nasfs
    }
#}
  x <- randomDat(x,
             type = type,
             samplesize = ssize,
             n.asf = if (type == "cs") max(nasfs) else NULL
  )
  return(list(x, no.replace))
}





reqsize <- function(N, noisefraction, dn){
  cand_size <- as.integer(N) / (1L - noisefraction)
  check <- cand_size %% dn
  if(check==0){
    return(cand_size)
  } else {
    N <- N + 1L
    reqsize(N, noisefraction, dn)
  }
}

# 
#   i <- (N + x/ (1L - noisefraction)) %% dn
#   i

#reqsize(100, 0.8, 5)


makedat <- function(x, size = nrow(x), bias = NULL, rep.rows = 0L){
  #ro <- 1:nrow(x)
  #rs <- sample(ro, size = size, prob = bias, replace = TRUE)
  if (nrow(x) != size){
    ro <- 1:nrow(x)
    rs <- sample(ro, size = size, prob = bias, replace = FALSE)
    out <- x[rs, ]
  } else {
    out <- x
  }
  
  # if (!is.null(bias)){
  #   return(x[sample(nrow(x), nrow(x), prob = bias, replace = TRUE),])
  # } 
# 
#   if (!identical(rep.rows, 0L)){
#     rep.rows <- rep.rows*nrow(x)
#     duprow <- rep(sample(1:nrow(x), 1), rep.rows)
#     if (length(duprow) >= nrow(x)){
#       out <- x[duprow,]
#     } else {
#       tempout <- dplyr::setdiff(x, x[duprow,])
#       out <- rbind(tempout[1:(nrow(tempout)-rep.rows+1),], x[duprow,])
#     }
#   } else {
#     out <- x
#   }
  if (!identical(rep.rows, 0L)){
    #rep.rows <- rep.rows*nrow(out)
    stopifnot(rep.rows <= nrow(out))
      rows <- 1:nrow(out)
      duprow <- rep(sample(1:nrow(out), 1), rep.rows)
      rows <- rows[which(rows!=duprow[1])]
      cdx <- sample(rows, (length(rows)+1)-length(duprow), replace = FALSE)
      r_indices <- c(cdx, duprow)
      out <- out[r_indices,]
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
          abs(ats[["fragmented"]]), "rows missing", "\n")
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




