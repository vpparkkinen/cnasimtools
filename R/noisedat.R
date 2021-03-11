#' @export
noisyDat <- function(x = randomDat(type = type),
                     noiselevel = 0.2,
                     add = TRUE,
                     cleanbias = NULL,
                     noisebias = NULL,
                     rep.noise = 0L,
                     rep.clean = 0L,
                     type = c("cs", "mv", "fs")){

  type <- match.arg(type)
  xarg <- substitute(x)
  if ("type" %in% names(xarg) & class(xarg$type) == "character"){type <- xarg$type}
  if(any(class(x) %in% c("numeric", "integer"))) x <- randomDat(x, type = type)
  allconfs <- ct2df(full.ct(x, type = type))
  pnoise <- dplyr::setdiff(allconfs, x)
  #dots <- substitute(list(...))
  no.replace <- if(add) (noiselevel * nrow(x)) / (1 - noiselevel) else nrow(x) * noiselevel

  # if (add){
  #   b <- makedat(pnoise, bias = bias, rep.rows = rep.noise)
  # } else {
  if(!add){
    x <- makedat(some(x, nrow(x)-no.replace, replace = F),
                 bias = cleanbias, rep.rows = rep.clean)
  }

  # }
  if (nrow(pnoise) < no.replace) {
    pnoise <- rbind(pnoise, some(pnoise, no.replace - nrow(pnoise)))
  }
  b <- makedat(some(pnoise, no.replace, replace = FALSE), bias = noisebias, rep.rows = rep.noise)


  return(rbind(x, b))
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
