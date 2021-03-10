noisyDat <- function(x,
                     noiselevel,
                     add = FALSE,
                     rep.noise = NULL,
                     rep.clean = NULL,
                     type = c("cs", "mv", "fs"),
                     ...){

  cleandata <- randomDat(x)
  pnoise <- dplyr::setdiff(ct2df(full.ct(x)), cleandata)
  dots <- substitute(list(...))

  if (add){
    a <- cleandata
  } else {
    a <- makedat(some(cleandata, nrow(cleandata)-nrow(cleandata)*noiselevel, replace = F),
                 bias = cleanbias, rep.rows = rep.clean)
  }
  if (nrow(pnoise) < no.replace) {
    pnoise <- rbind(pnoise, some(pnoise, no.replace - nrow(pnoise)))
  }
  b <- makedat(some(pnoise, no.replace, replace = FALSE), bias = noisebias, rep.rows = rep.noise)


  return(rbind(a, b))
}

makedat <- function(x, bias = NULL, rep.rows = NULL){
  if (!is.null(bias)){
    return(x[sample(nrow(x), nrow(x), prob = bias, replace = TRUE),])
  }
  if (!is.null(rep.rows)){

    duprow <- rep(sample(1:nrow(x), 1), rep.rows)
    if (length(duprow) >= nrow(x)){
      out <- x[duprow,]
    } else {
      tempout <- setdiff(x, x[duprow,])
      out <- rbind(tempout[1:(nrow(tempout)-rep.rows+1),], x[duprow,])
    }
  } else {
    out <- x
  }
  return(out)
}
