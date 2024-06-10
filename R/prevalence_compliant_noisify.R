#' Introduce noise to data set while keeping outcome prevalence fixed.
#'
#' @param model 
#' @param data 
#' @param outcome 
#' @param noiselevel 
#'
#' @return
#' @export
#'
#' @examples
prevalence_compliant_noisify <- function(model, data, outcome, noiselevel){
  if((noiselevel * nrow(data)) %% 1 != 0L) warning("noiselevel is not a fraction of N")
  if(noiselevel == 0L) {return(data)}
  N <- nrow(data)
  n_noise <- round(noiselevel * N)
  if(grepl("=", model)) {
    mm <- c(as.matrix(ct2df(full.ct(model))), as.matrix(data))
    mif <- min(mm)
    maf <- max(mm)
    dummydat <- replicate(length(data), mif:maf, simplify = FALSE)
    dummydat <- data.frame(setNames(dummydat, names(data)))
    dummydat <- ct2df(full.ct(dummydat))
  } else {
    dummydat <- full.ct(data)
  }
  cdat <- ct2df(selectCases(model, dummydat))
  ndat <- data.frame(dplyr::setdiff(ct2df(dummydat), cdat))
  ndatsplit <- split(ndat, ndat[,outcome])
  datasplit <- split(data, data[,outcome])
  ndatsplit <- ndatsplit[which(names(ndatsplit) %in% names(datasplit))]
  #mod <- n_noise %% length(datasplit)
  data_ps <- lapply(datasplit, function(x) round((nrow(x) / N) * n_noise))
  nn_dps_diff <- n_noise - sum(unlist(data_ps))
  
  if(nn_dps_diff != 0L){
    tocorrect_pss_idxs <- sample(1:length(data_ps), 
                                 abs(nn_dps_diff),
                                 prob = sapply(datasplit, nrow))
    for (i in tocorrect_pss_idxs){
      data_ps[[i]] <- data_ps[[i]] + nn_dps_diff / abs(nn_dps_diff)
    }
  }
  
  
  if(sum(unlist(data_ps)) == 0L){
    idx <- sample(1:length(datasplit),1)
    tc <- datasplit[[idx]][-sample(1:nrow(datasplit[[idx]]),1), ]
    tn <- ndatsplit[[idx]][sample(1:nrow(ndatsplit[[idx]]),1), ]
    temp_ndata <- tn
    temp_cdata <- datasplit
    temp_cdata[[idx]] <- tc
  } else {
    temp_cdata <- mapply(
      \(x,y){if (y == 0L || nrow(x) == 0L) x else x[-sample(1:nrow(x), y),]}, 
      datasplit, 
      data_ps,
      SIMPLIFY = FALSE)
    temp_ndata <- mapply(\(x,y){
      if (y == 0L) {x[0L,]} else {x[sample(1:nrow(x), y, replace = TRUE),]}
    }, 
    ndatsplit, 
    data_ps,
    SIMPLIFY = FALSE)  
  }
  
  
  ndata_all <- if(class(temp_ndata) == "data.frame") {
    temp_ndata
  } else {do.call(rbind, temp_ndata)}
  cdata_all <- if(class(temp_cdata) == "data.frame") {
    temp_cdata
  } else {
    do.call(rbind, temp_cdata)
  }
  out <- rbind(cdata_all, ndata_all)
  attr(out, "noise") <- ndata_all
  return(out)
}

