#' Make a noisy data set from an *asf* while controlling outcome prevalence.
#'
#' @param model A character string that expresses a `cna` model.
#' @param data  A data.frame; the data set to be manipulated.
#' @param outcome Named, unit length list; the outcome variable/factor name and value.
#' @param prevalence Numeric between zero and one.
#' @param noiselevel Numeric between zero and one.
#' @param N Integer.
#'
#' @details `makenoisy_asf()` generates noisy data sets while allowing the user
#'   to control outcome prevalence. Given an *asf*, a data set will be created
#'   where the outcome prevalence equals `prevalence`, number of rows equals
#'   `N`, and proportion of rows that do not conform to the input *asf* (i.e. the
#'   proportion of noise to ideal data) equals `noiselevel`. 
#' @return `data.frame`
#' @export
#'
#' @examples makenoisy_asf("A+B<->C", prevalence = 0.25, noiselevel = 0.2, N = 10)
makenoisy_asf <- function(model = randomAsf(6), 
                          data = ct2df(selectCases(model)), 
                          outcome = setNames(list(1), rhs(model)), 
                          prevalence = 0.5, 
                          noiselevel = 2/N,
                          N = nrow(data)){
  # oc_temp <- substitute(outcome)
  # oc <- deparse(oc_temp)
  # oc <- gsub(" ", "", strsplit(oc, "==")[[1]][1])
  stopifnot(is.list(outcome) || is.character(outcome))
  model <- noblanks(model)
  if(is.list(outcome)) {
    oc <- names(outcome)[[1]]
  } else {
    oc <- outcome
    outcome <- setNames(list(1), outcome)
  }
  prev_dat <- prevalence_fixer(data, outcome, prevalence, N)
  out <- prevalence_compliant_noisify(model, prev_dat, oc, noiselevel)
  info <- data.frame(model = model,
                     outcome = paste0(oc, "=", outcome[[1]]),
                     prevalence = prevalence,
                     noiselevel = noiselevel,
                     N = N)
  attr(out, "makenoisy_asf.info") <- info
  attr(out, "makenoisy_asf.input.data") <- data
  return(out)
}


prevalence_fixer <- function(data, outcome, prevalence, N){
  if(!isTRUE(all.equal((prevalence * N) %% 1, 0L))) warning("`prevalence` is not a fraction of `N`")
  # if(class(substitute(outcome, parent.frame())) == "call"){o <- outcome} else{
  #   o <- substitute(outcome)  
  # }
  # 
  # o_idx <- eval(o, data, parent.frame())
  stopifnot(is.list(outcome) || is.character(outcome))
  if (is.list(outcome)){
    o_idx <- data[,names(outcome)[[1]]] == outcome[[1]]  
  } else {
    o_idx <- data[,outcome] == 1
  }
  
  o_present <- data[o_idx,]
  opnr <- nrow(o_present)
  if (nrow(data) == N && prevalence == opnr / N){
    return(data)
  }
  o_absent <- data[!o_idx,]
  oanr <- nrow(o_absent)
  n_preval_rows <- round(N * prevalence)
  n_nonpreval_rows <- N - n_preval_rows
  if (opnr > n_preval_rows){
    prev_out <- o_present[sample(1:opnr, n_preval_rows),]
  } else {
    op_sample <- sample(1:opnr, n_preval_rows - opnr, replace = T)
    prev_out <- rbind(o_present, o_present[op_sample,])
  }
  if (oanr > n_nonpreval_rows){
    nonprev_out <- o_absent[sample(1:oanr, n_nonpreval_rows),]
  } else {
    oa_sample <- sample(1:oanr, n_nonpreval_rows - oanr, replace = T)
    nonprev_out <- rbind(o_absent, o_absent[oa_sample,])
  }
  out <- rbind(prev_out, nonprev_out)
  return(out)
}

#' Change outcome column values.
#'
#' Create a noisy data set by changing outcome column (factor) values for a
#' proportion of rows of a data set.
#'
#' @param data A `data.frame`
#' @param outcome Character; outcome column (factor) name.
#' @param proportion A numeric that determines a proportion of rows in `data`
#'   for which outcome is to be changed.
#'
#' @return A `data.frame`
#' @export
#'
#' @details Given a data frame `data` and the name of an outcome variable/factor
#'   `outcome`, `flipout()` changes the outcome value for a proportion of rows
#'   in `data`, the number of which is geiven by `proportion`. If `proportion`
#'   is not a (vulgar) fraction of `nrow(data)`, number of rows to be
#'   manipulated is determined as `round(proportion * nrow(data))`.
#'
#' @examples
flipout <- function(data, outcome, proportion) {
  N <- nrow(data)
  out_col <- which(names(data) == outcome)
  range_o <- min(data[, out_col]):max(data[, out_col])
  n_to_flip <- round(N * proportion)
  n_to_flip <- if (n_to_flip == 0L) 1L else n_to_flip
  w_rows <- sample(1:N, n_to_flip)
  for(row in w_rows){
    ov <- data[row, out_col]
    not_ov <- range_o[range_o != ov]
    change_ov_to <- sample(not_ov, 1)
    data[row, out_col] <- change_ov_to
  }
  return(data)
}
