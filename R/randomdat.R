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

  rargs <- if(length(randomCondsArgs) < 1){list(x)} else {c(x, randomCondsArgs)}
  mod <- if(ctype == "csf"){do.call(cna::randomCsf, rargs)}else{
    do.call(cna::randomAsf, rargs)
    }

  sargs <- if(length(selectCasesArgs) < 1){list(mod)} else {c(mod, selectCasesArgs)}
  ct <- do.call(cna::selectCases, sargs)
  df <- cna::ct2df(ct)
  if (length(df) < x){df <- eval.parent(call)}
  attributes(df)$target <- mod
  return(df)
}

#' @export
solgen <- function(x = 5, range = c(0.8, 0.6) , gran = 0.1, forceoutcome = TRUE, ...){
  r <- seq(min(range), max(range), by = gran)
  cc <- expand.grid(r, r)
  margs <- list(...)
  rdatargs_canuse <- c("condtype", "outcome", "compl", "how", "n.asf", "type", "cutoff")
  rdargs <- margs[which(names(margs) %in% rdatargs_canuse)]
  rms <- list(x, rm.const.factors = TRUE, rm.dup.factors = TRUE)
  rdargs <- c(rms, rdargs)
  cnaargs_canuse <- names(formals(cna))[!names(formals(cna)) %in% c("x", "con.msc", "con", "cov")]
  cnaargs <- margs[which(names(margs) %in% cnaargs_canuse)]
  not_used <- margs[which(!names(margs) %in% c(rdatargs_canuse, cnaargs_canuse))]
  if(length(not_used) >= 1){warning("following arguments are ignored as not applicable: ", names(not_used))}
  dat <- do.call(randomDat, rdargs)
  if(length(dat) < x){while(length(dat) < x){dat <- do.call(randomDat, rdargs)}}
  if (forceoutcome){
    if (!"ordering" %in% names(cnaargs)){
      target <- attributes(dat)$target
      tasfs <- unlist(cna:::extract_asf(target))
      os <- list(ordering = list(sample(cna:::rhs(tasfs), 1)))
      cnaargs <- if(length(cnaargs) < 1) {os} else {c(cnaargs, os)}
    }
  }

  re <- suppressWarnings(mapply(function(a, b, ...) cna::csf(cna::cna(dat, con = a, cov = b, ...), n.init = 20),
               cc[,1], cc[,2], SIMPLIFY = FALSE, MoreArgs = if(length(cnaargs) < 1){NULL} else {cnaargs}))
  re <- do.call(rbind, re)
  return(re[,2])


}

#' @export
is_correct <- function(a,b){
  if (is.na(a) | is.null(a)){TRUE} else {
    is.submodel(a,b)
  }
}


