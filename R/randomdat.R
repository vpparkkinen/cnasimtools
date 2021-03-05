#' @
#' @export
randomDat <- function(x = 6, condtype = c("csf", "asf"),
                      randomCondsArgs =
                        NULL,
                      selectCasesArgs = NULL){

  ctype <- match.arg(condtype)
  #margs <- list(...)
  rargs <- ifelse(is.null(randomCondsArgs), list(x), list(x, randomCondsArgs))
  mod <- if(ctype == "csf"){do.call(cna::randomCsf, rargs)}else{
    do.call(cna::randomAsf, rargs)
    }

  sargs <- ifelse(is.null(selectCasesArgs), list(mod), list(mod, selectCasesArgs))
  ct <- do.call(cna::selectCases, sargs)
  df <- cna::ct2df(ct)
  attributes(df)$target <- mod
  return(df)
}

#' @export
solgen <- function(x = 5, range = c(0.9, 0.6) ,gran = 0.1, ...){
  r <- seq(min(range), max(range), by = gran)
  cc <- expand.grid(r, r)
  #colnames(cc) <- c("con", "cov")
  #cc <- lapply(cc, '[')
  dat <- randomDat(x)
  target <- attributes(dat)$target
  #asfs <- cna:::extract_asf(target)
  #outs <- lapply(asfs, cna:::rhs)
  re <- mapply(function(a, b) cna::csf(cna::cna(dat, con = a, cov = b)),
               cc[,1], cc[,2], SIMPLIFY = FALSE)
  re <- do.call(rbind, re)
  return(re[,2])


}

#' @export
is_correct <- function(a,b){
  if (is.na(x) | is.null(a)){TRUE} else {
    is.submodel(a,b)
  }
}
