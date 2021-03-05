#' @import cna
#' @export
randomDat <- function(x = 6, condtype = c("csf", "asf"),
                      randomCondsArgs =
                        NULL,
                      selectCasesArgs = NULL){

  ctype <- match.arg(condtype)
  #margs <- list(...)
  rargs <- ifelse(is.null(randomCondsArgs), list(x), list(x, randomCondsArgs))
  mod <- if(ctype == "csf"){do.call(randomCsf, rargs)}else{
    do.call(randomAsf, rargs)
    }

  sargs <- ifelse(is.null(selectCasesArgs), list(mod), list(mod, selectCasesArgs))
  ct <- do.call(selectCases, sargs)
  df <- ct2df(ct)
  attributes(df)$target <- mod
  return(df)
}
