print_rollmatch <- function(wrkr){
  cat("\nCall: ", deparse(wrkr$call), sep = "\n")
  cat("\nSummary:\n")
  print.table(wrkr$summary)
  cat("\n")
}
