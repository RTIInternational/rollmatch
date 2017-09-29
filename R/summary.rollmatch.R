summary.rollmatch <- function(wrkr){
  cat("\nSummary:\n")
  print.table(wrkr$summary)
  cat("\n")
  cat("\nBalancing table for all data\n\n")
  print(wrkr$balance[, c("Full Treatment Mean", "Full Comparison Mean",
                        "Full Treatment Std Dev", "Full Comparison Std Dev")])
  cat("\n\nBalancing table for matched data\n\n")
  print(wrkr$balance[, c("Matched Treatment Mean", "Matched Comparison Mean",
                        "Matched Treatment Std Dev",
                        "Matched Comparison Std Dev")])
}
