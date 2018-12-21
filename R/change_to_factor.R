#' Convert character variables in formula to factor variables.
#' As it's an internal helper function to aid in testing, it is not exported for use outside of the package.
#'
#' @param df A dataframe.
#' @param vars A vector of dataframe column names
#' @return Dataframe where all character variables are now factors
#' 
#' @examples 
#' \dontrun{
#' df <- data.frame(x = 1:3, y = 3:1, z = c("a", "b", "c"),
#'                  stringsAsFactors = FALSE)
#' fm <- as.formula(x ~ y + z)
#' vars <- all.vars(fm)
#' new_df <- change_to_factor(df, vars)
#' lapply(new_df, class)
#' }
#'
#' @keywords internal
change_to_factor <- function(df, vars){
  if (is.data.frame(df) == TRUE ){
    if (is.vector(vars) == TRUE ){
      chars <- sapply(df[vars[-1]], is.character)
      df[vars[-1]][chars] <- data.frame(lapply(df[vars[-1]][chars], as.factor))
      return(df)
    } else {
      stop("Input variable list must be a vector")
    }
  } else {
    stop("Input data must a dataframe")
  }
}
