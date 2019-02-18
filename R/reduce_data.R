#' Preprocessing Step to Rolling Entry Matching 
#'
#' @param data Original dataset before reduce_data() was ran.
#' @param treat String for name of treatment variable in data.
#' @param tm String for time period indicator variable name in data.
#' @param entry String for name of time period in which the participant
#' enrolled in the intervention (in the same units as the tm variable).
#' @param id String for individual id variable name in data.
#' @param lookback The number of time periods to look back before the
#' time period of enrollment (1-...).
#'
#' @examples
#' data(package="rollmatch", "rem_synthdata_small")
#' reduced_data <- reduce_data(data = rem_synthdata_small, treat = "treat",
#'                             tm = "quarter", entry = "entry_q",
#'                             id = "indiv_id", lookback = 1)
#' reduced_data
#'
#' @return \code{reduced_data} returns a dataset of reduced data ready
#' for propensity scoring and to use in the function score_data()
#'
#' @export
reduce_data <- function(data, treat, tm, entry, id, lookback=1){

  if (is.data.frame(data) == FALSE){
    stop("'data' input parameter must be a data.frame")
  }

  # Check if we have valid inputs - Errors will print if they are not
  run_checks_one(data, treat, tm, entry, id)
  check_lookback(data, lookback, entry)

  # Set up reduced treatment and comparison set.
  treat_set <- data[data[[treat]] == 1 &
                      (data[[tm]] == data[[entry]] - lookback), ]
  comp_set  <- data[data[[treat]] == 0 &
                      (data[[tm]] %in% unique(treat_set[[tm]])), ]
  reduced_data <- dplyr::bind_rows(treat_set, comp_set)

  return(reduced_data)
}
