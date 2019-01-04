#' Run checks on variable inputs 
#'
#' @param data See rollmatch()
#' @param treat See rollmatch()
#' @param tm See rollmatch()
#' @param entry See rollmatch()
#' @param id See rollmatch()
#' 
#' @keywords internal
run_checks_one <- function(data, treat, tm, entry, id){

  # Treat Checks
  if (is.character(treat) == FALSE){
    stop("'treat' parameter must be a string")
  }
  try_treat <- try(data[treat], silent = TRUE)
  if (class(try_treat) == "try-error"){
    stop("'treat' parameter must be a column in the data.frame")
  }

  # Time Checks
  if (is.character(tm) == FALSE){
    stop("'tm' parameter must be a string")
  }
  try_tm <- try(data[tm], silent = TRUE)
  if (class(try_tm) == "try-error"){
    stop("'tm' parameter must be a column in the data.frame")
  } else if (all(data[tm] == floor(data[tm])) == FALSE){
    stop("'tm' variable must resolve to vector of whole numbers")
  } else if (all(data[tm] > 0) == FALSE){
    stop("'tm' variable must resolve to vector of positive numbers")
  }

  # Entry Checks
  if (is.character(entry) == FALSE){
    stop("'entry' parameter must be a string")
  }
  try_entry <- try(data[entry], silent = TRUE)
  if (class(try_entry) == "try-error"){
    stop("'entry' parameter must be a column in the data.frame")
  } else if (all(data[entry] == floor(data[entry])) == FALSE){
    stop("'entry' variable must resolve to vector of whole numbers")
  } else if (all(data[entry] > 0) == FALSE){
    stop("'entry' variable must resolve to vector of positive numbers")
  }

  # ID Checks
  if (is.character(id) == FALSE){
    stop("'id' parameter must be a string")
  }
  try_id <- try(data[id], silent = TRUE)
  if (class(try_id) == "try-error"){
    stop("'id' parameter must be a column in the data.frame")
  }
}
  
#' Run checks on variable inputs 
#'
#' @param data See rollmatch()
#' @param lookback See rollmatch()
#' @param alpha See rollmatch()
#' @param standard_deviaion See rollmatch()
#' @param num_matches See rollmatch()
#' @param match_on See score_data()
#' @param model_type See score_data()
#' @param replacement See rollmatch()
#' 
#' @keywords internal
run_checks_two <- function(data, alpha, standard_deviation,
                           num_matches, replacement){
  # Other Checks
 if (is.numeric(alpha) == FALSE){
    stop("'alpha' parameter must be numeric")
  } else if (alpha < 0){
    stop("'alpha' parameter must be a positive number")
  } else if (is.character(standard_deviation) == FALSE){
    stop("'standard_deviation' parameter must be of type 'character'")
  } else if (!(standard_deviation %in% c("average", "weighted", "None"))) {
    stop("'standard_deviation' must be 'average', 'weighted', or 'none'")
  } else if (is.numeric(num_matches) == FALSE){
    stop("'num_matches' parameter must be numeric")
  } else if ( (num_matches == floor(num_matches)) == FALSE){
    stop("'num_matches' parameter must be a whole number")
  } else if (num_matches < 0){
    stop("'num_matches' parameter must be a positive number")
  } else if (is.logical(replacement) == FALSE){
    stop("'replacement' parameter must be of type 'logical'")
  }
}

#' Run checks on variable lookback
#' @param lookback See rollmatch()
#' @keywords internal
check_lookback <- function(data, lookback, entry) {
  if (is.numeric(lookback) == FALSE){
    stop("'lookback' parameter must be of type numeric")
  } else if ( (lookback == floor(lookback)) == FALSE){
    stop("'lookback' parameter must be a whole number")
  } else if (between(lookback, 1, 10) == FALSE){
    stop("'lookback' parameter must be between 1 and 10")
  } else if (lookback > max(data[entry])){
    stop("'lookback' is greater than number of time periods in data set")
  }
}
