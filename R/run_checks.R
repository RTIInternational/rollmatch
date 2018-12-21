#' Run checks on variable inputs 
#' As it's an internal helper function to aid in testing,
#' it is not exported for use outside of the package.
#'
#' @param data dataset whose columns match the other parameters 
#' @param tm The time period indicator
#' @param entry The time period in which the participant enrolled in the
#' intervention (in the same units as the tm variable).
#' @param id The individual id variable.
#' 
#' @keywords internal
run_checks_one <- function(data, tm, entry, id){
  # Time Checks 
  if (is.character(tm) == FALSE){
    stop("'tm' input parameter must be a string")
  }
  try_tm <- try(data[tm], silent = TRUE)
  if (class(try_tm) == "try-error"){
    stop("'tm' input parameter must be a column in the data.frame")
  } else if (all(data[tm] == floor(data[tm])) == FALSE){
    stop("'tm' input variable must resolve to a vector of whole numbers")
  } else if (all(data[tm] > 0) == FALSE){
    stop("'tm' input variable must resolve to a vector of positive numbers")
  }
  
  # Entry Checks
  if (is.character(entry) == FALSE){
    stop("'entry' input parameter must be a string")
  }
  try_entry <- try(data[entry], silent = TRUE)
  if (class(try_entry) == "try-error"){
    stop("'entry' input parameter must be a column in the data.frame")
  } else if (all(data[entry] == floor(data[entry])) == FALSE){
    stop("'entry' input variable must resolve to a vector of whole numbers")
  } else if (all(data[entry] > 0) == FALSE){
    stop("'entry' input variable must resolve to a vector of positive numbers")
  }
  
  # ID Checks
  if (is.character(id) == FALSE){
    stop("'id' input parameter must be a string")
  }
  try_id <- try(data[id], silent = TRUE)
  if (class(try_id) == "try-error"){
    stop("'id' input parameter must be a column in the data.frame")
  }
}
  
#' Run checks on variable inputs 
#' As it's an internal helper function to aid in testing,
#' it is not exported for use outside of the package.
#'
#' @param data dataset whose columns match the other parameters 
#' @param lookback The number of time periods to look back before the
#' time period of enrollment (1-10).
#' @param alpha Part of the pre-specified distance within which to allow
#' matching. The caliper width is calculated as the \code{alpha} multiplied by
#' the pooled standard deviation of the propensity scores or the logit of the
#' propensity scores - depending on the value of \code{match_on}.
#' @param weighted_pooled_stdev Option that allows for weighted pooled standard
#' deviation for caliper matching. FALSE for average pooled standard
#' deviation and TRUE for weighted pooled standard deviation.
#' @param num_matches The number of comparison beneficiary matches to attempt
#' to assign to each treatment beneficiary
#' @param match_on Match on estimated propensity score ("pscore") or logit of
#' estimated propensity score ("logit").
#' @param model_type Use logistic regression ("logistic") or "probit"
#' regression ("probit") to estimate the predicted probability of participating
#' in the innovation.
#' @param replacement Assign comparison beneficiaries with replacement (TRUE)
#' or without replacement (FALSE). If \code{replacement} is TRUE, then
#' comparison beneficiaries will be allowed to be used with replacement within
#' a single quarter, but will not be allowed to match to different treatment
#' beneficiaries across multiple quarters.
#' 
#' @keywords internal
run_checks_two <- function(data, alpha, weighted_pooled_stdev,
                           num_matches, replacement){
  # Other Checks 
 if (is.numeric(alpha) == FALSE){
    stop("'alpha' input parameter must be numeric")
  } else if (alpha < 0){
    stop("'alpha' input parameter must be a positive number")
  } else if (is.logical(weighted_pooled_stdev) == FALSE){
    stop("'weighted_pooled_stdev' input parameter must be of type 'logical'")
  } else if (is.numeric(num_matches) == FALSE){
    stop("'num_matches' input parameter must be numeric")
  } else if ( (num_matches == floor(num_matches)) == FALSE){
    stop("'num_matches' input parameter must be a whole number")
  } else if (num_matches < 0){
    stop("'num_matches' input parameter must be a positive number")
  } else if (is.logical(replacement) == FALSE){
    stop("'replacement' input parameter must be of type 'logical'")
  }  
}

#' Run checks on variable lookback
#' As it's an internal helper function to aid in testing,
#' it is not exported for use outside of the package.
#'
#' @param lookback The number of time periods to look back before the
#' time period of enrollment (1-10).
#' 
#' @keywords internal
check_lookback <- function(data, lookback, entry) {
  if (is.numeric(lookback) == FALSE){
    stop("'lookback' input parameter must be of type numeric")
  } else if ( (lookback == floor(lookback)) == FALSE){
    stop("'lookback' input parameter must be a whole number")
  } else if (between(lookback, 1, 10) == FALSE){
    stop("'lookback' input parameter must be between 1 and 10")
  } else if (lookback > max(data[entry])){
    stop("'lookback' is greater than number of time periods in data set")
  }
}