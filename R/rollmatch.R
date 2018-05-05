#' Rolling entry matching
#'
#' \code{rollmatch} is the main function of the package
#' \emph{<rollmatch>}, which implements a comparison group selection
#' methodology for interventions with rolling participant entry over time.
#' A difficulty in evaluating rolling entry interventions is that a suitable
#' "entry" date is not observed for non-participants. This method, called
#' rolling entry matching, assigns potential comparison non-participants
#' multiple counterfactual entry periods which allows for matching of
#' participant and non-participants based on data immediately preceding each
#' participant's specific entry period, rather than using data from a fixed
#' pre-intervention period.
#'
#' Rolling entry matching requires three steps. First, a quasi-panel dataset
#' is constructed containing multiple observations of non-participants (one for
#' each entry period). Participants enter the data once in the baseline period
#' immediately preceding their unique entry into the intervention. Time-varying
#' covariates (e.g., health conditions, spending, utilization) are dynamic for
#' each entry period's non-participant observations.  Second, a predicted
#' probability of treatment is obtained for participants and non-participants
#' (e.g., through propensity score matching).  Finally, the pool of potential
#' comparisons for each participant is restricted to those that have the same
#' "entry period" into the intervention. The matching algorithm then selects
#' the best matched comparison(s) for each participant from the pool of
#' non-participants with the same entry period.
#'
#' @param formula A \code{\link[stats]{formula}} in the form
#' \code{treat ~ x1 + x2 ...} where \code{treat} is a binary treatment
#' indicator (Treat = 1, Control = 0) and \code{x1} and \code{x2} are
#' pre-treatment covariates. Both the treatment indicator and
#' pre-treatment covariates must be contained in the input dataset.
#' @param data The input panel dataset.
#' @param tm The time period indicator.
#' @param entry The time period in which the participant enrolled in the
#' intervention (in the same units as the tm variable).
#' @param id The individual id variable.
#' @param lookback The number of time periods to look back before the
#' time period of enrollment (1-10).
#' @param alpha Part of the pre-specified distance within which to allow
#' matching. The caliper width is calculated as the \code{alpha} multiplied by the
#' pooled standard deviation of the propensity scores or the logit of the
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
#' @examples
#' data(package="rollmatch", "rem_synthdata_small")
#'
#' formula <- as.formula(treat ~ qtr_pmt + yr_pmt + age)
#'
#' r_match <- rollmatch(formula, data = rem_synthdata_small, tm = "quarter",
#'                        entry = "entry_q", id = "indiv_id", alpha = 0.2)
#'
#' r_match
#'
#' @return \code{rollmatch} returns an object of class "rollmatch".
#'
#' An object of class "rollmatch" is a list containing the following components:
#' \item{call}{The original \code{rollmatch} call.}
#' \item{model}{The output of the model used to estimate the distance measure.}
#' \item{scores}{The propensity score and logit of the propensity score. }
#' \item{data}{The original dataset with matches, scores, and weights applied.}
#' \item{summary}{A basic summary table.}
#' \item{balance}{The balancing table.}
#'
#' @import dplyr
#' @import magrittr
#' @export
#'
rollmatch <- function(formula, data, tm, entry, id, lookback = 1, alpha = 0,
                      weighted_pooled_stdev = FALSE, num_matches = 3,
                      match_on = "logit", model_type = "logistic",
                      replacement=TRUE) {

  ############################################################################

  if (is.language(formula) == FALSE){
    stop("'formula' input parameter must be of type 'language'")
  }

  if (is.data.frame(data) == FALSE){
    stop("'data' input parameter must be a data.frame")
  }

  if (is.character(tm) == FALSE){
    stop("'tm' input parameter must be a string")
  }

  try_tm <- try(data[tm], silent = TRUE)
  if (class(try_tm) == "try-error"){
    stop("'tm' input parameter must be a column in the data.frame")
  }

  if (all(data[tm] == floor(data[tm])) == FALSE){
    stop("'tm' input variable must resolve to a vector of whole numbers")
  }

  if (all(data[tm] > 0) == FALSE){
    stop("'tm' input variable must resolve to a vector of positive numbers")
  }

  if (is.character(entry) == FALSE){
    stop("'entry' input parameter must be a string")
  }

  try_entry <- try(data[entry], silent = TRUE)
  if (class(try_entry) == "try-error"){
    stop("'entry' input parameter must be a column in the data.frame")
  }

  if (all(data[entry] == floor(data[entry])) == FALSE){
    stop("'entry' input variable must resolve to a vector of whole numbers")
  }

  if (all(data[entry] > 0) == FALSE){
    stop("'entry' input variable must resolve to a vector of positive numbers")
  }

  if (is.character(id) == FALSE){
    stop("'id' input parameter must be a string")
  }

  try_id <- try(data[id], silent = TRUE)
  if (class(try_id) == "try-error"){
    stop("'id' input parameter must be a column in the data.frame")
  }

  if (is.numeric(lookback) == FALSE){
    stop("'lookback' input parameter must be of type numeric")
  }

  if ( (lookback == floor(lookback)) == FALSE){
    stop("'lookback' input parameter must be a whole number")
  }

  if (between(lookback, 1, 10) == FALSE){
    stop("'lookback' input parameter must be between 1 and 10")
  }

  if (lookback > max(data[entry])){
    stop("'lookback' is greater than number of time periods in data set")
  }

  if (is.numeric(alpha) == FALSE){
    stop("'alpha' input parameter must be numeric")
  }

  if (alpha < 0){
    stop("'alpha' input parameter must be a positive number")
  }

  if (is.logical(weighted_pooled_stdev) == FALSE){
    stop("'weighted_pooled_stdev' input parameter must be of type 'logical'")
  }

  if (is.numeric(num_matches) == FALSE){
    stop("'num_matches' input parameter must be numeric")
  }

  if ( (num_matches == floor(num_matches)) == FALSE){
    stop("'num_matches' input parameter must be a whole number")
  }

  if (num_matches < 0){
    stop("'num_matches' input parameter must be a positive number")
  }

  if (is.logical(replacement) == FALSE){
    stop("'replacement' input parameter must be of type 'logical'")
  }

  orig.call <- match.call()

  vars <- all.vars(formula)
  # get treatment variable from formula
  treat <- vars[1]

  # Set up reduced treatment and comparison set.
  treat_set <- data[data[[treat]] == 1 &
                      (data[[tm]] == data[[entry]] - lookback), ]
  comp_set  <- data[data[[treat]] == 0 &
                      (data[[tm]] %in% unique(treat_set[[tm]])), ]
  reduced_data <- dplyr::bind_rows(treat_set, comp_set)

  # Convert Columns to Factor
  reduced_data <- chr_2_factor(reduced_data, vars)

  # Run model and save output
  model_output <- runModel(model_type, match_on, reduced_data, id, treat, entry,
                     tm, formula)

  #w <- tryCatch(runModel(model_type, match_on, reduced_data, id,
  #                                 treat, entry,tm, formula),
  #                        warning = function(w) w)

  lr_result <- model_output$lr_result
  pred_model <- model_output$pred_model

  # Create pool of possible matches
  comparison_pool <- createComparison(lr_result, tm, entry, id)
  # Trim pool based on specified caliper
  trimmed_pool <- trimPool(alpha = alpha, data_pool = comparison_pool,
                           lr_result = lr_result,
                           weighted_pooled_stdev = weighted_pooled_stdev)

  # Using matching algorithm to find top matches
  matches <- createMatches(trimmed_pool, num_matches, replacement)
  # Add additional clmns: total_matches, treatment_weight, control_matches
  matches <- addMatchesColumns(matches)
  # Create control weights - add to matches dataset and final dataset
  out_list <- createWeights(matches, data, id)
  matches <- out_list$matches
  data_full <- out_list$data_full
  # Combine datasets and values in preperation for output
  combined_output <- makeOutput(pred_model, lr_result, data_full, matches,
                                orig.call, formula, tm, entry, lookback)
  # Add balance table to the output
  out <- addBalanceTable(reduced_data, vars, tm, id, combined_output,
                         treat, matches)
  # Set the class
  class(out) <- "rem"

  return(out)
}
