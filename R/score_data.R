#' Run a logistic or probit model
#'
#' @param model_type Use logistic regression ("logistic") or "probit"
#' regression ("probit") to estimate the predicted probability of participating
#' @param match_on Match on estimated propensity score ("pscore") or logit of
#' estimated propensity score ("logit").
#' @param reduced_data Dataframe of reduced treatment and comparison data
#' @param tm The time period indicator.
#' @param entry Entry quarter variable in dataset
#' @param id ID variable in dataset
#' @param formula A \code{\link[stats]{formula}} in the form
#' \code{treat ~ x1 + x2 ...} where \code{treat} is a binary treatment
#' indicator (Treat = 1, Control = 0) and \code{x1} and \code{x2} are
#' pre-treatment covariates. Both the treatment indicator and
#' pre-treatment covariates must be contained in the input dataset.
#' 
#' @examples
#' \dontrun{
#' data(package="rollmatch", "rem_synthdata_small")
#' fm <- as.formula(treat ~ qtr_pmt + age + is_male + is_white)
#' reduced_data <- reduce_data(data = rem_synthdata_small, treat = "treat",
#'                             tm = "quarter", entry = "entry_q",
#'                             id = "indiv_id", lookback = 1)
#' scored_data <- score_data(model_type = "logistic", match_on = "logit",
#'                           fm = fm, reduced_data = reduced_data,
#'                           treat = "treat", tm = "quarter", entry = "entry_q",
#'                           id = "indiv_id")
#' head(scored_data)
#' }
#'
#' @return A dataframe with added propensity scores
#' 
#' @export

score_data <- function(model_type, match_on, fm, reduced_data,
                       treat, tm, entry, id){

  run_checks_one(reduced_data, tm, entry, id)
  # TODO - Write checks for fm
  
  # Convert Columns to Factor
  vars <- all.vars(fm)
  reduced_data <- change_to_factor(reduced_data, vars)
  
  # get treatment variable from formula
  treat <- vars[1]
  
  if (model_type %in% c("logistic", "probit")){
    if (model_type == "logistic") {
      link_type <- "logit"
    } else {
      link_type <- "probit"
    }
    pred_model <- glm(fm, data = reduced_data,
                      family = binomial(link = link_type))
  } else {
    stop("model_type must be set to either logistic or probit")
  }
  
  scored_data <- reduced_data[, c(id, treat, entry, tm)]
  new_obs <- length(pred_model$fitted.values)
  if (dim(scored_data)[1] != new_obs){
    cnames <- colnames(reduced_data)[colSums(is.na(reduced_data)) > 0]
    stop(paste0("Propensity model could not create prediction for all
                observations. Check your data for issues. If any columns had
                NAs, they are printed here: ", cnames))
  }
  
  if (match_on %in% c("logit", "pscore")){
    if (match_on == "logit") {
      scored_data$score <- qlogis(pred_model$fitted.values)
    } else {
      scored_data$score <- pred_model$fitted.values
    }
  } else {
    stop("match_on must be set to either logit or pscore.")
  }
  return(scored_data)
}