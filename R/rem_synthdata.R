#' Synthetic dataset to illustrate rolling entry
#' 
#' This dataset represents a synthetic population of individuals who resemble 
#' Medicare fee-for-service patients in terms of age, race, spending, 
#' inpatient visits, ED visits, chronic conditions, and dual eligibility.
#' The quasi-panel dataset contains multiple observations of non-participants 
#' (one for each entry period). Participants enter the data once in the baseline 
#' period immediately preceding their unique entry into the intervention. 
#' Time-varying covariates (e.g., health conditions, spending, utilization) are 
#' dynamic for each entry period's non-participant observations.
#'
#' @format A data frame with 254,400 observations and 20 variables:
#'\describe{
#'    \item{indiv_id}{The unique identifier for each individual.}
#'    \item{entry_q}{The period in which the individual enrolled in treatment / entered the intervention.}
#'    \item{lq}{Last baseline quarter before entry into the intervention.}
#'    \item{quarter}{Time variable, indicating the quarter that the variables are measured.}
#'    \item{treat}{Treatment indicator variable (=1 if in treatment group and =0 if in control group).}
#'    \item{age}{The patient's age.}
#'    \item{is_black}{Race indicator variable (=1 if identified as Black, =0 if not).}
#'    \item{is_disabled}{Physical disability indicator variable (=1 if identified as disabled, =0 if not).}
#'    \item{is_esrd}{Disease indicator variable (=1 if identified as having End Stage Renal Disease (ESRD), =0 if not).}
#'    \item{is_hispanic}{Ethnicity indicator variable (=1 if identified as Hispanic, =0 if not).}
#'    \item{is_male}{Gender indicator variable (=1 if identified as Male, =0 if not).}
#'    \item{is_white}{Race indicator variable (=1 if identified as White, =0 if not).}
#'    \item{lq_ed}{Indicates the person had an ED visit during LQ.}
#'    \item{lq_ip}{Indicates the person had an inpatient stay during LQ.}
#'    \item{yr_ed2}{Count of ED visits during quarters LQ-5 to LQ-1.}
#'    \item{yr_ip2}{Count of inpatient stays during quarters LQ-4 to LQ-1.}
#'    \item{months_dual}{Number of months of dual Medicare-Medicaid eligibility in the previous year.}
#'    \item{chron_num}{Number of chronic conditions.}
#'    \item{qtr_pmt}{Payments during the quarter.}
#'    \item{yr_pmt}{Payments during the previous 4 quarters.}
#'}
#'
"rem_synthdata"
