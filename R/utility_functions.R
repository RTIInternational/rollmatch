#----------------------------#
# UTILITY / HELPER FUNCTIONS #
#----------------------------#

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


#' Create a dataframe of comparisons between all treatment and control data.
#' 
#' @param scored_data The dataframe from score_data()
#' @param tm See rollmatch()
#' @param entry See rollmatch()
#' @param id See rollmatch()
#' 
#' @examples
#' \dontrun{ 
#' print('See add_balance_table for full example')
#' }
#'
#' @return Dataframe comparing all treatment and control data
#' @keywords internal
compare_pool <- function(scored_data, treat, tm, entry, id){
  comparison_pool <-
    dplyr::inner_join(scored_data[scored_data[[treat]] == 1, ],
                      scored_data[scored_data[[treat]] == 0, ], by = c(tm))
  comparison_pool$difference <-
    abs(comparison_pool$score.x - comparison_pool$score.y)
  comparison_pool <-
    dplyr::select(comparison_pool,
                  dplyr::one_of(tm, paste0(id, ".x"), paste0(id, ".y")),
                  paste0(entry, ".x"), paste0(entry, ".y"),
                  "score.x", "score.y", "difference")
  names(comparison_pool) <- c(tm, "treat_id", "control_id",
                              paste0("treat_", entry),
                              paste0("control_", entry),
                              "treat_score", "control_score", "difference")
  return(comparison_pool)
}

#' Use a caliper to trim the data to only observations within threshold
#'
#' @param alpha See rollmatch()
#' @param comparison_pool Dataframe of comparison data to be trimmed from
#' compare_pool()
#' @param scored_data Dataframe of results from score_data()
#' @param treat See rollmatch()
#' @param tm See rollmatch()
#' @param standard_deviaion See rollmatch()
#' 
#' @examples
#' \dontrun{ 
#' print('See add_balance_table for full example')
#' }
#'                          
#' @return Dataframe of the trimmed comparisons based on the alpha value
#' @keywords internal
trim_pool <- function(alpha, comparison_pool, scored_data, treat, tm,
                      standard_deviation = "average"){
  s_df <- scored_data

  if (dim(comparison_pool)[1] == 0){
    stop("comparison_pool is empty")
  }
  if (dim(scored_data)[1] == 0){
    stop("scored_data is empty")
  }

  if (alpha != 0) {
    var_treat <- var(s_df[(s_df[[treat]] == 1), "score"])
    var_untreat <- var(s_df[(s_df[[treat]] == 0), "score"])

    if (standard_deviation == "average"){
      pooled_stdev <- sqrt( (var_treat + var_untreat) / 2)
    } else if (standard_deviation == "weighted"){
      pooled_stdev <-
        sqrt( ( (nrow(s_df[(s_df[[treat]] == 1), ]) - 1) * var_treat +
                 (nrow(s_df[(s_df[[treat]] == 0), ]) - 1) * var_untreat) /
        (dim(s_df)[1] - 2))
    } else {
      pooled_stdev <- 1
    }

    width <- alpha * pooled_stdev
    trimmed_pool <-
      dplyr::filter(comparison_pool,
                    comparison_pool$difference <= as.numeric(width))
  } else {
    trimmed_pool <- comparison_pool
  }
  trimmed_pool <-
    trimmed_pool[order(trimmed_pool[[tm]],
                      trimmed_pool$treat_id, trimmed_pool$difference), ]
  return(trimmed_pool)
}


#' Algorithm to find best matches from the comparison pool
#'
#' @param trimmed_pool Dataframe containing the pool from which matches
#' should be found
#' @param tm See rollmatch()
#' @param num_matches See rollmatch()
#' @param replacement See rollmatch()
#' 
#' @examples
#' \dontrun{ 
#' print('See add_balance_table for full example')
#' }
#'
#' @return Dataframe containing top matches
#' @keywords internal
create_matches <- function(trimmed_pool, tm, num_matches = 3,
                           replacement = TRUE){
  names(trimmed_pool)[1] <- "time"
  difference <- ""; num.assigned <- ""; treat_id <- ""; control_id <- "";

  # initialize matches as empty
  matches <- trimmed_pool[0, ]

  count <- 1
  # Loop
  repeat {
    # first_choice is the first entry in the comparison pool for each treat_id
    first_choice <- trimmed_pool[!duplicated(trimmed_pool$treat_id), ]

    if (nrow(first_choice) == 0)
      break

    if (replacement == TRUE){
      # Deal with matches that match in more than one quarter
      multi_quarter <- aggregate(time ~ control_id, first_choice,
                                 function(x) length(unique(x)))
      multi_quarter <- multi_quarter[multi_quarter$time > 1, ]

      # Initialize empty multicompare data frame
      cnames <- c("time", "treat_id", "control_id", "difference")
      matched_multi_compare <-
        data.frame(matrix(vector(), 0, 4, dimnames = list(c(), cnames)))

      if (nrow(multi_quarter) != 0) {
        multi_compare <-
          aggregate(difference ~ time + control_id,
                    first_choice[first_choice$control_id %in%
                                   multi_quarter$control_id, ], FUN = mean)

        multi_compare <- multi_compare[order(multi_compare$control_id,
                                             multi_compare$difference), ]
        multi_compare_assigned <-
          multi_compare[!duplicated(multi_compare$control_id), ]

        matched_multi_compare <- # Todo - Change to DPLYR
          merge(multi_compare_assigned[, c("time", "control_id")], first_choice,
                by = c("control_id", "time")) #, "difference"))
      }

      # Deal with matches in single quarter - these can be assigned directly
      matched_single_compare <-
        first_choice[!first_choice$control_id %in% multi_quarter$control_id, ]
      current_matches <-
        dplyr::bind_rows(matched_multi_compare, matched_single_compare)
    } else {
      first_choice <- first_choice[order(first_choice$difference), ]
      first_choice <- first_choice[!duplicated(first_choice$control_id), ]
      current_matches <- first_choice[order(first_choice$difference), ]
    }
    # Break out of loop if no matches were assigned
    if (nrow(current_matches) == 0)
      break

    matches <- dplyr::bind_rows(matches, current_matches)

    if (nrow(trimmed_pool) > 0) {
      #filter out assigned treatment/match pairs
      trimmed_pool <- dplyr::setdiff(trimmed_pool, current_matches)

      # Keep records where control_id is not in unique(matches$control_id)
      diff_control_id <-
        dplyr::filter(trimmed_pool,
                      !(control_id %in%
                          unique(current_matches$control_id) ))

      if (replacement){
        # if replacement TRUE, keep records where control_id is in
        # matches$control_id and time is the same as the matched time
        keep <-
          dplyr::inner_join(
            trimmed_pool,
            unique(current_matches[, c("time", "control_id")]),
            by = c("control_id", "time"))
      }else{
        keep <- NULL
      }
      #combine the rows to keep and re-sort comparison pool
      trimmed_pool <-
        dplyr::arrange(dplyr::bind_rows(diff_control_id, keep),
                       time, treat_id, difference)

      # If num_matches matches have been assigned, remove the treatments
      # from the comparison pool
      matches_count <- matches[matches$treat_id %in%
                                 unique(current_matches$treat_id), ] %>%
        dplyr::group_by(treat_id) %>%
        dplyr::summarise(num.assigned = n()) %>%
        dplyr::filter(num.assigned == num_matches)
      trimmed_pool <-
        dplyr::filter(trimmed_pool,
                      !(treat_id %in% matches_count$treat_id ))

    } else break  # break out of loop if comparison pool is empty
    count <- count + 1
  }
  names(matches)[1] <- tm
  return(matches)
}


#' Create additional columns for the matches dataset
#'
#' @param Matches Dataframe containing the matches from comparison_pool
#' 
#' @examples
#' \dontrun{ 
#' print('See add_balance_table for full example')
#' }
#'
#' @return Dataframe containing top matches
#' @keywords internal
add_matches_columns <- function(matches){
  control_id <- ""; treatment_weight <- "";
  # Assign a number to the matches.  1st, 2nd 3rd, ...
  matches$match_rank <- ave(1:nrow(matches), matches$treat_id, FUN = seq_along)
  matches <- matches[order(matches$treat_id, matches$match_rank), ]
  # Calculate number of total matches for a given treat_id
  matches$total_matches <- ave(1:nrow(matches), matches$treat_id, FUN = length)
  # Add treatment_weight
  matches$treatment_weight <- 1
  # Calculate the weight of each control
  matches$control_matches <- ave(1:nrow(matches),
                                 matches$control_id, FUN = length)
  matches$treatment_weight <- 1 / matches$total_matches

  # Add the weight of the treatment and control variables
  agg_wt <- matches %>%
    dplyr::group_by(control_id) %>%
    dplyr::summarise(total.weight = sum(treatment_weight))
  names(agg_wt) <- c("control_id", "control_weight")
  # Add weight to matches dataset
  matches <- merge(matches, agg_wt, by = "control_id")
  return(matches)
}


#' Combine the results of rollmatch into a tidy list for output
#'
#' @param scored_data The dataframe from score_data()
#' @param data See rollmatch()
#' @param matches Dataframe containing the matches from comparison_pool
#' @param tm See rollmatch()
#' @param id See rollmatch()
#' @param entry See rollmatch()
#' @param lookback See rollmatch()
#' 
#' @examples
#' \dontrun{ 
#' print('See add_balance_table for full example')
#' }
#' 
#' @return \code{output} returns a list. See rollmatch()
#' @keywords internal
make_output <- function(scored_data, data, matches,
                        treat, tm, entry, id, lookback){

  out <- list()
  out$scores <- scored_data$score

  # Assign weights back to panel dataset
  m <- matches[, c(tm, "control_id", "treat_id", "match_rank")]
  matches_wide <- reshape(m, v.names = c("control_id"),
                          idvar = "treat_id", timevar = "match_rank",
                          direction = "wide",
                          drop = c("treatment_weight", "control_matches"))

  names(matches_wide)[2] <- id
  data_full <- dplyr::left_join(data, matches_wide, by = c(tm, id))

  out$data <- data_full
  # Number of Rows for Output
  treat_set <- data_full[data_full[[treat]] == 1 &
                           (data_full[[tm]] == data_full[[entry]] - lookback), ]
  comp_set <- data_full[data_full[[treat]] == 0 &
                          (data_full[[tm]] %in% unique(treat_set[[tm]])), ]

  nn <- matrix(0, ncol = 2, nrow = 3)
  treat_assigned <- length(unique(matches$treat_id))
  control_assigned <- length(unique(matches$control_id))
  nn[1, ] <- c(nrow(comp_set), nrow(treat_set))
  nn[2, ] <- c(control_assigned, treat_assigned)
  nn[3, ] <- c( (nrow(comp_set) - control_assigned),
                (nrow(treat_set) - treat_assigned))
  dimnames(nn) <- list(c("All", "Matched", "Unmatched"),
                       c("Control", "Treated"))
  out$summary <- nn

  all_ids <- unique(scored_data$indiv_id[scored_data[[treat]] == 1])
  discarded <- all_ids[ !(all_ids %in% unique(matches$treat_id))]
  out$ids_not_matched <- discarded
  out$total_not_matched <- length(discarded)
  out$matched_data <- matches
  return(out)
}

#' Add the balancing table to the final output
#'
#' @param scored_data The dataframe from score_data()
#' @param vars See rollmatch()
#' @param tm See rollmatch()
#' @param id See rollmatch()
#' @param combined_output A list of output for the rollmatch package. 
#' See make_output
#' @param treat See rollmatch()
#' @param matches Dataframe containing the matches from comparison_pool
#' 
#' @examples
#' \dontrun{
#' data(package="rollmatch", "rem_synthdata_small")
#' reduced_data <- reduce_data(data = rem_synthdata_small, treat = "treat",
#'                             tm = "quarter", entry = "entry_q",
#'                             id = "indiv_id", lookback = 1)
#' fm <- as.formula(treat ~ qtr_pmt + yr_pmt + age)
#' vars <- all.vars(fm)
#' scored_data <- score_data(model_type = "logistic", match_on = "logit", fm = fm,
#'                           reduced_data = reduced_data, treat = "treat",
#'                           tm = "quarter", entry = "entry_q", id = "indiv_id")
#' comparison_pool <- compare_pool(scored_data, treat = "treat",
#'                                 tm = "quarter", entry = "entry_q",
#'                                 id = "indiv_id")
#' trimmed_pool <- trim_pool(alpha = .2, comparison_pool = comparison_pool,
#'                           scored_data = scored_data, treat = "treat",
#'                           tm = "quarter", standard_deviation = 'average')
#' matches <- create_matches(trimmed_pool = trimmed_pool, tm = "quarter",
#'                           num_matches = 3, replacement = TRUE)
#' matches <- add_matches_columns(matches)
#' combined_output <- make_output(scored_data = scored_data,
#'                                data = rem_synthdata_small,
#'                                matches = matches,
#'                                treat = "treat", tm = "quarter",
#'                                entry = "entry_q", id = "indiv_id", lookback = 1)
#' # Add balance table to the output
#' output <- add_balance_table(scored_data = scored_data, vars = vars,
#'                             tm = "quarter", id = "indiv_id",
#'                             combined_output = combined_output,
#'                             treat = "treat", matches = matches)
#' }
#' 
#' @return \code{output} returns a list with the additional output:
#' \item{balance}{The balancing table.}
#' 
#' @keywords internal
add_balance_table <- function(scored_data, vars, tm, id, combined_output,
                              treat, matches){
  treat_group <- scored_data[, vars] %>% dplyr::group_by(treat)

  full_summary <-
    cbind(as.data.frame(t(dplyr::summarise_all(treat_group, mean))),
          as.data.frame(t(dplyr::summarise_all(treat_group, "sd"))))
  names(full_summary) <-
    c("Full Comparison Mean", "Full Treatment Mean",
      "Full Comparison Std Dev", "Full Treatment Std Dev")

  ta <- matches[, c(tm, "treat_id")]
  ca <- matches[, c(tm, "control_id")]
  names(ta) <- c(tm, id)
  names(ca) <- c(tm, id)

  data_assigned <- merge(scored_data, unique(rbind(ta, ca)))

  treat_group <- data_assigned[, vars] %>% dplyr::group_by(treat)

  matched_summary <-
    cbind(as.data.frame(t(dplyr::summarise_all(treat_group, mean))),
          as.data.frame(t(dplyr::summarise_all(treat_group, "sd"))))

  names(matched_summary) <-
    c("Matched Comparison Mean", "Matched Treatment Mean",
      "Matched Comparison Std Dev", "Matched Treatment Std Dev")

  combined_output$balance <- cbind(full_summary, matched_summary)

  combined_output$balance <-
    combined_output$balance[-1, c("Full Treatment Mean", "Full Comparison Mean",
                      "Full Treatment Std Dev", "Full Comparison Std Dev",
                      "Matched Treatment Mean", "Matched Comparison Mean",
                      "Matched Treatment Std Dev",
                      "Matched Comparison Std Dev")]

  return(combined_output)
}
