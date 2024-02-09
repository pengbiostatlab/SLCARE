#' @title Pre-processing Data
#' @description Process input data-set into a list of matrices.
#' @param data a long-format Data-frame.
#' @param id_col the unique identifier per subject.
#' @param start_col start time of the interval for the recurrent event.
#' @param stop_col ending time of the interval for the recurrent event.
#' @param event_col The status indicator; 1 if a recurrent event is observed.
#' @param formula A string specifying the variables of interest to be involved in the regression.
#' @return A list of matrices containing the following components:
#' \tabular{ll}{
#'    \code{id_long} \tab long-format subject identifier. \cr
#'    \tab \cr
#'    \code{time_long} \tab long-format observed event time. \cr
#'    \tab \cr
#'    \code{censor_long} \tab long-format censoring time (longest follow up time). \cr
#'    \tab \cr
#'    \code{Z_long} \tab long-format variables of interest. \cr
#'    \code{id_wide} \tab wide-format subject identifier. \cr
#'    \tab \cr
#'    \code{event_num} \tab wide-format number of observed events per subject. \cr
#'    \tab \cr
#'    \code{Z} \tab wide-format variables of interest. \cr
#'    \tab \cr
#'    \code{censor_wide} \tab wide-format censoring time (longest follow up time). \cr
#'    \tab \cr
#' }
#' @noRd
PreprocessData <- function(data = data, id_col = "id", start_col = "start", stop_col = "stop", event_col = "event", formula = "x1 + x2") {
  event_time <- NULL
  data <- data %>%
    group_by(.data[[id_col]]) %>%
    mutate(event_time = .data[[stop_col]] - .data[[start_col]][1])
  covariate_cols <- trimws(unlist(strsplit(formula, "\\+")))
  selected_data <- data[, c(id_col, "event_time", event_col, covariate_cols)]
  selected_long <- selected_data %>% filter(.data[[event_col]] == 1)
  selected_wide <- selected_data %>% filter(.data[[event_col]] == 0)
  id_censor <- selected_wide %>%
    select(.data[[id_col]], event_time) %>%
    rename(censor = event_time)
  selected_long <- selected_long %>% left_join(id_censor, by = id_col)
  id_long <- selected_long[, id_col]
  time_long <- selected_long[, "event_time"]
  censor_long <- selected_long[, "censor"]
  Z_long <- selected_long[, c(covariate_cols)]
  id_event_num <- selected_long %>%
    group_by(.data[[id_col]]) %>%
    summarise(event_num = n())
  selected_wide <- selected_wide %>%
    left_join(id_event_num, by = id_col) %>%
    replace_na(list(event_num = 0))
  id_wide <- selected_wide[, id_col]
  event_num <- selected_wide[, "event_num"]
  censor_wide <- selected_wide[, "event_time"]
  Z <- selected_wide[c(covariate_cols)]
  return(list(
    "id_wide" = id_wide, "event_num" = event_num, "Z" = Z, "censor_wide" = censor_wide,
    "id_long" = id_long, "time_long" = time_long, "censor_long" = censor_long, "Z_long" = Z_long
  ))
}
