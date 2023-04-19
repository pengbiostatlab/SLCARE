#' @title data pre-processing
#' @description pre-process data into long and wide formats that fit functions in this R package
#' @param dat a data frame containing the data in the model
#' @return A list containing the following components:
#' \tabular{ll}{
#'    \code{id_wide} \tab A vector of subjects ID in wide format \cr
#'    \tab \cr
#'    \code{d} \tab A vector of observed recurrent events for subjects of interest \cr
#'    \tab \cr
#'    \code{Z} \tab A vector of time-independent corvariates in wide format \cr
#'    \tab \cr
#'    \code{censor_wide} \tab A vector of censoring time (longest follow up time) in wide format \cr
#'    \tab \cr
#'    \code{id_long} \tab A vector of subjects ID in long format \cr
#'    \tab \cr
#'    \code{time_long} \tab A vector of long format time for events (excluding censoring time) \cr
#'    \tab \cr
#'    \code{censor_long} \tab A vector of censoring time (longest follow up time) in long format \cr
#'    \tab \cr
#'    \code{Z_long} \tab A vector of time-independent corvariates in long format \cr
#' }
PreprocessData <- function(dat = dat){
  long_format <- dat |> filter(event == 1)
  wide_format <- dat |> filter(event == 0)

  id_censor <- wide_format |> select(ID, time) |> rename(censor = time)

  long_format <- long_format |> left_join(id_censor, by = "ID")

  id_long <- long_format$ID
  time_long <- long_format$time
  censor_long <- long_format$censor
  Z_long <- long_format |> select(-c("ID","time","event","censor"))

  ID_d <- long_format |> group_by(ID) |> summarise(d = n())

  wide_format <- wide_format |> left_join(ID_d, by = "ID") |> replace_na(list(d = 0))


  id_wide <- wide_format$ID
  Z <- wide_format |> select(-c("ID","time","event","d"))
  censor_wide <- wide_format$time
  d <- wide_format$d
  #id_wide, d, Z, censor_wide,
  #id_long, time_long, censor_long
  return(list("id_wide" = id_wide, "d" = d, "Z" = Z, "censor_wide" = censor_wide,
              "id_long" = id_long, "time_long" = time_long, "censor_long" = censor_long, "Z_long" = Z_long))
}
