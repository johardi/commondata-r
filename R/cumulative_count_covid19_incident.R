#' Return the cumulative count of COVID-19 incident per case category for the
#' given the geographical names (i.e., zip codes, state codes, state names, 
#' county names) and the observation date period.
#' 
#' @param geo_names required, vector of string(s) of geographical names.
#' @param location_type optional, string indicating the location type of the
#'    geographical names. NA by default.
#' @param start_date optional, date indicating the start date of observation.
#'    "2020-01-01" by default.
#' @param end_date optional, date indicating the end date of observation.
#'    "2020-03-31" by default.
#' @param date optional, date indicating a single date of observation.
#'    This parameter overrides the start_date and end_date parameters when
#'    it is not NA, such that start_date=date and end_date=date. NA by default.
#' @return  A named list with each list item is a data frame containing the 
#'    cumulative count observations of COVID-19 incident per case category.
#'    The data frame is identified by the observation date.
#'
#' @export
#' @examples
#' states <- c("Alabama","California","New York","Pennsylvania","Washington")
#' 
#' # Count the cumulative count of COVID-19 incidents in the specified states
#' cumulative_count_covid19_incident(states)
#' 
#' # Count the cumulative count of COVID-19 incidents from mid-June until today
#' cumulative_count_covid19_incident(states, start_date="2020-06-30", end_date=Sys.Date())
#' 
#' # Count the cumulative count of COVID-19 incidents 10 days ago from today
#' cumulative_count_covid19_incident(states, date=Sys.Date()-10)
cumulative_count_covid19_incident <- function(geo_names,
                                              location_type=c(NA, "city", "zip", "county", "state"),
                                              start_date="2020-01-01",
                                              end_date="2020-03-31",
                                              date=NA) {
  
  location_type <- match.arg(location_type)
  geo_map <- .create_geo_dcid_map(geo_names, location_type)
  
  start_date <- as.Date(start_date, format="%Y-%m-%d")
  end_date <- as.Date(end_date, format="%Y-%m-%d")
  date <- if (!is.na(date)) as.Date(date, format="%Y-%m-%d") else date
  
  statvar_map <- sapply(COVID19_INCIDENT_CASES, 
                        function(x) paste0("CumulativeCount_MedicalConditionIncident_COVID_19_", x), 
                        simplify = FALSE, USE.NAMES = TRUE)
  
  return (.cumulative_count_covid19_incident(geo_map, statvar_map, start_date, end_date, date))
}

.cumulative_count_covid19_incident <- function(geo_map, statvar_map, start_date, end_date, date) {
  
  return (.get_statistical_data(geo_map, statvar_map, start_date, end_date, date))
}