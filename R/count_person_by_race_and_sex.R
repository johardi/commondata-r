#' Return the person count per age group for the given the geographical names
#' (i.e., zip codes, state codes, state names, county names, city names or
#' school district) and the observation year period. The count is sub-divided 
#' by race and sex category.
#' 
#' @param geo_names required, vector of string(s) of geographical names
#' @param location_type optional, string indicating the location type of the
#'    geographical names. If the location_type is blank then the function will
#'    try to guess the location type based on the input geo_names. NA by default.
#' @param start_year optional, integer indicating the start year of observation.
#'    2011 by default.
#' @param end_year optional, integer indicating the end year of observation.
#'    2018 by default.
#' @param year optional, integer indicating a single year of observation.
#'    This parameter overrides the start_year and end_year parameters when
#'    it is not NA, such that start_year=year and end_year=year. NA by default.
#' @return A named list with each list item is a data frame containing the 
#'    person count of each region per age group. The data frame is identified
#'    by the observation year, the race group and the sex category.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the population in the specified ZIP codes
#' count_person_by_race_and_sex(zips)
#' 
#' # Count the population from 2012 to 2015
#' count_person_by_race_and_sex(zips, start_year=2012, end_year=2015)
#' 
#' # Count the population in 2012
#' count_person_by_race_and_sex(zips, year=2012)
#' 
#' # Count the population in the state of California
#' count_person_by_race_and_sex(c("California")) # State name
#' count_person_by_race_and_sex(c("CA")) # State code
#' count_person_by_race_and_sex(c("06")) # 2-digit state FIPS code
count_person_by_race_and_sex <- function(geo_names,
                                   location_type=c(NA, "zip", "city", "county", "state", "school"),
                                   start_year=2011, end_year=2018, year=NA) {
  
  location_type <- match.arg(location_type)
  geo_map <- .create_geo_dcid_map(geo_names, location_type)
  
  statvar_map <- list()
  for (race_group in CENSUS_RACE_GROUPS) {
    for (sex_category in CENSUS_SEX_CATEGORIES) {
      statvar_map[[race_group]][[sex_category]] <- 
        sapply(CENSUS_PERSON_WITH_RACE_AGE_GROUPS,
               function(x) paste0("Count_Person_", x, "_", sex_category, "_", race_group),
               simplify = FALSE, USE.NAMES = TRUE)
    }
  }
  
  return (.get_statistical_data(geo_map, statvar_map, start_year, end_year, year))
}