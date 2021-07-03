.create_zip_dcid_map <- function(zips) {
  zip_map <- sapply(zips, 
                    function(x) paste0("zip/", x), 
                    simplify = FALSE, USE.NAMES = TRUE)
  return (zip_map)
}

.create_state_dcid_map <- function(states) {
  state_map <- sapply(states,
                      function(x) paste0("geoId/",
                                         if (.is_state_fips(x)) x
                                         else .get_state_fips(x)),
                      simplify = FALSE, USE.NAMES = TRUE)
  return (state_map)
}

.create_county_dcid_map <- function(counties) {
  county_map <- sapply(counties,
                      function(x) paste0("geoId/",
                                         if (.is_county_fips(x)) x
                                         else .get_county_fips(x)),
                      simplify = FALSE, USE.NAMES = TRUE)
  return (county_map)
}

.create_city_dcid_map <- function(cities) {
  city_map <- sapply(cities,
                     function(x) paste0("geoId/",
                                        if (.is_city_fips(x)) x
                                        else .get_city_fips(x)),
                       simplify = FALSE, USE.NAMES = TRUE)
  return (city_map)
}

.create_school_district_dcid_map <- function(school_districts) {
  school_district_map <- sapply(school_districts, 
                    function(x) paste0("geoId/sch", 
                                       if (.is_school_district_fips(x)) x
                                       else .get_school_district_fips(x)), 
                    simplify = FALSE, USE.NAMES = TRUE)
  return (school_district_map)
}

.create_geo_dcid_map <- function(geo_names, location_type=NA) {
  if (is.na(location_type)) {
    sample_name = geo_names[1]
    location_type <- .determine_location_type(sample_name)
  }
  switch(location_type,
         zip = .create_zip_dcid_map(geo_names),
         city = .create_city_dcid_map(geo_names),
         state = .create_state_dcid_map(geo_names),
         county = .create_county_dcid_map(geo_names),
         school = .create_school_district_dcid_map(geo_names))
}

.determine_location_type <- function(geo_name) {
  if (.is_zip_code(geo_name)) {
    return ("zip")
  } else if (.is_city_fips(geo_name)) {
    return ("city")
  } else if (.is_state_abbrv(geo_name)) {
    return ("state")
  } else if (.is_state_fips(geo_name)) {
    return ("state")
  } else if (geo_name %in% US_STATES) {
    return ("state")
  } else {
    stop("Unable to determine the location type automatically. Consider to specifying it using the 'location_type' argument")
  }
}

.is_zip_code <- function(s) {
  return (grepl("^\\d{5}$", s))
}

.is_city_fips <- function(s) {
  return (grepl("^\\d{7}$", s))
}

.is_county_fips <- function(s) {
  return (grepl("^\\d{5}$", s))
}

.is_state_fips <- function(s) {
  return (grepl("^\\d{2}$", s))
}

.is_state_abbrv <- function(s) {
  return (grepl("^[A-Z]{2}$", s))
}

.is_school_district_fips <- function(s) {
  return (grepl("^\\d{7}$", s))
}