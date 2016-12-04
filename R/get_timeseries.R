#' @title Mesowest API station metadata interface
#' @description Collects station metadata from Mesowest.
#' @param token Your API token
#' @details See \url{https://synopticlabs.org/api/mesonet/reference/#stationsmetadata} for query descriptions.
get_timeseries <- function(token,
                           stations_args = list(stids = NULL, state = NULL, country = NULL,
                                              nwszone = NULL, nwsfirezone = NULL, cwa = NULL, gacc = NULL,
                                              subgacc = NULL, county = NULL, vars = NULL, network = NULL,
                                              radius = NULL, bbox = NULL, status = NULL, groupby = NULL,
                                              complete = NULL),
                         timeseries_args = list(start = NULL, end = NULL, obtimezone = NULL,
                                                showemptystations = NULL, units = NULL,
                                                recent = NULL, timeformat = NULL,
                                                output = NULL, callback = NULL)){
  base_url <- getOption("mesowest.base_url")

  stations_string <- get_argstring(stations_args)

  timeseries_string <- get_argstring(timeseries_args)

  api_string <- paste0("stations/timeseries?&token=", token, stations_string, timeseries_string)

  stations_list <- jsonlite::fromJSON(paste0(base_url, api_string))

  if(stations_list$SUMMARY$RESPONSE_MESSAGE == "OK"){
    stations_list$STATION$PERIOD_OF_RECORD_start <-  stations_list$STATION$PERIOD_OF_RECORD$start
    stations_list$STATION$PERIOD_OF_RECORD_end <-  stations_list$STATION$PERIOD_OF_RECORD$end
    stations_list$STATION$PERIOD_OF_RECORD <- NULL

    # These seem to be empty
    stations_list$STATION$SENSOR_VARIABLES <- NULL

    # Data are basically arranged as a nested data.frame, they
    # just need to be told how.
    stations_list$STATION$OBSERVATIONS <-
      stations_list$STATION$OBSERVATIONS %>%
      purrr::by_row(function(obs_row){
        purrr::map(obs_row, function(col){
          col[[1]]
        }) %>%
          tibble::as_tibble()
      }) %>%
      dplyr::select_(".out") %>%
      .[[1]]

    stations_list$STATION <- stations_list$STATION %>%
      tibble::as_tibble()

    stations_list$STATION <- stations_list$STATION %>%
      dplyr::mutate(ELEVATION = as.double(ELEVATION),
                    LONGITUDE = as.double(LONGITUDE),
                    LATITUDE = as.double(LATITUDE),
                    ID = as.integer(ID),
                    PERIOD_OF_RECORD_start =
                      lubridate::parse_date_time(PERIOD_OF_RECORD_start, orders = "YmdHMS"),
                    PERIOD_OF_RECORD_end =
                      lubridate::parse_date_time(PERIOD_OF_RECORD_end, orders = "YmdHMS"))
    return(stations_list)
  } else {
    stop("Station timeseries download failed.")
  }
}
