#' @title Mesowest API station metadata interface
#' @description Collects station metadata from Mesowest.
#' @param token Your API token
#' @details See \url{https://synopticlabs.org/api/mesonet/reference/#stationsmetadata} for query descriptions.
get_metadata <- function(token,
                         stations_args = list(stids = NULL, state = NULL, country = NULL,
                         nwszone = NULL, nwsfirezone = NULL, cwa = NULL, gacc = NULL,
                         subgacc = NULL, county = NULL, vars = NULL, network = NULL,
                         radius = NULL, bbox = NULL, status = NULL, groupby = NULL,
                         complete = NULL),
                         metadata_args = list(sensorvars = NULL, obrange_start = NULL,
                                              obrange_end = NULL)){
  base_url <- getOption("mesowest.base_url")

  stations_string <- get_argstring(stations_args)

  metadata_string <- get_argstring(metadata_args)

  api_string <- paste0("stations/metadata?&token=", token, stations_string, metadata_string)

  stations_list <- jsonlite::fromJSON(paste0(base_url, api_string))
  if(stations_list$SUMMARY$RESPONSE_MESSAGE == "OK"){
    stations_list$STATION$PERIOD_OF_RECORD_start <-  stations_list$STATION$PERIOD_OF_RECORD$start
    stations_list$STATION$PERIOD_OF_RECORD_end <-  stations_list$STATION$PERIOD_OF_RECORD$end
    stations_list$STATION$PERIOD_OF_RECORD <- NULL

    stations_df <- stations_list$STATION %>%
      tibble::as_tibble() %>%
      dplyr::mutate(ELEVATION = as.double(ELEVATION),
                    LONGITUDE = as.double(LONGITUDE),
                    LATITUDE = as.double(LATITUDE),
                    ID = as.integer(ID),
                    PERIOD_OF_RECORD_start =
                      lubridate::parse_date_time(PERIOD_OF_RECORD_start, orders = "YmdHMS"),
                    PERIOD_OF_RECORD_end =
                      lubridate::parse_date_time(PERIOD_OF_RECORD_end, orders = "YmdHMS"))
    return(stations_df)
  } else {
    stop("Station metadata download failed.")
  }
}
