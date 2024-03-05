
#' Title: A function for making weather forecasts
#'
#' @param value A string of length 1 or a numeric vector of length 2 for Address or Latitude and Longitude values.
#'
#' @description
#' This function uses the value of the object provided to the function and returns weather forecasts.
#'
#' @return A tibble containing the forecast about the temperature, the apparent temperature, the precipitation probability, and the precipitation in mm.
#'
#' @export
#' @import tibble
#' @import httr2
#' @import tidygeocoder
#'
#' @importFrom tibble as.tibble
#' @importFrom tibble tibble
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 req_perform
#' @importFrom httr2 req_url_query
#' @importFrom httr2 request
#' @importFrom tidygeocoder geocode
#'
#' @examples
#' get_forecast("1600 Amphitheatre Parkway, Mountain View, CA")
#' get_forecast(c(68, 4.2))
#'
#'
#' @family weather functions
get_forecast <- function(value){
  # perfom
  perform_request <- function(lat,lon){
    url <- "https://api.open-meteo.com/v1/forecast"
    table <-request(url) |>
      req_url_query(latitude=lat,longitude=lon, hourly= c("temperature_2m",
                                                          "apparent_temperature",
                                                          "precipitation_probability",
                                                          "precipitation"),
                    .multi = "comma") |> req_perform() |> resp_body_json() |>
      as.tibble()
    table
  }
  #unnest
  unnest_response <- function(list_hourly) {
    output_table <- tibble(
      "date_heure"=list_hourly$hourly[1][[1]],
      "temperature_celsius"=list_hourly$hourly[2][[1]],
      "temperature_ressentie_celsius"=list_hourly$hourly[3][[1]],
      "precipitation_proba"=list_hourly$hourly[4][[1]],
      "precipitation"=list_hourly$hourly[5][[1]]
    )
    return(output_table)
  }
  # address to gps
  address_to_gps <- function(adresse){

    coord <- tibble(
      "addresse"= adresse
    )
    dta_lat_lon <- coord |>
      geocode(address = addresse,lat = latitude,long = longitude
      )
    coordonnees <- c(dta_lat_lon$latitude,dta_lat_lon$longitude)
    return(coordonnees)
  }
  # traetment
  # numerique
  get_forecast.numeric <- function(xy){
    if (!is.numeric(xy)) {
      stop("Le vecteur doit etre de type numerique.")
    }
    if (length(xy) != 2) {
      stop("Le vecteur doit etre de taille 2.")
    }
    reponse <- perform_request(lat = xy[1],lon = xy[2])
    resultat_tib <- unnest_response(reponse)
    return(resultat_tib)
  }
  # character
  get_forecast.character <- function(address){
    if (!is.character(address)) {
      stop("Le vecteur doit etre de type character.")
    }
    if (length(address) != 1) {
      stop("Le vecteur doit etre de taille 1.")
    }

    vect_cord <- address_to_gps(address)
    table_finale <- get_forecast.numeric(vect_cord)
    return(table_finale)
  }
# execution de la fonction
  if (is.numeric(value)){
    get_forecast.numeric(value)
  }
  if(is.character(value)){
    get_forecast.character(value)
  }
# fin
}








