#'@title eq_create_label
#'@description creates an HTML label of location name, eq magnitude & total deaths.
#'@param .df is the data frame inputed
#'@return label the new HTML label
#'@importFrom stringr str_c
#'@examples \dontrun{
#'data <- readr::read_delim('signif.txt',delim ='\t')
#'data <- eq_clean_data(data,T)
#'eq_create_label(data)
#'}
#'@export
eq_create_label <- function(.df) {

  location  <- stringr::str_c(
    "<strong>Location:</strong>", .df$LOCATION_NAME
  )
  magnitude <- stringr::str_c(
    "<br><strong>Magnitude:</strong>", .df$EQ_PRIMARY
  )

  deaths <- stringr::str_c(
    "<br><strong>Total deaths:</strong>", .df$TOTAL_DEATHS
  )

  label <- stringr::str_c(location, magnitude, deaths)
  return(label)
}





#'@title eq_map
#'@description Draws an interactive map that plots the NOAA eq data.
#'@param data The input data frame
#'@param annot_col Column to be used as marker label
#'@return map the plotted leaflet map
#'@importFrom leaflet leaflet addCircleMarkers addTiles
#'@examples \dontrun{
#'data <- readr::read_delim('signif.txt',delim ='\t')
#'data <- eq_clean_data(data,T)
#'eq_map(data)
#'}
#'@export
eq_map <-function(data,annot_col){
map <-  leaflet::leaflet(data) %>%
            leaflet::addTiles() %>%
                leaflet::addCircleMarkers(
                    lng = ~LONGITUDE,
                    lat = ~LATITUDE,
                    weight = 1,
                    radius = ~EQ_PRIMARY*2,
                    popup = data[[annot_col]],
                    color = 'red'
                    )

return(map)
}
