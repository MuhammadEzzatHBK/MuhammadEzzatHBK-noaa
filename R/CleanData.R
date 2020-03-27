
#'@title eq_location_clean
#'@description Cleans the location name by removing the country's name with the colon using regex's.
#'Can be used with the function mutate() to obtain the clean column.
#'@param location the location name column
#'@return clean_loc : The clean location name.
#'@importFrom stringr str_remove
#'@examples \dontrun{
#'data <- readr::read_delim('signif.txt',delim ='\t')
#'eq_location_clean(data[['LOCATION_NAME]])
#'}
#'@export
eq_location_clean <- function(location){
  clean_loc <- stringr::str_remove(location,'^[^:]*:')
  return(clean_loc)
}

#'@title eq_clean_data
#'@description Cleans the whole NOAA data by accomplishing all the requirments regrading lattitude, longitude,
#'date & location name
#'@param data A data frame, should be the NOAA data
#'@param na.rm A logical that determines whether missing values should be omitted or not
#'@return The clean data frame
#'@importFrom dplyr select mutate %>%
#'@importFrom stats na.omit
#'@importFrom tidyr unite
#'@importFrom lubridate ymd
#'@examples \dontrun{
#'data <- readr::read_delim('signif.txt',delim ='\t')
#'F <- eq_clean_data(data)
#'T <- eq_clean_data(data,T)
#'}
#'@export
eq_clean_data <- function(data,na.rm = F){
  clean_data <- data %>% dplyr::select(I_D,YEAR,MONTH,DAY,EQ_PRIMARY,LATITUDE,LONGITUDE,
                                       LOCATION_NAME,COUNTRY,STATE,TOTAL_DEATHS)

  if(na.rm)
    clean_data <- stats::na.omit(clean_data,LOCATION_NAME)


  clean_data <- clean_data %>% dplyr::mutate(LOCATION_NAME = eq_location_clean(LOCATION_NAME))
  clean_data <- clean_data %>%  dplyr::mutate(
    LONGITUDE = as.numeric(LONGITUDE),
    LATITUDE = as.numeric(LATITUDE),
    MONTH = ifelse(is.na(MONTH), 1, MONTH),
    DAY = ifelse(is.na(DAY), 1, DAY),
    TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
    EQ_PRIMARY = as.numeric(EQ_PRIMARY),
    TIME = ifelse(YEAR > 0,'AD','BC'))
  clean_data <- clean_data %>% tidyr::unite(col ='DATE',YEAR,MONTH,DAY,sep='/',remove=F)
  clean_data <- clean_data %>% dplyr::mutate(DATE = lubridate::ymd(DATE))

  return(clean_data)
}
