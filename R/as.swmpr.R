#' Create a swmpr object from a dataframe
#' 
#' Function for creating a swmpr object from arbitrary time series data
#' 
#' @param  df_init \code{data.frame} of time series data
#' @param  stn_code chr string for station code (7 or 8 characters), can be multiple stations if data are combined
#' @param  timezone
#' 
#' @export as.swmpr
#' 
#' @return Returns a swmpr object to be used with S3 methods
#' 
#' @details 
#' This function performs some preliminary formatting and qa/qc tasks on arbitrary time-series data, then uses the swmpr() function to convert that data set into a swmpr object.
#' 
#' The function will convert all variable names to lower case, and convert the datatimestamp field (required) to a POSIXct format if needed.
#' 
as.swmpr <- function(df_init, stn_code){
  
  if(!is.data.frame(df_init)) 
    stop('df_init must be data.frame')
  
  names(df_init) <- tolower(names(df_init))
  
  qa_names <- names(df_init)[grepl('^f_', names(df_init))]
  nonqa_names <- names(df_init)[(!grepl('^[cf]_', names(df_init)))]
  cens_names <- names(df_init)[grepl('^c_', names(df_init))]
  
  # # qaqc attribute
  # qaqc_cols <- FALSE
  # if(any(grepl('^f_', names(df_init)))) qaqc_cols <- TRUE
  # 
  # # cens attribute
  # cens_cols <- FALSE
  # if(any(grepl('^c_', names(df_init)))) cens_cols <- TRUE
  # 
  # # parameters attribute
  # parameters <- grep('datetimestamp|^f_|^c_', names(df_init), invert = TRUE, value = TRUE)
  # 
  # # get stations, param_types attributes
  # param_types <- param_names()
  # param_types <- unlist(lapply(param_types, function(x) any(x %in% parameters)))
  # param_types <- names(param_names())[param_types]
  # station <- grep(paste0(param_types, collapse = '|'), stn_code, value = TRUE)
  # 
  # # remove trailing blanks in qaqc columns
  # if(qaqc_cols){
  #   
  #   fcols <- grep('^f_', names(df_init),value = TRUE)
  #   df_init[, fcols] <- sapply(fcols, function(x){
  #     
  #     out <- trimws(df_init[, x], which = 'right') #gsub('\\s+$', '', df_init[, x])
  #     return(out)
  #     
  #   })
  #   
  # }
  # 
  # timezone using time_vec function
  timezone <- time_vec(station_code = station, tz_only = TRUE)
  if( !(length(timezone) > 0) ) {
    print("No timezone identifiable, defaulting to 'America/Jamaica'")
    timezone <- "America/Jamaica"
  }
  if( !grepl('POSIXct',class(df_init$datetimestamp)[1]))
    df_init$datetimestamp <- as.POSIXct(df_init$datetimestamp, 
                                        tz = timezone, format = '%m/%d/%Y %H:%M')    
  
  # # create class, with multiple attributes
  # structure(
  #   .Data = df_init, 
  #   class = c('swmpr', 'data.frame'), 
  #   station = station,
  #   parameters = parameters, 
  #   qaqc_cols = qaqc_cols,
  #   cens_cols = cens_cols, 
  #   date_rng = range(df_init$datetimestamp),
  #   timezone = timezone, 
  #   stamp_class = class(df_init$datetimestamp),
  #   metabolism = NULL, 
  #   metab_units = NULL
  # )
  
  stat_out <- SWMPr::swmpr(df_init, stn_code)
  return(stat_out)
  
}
