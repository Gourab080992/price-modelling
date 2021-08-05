detect.seasonality <- function(raw.data, dataclass, max.zero.pct) {
  print("Running seasonality detection")
  #browser()
  dataclass.data <- raw.data %>%
    group_by_(.dots = append(dataclass, "DATE")) %>%
    summarise(WEEK_UNITS = sum(UNITS, na.rm = TRUE)) %>%
    ungroup()
  
  seasonality.data <- dataclass.data %>%
    # calculate the seasonality strength for each of the dataclass
    group_by_(.dots = dataclass) %>%
    nest() %>%
    mutate(SEASONALITY_TEMP = map(data, ~ calculate.seasonlity.strength(., max.zero.pct = max.zero.pct))) %>%
    unnest(SEASONALITY_TEMP) %>%
    ungroup() %>%
    
    # calculate seasonality strength and class for each dataclass
    group_by_(.dots = dataclass) %>%
    summarise(SEASONALITY_STRENGTH = max(SEASONALITY_STRENGTH),
              SEASONALITY = max(SEASONALITY)) %>%
    ungroup()
  
  raw.data <- raw.data %>% left_join(seasonality.data)
  return(raw.data)
}

calculate.seasonlity.strength <- function(data, max.zero.pct) {
  #browser()
  start.date <- min(data$DATE)
  start.date.y <- lubridate::year(start.date)
  start.date.m <- lubridate::month(start.date)
  
  end.date <- max(data$DATE)
  end.date.y <- lubridate::year(end.date)
  end.date.m <- lubridate::month(end.date)
  
  units <- data$WEEK_UNITS
  
  # Fixing for having atleast 105 weeks data for seasonality calculation
  unit_zero <- c(0)
  if (length(units) <= 104){
    units <- c(units, unit_zero)
  }
  
  if (sum(units, na.rm = TRUE) <= 300) {
    data$SEASONALITY_STRENGTH <- 0.0
    data$SEASONALITY <- 13
    #browser()
    return(data)
  }
  
  zero.count <- sum(units == 0)
  if (zero.count > length(units) * max.zero.pct | sum(units[1:104])==0) {
    data$SEASONALITY_STRENGTH <- 0.0
    data$SEASONALITY <- 13
    return(data)
  }
  
  #Change by Divyanshu. End date should not be mentioned as month format. 
  #It represnt week of year. So either year,week or leave it blank
  #tsdata <- ts(units, frequency = 52,
  #             start = c(start.date.y, start.date.m),
  #            end = c(end.date.y, end.date.m))
  tsdata <- ts(units, frequency = 52,
               start = c(start.date.y, start.date.m))
  #tsdata <- ts(units, frequency = 52,
  #            start = c(start.date.y),end = c(end.date.y))               
  
  
  # seasonality measures
  measures <- tsfeatures::tsfeatures(tsdata)
  seasonal_strength <- measures$seasonal_strength
  data$SEASONALITY_STRENGTH <- seasonal_strength
  
  # based on strength define the baseline parameter
  if (seasonal_strength >= 0.8) {
    data$SEASONALITY <- 6
  } else if (seasonal_strength >= 0.5 && seasonal_strength < 0.8) { # >= 0.6
    data$SEASONALITY <- 8
  } else {
    data$SEASONALITY <- 13
  }
  #browser()
  return(data)
}



