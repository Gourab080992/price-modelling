run.baseline <- function(raw.data, max.roll.impute, dataclass, week, quant.top, quant.below, promo.flag.condns) {
  print(paste("Running baseline modeling with seasonality: ", week, ", quant top: ",
              quant.top, ", quant below: ", quant.below, sep = ""))
  
  if (max.roll.impute) {
    max.roll.impute.window <- 8
    impute.function <- max
    
    raw.data <- raw.data %>% 
      group_by_(.dots = dataclass) %>%
      mutate(
        UNITS_ROLL_1 = zoo::rollapply(UNITS, max.roll.impute.window, FUN = impute.function, partial = TRUE, align = "left"),
        UNITS_ROLL_2 = zoo::rollapply(UNITS, max.roll.impute.window, FUN = impute.function, partial = TRUE, align = "right"),
        UNITS_ROLL = pmax(UNITS_ROLL_1, UNITS_ROLL_2),
        
        SALES_ROLL_1 = zoo::rollapply(SALES, max.roll.impute.window, FUN = impute.function, partial = TRUE, align = "left"),
        SALES_ROLL_2 = zoo::rollapply(SALES, max.roll.impute.window, FUN = impute.function, partial = TRUE, align = "right"),
        SALES_ROLL = pmax(SALES_ROLL_1, SALES_ROLL_2),
        
        #VOLUME_ROLL_1 = zoo::rollapply(VOLUME, max.roll.impute.window, FUN = impute.function, partial = TRUE, align = "left"),
        #VOLUME_ROLL_2 = zoo::rollapply(VOLUME, max.roll.impute.window, FUN = impute.function, partial = TRUE, align = "right"),
        #VOLUME_ROLL = pmax(VOLUME_ROLL_1, VOLUME_ROLL_2),
        
        UNITS = if_else(RAW == 1, UNITS, UNITS_ROLL),
        SALES = if_else(RAW == 1, SALES, SALES_ROLL),
        #VOLUME = if_else(RAW == 1, VOLUME, VOLUME_ROLL)
      ) %>% 
      ungroup() %>% 
      select(-c(UNITS_ROLL, SALES_ROLL, UNITS_ROLL_1, UNITS_ROLL_2, 
                SALES_ROLL_1, SALES_ROLL_2)) # VOLUME_ROLL, VOLUME_ROLL_1, VOLUME_ROLL_2, 
  }
  
  baseline.data <- raw.data %>%
    group_by_(.dots = dataclass) %>%
    nest() %>%
    mutate(BASELINE_TEMP = map(data, ~ run.baseline.model(.,
                                                          seasonality = week,
                                                          quant.top = quant.top,
                                                          quant.below = quant.below,
                                                          promo.flag.condns = promo.flag.condns))) %>%
    unnest(BASELINE_TEMP) %>%
    ungroup()
  
  return(baseline.data)
}

run.baseline.model <- function(data,
                               seasonality,
                               window.size = 7,
                               min.unit.per.week = 10,
                               quant.top = 15,
                               quant.below = 5,
                               threshold = 1.2,
                               min.unit.size = 2,
                               min.avgp.sum = 10,
                               promo.flag.condns) {
   
  no.of.weeks <- nrow(data)
  if(seasonality == -1) {
    seasonality <- max(data$SEASONALITY)
  }
  
  # calculate the EDRP
  if (sum(data$AVGP, na.rm = TRUE) <= min.avgp.sum) {
    data$EDRP <- 0
  } else {
    ## calculate EDRP for AVGP and MAX_VAR
    compute.edrp <- function(AVGP, MAX_VAR) {
       
      EDRP <- c()
      for (i in 1:length(AVGP)) {
        if (i == 1) {
          if ((MAX_VAR[i] >= AVGP[i]) && (MAX_VAR[i] >= AVGP[i + 1])) {
            EDRP[i] <- MAX_VAR[i]
          } else {
            EDRP[i] <- MAX_VAR[i + 1]  ## (MAX_VAR[i] < AVGP[i]) | (MAX_VAR[i] < AVGP[i + 1]) -> not possible?
          }
        } else {
          if ((EDRP[i - 1] >= AVGP[i]) && (MAX_VAR[i] > AVGP[i])) {
            EDRP[i] <- EDRP[i - 1]
          } else {                                ## EDRP[i-1] < AVGP[i] | MAX_VAR[i] <= AVGP[i]
            if (MAX_VAR[i - 1] >= AVGP[i]) {
              EDRP[i] <- MAX_VAR[i - 1]
            } else {
              EDRP[i] <- MAX_VAR[i]
            }
          }
        }
      }
      return(EDRP)
    }
    
    data <- data %>%
      
      ## calculate the window size based maximum
      mutate(
        AVGP_REV = rev(AVGP),
        MAX_VAR = zoo::rollapply(AVGP, window.size, FUN = max, partial = TRUE, align = "center"),
        MAX_VAR_REV = zoo::rollapply(AVGP_REV, window.size, FUN = max, partial = TRUE, align = "center")
      ) %>%
      
      ## calculate the forward and backward edrp
      mutate(
        EDRP_FORWARD = compute.edrp(AVGP, MAX_VAR),
        EDRP_BACKWARD = rev(compute.edrp(AVGP_REV, MAX_VAR_REV)),
        EDRP = pmin(EDRP_FORWARD, EDRP_BACKWARD)
      ) %>%
      
      ## remove unnecessary columns
      select(-c(AVGP_REV, MAX_VAR, MAX_VAR_REV, EDRP_FORWARD, EDRP_BACKWARD))
  }
  
  data <- get.base.units(data, seasonality, window.size, min.unit.per.week, quant.top, quant.below,
                         threshold, min.unit.size, min.avgp.sum, no.of.weeks)
  
  # promo flag calculation
  units <- data$UNITS
  baseline <- data$BASE_UNITS
  edrp <- data$EDRP
  avgp <- data$AVGP
  threshold.local <- (threshold-1)/2+1;
  
  data <- data %>%
    mutate(
      AVGP_EDRP = if_else(EDRP == 0, 1.01, round(AVGP / EDRP, 2)), # 0.98 -> 1.01
      UNIT_BASE = if_else(BASE_UNITS == 0, 1.5, round(UNITS / BASE_UNITS, 2))
    ) %>%
    
    mutate(
      AVGP_EDRP = if_else(AVGP_EDRP == 0, 0.98, AVGP_EDRP),
      UNIT_BASE = if_else(UNIT_BASE > 10, 1.5, UNIT_BASE),
      
      DUMMY1 = if_else(((AVGP_EDRP < promo.flag.condns$condn1$tpr_frac_lt) &
                          (UNIT_BASE >= promo.flag.condns$condn1$lift_frac_gte)), 1, 0),
      
      DUMMY2 = if_else(((AVGP_EDRP >= promo.flag.condns$condn2$tpr_frac_gte) &
                          (AVGP_EDRP < promo.flag.condns$condn2$tpr_frac_lt) &
                          (UNIT_BASE > promo.flag.condns$condn2$lift_frac_gt)), 1, 0), # 0.98 -> 1.01
      
      DUMMY3 = if_else(((AVGP_EDRP >= promo.flag.condns$condn3$tpr_frac_gte) &
                          (AVGP_EDRP < promo.flag.condns$condn3$tpr_frac_lt) &
                          (UNIT_BASE > promo.flag.condns$condn3$lift_frac_gt)), 1, 0), # 0.98 -> 1.01
      
      DUMMY4 = if_else(((AVGP_EDRP < promo.flag.condns$condn4$tpr_frac_lt) &
                          (UNIT_BASE > promo.flag.condns$condn4$lift_frac_gt)), 1, 0)
    ) %>%
    
    mutate(
      DUMMY = pmax(DUMMY1, DUMMY2, DUMMY3, DUMMY4)
    )
  
  dummy <- data$DUMMY
  for(i in 1:no.of.weeks){
    if(i < no.of.weeks){
      if(dummy[i] == 1){
        ratio.test <- units[i+1]/baseline[i+1]
        if(is.nan(ratio.test)) ratio.test <- 0;
        if(ratio.test >= threshold.local){
          dummy[i+1] <- 1;
        }
      }
    }
  }
  
  data$PROMO_FLAG <- dummy
  data <- data %>%
    mutate(
      PROMO_FLAG = if_else(UNITS < min.unit.size, 0, PROMO_FLAG),
      PROMO_FLAG = if_else(BASE_UNITS < min.unit.size, 0, PROMO_FLAG),
      PROMO_FLAG = if_else(EDRP == 0, 0, PROMO_FLAG),
      
      BASE_UNITS = if_else(UNITS < min.unit.size, 0, BASE_UNITS),
      EDRP = if_else(UNITS < min.unit.size, 0, EDRP),
      PROMO_FLAG = if_else(UNITS < min.unit.size, 0, PROMO_FLAG)
    )
  
  
  # make all imputations 0
  data <- data %>%
    mutate(
      UNITS = if_else(RAW == 0, 0, UNITS),
      BASE_UNITS = if_else(RAW == 0, 0, BASE_UNITS),
      AVGP = if_else(RAW == 0, 0, AVGP),
      EDRP = if_else(RAW == 0, 0, EDRP),
      # VOLUME = if_else(RAW == 0, 0, VOLUME),
      PROMO_FLAG = if_else(RAW == 0, 0, PROMO_FLAG)
    ) %>%
    select(-c(AVGP_EDRP, UNIT_BASE, DUMMY1, DUMMY2, DUMMY3, DUMMY4, DUMMY))
  
  return(data)
}


get.base.units <- function(data, seasonality, window.size, min.unit.per.week, quant.top, quant.below,
                           threshold, min.unit.size, min.avgp.sum, no.of.weeks) {
  #browser() 
  units <- data$UNITS
  
  # calculate the baseline based on seasonality
  seq.last <- function (from, to, by) {
     
    vec <- do.call(what = seq, args = list(from, to, by))
    if ( tail(vec, 1) != to ) return(c(vec, to)) else return(vec)
  }
  
  first.interval <- seq.last(13, no.of.weeks, seasonality)
  second.interval <- seq.last(4, no.of.weeks, seasonality)
  third.interval <- seq.last(9, no.of.weeks, seasonality)
  
  intervals <- list(FIRST = first.interval, SECOND = second.interval, THIRD = third.interval)
  
  # initialize the baseline calculation
  baseline.temp <- data.frame(matrix(NA, nrow = no.of.weeks, ncol = 3))
  colnames(baseline.temp) <- c("FIRST", "SECOND", "THIRD")
  
  # count the number of leading and trailing zeroes
  leading.imputed <- 0
  trailing.imputed <- 0
  raw <- data$RAW
  raw.rle <- rle(raw)
  if(head(raw.rle$values, 1) == 0) {
    leading.imputed <- leading.imputed + head(raw.rle$lengths, 1)
  }
  
  if(tail(raw.rle$values, 1) == 0) {
    trailing.imputed <- trailing.imputed + tail(raw.rle$lengths, 1)
  }
  
  leading <- 4 + leading.imputed
  trailing <- 4 + trailing.imputed
  
  if (sum(units) > min.unit.per.week &&
      ((sum(head(units, leading)) > 0 && sum(tail(units, trailing)) == 0) ||
       (sum(head(units, leading)) == 0 && sum(tail(units, trailing)) > 0) ||
       (sum(head(units, leading)) > 0 && sum(tail(units, trailing)) > 0))) {
    for (name in names(intervals)) {
      interval <- intervals[[name]]
      base.units <- c()
      
      for (j in 1:length(interval)) {
        if (j == 1) {
          units.in.interval <- units[1:interval[j]]
        } else {
          units.in.interval <- units[(interval[j - 1] + 1):interval[j]]
        }
        
        pos.zero <- which(units.in.interval <= 0)
        if (length(pos.zero) >= floor(0.9 * length(units.in.interval))) {
          mean.units.in.interval <- 0
          base.units <- append(base.units, rep(mean.units.in.interval, length(units.in.interval)))
        } else if (!is.null(pos.zero) &&
                   length(pos.zero) >= 4 &&
                   length(pos.zero) < floor(0.9 * length(units.in.interval))) {
          mean.units.in.interval <- mean(units.in.interval, na.rm = TRUE)
          if (is.nan(mean.units.in.interval)) {
            mean.units.in.interval <- 0
          }
          
          if (j == 1) {
            base.units <- append(base.units, rep(mean.units.in.interval, length(units.in.interval)))
          } else if (j == 2) {
            datause <- base.units[1:interval[j - 1]]
            mean.datause <- mean(datause)
            if (!is.nan(mean.datause)) {
              if (mean.units.in.interval < 0.75 * mean.datause) {
                mean.units.in.interval <- mean.datause
                base.units.temp <- units.in.interval
                
                above.prev.base <- which(base.units.temp[which(base.units.temp > 0)] >= mean.units.in.interval)
                below.prev.base <- which(base.units.temp[which(base.units.temp > 0)] < mean.units.in.interval)
                
                if (length(above.prev.base) != 0) {
                  base.units.temp[above.prev.base] <- rep(mean.units.in.interval, length(above.prev.base))
                }
                
                base.units.temp[which(base.units.temp <= 0)] <- 0
              } else {
                base.units.temp <- rep(mean.units.in.interval, length(units.in.interval))
              }
            } else {
              base.units.temp <- rep(mean.units.in.interval, length(units.in.interval))
            }
            base.units <- append(base.units, base.units.temp)
          } else {
            datause <- base.units[(interval[j - 2] + 1):interval[j - 1]]
            mean.units.in.interval <- mean(datause)
            if (is.nan(mean.units.in.interval)) {
              mean.units.in.interval <- 0
            }
            
            base.units.temp <- units.in.interval
            above.prev.base <- which(base.units.temp[which(base.units.temp > 0)] >= mean.units.in.interval)
            below.prev.base <- which(base.units.temp[which(base.units.temp > 0)] < mean.units.in.interval)
            
            if (!is.null(above.prev.base)) {
              base.units.temp[above.prev.base] <- rep(mean.units.in.interval, length(above.prev.base))
            }
            
            base.units.temp[which(base.units.temp <= 0)] <- 0
            base.units <- append(base.units, base.units.temp)
          }
        } else {
          cv <- sd(units.in.interval) / mean(units.in.interval)
          pos.zero <- max(which(units <= 0))
          
          if (!is.infinite(pos.zero)) {
            if ((cv > 0.5) && (quant.below > 0) && (pos.zero < no.of.weeks)) {
              quant.top.used <- quant.top + 10
              quant.below.used <- quant.top - 5
            } else if ((cv > 0.5) && (quant.below > 5) && (pos.zero == no.of.weeks)) {
              quant.top.used <- quant.top + 10
              quant.below.used <- quant.top - 5
            } else if ((cv > 0.5) && (quant.below < 5) && (pos.zero == no.of.weeks)) {
              quant.top.used <- quant.top + 10
              quant.below.used <- quant.below
            } else {
              quant.top.used <- quant.top
              quant.below.used <- quant.below
            }
          } else {
            quant.top.used <- quant.top
            quant.below.used <- quant.below
          }
          
          quant.units <- quantile(units.in.interval, probs = quant.top.used * 0.01, type = 5)
          
          if (quant.below.used > 0) {
            quant.units.2 <- quantile(units.in.interval, probs = quant.below.used * 0.01, type = 5)
            units.in.interval.2 <- subset(units.in.interval, units.in.interval >= quant.units.2)
            units.in.interval.2 <- subset(units.in.interval.2, units.in.interval.2 <= quant.units)
            
            if ((mean(units.in.interval, na.rm = TRUE) < 20) && length(units.in.interval.2) == 0) {
              units.in.interval.2 <- min(units.in.interval[which(units.in.interval >= quant.units)])
            }
          } else {
            units.in.interval.2 <- subset(units.in.interval, units.in.interval <= quant.units)
          }
          
          mean.units.in.interval <- mean(units.in.interval.2, na.rm = TRUE)
          # mean.quant.units <- mean(units.in.interval[which(units.in.interval > quant.units)])
          # TODO(KUNTAL): I have added GT to GTE
          mean.quant.units <- mean(units.in.interval[which(units.in.interval >= quant.units)])
          
          if (j == 1) {
            base.units <- append(base.units, rep(mean.units.in.interval, length(units.in.interval)))
          } else if (j == 2) {
            interm <- na.omit(base.units[1:interval[j - 1]])
            mean.datause <- mean(interm)
            
            if (!is.nan(mean.datause)) {
              if ((mean.units.in.interval < 0.75 * mean.datause) && (mean.datause < mean.quant.units)) {
                mean.units.in.interval <- mean.datause
                base.units.temp <- units.in.interval
                
                above.prev.base <- base.units.temp[which(base.units.temp > 0)]
                above.prev.base <- which(above.prev.base >= mean.units.in.interval)
                
                below.prev.base <- base.units.temp[which(base.units.temp > 0)]
                below.prev.base <- which(below.prev.base < mean.units.in.interval)
                
                
                if (!is.null(above.prev.base)) {
                  base.units.temp[above.prev.base] <- rep(mean.units.in.interval, length(above.prev.base))
                }
                
                base.units.temp[which(base.units.temp <= 0)] <- 0
              } else {
                base.units.temp <- rep(mean.units.in.interval, length(units.in.interval))
              }
            } else {
              base.units.temp <- rep(mean.units.in.interval, length(units.in.interval))
            }
            
            base.units <- append(base.units, base.units.temp)
          } else {
            interm <- na.omit(base.units[(interval[j - 2] + 1):interval[j - 1]])
            interm[interm == "NULL"] <- 0
            mean.datause <- mean(interm)
            
            if (!is.na(mean.units.in.interval)) {
              if ((mean.units.in.interval < 0.75 * mean.datause) && (mean.datause < mean.quant.units)) {
                mean.units.in.interval <- mean.datause
                base.units.temp <- units.in.interval
                
                above.prev.base <- base.units.temp[which(base.units.temp > 0)]
                above.prev.base <- which(above.prev.base >= mean.units.in.interval)
                
                below.prev.base <- base.units.temp[which(base.units.temp > 0)]
                below.prev.base <- which(below.prev.base < mean.units.in.interval)
                
                if (!is.null(above.prev.base)) {
                  base.units.temp[above.prev.base] <- rep(mean.units.in.interval, length(above.prev.base))
                }
                
                base.units.temp[which(base.units.temp <= 0)] <- 0
              } else {
                base.units.temp <- rep(mean.units.in.interval, length(units.in.interval))
              }
            } else {
              base.units.temp <- rep(mean.units.in.interval, length(units.in.interval))
            }
            
            base.units <- append(base.units, base.units.temp)
          }
        }
      }
      
      base.units[is.nan(base.units)] <- NA
      baseline.temp[[name]] <- base.units
    }
  
    baseline.temp <- baseline.temp %>%
      mutate(BASELINE_TEMP = rowMeans(.[, c("FIRST", "SECOND", "THIRD")], na.rm = TRUE))
    
    baseline.model.temp.data <- baseline.temp$BASELINE_TEMP
    ma <- function(ma.data, n = 5) {
      stats::filter(ma.data, rep(1 / n, n), sides = 2)
    }
    
    baseline.model.data <- as.numeric(ma(baseline.model.temp.data, 5))
    baseline.model.data[1] <- baseline.model.temp.data[1]
    baseline.model.data[2] <- (baseline.model.temp.data[1] + baseline.model.temp.data[2] + baseline.model.temp.data[3]) / 3
    
    baseline.model.data[no.of.weeks] <- baseline.model.temp.data[no.of.weeks]
    baseline.model.data[no.of.weeks - 1] <- (baseline.model.temp.data[no.of.weeks - 2] +
                                               baseline.model.temp.data[no.of.weeks - 1] +
                                               baseline.model.temp.data[no.of.weeks]) / 3
    
    baseline.model.data[is.na(baseline.model.data)] <- 0
    data$BASE_UNITS <- round(baseline.model.data, 0) # basline precision is zero
  } else {
    data$BASE_UNITS <- 0
  }
  
  return(data)
}
