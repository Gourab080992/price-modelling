get.lift.model <- function(data, other.flag) {
  ## at least 12 data points otherwise no run (n-k-1 degrees of freedom)
  
  # filter the relevant data
  lift.model.data <- data %>% filter(PROMO_FLAG == 1)
  
  lift.model.data <- lift.model.data %>%
    select(LIFT_FRAC, TPR_DISC, TPR_DISC_SQ, MINOR_PROMO, MAJOR_PROMO, AD_DISPLAY)
  
  if (nrow(lift.model.data) < 12) {
    fit.data <- data.frame(c(NA), c(NA), c(NA), c(NA), c(NA), c(NA), c(NA), c(NA))
    colnames(fit.data) <- c("COEFF_A", "COEFF_B", "COEFF_C", "COEFF_MINOR_PROMO", "COEFF_MAJOR_PROMO",
                            "COEFF_AD_DISPLAY", "RSQUARED", "ADJRSQUARED")
    return(fit.data)
  }
  
  model <- lm(formula = LIFT_FRAC ~ TPR_DISC_SQ + TPR_DISC + MINOR_PROMO + MAJOR_PROMO + AD_DISPLAY - 1,
              data = lift.model.data)
  model.param <- coef(model)
  
  rsquared <- summary(model)$r.squared
  adjrsquared <- summary(model)$adj.r.squared
  
  COEFF_A <- c(model.param["TPR_DISC_SQ"])
  COEFF_B <- c(model.param["TPR_DISC"])
  COEFF_C <- 0
  COEFF_MINOR_PROMO <- c(model.param["MINOR_PROMO"])
  COEFF_MAJOR_PROMO <- c(model.param["MAJOR_PROMO"])
  COEFF_AD_DISPLAY <- c(model.param["AD_DISPLAY"])
  RSQUARED <- rsquared
  ADJRSQUARED <- adjrsquared
  
  model.param.df <- data.frame(COEFF_A, COEFF_B, COEFF_C, COEFF_MINOR_PROMO, COEFF_MAJOR_PROMO, COEFF_AD_DISPLAY,
                                 RSQUARED, ADJRSQUARED, row.names = FALSE)
  return(model.param.df)
}


run.lift <- function(raw.data, lift.dataclass, other.flag, lift.flag.condns) {
  print("Running lift modeling")
  # function to calculate cumsum but reset at value
  sum.reset.at <- function(value) {
    function(x) {
      purrr::accumulate(x, ~if_else(.y == value, .y, .x+.y))
    }  
  }
  
  # create the necessary promo tactic flags
  raw.data <- raw.data %>%
    mutate(
      TPR_FRAC = if_else(EDRP == 0, 0, round(AVGP/EDRP, 2)),
      LIFT_FRAC = if_else(BASE_UNITS == 0, 0, round(UNITS/BASE_UNITS, 2)),
      LIFT = LIFT_FRAC,
      TPR_FLAG = if_else((TPR_FRAC <= lift.flag.condns$tpr_flag$tpr_frac_lte) &
                           (LIFT_FRAC >= lift.flag.condns$tpr_flag$lift_frac_gte) &
                           (UNITS>=100), 1, 0),
      CUM_TPR_FLAG = sum.reset.at(0)(TPR_FLAG),
      
      TPR_UNITS = TPR_FLAG * UNITS,
      PROMO_UNITS = PROMO_FLAG * UNITS,
      
      PCT_DISCOUNT = if_else(EDRP == 0, 0, round(((AVGP - EDRP) / EDRP), 2)),
      PCT_DISCOUNT = abs(PCT_DISCOUNT),  ### required?
      PRICE_ELAS = if_else(((TPR_FLAG == 1) & (BASE_UNITS != 0) & (PCT_DISCOUNT != 0)),
                           round((((UNITS / BASE_UNITS) - 1) / PCT_DISCOUNT), 3), 0),
      
      
      PROMOELAS = if_else(PCT_DISCOUNT == 0, 0, (LIFT - 1)/PCT_DISCOUNT),
      
      PRIMARY_TACTIC = if_else(PROMO_FLAG == 1 & 
                                     TPR_FLAG == 0 & 
                                     LIFT < 1.20, "MINOR_PROMO", NA_character_),
      
      PRIMARY_TACTIC = if_else(PROMO_FLAG == 1 & 
                                     TPR_FLAG == 0 & 
                                     1.20 <= LIFT & LIFT < 1.50, "MAJOR_PROMO", PRIMARY_TACTIC),
      
      PRIMARY_TACTIC = if_else(PROMO_FLAG == 1 & 
                                     TPR_FLAG == 0 & 
                                     1.50 <= LIFT, "MEGA_PROMO", PRIMARY_TACTIC),
      
      PRIMARY_TACTIC = if_else(PROMO_FLAG == 1 & 
                                     TPR_FLAG == 1 & 
                                     PROMOELAS < 3.0, "MINOR_PROMO", PRIMARY_TACTIC),
      
      PRIMARY_TACTIC = if_else(PROMO_FLAG == 1 & 
                                     TPR_FLAG == 1 & 
                                     3.0 <= PROMOELAS & PROMOELAS < 6.0, "MAJOR_PROMO", PRIMARY_TACTIC),
      
      PRIMARY_TACTIC = if_else(PROMO_FLAG == 1 & 
                                     TPR_FLAG == 1 & 
                                     6.0 <= PROMOELAS, "MEGA_PROMO", PRIMARY_TACTIC),
      
      MINOR_PROMO = if_else(PRIMARY_TACTIC == "MINOR_PROMO", 1, 0),
      MAJOR_PROMO = if_else(PRIMARY_TACTIC == "MAJOR_PROMO", 1, 0),
      AD_DISPLAY = if_else(PRIMARY_TACTIC == "MEGA_PROMO", 1, 0),
      
      TPR_DISC = TPR_FLAG * PCT_DISCOUNT,
      TPR_DISC_SQ = TPR_DISC * TPR_DISC,
      
      KEY = paste(BRANDSIZE, sep = "-")
    )
  
  lift.model.data <- raw.data %>%
    filter(RAW == 1) %>%
    group_by_(.dots = append(lift.dataclass, "DATE")) %>%
    summarise(
      TPR_FLAG = first(TPR_FLAG),
      PRIMARY_TACTIC = first(PRIMARY_TACTIC),
      PROMO_FLAG = first(PROMO_FLAG),
      
      LIFT_FRAC = mean(LIFT_FRAC, na.rm = TRUE),
      TPR_DISC = mean(TPR_DISC, na.rm = TRUE),
      TPR_DISC_SQ = mean(TPR_DISC_SQ, na.rm = TRUE),
      
      MINOR_PROMO = first(MINOR_PROMO),
      MAJOR_PROMO = first(MAJOR_PROMO),
      AD_DISPLAY = first(AD_DISPLAY),
      
      DATACLASS = first(DATACLASS),
      KEY = first(KEY)
    ) %>%
    ungroup()
  
  lift.data <- lift.model.data %>%
    group_by_(.dots = lift.dataclass) %>%
    do(get.lift.model(., other.flag = other.flag))
  
  raw.data <- raw.data %>% left_join(lift.data)
  return(raw.data)
}
