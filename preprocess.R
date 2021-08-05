preprocess.data <- function(file, in.out.week.count, minor.sales.pct, min.no.of.weeks, zero.pad) {
  
  print("Preprocessing data")
  
  # rsi.raw.data <- data.table::fread("/opt/amesrm_container/from_adls/sa_fds/sa_fds_candlelight.csv",stringsAsFactors = FALSE) %>%
  rsi.raw.data <- data.table::fread(file, stringsAsFactors = FALSE) %>%
    # rsi.raw.data <- rsi.raw.data %>%
    # Perform cleaning
    # dplyr::rename_(.dots = setNames(names(.), tolower(gsub("^.*\\.", "", names(.))))) %>%
    # mutate_all(funs(type.convert(as.character(replace(., .=='NULL', NA))))) %>%
    dplyr::filter(!is.na(brnd_desc) & !is.na(sub_brnd_desc ) & !is.na(mstr_itm) & !is.na(geo2)) %>%
    
    # Filter out sales and units consistencies arriving from the raw data
    dplyr::filter(
      !is.na(trans_dt) & 
        # !is.na(sls_unt_out) & !is.na(sls_val_out) & 
        cntry != '' & ctgy_desc != '' & sgmnt_desc != ''&
        brnd_desc != '' & sub_brnd_desc != '' & pck_sz_desc != '' & mfrr != '')%>%
    # &
    #   sls_unt_out != '' & sls_val_out != '')  %>%
    #& !is.na(vol_own_prc_elas_bsz) & !is.na(unit_own_prc_elas_bsz)
    # select only appropriate columns
    dplyr::select(cntry, geolevel, chnl, geo2, ctgy_desc, sgmnt_desc, brnd_desc, mstr_itm,
                  sub_brnd_desc , pck_sz_desc, mfrr, brnd_sz, sub_brnd_sz, sls_val_out,
                  sls_vol_gms_out, sls_unt_out, sls_val_in, sls_unt_in, sls_vol_gms_in,
                  trans_yr, trans_mnth, trans_wk, trans_dt,wk_of_yr, qtr_no, base_vol,
                  base_cnt, sales_finance, volume_finance, cost_cogs,da_var_discount,
                  da_var_spend,da_promo_discount, da_promo_spend, da_stales, #wtd_dist_out,wtd_dist_in,
                  brnd_sz_ownprcelas_out_ppv,brnd_sz_ownprcelas_out_ppu,
                  brnd_sz_ownprcelas_in_ppv,brnd_sz_ownprcelas_in_ppu) %>%
    
    # Rename some of the columns
    dplyr::rename(
      COUNTRY = cntry,
      GEOLEVEL = geolevel,
      CHANNEL = chnl,
      GEO2 = geo2,
      CATEGORY = ctgy_desc,
      SEGMENT = sgmnt_desc,
      BRAND = brnd_desc,
      SUBBRAND = sub_brnd_desc,
      BRANDSIZE = brnd_sz,
      SUBBRANDSIZE = sub_brnd_sz,
      PACK_SIZE = pck_sz_desc,
      MASTERITEM = mstr_itm,
      VENDOR = mfrr,
      SALES = sls_val_out,
      VOLUME = sls_vol_gms_out,
      UNITS = sls_unt_out,
      DATE = trans_dt,
      WEEKOFYEAR = wk_of_yr,
      BASE_VOL = base_vol,
      BASE_COUNT = base_cnt
      #TOTAL_DA = da,
      #VAR_COST_OF_GOODS = var_cost
    ) %>%
    
    rename_all(toupper) %>%
    
    mutate_if(is.factor, as.character) %>%
    mutate(DATE = lubridate::ymd(DATE)) %>%
    
    # Select the columns which are relevant for the analysis
    select(c("QTR_NO","TRANS_YR", "TRANS_MNTH", "TRANS_WK","WEEKOFYEAR",
             "COUNTRY", "GEOLEVEL", "CHANNEL", "GEO2", "VENDOR", "CATEGORY", "SEGMENT", "BRAND",
             "SUBBRAND", "PACK_SIZE", "BRANDSIZE", "SUBBRANDSIZE", "MASTERITEM", "DATE", "SALES",
             "UNITS","VOLUME", "SLS_VAL_IN", "SLS_UNT_IN", "SLS_VOL_GMS_IN",
             "BASE_VOL", "BASE_COUNT", "SALES_FINANCE", "VOLUME_FINANCE", "COST_COGS",
             "DA_VAR_DISCOUNT","DA_VAR_SPEND","DA_PROMO_DISCOUNT", "DA_PROMO_SPEND","DA_STALES", 
             #"WTD_DIST_OUT", "WTD_DIST_IN",
             "BRND_SZ_OWNPRCELAS_OUT_PPV","BRND_SZ_OWNPRCELAS_OUT_PPU",
             "BRND_SZ_OWNPRCELAS_IN_PPV","BRND_SZ_OWNPRCELAS_IN_PPU"
             
    ))
  
  dataclass <- c(
    "COUNTRY", "GEOLEVEL", "CHANNEL", "GEO2", "VENDOR", "CATEGORY", "SEGMENT", "BRAND", "SUBBRAND",
    "BRANDSIZE"
    , "SUBBRANDSIZE", "PACK_SIZE"#, "MASTERITEM"
  )
  
  dataclass.yearweek <- append(dataclass, c("DATE","QTR_NO","TRANS_YR", "TRANS_MNTH", "TRANS_WK","WEEKOFYEAR"))
  
  #data_1234 <- rsi.raw.data %>% 
  # group_by(COUNTRY, GEOLEVEL, CHANNEL, GEO2, VENDOR, CATEGORY, SEGMENT, BRAND, SUBBRAND,
  #         BRANDSIZE#,SUBBRANDSIZE,PACK_SIZE#,MASTERITEM
  #        ) %>% tally()
  
  # Roll up and aggregate data
  rsi.data <- rsi.raw.data %>%
    group_by_(.dots = dataclass.yearweek) %>%
    summarise(
      SALES = sum(SALES, na.rm = TRUE),
      UNITS = sum(UNITS, na.rm = TRUE),
      VOLUME = sum(VOLUME, na.rm = TRUE),
      LISTP  = round(SALES/UNITS,2),
      SLS_VAL_IN = sum(SLS_VAL_IN, na.rm = TRUE), 
      SLS_UNT_IN = sum(SLS_UNT_IN, na.rm = TRUE), 
      SLS_VOL_GMS_IN = sum(SLS_VOL_GMS_IN, na.rm = TRUE),
      BASE_VOL = ifelse(UNITS==0,0,VOLUME/UNITS),
      BASE_COUNT = sum(BASE_COUNT, na.rm = TRUE),
      # TOTAL_DA = sum(TOTAL_DA, na.rm = TRUE),
      # VAR_COST_OF_GOODS = sum(VAR_COST_OF_GOODS, na.rm = TRUE),
      # VOL_OWN_PRC_ELAS_BSZ= mean(VOL_OWN_PRC_ELAS_BSZ, na.rm = TRUE),
      # UNIT_OWN_PRC_ELAS_BSZ= mean(UNIT_OWN_PRC_ELAS_BSZ, na.rm = TRUE)
      SALES_FINANCE = sum(SALES_FINANCE, na.rm = TRUE), 
      VOLUME_FINANCE = sum(VOLUME_FINANCE, na.rm = TRUE), 
      COST_COGS = sum(COST_COGS, na.rm = TRUE),
      DA_VAR_DISCOUNT = sum(DA_VAR_DISCOUNT, na.rm = TRUE),
      DA_VAR_SPEND = sum(DA_VAR_SPEND, na.rm = TRUE),
      DA_PROMO_DISCOUNT = sum(DA_PROMO_DISCOUNT, na.rm = TRUE), 
      DA_PROMO_SPEND = sum(DA_PROMO_SPEND, na.rm = TRUE), 
      DA_STALES = sum(DA_STALES, na.rm = TRUE), 
      # WTD_DIST_OUT = sum(WTD_DIST_OUT, na.rm = TRUE),
      # WTD_DIST_IN = sum(WTD_DIST_IN, na.rm = TRUE),
      BRND_SZ_OWNPRCELAS_OUT_PPV = mean(BRND_SZ_OWNPRCELAS_OUT_PPV, na.rm = TRUE),
      BRND_SZ_OWNPRCELAS_OUT_PPU = mean(BRND_SZ_OWNPRCELAS_OUT_PPU, na.rm = TRUE),
      BRND_SZ_OWNPRCELAS_IN_PPV = mean(BRND_SZ_OWNPRCELAS_IN_PPV, na.rm = TRUE),
      BRND_SZ_OWNPRCELAS_IN_PPU = mean(BRND_SZ_OWNPRCELAS_IN_PPU, na.rm = TRUE)
      
    ) %>%
    ungroup() %>%
    arrange_(.dots = dataclass.yearweek)
  
  # detect minor sales
  minor.sales <- rsi.data %>%
    group_by_(.dots = dataclass) %>%
    summarise(TOTAL_DATACLASS_SALES = sum(SALES, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(PCT = TOTAL_DATACLASS_SALES / sum(TOTAL_DATACLASS_SALES, na.rm = TRUE)) %>%
    arrange(dplyr::desc(PCT)) %>%
    mutate(
      CUM_PCT = cumsum(PCT) * 100,
      MINOR_ITEM_SALES = ifelse(CUM_PCT <= 100 - minor.sales.pct, 0, 1)
    )
  
  rsi.data <- rsi.data %>% left_join(minor.sales)
  
  # detect inout units
  minor.in.out <- rsi.data %>% 
    group_by_(.dots = dataclass.yearweek) %>%
    mutate(TOTAL_UNITS = sum(UNITS)) %>%
    ungroup() %>%
    group_by_(.dots = dataclass) %>%
    mutate(
      row = row_number(),
      top_bottom_week = ifelse(row <= in.out.week.count | row > max(row - in.out.week.count), 1, 0)
    ) %>%
    ungroup() %>% filter(top_bottom_week == 1) %>% 
    #group_by(.dots = append(dataclass, "top_bottom_week")) %>%
    group_by(.dots = dataclass) %>% 
    mutate(
      inout_units = sum(TOTAL_UNITS),
      MINOR_ITEM_INOUT = ifelse(inout_units < 100, 1, 0)
    ) %>% 
    #ungroup() %>% 
    select(-c(inout_units, row, top_bottom_week, TOTAL_UNITS, CUM_PCT, PCT, TOTAL_DATACLASS_SALES))
    

  minor.in.out.dis <- minor.in.out %>% 
    #group_by(.dots = append(dataclass, "MINOR_ITEM_INOUT"))
    dplyr::distinct(COUNTRY, GEOLEVEL, CHANNEL, GEO2, VENDOR, CATEGORY, SEGMENT, BRAND, SUBBRAND, BRANDSIZE, SUBBRANDSIZE, PACK_SIZE, MINOR_ITEM_INOUT)
  
  rsi.data <- rsi.data %>% left_join(minor.in.out.dis) %>% 
    select(-c(PCT,CUM_PCT,TOTAL_DATACLASS_SALES))

  
  # Add a flag to determine if raw value or imputed value
  rsi.data$RAW <- 1  ## This value will be 0 for imputed values
  
  # Find the minimum date and maximum date within the group
  min.date <- min(rsi.data$DATE)
  max.date <- max(rsi.data$DATE)
  
  if(zero.pad == TRUE) {
    max.date.temp <- lubridate::ymd(min.date) + lubridate::weeks(min.no.of.weeks - 1)
    if (max.date < max.date.temp) {
      max.date <- max.date.temp
    }
  }
  
  # Normalize the number of weeks
  pad.dates <- function(data, min.date, max.date) {
    data <- data %>%
      pad(interval = "week", start_val = min.date, end_val = max.date) %>%
      fill_by_value(value = 0)
    
    return(data)
  }
  
  rsi.padded <- rsi.data %>%
    group_by_(.dots = append(dataclass, c("MINOR_ITEM_SALES", "MINOR_ITEM_INOUT"))) %>%
    nest() %>%
    # Map the function to the nested data at the group by level
    mutate(paddata = map(data, ~pad.dates(., min.date, max.date))) %>%
    # Get the padded data , drop the duplicate columns from the nested data
    unnest(paddata) %>%
    ungroup() %>%
    select(-c(data))
  
  ## make all data of same number of weeks
  impute.missing <- function(data) {
    n.weeks <- length(unique(data$DATE))
    
    if (n.weeks < min.no.of.weeks) {
      diff <- min.no.of.weeks - n.weeks
      
      last.data <- tail(data, 1)
      last.week <- last.data$DATE
      
      last.week.year <- lubridate::year(last.week)
      last.week.month <- lubridate::month(last.week)
      last.week.day <- lubridate::day(last.week)
      
      filtered.data <- data %>%
        filter(DATE > lubridate::ymd(paste(last.week.year - 1,
                                           last.week.month,
                                           last.week.day, sep = "-"))) %>%
        head(n = diff)
      
      for (i in 1:diff) {
        last.week <- last.week + lubridate::days(7)
        filtered.data[i, ]$DATE <- last.week
      }
      
      # all these imputed values are not raw
      filtered.data$RAW <- 0
      data <- rbind(data, filtered.data)
    }
    
    return(data)
  }
  
  rsi.padded <- rsi.padded %>%
    group_by_(.dots = append(dataclass, c("MINOR_ITEM_SALES", "MINOR_ITEM_INOUT"))) %>%
    nest() %>%
    mutate(BASELINE = map(data, ~ impute.missing(.))) %>%
    unnest(BASELINE) %>%
    ungroup() %>%
    select(-c(data))
  
  # For any internal data with (IN) flag as 1
  rsi.padded <- rsi.padded %>% mutate(
    INFLAG = ifelse(str_detect(GEO2,"(IN)"),1,0)
  )
  
  NONPOS <- rsi.padded %>% filter(INFLAG == 1) %>%
    mutate(DA_TOTAL = DA_VAR_DISCOUNT + DA_VAR_SPEND + DA_PROMO_DISCOUNT + DA_PROMO_SPEND,
           NETSALES = SALES - DA_TOTAL) %>% select (-c(SALES,DA_TOTAL)) %>%
    dplyr::rename(SALES = NETSALES) %>%
    select ( c(COUNTRY	,
               GEOLEVEL	,
               CHANNEL	,
               GEO2	,
               VENDOR	,
               CATEGORY	,
               SEGMENT	,
               BRAND	,
               SUBBRAND	,
               BRANDSIZE	,
               SUBBRANDSIZE	,
               PACK_SIZE	,
               #MASTERITEM	,
               MINOR_ITEM_SALES	,
               MINOR_ITEM_INOUT	,
               DATE	,
               QTR_NO	,
               TRANS_YR	,
               TRANS_MNTH	,
               TRANS_WK	,
               WEEKOFYEAR,
               SALES	,
               UNITS	,
               VOLUME	,
               LISTP,
               SLS_VAL_IN	,
               SLS_UNT_IN	,
               SLS_VOL_GMS_IN	,
               BASE_VOL	,
               BASE_COUNT	,
               SALES_FINANCE	,
               VOLUME_FINANCE	,
               COST_COGS	,
               DA_VAR_DISCOUNT	,
               DA_VAR_SPEND	,
               DA_PROMO_DISCOUNT	,
               DA_PROMO_SPEND	,
               DA_STALES	,
               BRND_SZ_OWNPRCELAS_OUT_PPV	,
               BRND_SZ_OWNPRCELAS_OUT_PPU	,
               BRND_SZ_OWNPRCELAS_IN_PPV	,
               BRND_SZ_OWNPRCELAS_IN_PPU	,
               RAW	,
               INFLAG	
    ))
  
  # only considering POS data
  rsi.padded1 <- rsi.padded %>% filter(INFLAG==0)
  
  # Combining both POS and NONPOS data
  rsi.padded <-rbind(rsi.padded1,NONPOS) %>% select (-INFLAG)
  
  
  rsi.baseline.data <- rsi.padded %>%
    mutate(
      AVGP = if_else(UNITS <= 0, 0, round((SALES / UNITS), 2)),
      VOLUME_ZERO = if_else(VOLUME == 0 & UNITS > 0, 1, 0)
    )
  
  # add dataclass
  rsi.baseline.data$DATACLASS <- rsi.baseline.data %>% group_indices_(.dots = dataclass)
  
  # create a flag for short term products
  short.term <- rsi.baseline.data %>%
    group_by_(.dots = dataclass) %>%
    summarise(
      TOTAL_RAW = sum(RAW==1),
      RAW_FRAC = TOTAL_RAW / min.no.of.weeks
    ) %>%
    ungroup() %>%
    select(-c(TOTAL_RAW))
  
  rsi.baseline.data <- rsi.baseline.data %>% left_join(short.term)
  
  rsi.baseline.data <- rsi.baseline.data %>% #left_join(data_1234) %>%
    mutate(MASTERITEM = BRANDSIZE) #%>% select (-n)
  return(rsi.baseline.data)
}
