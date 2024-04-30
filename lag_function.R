#function for lag of a given week (t).

#TODO: add all notes here

#needs better testing (for bounding of resp and pred data)

#requires: 
# - dplyr (tidyverse)

lag_function <- function(week, response, predictor, ...){
  resp_dates <- as_date(response[response$week == week, ]$time)
  pred_dates <- as_date(predictor[predictor$week == week, ]$time)
  
  #get minimum prediction date from response data
  low_bound <- pred_dates[which(pred_dates == min(resp_dates)) - 1]
  #get maximum response date frrom predictor data
  upper_bound <- max(pred_dates)
  
  rm(resp_dates, pred_dates)
  #trimmed dataframes (bounded)
  bounded_pred_df <- predictor[predictor$time >= low_bound & predictor$time < upper_bound, ]
  bounded_resp_df <- response[response$time <= upper_bound & response$week == week, ]
  
  #trim predictor to be the year before each response
  ##get years from response data
  year_select <- unique(bounded_resp_df$year)
  
  ##select for the prior 52 weeks for each week in the response data
  pred_col <- length(bounded_pred_df[1,])
  pred_df <- data.frame(matrix(ncol = pred_col))
  names(pred_df) <- names(bounded_pred_df)
  for( j in year_select) {
    temp_resp_time <- bounded_resp_df[bounded_resp_df$year == j, ]
    time_temp <- as_date(temp_resp_time$time)
    new_bound <- as_date(time_temp - dyears(1))
    int_select <- interval(new_bound, time_temp)
    new_pred <- bounded_pred_df[as_date(bounded_pred_df$time) %within% int_select, ]
    pred_df <- rbind(pred_df, new_pred)
  }
  pred_df <- pred_df[-1, ]
  pred_df <- distinct(pred_df)
  
  #empty predictor dataframe
  resp_row <- length(bounded_resp_df[,1])
  temp_df <- data.frame(matrix(NA, nrow = resp_row, ncol = (52*4)))
  
  #populate dataframe
  loc <- 1
  for (i in 1:52) {
    lag_week <- week - i
    if (lag_week <= 0) {
      lag_week <- 52 + lag_week
    }
    for (j in 1:4){
      inter_df <- pred_df[pred_df$week == lag_week, 2+j]
      temp_df[,loc] <- inter_df
      
      var_name <- paste0(names(pred_df)[2 + j], "_lag_", i)
      names(temp_df)[loc] <- var_name
      
      loc <- loc + 1
    }
  }
  
  #create full dataframe with response and predictor data
  resp_week_df <- bounded_resp_df[,-1]
  return_df <- cbind(resp_week_df, temp_df)
  
  return(return_df)
}