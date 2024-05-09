#formal VAR/VARMA models for NE Aus and SE Aus.
#file to be submitted with final project


#library:
#for data/date manipulating
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))
suppressMessages(library(fields)) #test.for.zero()

#modeling checks
suppressMessages(library(MASS))

#ts data types
suppressMessages(library(xts))
suppressMessages(library(zoo))

#time series 
suppressMessages(library(vars)) #vector autoregression
suppressMessages(library(forecast)) #spectral methods
suppressMessages(library(marima)) #VARIMA

#Data Import
#setwd("~/CO_AUS/Aus_CO-main")

#anomaly only response with week data
response_anoms <- read.csv("Data/response_anoms.csv", header = TRUE, stringsAsFactors = FALSE)

#predictors with week data
predictor_anoms <- read.csv("Data/predictor_anoms.csv", header = TRUE, stringsAsFactors = FALSE)

#Data cleaning/sorting
#cleaning data for time series work
week <- 1

response <- response_anoms
predictor <- predictor_anoms

resp_dates <- as_date(response[response$week == week, ]$time)
pred_dates <- as_date(predictor[predictor$week == week, ]$time)

#get minimum prediction date from response data
low_bound <- pred_dates[which(pred_dates == min(resp_dates)) - 1]
#get maximum response date from predictor data
upper_bound <- max(as_date(predictor$time)) + days(7)

#this requires week being set correctly above, that req should be fixed
bounded_pred_df <- predictor[predictor$time >= low_bound & predictor$time <= upper_bound, ]

bounded_resp_df <- response[response$time <= upper_bound, ]#temporarily changed so that each week has the same number of years

#2019 boundary (week 14 2019: end of 2018/2019 season)
max_date <- as_date(response_anoms[response_anoms$week == 14 & 
                                     response_anoms$year == 2019, ]$time)

bounded_pred_df <- bounded_pred_df[bounded_pred_df$time <= max_date, ]
bounded_resp_df <- bounded_resp_df[bounded_resp_df$time <= max_date, ]

rm(response, predictor, low_bound, upper_bound, 
   week, resp_dates, pred_dates, max_date)


#correct for week 53
week_53 <- which(bounded_resp_df$week == 53, )
for (k in week_53) {
  bounded_resp_df[k-1, 3] <- mean(c(bounded_resp_df[k-1, 3], 
                                    bounded_resp_df[k, 3]))
  bounded_resp_df[k-1, 4] <- mean(c(bounded_resp_df[k-1, 4], 
                                    bounded_resp_df[k, 4]))
}
rm(k)
bounded_resp_df <- bounded_resp_df[-week_53, ]

#fix for the correct columns
for (j in week_53) {
  bounded_pred_df[j-1, 3] <- mean(c(bounded_pred_df[j-1, 3], 
                                    bounded_pred_df[j, 3]))
  bounded_pred_df[j-1, 4] <- mean(c(bounded_pred_df[j-1, 4],
                                    bounded_pred_df[j, 4]))
  bounded_pred_df[j-1, 5] <- mean(c(bounded_pred_df[j-1, 5],
                                    bounded_pred_df[j, 5]))
  bounded_pred_df[j-1, 6] <- mean(c(bounded_pred_df[j-1, 6], 
                                    bounded_pred_df[j, 6]))
}
rm(j)
bounded_pred_df <- bounded_pred_df[-week_53, ]

#alter the data to align better, then we can make adjustments to this later
min_pred <- min(bounded_resp_df$time)
max_resp <- max(bounded_pred_df$time)

bounded_resp_df <- bounded_resp_df[bounded_resp_df$time <= max_resp, ]
bounded_pred_df <- bounded_pred_df[bounded_pred_df$time >= min_pred, ]

rm(min_pred, max_resp)


## CCF check

NEaus_ts <- ts(bounded_resp_df$NE_Aus_anomaly_co, 
               start = as.Date(min(bounded_resp_df$time)), frequency = 52)
SEaus_ts <- ts(bounded_resp_df$SE_Aus_anomaly_co, 
               start = as.Date(min(bounded_resp_df$time)), frequency = 52)

detr_NE <- lm(NEaus_ts ~ time(NEaus_ts))
detr_SE <- lm(SEaus_ts ~ time(SEaus_ts))

resid_NE <- resid(detr_NE)
resid_SE <- resid(detr_SE)

nino_ts <- ts(bounded_pred_df$nino.anomaly, 
              start = as.Date(min(bounded_pred_df$time)), frequency = 52)
dmi_ts <- ts(bounded_pred_df$dmi.anomaly, 
             start = as.Date(min(bounded_pred_df$time)), frequency = 52)
tsa_ts <- ts(bounded_pred_df$tsa.anomaly, 
             start = as.Date(min(bounded_pred_df$time)), frequency = 52)
aao_ts <- ts(bounded_pred_df$aao.anomaly, 
             start = as.Date(min(bounded_pred_df$time)), frequency = 52)

ccf(as.numeric(resid_NE), as.numeric(nino_ts), type = "correlation", 
    lag = 52, main = "NE Aus & Nino")
ccf(as.numeric(resid_SE), as.numeric(nino_ts), type = "correlation", 
    lag = 52, main = "SE Aus & Nino")
ccf(as.numeric(resid_NE), as.numeric(dmi_ts), type = "correlation", 
    lag = 52, main = "NE Aus & DMI")
ccf(as.numeric(resid_SE), as.numeric(dmi_ts), type = "correlation", 
    lag = 52, main = "SE Aus & DMI")
ccf(as.numeric(resid_NE), as.numeric(tsa_ts), type = "correlation", 
    lag = 52, main = "NE Aus & TSA")
ccf(as.numeric(resid_SE), as.numeric(tsa_ts), type = "correlation", 
    lag = 52, main = "SE Aus & TSA")
ccf(as.numeric(resid_NE), as.numeric(aao_ts), type = "correlation", 
    lag = 52, main = "NE Aus & SAM")
ccf(as.numeric(resid_SE), as.numeric(aao_ts), type = "correlation", 
    lag = 52, main = "SE Aus & SAM")

#VAR models

#NE Aus
#clean data a little bit more
ne_aus_df <- bounded_resp_df[ ,c(2,3,5,6)]
nino_df <- bounded_pred_df[ ,c(2,3,7,8)]
dmi_df <- bounded_pred_df[ ,c(2,4,7,8)]
tsa_df <- bounded_pred_df[ ,c(2,5,7,8)]
aao_df <- bounded_pred_df[ ,c(2,6,7,8)]

#alter the data to align better, then we can make adjustments to this later
min_pred <- min(ne_aus_df$time)
max_resp <- max(nino_df$time)

ne_aus_df <- ne_aus_df[ne_aus_df$time <= max_resp, ]
nino_df <- nino_df[nino_df$time >= min_pred, ]
dmi_df <- dmi_df[dmi_df$time >= min_pred, ]
tsa_df <- tsa_df[tsa_df$time >= min_pred, ]
aao_df <- aao_df[aao_df$time >= min_pred, ]

min_date <- as.Date(min(nino_df$time))

#create a df and ts-object
dates <- ne_aus_df$time
NE_aus_tsdf <- data.frame(Date = dates, NE_AUS = ne_aus_df$NE_Aus_anomaly_co,
                          Nino = nino_df$nino.anomaly,
                          dmi = dmi_df$dmi.anomaly,
                          tsa = tsa_df$tsa.anomaly,
                          aao = aao_df$aao.anomaly)

NE_aus_zoo <- zoo(NE_aus_tsdf[, -1], order.by = NE_aus_tsdf$Date)


aic_test <- rep(NA, 10)
for (i in 1:10) {
  NE_temp <- vars::VAR(NE_aus_zoo, p = i, type = "both")
  NE_aus_temp <- NE_temp$varresult$NE_AUS
  aic_test[i] <- AIC(NE_aus_temp, k = i + 2)
}

plot(1:10, aic_test, type = "l", xlab = "p", ylab = "AIC", main = "NE Aus VAR(p)")

NE_model <- vars::VAR(NE_aus_zoo, p = 4, type = "both")

NE_aus_model <- NE_model$varresult$NE_AUS

AIC(NE_aus_model, k =2)

summary(NE_model$varresult$NE_AUS)

#change var names and titles below
residuals_var <- residuals(NE_model)

# Plot ACF and PACF for the first variable's residualsindices 
acf(residuals_var[,1], main="ACF of Residuals for NE Aus")
Pacf(residuals_var[,1], main="PACF of Residuals for NE Aus")

# SE Aus

se_aus_df <- bounded_resp_df[ ,c(2,4,5,6)]

#alter the data to align better, then we can make adjustments to this later
max_resp <- max(nino_df$time)

se_aus_df <- se_aus_df[se_aus_df$time <= max_resp, ]

min_date <- as.Date(min(nino_df$time))

#create a df and ts-object
dates <- se_aus_df$time
SE_aus_tsdf <- data.frame(Date = dates, SE_AUS = se_aus_df$SE_Aus_anomaly_co,
                          Nino = nino_df$nino.anomaly,
                          dmi = dmi_df$dmi.anomaly,
                          tsa = tsa_df$tsa.anomaly,
                          aao = aao_df$aao.anomaly)

SE_aus_zoo <- zoo(SE_aus_tsdf[, -1], order.by = SE_aus_tsdf$Date)


aic_test <- rep(NA, 10)
for (i in 1:10) {
  SE_temp <- vars::VAR(SE_aus_zoo, p = i, type = "both")
  SE_aus_temp <- SE_temp$varresult$SE_AUS
  aic_test[i] <- AIC(SE_aus_temp, k = i + 2)
}

plot(1:10, aic_test, type = "l", xlab = "p", ylab = "AIC", main = "SE Aus VAR(p)")

SE_model <- vars::VAR(SE_aus_zoo, p = 4, type = "both")

SE_aus_model <- SE_model$varresult$SE_AUS

AIC(SE_aus_model, k =2)

summary(SE_model$varresult$SE_AUS)

#change var names and titles below
residuals_var <- residuals(SE_model)

# Plot ACF and PACF for the first variable's residuals
acf(residuals_var[,1], main="ACF of Residuals for SE Aus")
pacf(residuals_var[,1], main="PACF of Residuals for SE Aus")

#VAR plots
#NE aus 

NEaus_ts <- ts(bounded_resp_df$NE_Aus_anomaly_co, start = as.Date(min(bounded_resp_df$time)), frequency = 52)
detr <- lm(NEaus_ts ~ time(NEaus_ts))
#t(fitted(NE_aus_model))
test_fit <- fitted(NE_aus_model)

pred_NE <- ts(fitted(NE_aus_model), start = start(NEaus_ts), freq = frequency(NEaus_ts))
#+ detr$coef[1] + detr$coef[2]*time(NEaus_ts)

plot.ts(pred_NE, lwd = 2, col = "blue", ylab = "Anomaly CO", main = "NE Aus Fitted")
#lines(NEaus_ts, lwd = 1, lty = 3)
points(NEaus_ts, pch = 16, cex  = 0.5)

resid_NE <- resid(NE_aus_model)

plot(resid_NE, pch = 16, ylab = "Residuals", main = "NE Aus Residuals", xlab = "Time")

hist(resid_NE, breaks = 12, col = 'steelblue', 
     main = "NE Aus Residuals",  xlab = "Residuals") 

#SE Aus

SEaus_ts <- ts(bounded_resp_df$SE_Aus_anomaly_co, start = as.Date(min(bounded_resp_df$time)), frequency = 52)
detr <- lm(SEaus_ts ~ time(SEaus_ts))
#t(fitted(NE_aus_model))
test_fit <- fitted(SE_aus_model)

pred_se <- ts(fitted(SE_aus_model), start = start(SEaus_ts), freq = frequency(SEaus_ts))
#+ detr$coef[1] + detr$coef[2]*time(SEaus_ts)

plot(pred_se, lwd = 2, col = "blue", ylab = "Anomaly CO", main = "SE Aus Fitted")
#lines(SEaus_ts)
points(SEaus_ts, pch = 16, cex  = 0.5)


resid_SE <- resid(SE_aus_model)
plot(resid_SE, pch = 16, ylab = "Residuals", main = "SE Aus Residuals", xlab = "Time")

hist(resid_SE, breaks = 12, col = 'steelblue', 
     main = "SE Aus Residuals",  xlab = "Residuals") 


##-----------Additional Work-----------------##

#subset_selection

#NE aus
NE_aus_model <- NE_model$varresult$NE_AUS

summary_neVAR <- summary(NE_aus_model)
coeffs<- summary_neVAR$coefficients 

#threshold:
s <- 1.5
sig_varsNE <- rownames(coeffs)[abs(coeffs[, "t value"]) > s]
sig_varsNE

#SE aus
SE_aus_model <- SE_model$varresult$SE_AUS

summary_seVAR <- summary(SE_aus_model)
coeffs<- summary_seVAR$coefficients 

#threshold:
s <- 1.5
sig_varsSE <- rownames(coeffs)[abs(coeffs[, "t value"]) > s]
sig_varsSE

#Potential Transformtations

#negative adjustments
y <- bounded_resp_df$NE_Aus_anomaly_co
c <- if(min(y) <= 0) abs(min(y)) + 1.01 else 0
y_pos_NEaus <- log(y + c)

y <- bounded_resp_df$SE_Aus_anomaly_co
c <- if(min(y) <= 0) abs(min(y)) + 1.01 else 0
y_pos_SEaus <- log(y + c)

#new predictor df w/ only pred values
new_pred_df <- bounded_pred_df[ ,3:6]

#fit for box cox
fit_test  <- lm(y_pos_NEaus ~ 1, data = new_pred_df)

boxcox_result <- boxcox(fit_test, lambda = seq(0, 5, by = 0.1))
lambda_optimal <- boxcox_result$x[which.max(boxcox_result$y)]

trans_yposNEaus <- (y_pos_NEaus^lambda_optimal - 1) / lambda_optimal

fit_test  <- lm(y_pos_SEaus ~ 1, data = new_pred_df)

boxcox_result <- boxcox(fit_test, lambda = seq(0, 5, by = 0.1))
lambda_optimal <- boxcox_result$x[which.max(boxcox_result$y)]

trans_yposSEaus <- (y_pos_NEaus^lambda_optimal - 1) / lambda_optimal

#plot variance wrt time
temp_bounded_resp_df <- bounded_resp_df
temp_bounded_resp_df$NE_Aus_anomaly_co <- trans_yposNEaus
temp_bounded_resp_df$SE_Aus_anomaly_co <- trans_yposSEaus

tempNEaus_ts <- ts(temp_bounded_resp_df$NE_Aus_anomaly_co, 
                   start = as.Date(min(temp_bounded_resp_df$time)), frequency = 52)
tempSEaus_ts <- ts(temp_bounded_resp_df$SE_Aus_anomaly_co, 
                   start = as.Date(min(temp_bounded_resp_df$time)), frequency = 52)

plot.ts(tempNEaus_ts)
plot.ts(tempSEaus_ts)

#variance by week
aus_var <- as.data.frame(matrix(ncol = 3) )
colnames(aus_var) <- c("NEaus_var", "SEaus_var", "week")
for (i in 1:52) {
  temp_week <- temp_bounded_resp_df[temp_bounded_resp_df$week == i, ]
  temp_row <- c(var(temp_week[,3]), var(temp_week[,4]), i)
  aus_var <- rbind(aus_var, temp_row)
}
aus_var <- aus_var[-1, ]

plot(aus_var$week, aus_var$NEaus_var, type = "l")
plot(aus_var$week, aus_var$SEaus_var, type = "l")

#histograms

hist(bounded_resp_df$NE_Aus_anomaly_co, breaks = 12, col = 'steelblue', 
     main = "NE AUS, no adj",  xlab = "NE Aus CO Anomalies") 
hist(temp_bounded_resp_df$NE_Aus_anomaly_co, breaks = 12, col = 'steelblue',
     main = "NE AUS, box-cox",  xlab = "NE Aus CO Anomalies")

hist(bounded_resp_df$SE_Aus_anomaly_co, breaks = 12, col = 'steelblue',
     main = "SE AUS, no adj.", xlab = "SE Aus CO Anomalies")
hist(temp_bounded_resp_df$SE_Aus_anomaly_co, breaks = 12, col = 'steelblue',
     main = "SE AUS, box-cox",  xlab = "SE Aus CO Anomalies")


#varma from Shumway & Stoffer Time Series book
model <- define.model(kvar=5, ar=c(1), ma=c(1,4))

arp <- model$ar.pattern 
map <- model$ma.pattern
neaus.d <- resid(detr <- lm(detr <- lm(NEaus_ts ~ time(NEaus_ts)),
                            na.action=NULL))
xdata <- matrix(cbind(neaus.d, nino_ts, dmi_ts, tsa_ts, aao_ts), ncol=5) 

fit <- marima(xdata, ar.pattern=arp, ma.pattern=map, penalty=2)
innov <- t(resid(fit))

plot.ts(innov[,1])
lines(resid_NE, lwd = 0.75, col = "red", lty = 2)

acf(innov, na.action=na.pass)
pred <- ts(t(fitted(fit))[,1], start=start(NEaus_ts), 
           freq=frequency(NEaus_ts)) + detr$coef[1] + detr$coef[2]*time(NEaus_ts)
plot(pred, lwd = 2, col = "blue")
lines(pred_NE, lwd = 2, col = "red")
#lines(NEaus_ts)
points(NEaus_ts, pch = 16, cex  = 0.5)

residuals_var <- resid(fit)
residuals_var <- residuals_var[,-1]
# Plot ACF and PACF for the first variable's residuals
Acf(residuals_var[,1], main="ACF of Residuals for NE Aus")
Pacf(residuals_var[,1], main="PACF of Residuals for NE Aus")
