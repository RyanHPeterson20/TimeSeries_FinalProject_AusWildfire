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
#suppressMessages(library(MTS)) #try mts if needed (remove if not used)


