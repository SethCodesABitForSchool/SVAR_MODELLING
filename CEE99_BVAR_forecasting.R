# Replication of Monetary VAR by Christiano, Eichenbaum, and Evans(1999)
# initialize
# clear workspace
rm(list = ls())
graphics.off()
# load libraries and data
library(fredr)
#Get API key by registering on the FREED website and optain an API. then set the API key in R as follows
FRED_API_KEY <- fredr_set_key("YOUR_KEY_HERE")  #this sets the API. 
fredr_has_key()      #returns TRUE if a key can be found.
fredr_get_key()     #returns the API key set
#get CPI data
US_CPI <- fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1959-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency = "m", # for monthly
  units = "lin") # levels of data. no transformation. if you need change replace with "chg".

#View(US_CPI)
#drop real time start and real time end
US_CPI <- subset(US_CPI, select = -c(series_id, realtime_start, realtime_end))
View(US_CPI)
#rename "value" as CPI
colnames(US_CPI)[2] <- "CPI" 

# get US employment quantities
US_EMPL <- fredr(
  series_id = "USPRIV",
  observation_start = as.Date("1959-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency = "m", # for monthly
  units = "lin") # thousands of persons.

#View(US_EMPL)
#drop real time start and real time end
US_EMPL <- subset(US_EMPL, select = -c(series_id, realtime_start, realtime_end))
View(US_EMPL)
#rename "value"
colnames(US_EMPL)[2] <- "EMPL" 

# get Federal funds rate
FEDFUNDS <- fredr(
  series_id = "FEDFUNDS",
  observation_start = as.Date("1959-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency = "m", # for monthly
  units = "lin") # Percent, NSA 

View(FEDFUNDS)
#drop real time start and real time end
FEDFUNDS <- subset(FEDFUNDS, select = -c(series_id, realtime_start, realtime_end))
#rename "value"
colnames(FEDFUNDS)[2] <- "FEDFUNDS" 

# get crude Oil prices
P_COMM <- fredr(
  series_id = "WTISPLC",
  observation_start = as.Date("1959-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency = "m", # for monthly
  units = "lin") # Dollars per barrel, NSA 

#View(P_COMM)
#drop real time start and real time end
P_COMM <- subset(P_COMM, select = -c(series_id, realtime_start, realtime_end))
View(P_COMM)
#rename "value"
colnames(P_COMM)[2] <- "P_OIL" 

# get M2 money supply
M2 <- fredr(
  series_id = "M2SL",
  observation_start = as.Date("1959-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency = "m", # for monthly
  units = "lin") # Dollars, SA 

#View(M2)
#drop real time start and real time end
M2 <- subset(M2, select = -c(series_id, realtime_start, realtime_end))
View(M2)
#rename "value"
colnames(M2)[2] <- "M2" 

# get tOTAL RESERVES
RES <- fredr(
  series_id = "TOTRESNS",
  observation_start = as.Date("1959-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency = "m", # for monthly
  units = "lin") # Billions Dollars, SA 

#View(RES)
#drop real time start and real time end
RES <- subset(RES, select = -c(series_id, realtime_start, realtime_end))
View(RES)
#rename "value"
colnames(RES)[2] <- "MB" 

# Get Non Borrowed Reserves
NBR <- fredr(
  series_id = "NONBORRES",
  observation_start = as.Date("1959-01-01"),
  observation_end = as.Date("2023-09-01"),
  frequency = "m", # for monthly
  units = "lin") # Millions Dollars, SA 

#View(NBR)
#drop real time start and real time end
NBR <- subset(NBR, select = -c(series_id, realtime_start, realtime_end))
View(NBR)
#rename "value"
colnames(NBR)[2] <- "NBR" 

# Transform variables to Time series format

library(TSstudio)
library(tidyverse)
library(zoo)

cpi<-ts(US_CPI$CPI,start=c(1959,1,1),frequency=12) #create TS variable from dataset
ts_plot(cpi)
lcpi<-log(cpi)
ts_plot((lcpi))
# plot unemployment rate
empl<-ts(US_EMPL$EMPL,start=c(1959,1,1),frequency=12) #create TS variable from dataset
empl <-empl*1000
lempl <-log(empl)
ts_plot(lempl)

# plot real crude oil prices (dollars per barrel)
p_oil<-ts(P_COMM$P_OIL,start=c(1959,1,1),frequency=12) #create TS variable from dataset
rp_oil <-p_oil/cpi

ts_plot(p_oil)
ts_plot(rp_oil)

# fed funds
ffr<-ts(FEDFUNDS$FEDFUNDS,start=c(1959,1,1),frequency=12) #create TS variable from dataset
ts_plot(ffr)

# M2 money supply
m2<-ts(M2$M2,start=c(1959,1,1),frequency=12) #create TS variable from dataset
m2<-m2*10e9  # in dollars
lm2<-log(m2)
ts_plot(lm2)

# Total reserves
mb<-ts(RES$MB,start=c(1959,1,1),frequency=12) #create TS variable from dataset
mb<-mb*10e9
lmb<-log(mb)
ts_plot(lmb)

# Non-Borrowed reserves
nbr<-ts(NBR$NBR,start=c(1959,1,1),frequency=12) #create TS variable from dataset
nbr<-nbr*10e6
lnbr<-log(nbr + 3.5e+12) # offset negative reserves for log transformation
ts_plot(lnbr)
ts_plot(nbr)

# create 7-variable ts dataset / not taking logs on nbr (due to spell of neg RES)
dat.bv <- cbind(lempl, lcpi, ffr, rp_oil, lnbr, lmb, lm2)
# plot ordered dataset
plot(dat.bv)

# diagnostics
library(tsm)

#1. 7-variable VAR (Reduced-form) 
# VAR Model selection and estimation
library(vars)
library(mFilter)
# label 7-variable object and use information criteria
colnames(dat.bv) <- c("Employment", "LCPI", "FFR","P oil", "NBR", "MB","M2")

info.bv <- VARselect(dat.bv, lag.max = 24, type = "const")
info.bv$selection

# let's pick a AIC/FPE criterion selected model (p=10-lags) 
# Estimate VAR
bv.est <- VAR(dat.bv, p = 10, type = "const", season = NULL, exog = NULL)
#summary(bv.est)
# Note that characteristic roots are within the unit circle -> system is stable

# residual diagnostics
bv.serial <- serial.test(bv.est, lags.pt = 24, type = "PT.asymptotic")
bv.serial
# p-value is <.05 so evidence for serial correlation

plot(bv.serial, names = "employment")
plot(bv.serial, names = "lCPI")

# To keep it simple, we'll proceed regardless to forecast (i.e. more lags should be added)
# 8-step ahead forecast w/ 95 confidence interval

predictions <- predict(bv.est, n.ahead = 36, ci = 0.95)
plot(predictions, names = "Employment")
plot(predictions, names = "LCPI")

# Display a fanchart for the forecasts
fanchart(predictions, names = "Employment")
fanchart(predictions, names = "LCPI")

#2. 3-variable Bayesian VAR
# Install necessary packages
library(Rcpp)
library(ggplot2)
#devtools::install_github("kthohr/BMR")
# load package
library(BMR)
dat.small <- cbind(lempl, lcpi, ffr)
#plot data
gtsplot(dat.small, dates= FEDFUNDS$date)

# create model object to save estimation
bvar_obj <- new(bvarm)

#Estimate 4-variable VAR(4)
bvar_obj$build(data.matrix(dat.small),
               TRUE, # constant
               4) # lags
# Use Random Walk priors for non-stationary variables
prior <- c(1, 1, 1)
# Complete prior using hyperparameters from the literature
bvar_obj$prior(prior, # prior mean value
               1, # var_type
               1, # decay_type
               0.2, # HP1
               0.5, # HP2
               10^5, # HP3
               1.0) # HP4

# specify number of draws from Gibbs samples
bvar_obj$gibbs(10000)
# plot density functions of parameter estimates
plot(bvar_obj, var_names = colnames(dat.small), save = FALSE)

# plot IRFs
#IRF(bvar_obj, 48, var_names = colnames(dat.small), save = FALSE)
IRF.Rcpp_bvars(bvar_obj,periods=48,var_names = colnames(dat.small), save = FALSE)
# forecast
forecast(bvar_obj, shocks = TRUE, var_names=colnames(dat.small), 
         back_data =24 , save = FALSE)#, height=5, width=1)

# Extract specific values for the forecasts of the individual variables
predict <- forecast(bvar_obj, shocks = TRUE, var_names = colnames(dat.small), 
                    save = TRUE)
