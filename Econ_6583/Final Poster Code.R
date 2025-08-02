library(vars)
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(svars)
library(bsvars)
library(bvarsv)
library(bsvarSIGNs)

Oil <- read.csv("C:/Users/Kolby/OneDrive/Documents/School Stuff/ECO 6583/Poster/114281-V1/2011-0065_code/BP2012codes/data/oil.csv")
Plas <- read.csv("C:/Users/Kolby/OneDrive/Documents/School Stuff/ECO 6583/Poster/NewData/WPU0711.csv")

Data <- Oil[1:4]

custom_lag <- function(x, n = 1) {
  c(rep(NA, n), x[1:(length(x) - n)])
}

colnames(Data) <- c("Production", "Price", "GDP", "CPI")

LN_Data <- Data %>%
  mutate(LN_Production = log(Production),
         LN_Price = log(Price),
         LN_GDP = log(GDP), 
         LN_CPI = log(CPI))

LN_Data <- LN_Data[,c(5:8)]

FD_Data <- LN_Data %>%
  mutate(FD_Production = custom_lag(LN_Production)- LN_Production,
         FD_Price = custom_lag(LN_Price)-LN_Price,
         FD_GDP = custom_lag(LN_GDP)-LN_GDP,
         FD_CPI = custom_lag(LN_CPI)-LN_CPI)
FD_Data <- FD_Data[,c(5:8)]
FD_Data <- FD_Data[-1,]

######################################
#### Time Varying Parameter Model ####
######################################

#tau = 100

#SFD_Data <- scale(FD_Data)


# Example: Bayesian estimation of TVP-VAR
#model <- bvar.sv.tvp(
#  as.matrix(SFD_Data),
#  p = 4,              # Lag length
#  tau = 100,          # Training sample length
#  nf = 10,            # Forecast horizon
#  nrep = 50000,       # Number of MCMC draws
#  nburn = 5000,       # Burn-in iterations
#  thinfac = 10,       # Thinning factor
#  itprint = 10000,    # Iteration print interval
#  k_B = 4,            # Prior for initial state of coefficients
#  k_A = 4,            # Prior for initial state of covariance off-diagonal
#  k_sig = 1,          # Prior for initial state of covariance diagonal
#  k_Q = 0.001,         # Rate of time variation in coefficients
#  k_W = 0.001,         # Rate of time variation in covariance off-diagonal
#  k_S = 0.01           # Rate of time variation in covariance diagonal
#)

#summary(model)

#TestIRF <- impulse.responses(model, impulse.variable = 1, response.variable = 3, nhor = 24, scenario = 2, draw.plot = TRUE)
#TestIRF2 <- impulse.responses(model, impulse.variable = 1, response.variable = 4, nhor = 24, scenario = 2, draw.plot = TRUE)


##########################################
#### Sign Restrictions Model #############
##########################################

head(FD_Data)


adf_test1 <- ur.df(FD_Data$FD_Production, type = "drift", lags = 4)
summary(adf_test1)
adf_test2 <- ur.df(FD_Data$FD_Price, type = "drift", lags = 4)
summary(adf_test2)
adf_test3 <- ur.df(FD_Data$FD_GDP, type = "drift", lags = 4)
summary(adf_test3)
adf_test4 <- ur.df(FD_Data$FD_CPI, type = "drift", lags = 4)
summary(adf_test4)

Data_ts <- ts(FD_Data, frequency = 4)
Data_ts47 <- as.ts(Data_ts[1:107, ])
Data_ts74 <- as.ts(Data_ts[108:255, ])
Data_ts7484 <- as.ts(Data_ts[108:147, ])
Data_ts8494 <- as.ts(Data_ts[148:187, ])
Data_ts9404 <- as.ts(Data_ts[188:227, ])

var_model <- VAR(Data_ts, p = 4)
summary(var_model)

#########################
###### Parameters #######
#########################

# Define contemporaneous sign restrictions
sign_irf <- matrix(c(
  1, NA, NA,  NA,
  -1, 1, NA, NA,  # Oil Price: Reduces production, negatively impacts GDP, and CPI.
  -1, -1,  1,  NA,  # GDP: Increases production and CPI, reduces price.
  -1, -1,  -1,  1   # CPI: Increases production and GDP, reduces price.
), nrow = 4, byrow = TRUE)


rownames(sign_irf) <- c("Production", "Price", "GDP", "CPI")
colnames(sign_irf) <- c("Production", "Price", "GDP", "CPI")


sign_structural <- sign_irf
rownames(sign_structural) <- c("Production", "Price", "GDP", "CPI")
colnames(sign_structural) <- c("Production", "Price", "GDP", "CPI")

################
#### Models ####
################

#### 1947 - 2011 ####

bsvar_spec <- specify_bsvarSIGN$new(
  Data_ts74,
  p = 4L,
  sign_irf = sign_irf,
  sign_structural = sign_structural,
  max_tries = Inf,
  exogenous = NULL,
  stationary = rep(TRUE, ncol(Data_ts))
)

print(bsvar_spec)

posterior74 <- estimate(bsvar_spec, S = 5000, thin = 10, show_progress = TRUE)

irf74 = compute_impulse_responses(posterior74, horizon = 16)
plot(irf74)
sign_irf
summary(irf74)

irf74_shock2_var1 <- irf74[2, 1, , ]
 
median_response <- apply(irf74_shock2_var1, 1, median)

lower_bound <- apply(irf74_shock2_var1, 1, quantile, probs = 0.16)
upper_bound <- apply(irf74_shock2_var1, 1, quantile, probs = 0.84)

plot(0:(nrow(irf74_shock2_var1)-1), median_response, type = "l", lwd = 2, 
     ylab = "Response", xlab = "Horizon", main = "Production Shocks on Price 1974 - 2011")

polygon(c(0:(nrow(irf74_shock2_var1)-1), rev(0:(nrow(irf74_shock2_var1)-1))), 
        c(lower_bound, rev(upper_bound)), 
        col = "pink", border = NA)
lines(0:(nrow(irf74_shock2_var1)-1), median_response, lwd = 3, col = "red")
abline(h = 0, col = "black", lwd = 2, lty = 1)

Response1974_2011 <- median_response

#### 1974 - 1984 ####

bsvar_spec84 <- specify_bsvarSIGN$new(
  Data_ts7484,
  p = 4L,
  sign_irf = sign_irf,
  sign_structural = sign_structural,
  max_tries = Inf,
  exogenous = NULL,
  stationary = rep(TRUE, ncol(Data_ts7484))
)

print(bsvar_spec84)

posterior84 <- estimate(bsvar_spec84, S = 5000, thin = 10, show_progress = TRUE)

irf84 = compute_impulse_responses(posterior84, horizon = 16)
plot(irf84)
summary(irf84)

irf84_shock2_var1 <- irf84[2, 1, , ]

median_response84 <- apply(irf84_shock2_var1, 1, median)

lower_bound84 <- apply(irf84_shock2_var1, 1, quantile, probs = 0.16)
upper_bound84 <- apply(irf84_shock2_var1, 1, quantile, probs = 0.84)

# Plot the median response
plot(0:(nrow(irf84_shock2_var1)-1), median_response84, type = "l", lwd = 2, 
     ylab = "Response", xlab = "Horizon", main = "Production Shocks on Price 1974-1984")

# Add the shaded confidence interval
polygon(c(0:(nrow(irf84_shock2_var1)-1), rev(0:(nrow(irf84_shock2_var1)-1))), 
        c(lower_bound84, rev(upper_bound84)), 
        col = "violet", border = NA)
lines(0:(nrow(irf84_shock2_var1)-1), median_response84, lwd = 3, col = "blue")
abline(h = 0, col = "black", lwd = 2, lty = 1)
legend("right", legend = c("Median Response", "68% CI"), 
       col = c("blue", "violet"), lwd = 2)

Response1974_1984 <- median_response84

#### 1984 - 1994 ####

bsvar_spec94 <- specify_bsvarSIGN$new(
  Data_ts8494,
  p = 4L,
  sign_irf = sign_irf,
  sign_structural = sign_structural,
  max_tries = Inf,
  exogenous = NULL,
  stationary = rep(TRUE, ncol(Data_ts8494))
)

print(bsvar_spec94)

posterior94 <- estimate(bsvar_spec94, S = 5000, thin = 10, show_progress = TRUE)

irf94 = compute_impulse_responses(posterior94, horizon = 16)
plot(irf94)
sign_irf
summary(irf94)

irf94_shock2_var1 <- irf94[2, 1, , ]

median_response94 <- apply(irf94_shock2_var1, 1, median)

lower_bound94 <- apply(irf94_shock2_var1, 1, quantile, probs = 0.16)
upper_bound94 <- apply(irf94_shock2_var1, 1, quantile, probs = 0.84)

plot(0:(nrow(irf94_shock2_var1)-1), median_response94, type = "l", lwd = 2, 
     ylab = "Response", xlab = "Horizon", main = "Production Shocks on Price 1984-1994")

polygon(c(0:(nrow(irf94_shock2_var1)-1), rev(0:(nrow(irf94_shock2_var1)-1))), 
        c(lower_bound94, rev(upper_bound94)), 
        col = "yellow", border = NA)
lines(0:(nrow(irf94_shock2_var1)-1), median_response94, lwd = 3, col = "brown")
abline(h = 0, col = "black", lwd = 2, lty = 1)

Response1984_1994 <- median_response94
  
#### 1994 - 2004 ####

bsvar_spec04 <- specify_bsvarSIGN$new(
  Data_ts9404,
  p = 4L,
  sign_irf = sign_irf,
  sign_structural = sign_structural,
  max_tries = Inf,
  exogenous = NULL,
  stationary = rep(TRUE, ncol(Data_ts8494))
)

print(bsvar_spec04)

posterior04 <- estimate(bsvar_spec04, S = 5000, thin = 10, show_progress = TRUE)

irf04 = compute_impulse_responses(posterior04, horizon = 16)
plot(irf04)
sign_irf
str(irf04)

irf04_shock2_var1 <- irf04[2, 1, , ]

median_response04 <- apply(irf04_shock2_var1, 1, median)

lower_bound04 <- apply(irf04_shock2_var1, 1, quantile, probs = 0.16)
upper_bound04 <- apply(irf04_shock2_var1, 1, quantile, probs = 0.84)

plot(0:(nrow(irf04_shock2_var1)-1), median_response04, type = "l", lwd = 2, 
     ylab = "Response", xlab = "Horizon", main = "Price Shocks on Production 1994-2004")

polygon(c(0:(nrow(irf94_shock2_var1)-1), rev(0:(nrow(irf04_shock2_var1)-1))), 
        c(lower_bound04, rev(upper_bound04)), 
        col = "green", border = NA)
lines(0:(nrow(irf04_shock2_var1)-1), median_response04, lwd = 3, col = "gold")
abline(h = 0, col = "black", lwd = 2, lty = 1)
legend("right", legend = c("Median Response", "68% CI"), 
       col = c("gold", "green"), lwd = 2)

Response1994_2004 <- median_response04

Responses <- data.frame(
  `74_84` = Response1974_1984,
  `84_94` = Response1984_1994,
  `94_04` = Response1994_2004
)

Responses

plot(1:17, Responses$X74_84, type = "l", col = "blue", lwd = 2, ylim = range(Responses), 
     xlab = "Time Horizon", ylab = "Response", main = "Impulse Responses by Decade")
lines(1:17, Responses$X94_04, col = "green", lwd = 2)
legend("topright", legend = c("1974-1984","1994-2004"), 
       col = c("blue", "green"), lwd = 2)

###########################
#### Graphs for Poster ####
###########################

Date <- seq.Date(from = as.Date("1948-01-01"), to = as.Date("2011-12-31"), by = "quarter")
length(Date)

Dated <- Data
Dated$Date <- Date

library(ggplot2)


instability_periods <- data.frame(
  start = as.Date(c("1973-10-01", "1990-08-01", "2008-06-01")),  # Start of instability
  end = as.Date(c("1974-03-01", "1991-02-01", "2009-01-01")),    # End of instability
  label = c("1973 Oil Crisis", "Gulf War", "2008 Crisis")         # Optional labels
)

instability_periods$midpoint <- as.Date((as.numeric(instability_periods$start) + as.numeric(instability_periods$end)) / 2, origin = "1970-01-01")

ggplot(data = Data, aes(x = Date, y = Price)) +
  # Line for price
  geom_line(color = "black") +
  geom_rect(data = instability_periods, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "red", alpha = 0.2) +
  theme_bw() +
  labs(title = "Oil Prices Over Time with Instability Periods",
       x = "Date", y = "Price per Barrel (USD)")+
  geom_text(data = instability_periods, aes(x = midpoint, y = max(Data$Price), label = label),
            inherit.aes = FALSE, vjust = 2.0, hjust = 1.1, size = 4, color = "black")

###################
#### Extension ####
###################

###################
#### Extension ####
###################

new_data <- read_xls("C:/Users/Kolby/OneDrive/Documents/School Stuff/ECO 6583/Poster/NewData/Crude_oil_cost.xls")

new_data <- new_data %>%
  mutate(Quarter = paste(year(Date), quarter(Date), sep = "Q"))

new_data <- new_data %>%
  group_by(Quarter) %>%
  summarize(
    QRAC = (sum(Monthly_RAC, na.rm = TRUE)/3),
    QCPI = (sum(CPI, na.rm = TRUE)/3),
    .groups = 'drop'
  ) %>%
  mutate(Date = as.Date(as.yearqtr(Quarter, format = "%YQ%q"), frac = 1))

gdpc1 <- read.csv("C:/Users/Kolby/OneDrive/Documents/School Stuff/ECO 6583/Poster/NewData/GDPC1 (1).csv")
gdpc1$DATE <- as.Date(gdpc1$DATE, format = "%m/%d/%Y")

gdpc1 <- gdpc1 %>%
  mutate(CPI = new_data$QCPI)

avegdp2005 = 195.3
avegdp2017 = 245.12


gdpc1 <- gdpc1 %>%
  mutate(nom_gdp = (GDPC1*(avegdp2005/avegdp2017)),
         rac = new_data$QRAC)
ext_data <- gdpc1 %>%
  select(DATE, Production, rac, nom_gdp, CPI)

colnames(ext_data) <- c("date", "production", "price", "gdp", "cpi")

A <- ggplot(ext_data, aes(x = date, y = cpi))+
  geom_line()+
  theme_bw()
B <- ggplot(ext_data, aes(x = date, y = gdp))+
  geom_line()+
  theme_bw()
C <- ggplot(ext_data, aes(x = date, y = price))+
  geom_line()+
  theme_bw()
D <- ggplot(ext_data, aes(x = date, y = production))+
  geom_line()+
  theme_bw()
(A|B)/(C|D)

custom_lag <- function(x, n = 1) {
  c(rep(NA, n), x[1:(length(x) - n)])
}

log_data <- ext_data %>%
  mutate(LN_Production = log(production),
         LN_Price = log(price),
         LN_GDP = log(gdp), 
         LN_CPI = log(cpi))
log_data <- log_data[,c(1, 6:9)]

fd_data <- log_data %>%
  mutate(FD_Production = custom_lag(LN_Production)- LN_Production,
         FD_Price = custom_lag(LN_Price)-LN_Price,
         FD_GDP = custom_lag(LN_GDP)-LN_GDP,
         FD_CPI = custom_lag(LN_CPI)-LN_CPI)
fd_data <- fd_data[-c(1), c(1, 6:9)]

adf_test11 <- ur.df(fd_data$FD_Production, type = "none", lags = 4)
summary(adf_test11)
adf_test21 <- ur.df(fd_data$FD_Price, type = "none", lags = 4)
summary(adf_test21)
adf_test31 <- ur.df(fd_data$FD_GDP, type = "none", lags = 4)
summary(adf_test31)
adf_test41 <- ur.df(fd_data$FD_CPI, type = "none", lags = 4)
summary(adf_test41)

colnames(fd_data) <- c("Date", "Production", "Price", "GDP", "CPI")

A1 <- ggplot(fd_data, aes(x = Date, y = CPI))+
  geom_line()+
  theme_bw()
B1 <- ggplot(fd_data, aes(x = Date, y = GDP))+
  geom_line()+
  theme_bw()
C1 <- ggplot(fd_data, aes(x = Date, y = Price))+
  geom_line()+
  theme_bw()
D1 <- ggplot(fd_data, aes(x = Date, y = Production))+
  geom_line()+
  theme_bw()
(A1|B1)/(C1|D1)

Data_ts1 <- ts(fd_data[,-1], frequency = 4)

sign_irf <- matrix(c(
  1, NA, NA,  NA,
  -1, 1, NA, NA,  # Oil Price: Reduces production, negatively impacts GDP, and CPI.
  -1, -1,  1,  NA,  # GDP: Increases production and CPI, reduces price.
  -1, -1,  -1,  1   # CPI: Increases production and GDP, reduces price.
), nrow = 4, byrow = TRUE)

rownames(sign_irf) <- c("Production", "Price", "GDP", "CPI")
colnames(sign_irf) <- c("Production", "Price", "GDP", "CPI")

sign_structural <- sign_irf
rownames(sign_structural) <- c("Production", "Price", "GDP", "CPI")
colnames(sign_structural) <- c("Production", "Price", "GDP", "CPI")

bsvar_new <- specify_bsvarSIGN$new(
  Data_ts1,
  p = 4L,
  sign_irf = sign_irf,
  sign_structural = sign_structural,
  max_tries = Inf,
  exogenous = NULL,
  stationary = rep(TRUE, ncol(Data_ts1))
)

print(bsvar_new)

posterior_new <- estimate(bsvar_new, S = 5000, thin = 10, show_progress = TRUE)

irf_new = compute_impulse_responses(posterior_new, horizon = 16)
plot(irf_new)
summary(irf_new)

irf_s2v1 <- irf_new[2, 1, , ]

median_response_new <- apply(irf_s2v1, 1, median)

lower_bound_new <- apply(irf_s2v1, 1, quantile, probs = 0.16)
upper_bound_new <- apply(irf_s2v1, 1, quantile, probs = 0.84)

plot(0:(nrow(irf_s2v1)-1), median_response_new, type = "l", lwd = 2, 
     ylab = "Response", xlab = "Horizon", main = "Price Shocks on Production 2011 - 2024")

polygon(c(0:(nrow(irf_s2v1)-1), rev(0:(nrow(irf_s2v1)-1))), 
        c(lower_bound_new, rev(upper_bound_new)), 
        col = "pink", border = NA)
lines(0:(nrow(irf_s2v1)-1), median_response_new, lwd = 3, col = "red")
abline(h = 0, col = "black", lwd = 2, lty = 1)

New_Response <- median_response_new

Responses$New <- New_Response

Responses


plot(1:17, Responses$X74_84, type = "l", col = "blue", lwd = 2, ylim = range(Responses), 
     xlab = "Time Horizon", ylab = "Response", main = "Impulse Responses Over Time")
lines(1:17, Responses$New, col = "red", lwd = 2)
lines(1:17, Responses$X94_04, col = "green", lwd = 2)

abline(h = 0, col = "black", lwd = 2, lty = 3)
legend("right", legend = c("1974-1984","1994-2004", "2011-2024"), 
       col = c("blue", "green", "red"), lwd = 2)



