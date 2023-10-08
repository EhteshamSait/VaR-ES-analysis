### Exploratory Analysis

filepath <- "/Users/ehteshamsait/Downloads/ROBO2.csv"
df <- read.csv(filepath)

#summary of data
head(df) #prints first six rows
tail(df) #prints last six rows
summary(df)

#visualisation
plot(df) #provides correlation between all variables
hist(df$today, main = "Distribution of Returns")


### Define VaR and CVaR Functions

#Historical VaR
var_Hist = function(returns, confidence, days){
  var <- -quantile(returns, 1-confidence, na.rm = TRUE) * sqrt(days)
}

#Parametric VaR
var_Par = function(returns, confidence, days){
  var <- -1 * (mean(returns) + sd(returns) * qnorm(1-confidence)) * sqrt(days)
}

#Conditional VaR
cVar = function(returns, confidence, days){
  var <- mean(returns) + sd(returns) * exp((-qnorm(1-confidence)^2)/2) / (sqrt(2*pi)*(1-confidence)) * sqrt(days)
}


### Define randomly generated returns to test functions
n <-  100; mean <- 0; sd <-  20; #define variables for artificial returns
returns <- rnorm(n, mean, sd) #generate returns
confidence <- 0.99; days <- 1;  #declare confidence and days

#Test VaR/CVaR functions
print(var_Hist(returns, confidence, days))
print(var_Par(returns, confidence, days))
print(cVar(returns, confidence, days))

### Calculating 30-day Rolling Estimates with 2-year window of returns 

#Declare Variables
confidence <- 0.95
years <- 2
dperM <- 5*4 #days per month
dperY <- dperM*12 #days per year
d <- dperY * years #window of returns used
rolling_VaR <- c()
rolling_ES <- c()
sequence <- seq(d, nrow(df), dperM)

for(i in sequence){
  n <- i-d
  returns <- df$today[i:n]
  volatility <- sqrt(df$hvt30d[i])
  rolling_VaR[(i - d + dperM) / dperM] <- var_Hist(returns, confidence, 30)
  rolling_ES[(i - d + dperM) / dperM] <- cVar(returns, confidence, 30)
}

par(mfrow = c(1,2))
plot(rolling_VaR, type = "l")
plot(rolling_ES, type = "l")