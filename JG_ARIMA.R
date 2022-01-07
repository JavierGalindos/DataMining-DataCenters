# ARIMA model for forecast and outliers
# Javier Galindos

rm(list = ls()) # Remove all the objects we created so far.
set.seed(123)

#Libraries
library("rstudioapi")  
library(dplyr) # Manipulate data
library(ggplot2) # Plots
library(forecast) # Time series
library(tseries) # Time series

# Set working directory
curDir <- dirname(getActiveDocumentContext()$path)# Current directory
dataDir <- file.path(curDir,"fastStorage") # Concatenate paths
setwd(dataDir) 
getwd()  
#create a list of the files from your target directory
file_list <- list.files(path=dataDir)



# Load dataset (for loop to load everything)
VM <- read.csv(file = file_list[31], header = TRUE, sep = ";")

# Data preparation

# Selecting relevant variables
VM <- VM  %>% select(Timestamp..ms.,CPU.usage....,Memory.usage..KB.,Memory.capacity.provisioned..KB.)
# Memory usage
VM <- VM  %>% mutate(memoryUtilization= 100*Memory.usage..KB./Memory.capacity.provisioned..KB.)
# Rename column
VM <- VM  %>% rename(CPU.utilization= CPU.usage....)
VM <- VM  %>% rename(Timestamp= Timestamp..ms.)
VM <- VM  %>% select(Timestamp,CPU.utilization,memoryUtilization)
#attach(VM)


# Visualization
# CPU usage
CPU.utilization_df <- data.frame(
  day = VM$Timestamp,
  value = VM$CPU.utilization
)
p1 <- ggplot(CPU.utilization_df, aes(x=day, y=value)) +
  geom_line(color="indianred") + 
  xlab("Time (ms)") +
  ylab("CPU usage (%)")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("CPU usage")
p1


# Memory usage
memory.usage_df <- data.frame(
  day = VM$Timestamp,
  value = VM$memoryUtilization
)
p2 <- ggplot(memory.usage_df, aes(x=day, y=value)) +
  geom_line(color="steelblue") + 
  xlab("Time (ms)") +
  ylab("Memory usage (%)")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Memory usage")
p2

# ARIMA models

# CPU utilization
CPU_serie <- ts(VM$CPU.utilization, start =c(1,1), frequency = 1 )
autoplot(CPU_serie)
#Dickey-Fuller test to check stationarity in mean
adf.test(CPU_serie, alternative = "stationary")

# Diff time series
dif1.CPU_serie = diff(CPU_serie)
autoplot(dif1.CPU_serie) +
  geom_line(color="indianred") + 
  xlab("Time (ms)") +
  ylab("CPU usage diff")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("CPU usage diff")
par(mfrow=c(2,1))
# Check ACF and PACF
Acf(dif1.CPU_serie, main="ACF CPU utilization")
Pacf(dif1.CPU_serie,main="PACF CPU utilization")

# Split training and testing for validation
CPU_serie_train <- subset(CPU_serie, start=1, end = length(CPU_serie)-100)
autoplot(CPU_serie_train)
CPU_serie_test <- subset(CPU_serie, start=length(CPU_serie)-100)

# Source: https://otexts.com/fpp2/arima-r.html
auto.arima(CPU_serie, trace=TRUE)

# Fit ARIMA model
fit1_CPU<- Arima(CPU_serie_train, order = c(1,1,2), method=(c("ML")))
summary(fit1_CPU)

residFIT1<-residuals(fit1_CPU)
Acf(residFIT1, main="ACF Residuals CPU utilization")
Pacf(residFIT1, main="PACF Residuals CPU utilization")
#Compute forecasts with model named as fit1
fcast1_CPU <- forecast(fit1_CPU, h=100)
autoplot(fcast1_CPU)

# Model diagnosis
tsdiag(fit1_CPU) #gr?ficos de residuos y Ljung-Box gr?fico
diagnosis1<-checkresiduals(fit1_CPU)

# Check outliers
# Source: https://robjhyndman.com/hyndsight/tsoutliers/
tsoutliers(CPU_serie)
# Replace outlier for interpolated values
autoplot(tsclean(CPU_serie), series="clean", color='red', lwd=0.9) +
  autolayer(CPU_serie, series="original", color='gray', lwd=1) +
  geom_point(data = tsoutliers(CPU_serie) %>% as.data.frame(), 
             aes(x=index, y=replacements), col='blue') +
  labs(x = "Time (ms)", y = "CPU usage (%)") +
  ggtitle("CPU usage - Outlier detection")


# Memory utilization
memory_serie <- ts(VM$memoryUtilization, start =c(1,1), frequency = 1 )
autoplot(memory_serie)
#Dickey-Fuller test to check stationarity in mean
adf.test(memory_serie, alternative = "stationary")


# Check ACF and PACF
Acf(memory_serie, main="ACF Memory utilization")
Pacf(memory_serie,main="PACF Memory utilization")

# Split training and testing for validation
memory_serie_train <- subset(memory_serie, start=1, end = length(memory_serie)-100)
autoplot(memory_serie_train)
memory_serie_test <- subset(memory_serie, start=length(memory_serie)-100)

# Source: https://otexts.com/fpp2/arima-r.html
auto.arima(memory_serie_train, trace=TRUE)

# Fit ARIMA model
fit1_memory<- Arima(memory_serie_train, order = c(2,0,1), method=(c("ML")))
summary(fit1_memory)

residFIT1<-residuals(fit1_memory)
Acf(residFIT1, main="ACF Residuals memory utilization")
Pacf(residFIT1, main="ACF Residuals memory utilization")
#Compute forecasts with model named as fit1
fcast1_memory <- forecast(fit1_memory, h=100)
autoplot(fcast1_memory)

# Model diagnosis
tsdiag(fit1_memory) #gr?ficos de residuos y Ljung-Box gr?fico
diagnosis1<-checkresiduals(fit1_memory)

# Check outliers
# Source: https://robjhyndman.com/hyndsight/tsoutliers/
tsoutliers(memory_serie)
# Replace outlier for interpolated values
autoplot(tsclean(memory_serie), series="clean", color='red', lwd=0.9) +
  autolayer(memory_serie, series="original", color='gray', lwd=1) +
  geom_point(data = tsoutliers(memory_serie) %>% as.data.frame(), 
             aes(x=index, y=replacements), col='blue') +
  labs(x = "Time (ms)", y = "Memory usage (%)") +
  ggtitle("Memory usage - Outlier detection")

# Validation
# Source: https://robjhyndman.com/papers/foresight.pdf
# Source: https://towardsdatascience.com/measures-performance-for-a-time-series-model-ets-or-arima-18b0a3e91e83
# Source: https://otexts.com/fpp2/accuracy.html

# CPU utilization
accuracy(fcast1_CPU,CPU_serie_test)

fcast2_CPU <- meanf(CPU_serie_train, h=100)
fcast3_CPU <- rwf(CPU_serie_train, h=100)
autoplot(window(CPU_serie, start=7000)) +
  autolayer(fcast1_CPU, PI=FALSE, series="ARIMA") +
  autolayer(fcast2_CPU, PI=FALSE, series="Mean") +
  autolayer(fcast3_CPU, PI=FALSE, series="Naïve") +
  labs(x = "Time (ms)", y = "CPU usage (%)") +
  ggtitle("CPU usage - Forecast") +
  guides(colour=guide_legend(title="Forecast"))

# Time-series cross-validation
arima_122 <- function(x, h){forecast(Arima(x, order=c(1,1,2)), h=h)}
e <- tsCV(CPU_serie, forecastfunction=arima_122, h=10)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:10, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()

# Memory utilization
accuracy(fcast1_memory,memory_serie_test)

fcast2_memory <- meanf(memory_serie_train, h=100)
fcast3_memory <- rwf(memory_serie_train, h=100)
autoplot(window(memory_serie, start=7000)) +
  autolayer(fcast1_memory, PI=FALSE, series="ARIMA") +
  autolayer(fcast2_memory, PI=FALSE, series="Mean") +
  autolayer(fcast3_memory, PI=FALSE, series="Naïve") +
  labs(x = "Time (ms)", y = "Memory usage (%)") +
  ggtitle("Memory usage - Forecast") +
  guides(colour=guide_legend(title="Forecast"))

# Time-series cross-validation
arima_201 <- function(x, h){forecast(Arima(x, order=c(2,0,1)), h=h)}
e <- tsCV(memory_serie, forecastfunction=arima_201, h=10)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:10, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()
