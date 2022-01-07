# Descriptive statistics
# Javier Galindos

rm(list = ls()) # Remove all the objects we created so far.
set.seed(123)

#Libraries
library("rstudioapi")  
library(dplyr) # Manipulate data
library(ggplot2) # Plots

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


# Descriptive statistics
summary(VM)
# Histograms
hist_CPU <- ggplot(VM, aes(x=CPU.utilization)) + 
  geom_histogram(color="black", fill="indianred") +
  ggtitle("Histogram of CPU usage")
hist_CPU
hist_memory <- ggplot(VM, aes(x=memoryUtilization)) + 
  geom_histogram(color="black", fill="steelblue") +
  ggtitle("Histogram of Memory usage")
hist_memory

# Normalizing data
z_score_cpu <- (CPU.utilization - mean(CPU.utilization))/sd(CPU.utilization)
summary(z_score_cpu)
z_score_memory <- (memoryUtilization - mean(memoryUtilization))/sd(memoryUtilization)
summary(z_score_memory)

# t-test
t.test(CPU.utilization)
t.test(memoryUtilization)

# Finding outliers by counting quantiles
s1 <- ts(VM, start = VM$Timestamp[1], end= VM$Timestamp[length(VM$Timestamp)], frequency = 1 )
# CPU utilization
qu <- quantile(CPU.utilization)
qu <- as.data.frame(qu)
q1 <- qu[2,1]
q2 <- qu[3,1]
q3 <- qu[4,1]
iqr <- q3-q1
w1 <- q1-1.5*iqr
w2 <- q3 + 1.5*iqr
# then combine all together 
o.h <- subset(VM, CPU.utilization < q1-1.5*iqr)   
o2.h <- subset(VM, CPU.utilization > q3 + 1.5*iqr)
outliers_h <- rbind(o.h, o2.h) 
head(outliers_h)
dim(outliers_h)
summary(outliers_h)
plot(outliers_h[,2])
# Memory utilization
qu <- quantile(memoryUtilization)
qu <- as.data.frame(qu)
q1 <- qu[2,1]
q2 <- qu[3,1]
q3 <- qu[4,1]
iqr <- q3-q1
w1 <- q1-1.5*iqr
w2 <- q3 + 1.5*iqr
# then combine all together 
o.h <- subset(VM, memoryUtilization < q1-1.5*iqr)   
o2.h <- subset(VM, memoryUtilization > q3 + 1.5*iqr)
outliers_h <- rbind(o.h, o2.h) 
head(outliers_h)
dim(outliers_h)
summary(outliers_h)
plot(outliers_h[,3])





