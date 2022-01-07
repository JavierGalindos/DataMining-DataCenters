# Plot Time series VMs
# Javier Galindos

rm(list = ls()) # Remove all the objects we created so far.

#Libraries
library("rstudioapi")  
library(dplyr) # Manipulate data
library(ggplot2) # Plots
library(forecast) # Time series
library(tseries) # Time series
library(mclust) # Clustering

# Set working directory
curDir <- dirname(getActiveDocumentContext()$path)# Current directory
dataDir <- file.path(curDir,"fastStorage") # Concatenate paths
setwd(dataDir) 
getwd()  
#create a list of the files from your target directory
file_list <- list.files(path=dataDir)



# Load dataset (for loop to load everything)
for (i in seq_along(1:100)){
  VM <- read.csv(file = file_list[i], header = TRUE, sep = ";")
  
  # Data preparation
  
  # Selecting relevant variables
  VM <- VM  %>% select(Timestamp..ms.,CPU.usage....,Memory.usage..KB.,Memory.capacity.provisioned..KB.)
  # Memory usage
  VM <- VM  %>% mutate(memoryUsage= 100*Memory.usage..KB./Memory.capacity.provisioned..KB.)
  # Rename column
  VM <- VM  %>% rename(CPU.usage= CPU.usage....)
  VM <- VM  %>% rename(Timestamp= Timestamp..ms.)
  VM <- VM  %>% select(Timestamp,CPU.usage,memoryUsage)
  attach(VM)
  
  
  # Visualization
  # CPU usage
  CPU.usage_df <- data.frame(
    day = VM$Timestamp,
    value = CPU.usage
  )
  p1 <- ggplot(CPU.usage_df, aes(x=day, y=value)) +
    geom_line(color="indianred") + 
    xlab("Time (ms)") +
    ylab("CPU usage (%)")+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    ggtitle(paste("CPU usage",i))
  print(p1)
}

# 90, 73, 44, 43, 31, 1049

# Memory usage
memory.usage_df <- data.frame(
  day = VM$Timestamp,
  value = memoryUsage
)
p2 <- ggplot(memory.usage_df, aes(x=day, y=value)) +
  geom_line(color="steelblue") + 
  xlab("Time (ms)") +
  ylab("Memory usage (%)")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle("Memory usage")
p2