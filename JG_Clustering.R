# Clustering
# Javier Galindos

# Load data

rm(list = ls()) # Remove all the objects we created so far.
set.seed(17)

#Libraries
library("rstudioapi")  
library(dplyr) # Manipulate data
library(ggplot2) # Plots
library(tseries) # Time series
library(mclust) # Clustering
library (NbClust)
library (cluster)
library (clustertend)
library (factoextra)

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

# Clustering tendency
hopkins_CPU <- hopkins (VM[,1:2], n=100)
hopkins_memory <- hopkins (VM[,c(1,3)], n=100)

# Number of clusters
fviz_nbclust(VM[,1:2], kmeans, method = "silhouette")+ theme_classic()
fviz_nbclust(VM[,c(1,3)], kmeans, method = "silhouette")+ theme_classic()

# EM clustering

# CPU utilization
EM_fit_CPU <- Mclust(VM[,1:2],2)
summary(EM_fit_CPU)
plot(EM_fit_CPU, main="CPU usage - EM clustering")
p3 <- ggplot(CPU.utilization_df, aes(x=day, y=value, color=factor(EM_fit_CPU$classification) )) +
  geom_point() + 
  xlab("Time (ms)") +
  ylab("CPU usage (%)")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none") +
  ggtitle("CPU usage - EM clustering") +
  stat_ellipse()
p3

# Memory utilization
EM_fit_memory <- Mclust(VM[,c(3)],2)
summary(EM_fit_memory)
plot(EM_fit_memory, main="Memory usage- EM clustering")
p4 <- ggplot(memory.usage_df, aes(x=day, y=value, color=factor(EM_fit_memory$classification))) +
  geom_point() + 
  xlab("Time (ms)") +
  ylab("Memory usage (%)")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none") +
  ggtitle("Memory usage- EM clustering") +
  stat_ellipse()
p4


# K-means

# CPU utilization
kmeans_fit_CPU <- kmeans(VM[,2], 2)
p5 <- ggplot(CPU.utilization_df, aes(x=day, y=value,color=factor(kmeans_fit_CPU$cluster+1))) +
  geom_point() + 
  xlab("Time (ms)") +
  ylab("CPU usage (%)")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none") +
  ggtitle("CPU usage - K-means clustering")
p5

# Memory utilization
kmeans_fit_memory <- kmeans(VM[,3], 2)
p6 <- ggplot(memory.usage_df, aes(x=day, y=value,color=factor(kmeans_fit_memory$cluster+1))) +
  geom_line() + 
  xlab("Time (ms)") +
  ylab("Memory usage (%)")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none") +
  ggtitle("Memory usage- k-means clustering") 
p6
