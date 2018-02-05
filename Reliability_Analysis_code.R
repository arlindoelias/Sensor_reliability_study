# Reliability Analysis of the joint angular parameters



# Packages and working directory
install.packages("psych")
install.packages("ggplot2")
install.packages("car")
library(ggplot2)
library(reshape2)
library(car)
library(psych)


setwd("C:/Users/arlin/Desktop/Projetos/DOUTORADO/CAP02.1-SENSOR_RELIABILITY/Dataset")



# Part 01: Analysis of the hip parameters

# Loading hip dataset
con <- "https://raw.githubusercontent.com/arlindoelias/Sensor_reliability_study/master/hipData.csv"
hipdata <- read.csv(con, sep=";")
names(hipdata)[1] <- "H1"
# View(hipdata) # inspection of the dataset

# Descriptive statistics
summary(hipdata)
describe(hipdata) 


# Normality assessment (Shapiro-Wilk test)
test.norm.vector <- NULL

for(i in 1:24){
  test.norm <- shapiro.test(hipdata[,i])
  test.norm.vector[i] <- round(test.norm$p.value,2)
}
test.norm.vector
names(test.norm.vector) <- names(hipdata)


# Homogeneity of variance (Levene test)
# Reorganizing the Hip dataset
hipdata2 <- hipdata[c("H1", "H1_2", "H2", "H2_2", "H3", "H3_2", "H4", "H4_2", "H5", "H5_2", "H6", "H6_2", "H7", "H7_2", "H8", "H8_2", "H9", "H9_2", "H10", "H10_2", "H11", "H11_2", "H12", "H12_2")]
hipdata2$Participants <- c(1:14)

hipdata.m <- melt(hipdata2, id.var = "Participants") # Changing dataset into "long" format
hipdata.m$days <- c(rep(c(rep("Day 01", 14), rep("Day 02", 14)), 12)) # Add test days as a factor variable

leveneTest(hipdata.m$value, hipdata.m$days, center = mean)



# Analysing parameter means between days
hip01 <- hipdata[1:12]  # Dataframe with hip paramenters from day 1
hip02 <- hipdata[13:24]  # Dataframe with hip paramenters from day 2

hip.statistics <- data.frame(statistic = rep(NA, 12), df = rep(NA, 12), p.value = rep(NA, 12), conf.int.lower = rep(NA, 12), conf.int.upper = rep(NA, 12)) # Definition of a dataframe to store t.test parameters
for(i in 1:12){
  test <- t.test(hip01[,i], hip02[,i], paired = T)
  hip.statistics$statistic[i] <- round(test$statistic[[1]], 2)
  hip.statistics$df[i] <- round(test$parameter[[1]], 2)
  hip.statistics$p.value[i] <- round(test$p.value, 2)
  hip.statistics$conf.int.lower[i] <- round(test$conf.int[1], 2)
  hip.statistics$conf.int.upper[i] <- round(test$conf.int[2], 2)
}
hip.statistics



# ICC estimation

# H1
data <- matrix(c(hipdata$H1, hipdata$H1_2), ncol= 2, byrow = FALSE)
test <- ICC(data)

#H2
data <- matrix(c(hipdata$H2, hipdata$H2_2), ncol= 2, byrow = FALSE)
ICC(data)

#H3
data <- matrix(c(hipdata$H3, hipdata$H3_2), ncol= 2, byrow = FALSE)
ICC(data)

#H4
data <- matrix(c(hipdata$H4, hipdata$H4_2), ncol= 2, byrow = FALSE)
ICC(data)

#H5
data <- matrix(c(hipdata$H5, hipdata$H5_2), ncol= 2, byrow = FALSE)
ICC(data)

# H6
data <- matrix(c(hipdata$H6, hipdata$H6_2), ncol= 2, byrow = FALSE)
ICC(data)

# H7
data <- matrix(c(hipdata$H7, hipdata$H7_2), ncol= 2, byrow = FALSE)
ICC(data)

# H8
data <- matrix(c(hipdata$H8, hipdata$H8_2), ncol= 2, byrow = FALSE)
ICC(data)

# H9
data <- matrix(c(hipdata$H9, hipdata$H9_2), ncol= 2, byrow = FALSE)
ICC(data)

# H10
data <- matrix(c(hipdata$H10, hipdata$H10_2), ncol= 2, byrow = FALSE)
ICC(data)

#H11
data <- matrix(c(hipdata$H11, hipdata$H11_2), ncol= 2, byrow = FALSE)
ICC(data)

#H12
data <- matrix(c(hipdata$H12, hipdata$H12_2), ncol= 2, byrow = FALSE)
ICC(data)



# ICC hip parameters
hip.icc <- data.frame(type = rep(NA, 12), icc = rep(NA, 12), f = rep(NA, 12), df1 = rep(NA, 12), df2 = rep(NA, 12), lower.bound = rep(NA, 12), upper.bound = rep(NA, 12))
for(i in 1:12){
  data <- matrix(c(hip01[,i], hip02[,i]), ncol= 2, byrow = FALSE)
  test <- ICC(data)
  hip.icc[i,] <- test$results[3,]
}
hip.icc$parameter <- names(hip01)
hip.icc <- hip.icc[, c(8, 1:7)]
hip.icc


# Part02: knee ICC analysis
kneedata <- read.csv("joelhoFull14v02.csv", sep=";")
# View(kneedata)
names(kneedata)[1] <- "K1"


# Descriptive statistics
summary(kneedata)
describe(kneedata)


# Normality assessment (Shapiro-Wilk test)
test.norm.vector <- NULL

for(i in 1:24){
  test.norm <- shapiro.test(kneedata[,i])
  test.norm.vector[i] <- round(test.norm$p.value,2)
}
test.norm.vector



# Homogeneity of variance (levene test)
# Reorganizing the Knee dataset
kneedata2 <- kneedata[c("K1", "K1_2", "K2", "K2_2", "K3", "K3_2", "K4", "K4_2", "K5", "K5_2", "K6", "K6_2", "K7", "K7_2", "K8", "K8_2", "K9", "K9_2", "K10", "K10_2", "K11", "K11_2", "K12", "K12_2")]
kneedata2$Participants <- c(1:14)

kneedata.m <- melt(kneedata2, id.var = "Participants") # Changing dataset into "long" format
kneedata.m$days <- c(rep(c(rep("Day 01", 14), rep("Day 02", 14)), 12))

leveneTest(kneedata.m$value, kneedata.m$days)


# Analysing parameter means between days
knee01 <- kneedata[1:12]  # Dataframe with knee paramenters from day 1
knee02 <- kneedata[13:24]  # Dataframe with knee paramenters from day 2
knee.statistics <- data.frame(statistic = rep(NA, 12), df = rep(NA, 12), p.value = rep(NA, 12), conf.int.lower = rep(NA, 12), conf.int.upper = rep(NA, 12)) # Definition of a dataframe to store t.test parameters

for(i in 1:12){
  test <- t.test(knee01[,i], knee02[,i], paired = T)
  knee.statistics$statistic[i] <- round(test$statistic[[1]], 2)
  knee.statistics$df[i] <- round(test$parameter[[1]], 2)
  knee.statistics$p.value[i] <- round(test$p.value, 2)
  knee.statistics$conf.int.lower[i] <- round(test$conf.int[1], 2)
  knee.statistics$conf.int.upper[i] <- round(test$conf.int[2], 2)
}
knee.statistics



# K1
data <- matrix(c(kneedata$K1, kneedata$K1_2), ncol= 2, byrow = FALSE)
ICC(data)

# K2
data <- matrix(c(kneedata$K2, kneedata$K2_2), ncol= 2, byrow = FALSE)
ICC(data)

# K3
data <- matrix(c(kneedata$K3, kneedata$K3_2), ncol= 2, byrow = FALSE)
ICC(data)

# K4
data <- matrix(c(kneedata$K4, kneedata$K4_2), ncol= 2, byrow = FALSE)
ICC(data)

# K5
data <- matrix(c(kneedata$K5, kneedata$K5_2), ncol= 2, byrow = FALSE)
ICC(data)

# K6
data <- matrix(c(kneedata$K6, kneedata$K6_2), ncol= 2, byrow = FALSE)
ICC(data)

# K7
data <- matrix(c(kneedata$K7, kneedata$K7_2), ncol= 2, byrow = FALSE)
ICC(data)

# K8
data <- matrix(c(kneedata$K8, kneedata$K8_2), ncol= 2, byrow = FALSE)
ICC(data)

# K9
data <- matrix(c(kneedata$K9, kneedata$K9_2), ncol= 2, byrow = FALSE)
ICC(data)

# K10
data <- matrix(c(kneedata$K10, kneedata$K10_2), ncol= 2, byrow = FALSE)
ICC(data)

# K11
data <- matrix(c(kneedata$K11, kneedata$K11_2), ncol= 2, byrow = FALSE)
ICC(data)

# K12
data <- matrix(c(kneedata$K12, kneedata$K12_2), ncol= 2, byrow = FALSE)
ICC(data)



# Part 03: ICC analysis of the ankle variables
ankledata <- read.csv("ankleFull14.csv", sep=";")
View(ankledata)
names(ankledata)[1] <- "A1"


# Descriptive statistics
summary(ankledata)
describe(ankledata)


# Normality assessment (Shapiro-Wilk test)
test.norm.vector <- NULL

for(i in 1:18){
  test.norm <- shapiro.test(ankledata[,i])
  test.norm.vector[i] <- round(test.norm$p.value,2)
}
test.norm.vector


# Homogeneity of variance (leveneTest)
ankledata2 <- ankledata[c("A1", "A1_2", "A2", "A2_2", "A3", "A3_2", "A4", "A4_2", "A5", "A5_2", "A6", "A6_2", "A7", "A7_2", "A8", "A8_2", "A9", "A9_2")]
ankledata2$Participants <- c(1:14)

ankledata.m <- melt(ankledata2, id.var = "Participants") # Changing dataset into "long" format
ankledata.m$days <- c(rep(c(rep("Day 01", 14), rep("Day 02", 14)), 9))

leveneTest(ankledata.m$value, ankledata.m$days)



# Analysis of meand difference between days

ankle01 <- ankledata[1:9]  # Dataframe with ankle paramenters from day 1
ankle02 <- ankledata[10:18]  # Dataframe with ankle paramenters from day 2
ankle.statistics <- data.frame(statistic = rep(NA, 9), df = rep(NA, 9), p.value = rep(NA, 9), conf.int.lower = rep(NA, 9), conf.int.upper = rep(NA, 9)) # Definition of a dataframe to store t.test parameters

for(i in 1:9){
  test <- t.test(ankle01[,i], ankle02[,i], paired = T)
  ankle.statistics$statistic[i] <- round(test$statistic[[1]], 2)
  ankle.statistics$df[i] <- round(test$parameter[[1]], 2)
  ankle.statistics$p.value[i] <- round(test$p.value, 2)
  ankle.statistics$conf.int.lower[i] <- round(test$conf.int[1], 2)
  ankle.statistics$conf.int.upper[i] <- round(test$conf.int[2], 2)
}
ankle.statistics


# A1
data <- matrix(c(ankledata$A1, ankledata$A1_2), ncol= 2, byrow = FALSE)
ICC(data)

# A2
data <- matrix(c(ankledata$A2, ankledata$A2_2), ncol= 2, byrow = FALSE)
ICC(data)

# A3
data <- matrix(c(ankledata$A3, ankledata$A3_2), ncol= 2, byrow = FALSE)
ICC(data)

# A4
data <- matrix(c(ankledata$A4, ankledata$A4_2), ncol= 2, byrow = FALSE)
ICC(data)

# A5
data <- matrix(c(ankledata$A5, ankledata$A5_2), ncol= 2, byrow = FALSE)
ICC(data)

# A6
data <- matrix(c(ankledata$A6, ankledata$A6_2), ncol= 2, byrow = FALSE)
ICC(data)

# A7
data <- matrix(c(ankledata$A7, ankledata$A7_2), ncol= 2, byrow = FALSE)
ICC(data)

#A8
data <- matrix(c(ankledata$A8, ankledata$A8_2), ncol= 2, byrow = FALSE)
ICC(data)

# A9
data <- matrix(c(ankledata$A9, ankledata$A9_2), ncol= 2, byrow = FALSE)
ICC(data)



# Computation of the Standard Error of Measurement and Minimal Detectable Change (MDC)
# Formula: SEM = SD * (sqrt(1-ICC))
# Formula: 1.96 * SEM * sqrt(2)

# HIP DATA
hip01 <- hipdata[1:12]  # Dataframe with hip paramenters from day 1
hip02 <- hipdata[13:24]  # Dataframe with hip paramenters from day 2
ICCvec <-c(0.82, 0.79, 0.85, 0.84, 0.72, 0.87, 0.61, 0.31, 0.54, 0.51, 0.72, 0.61)  # A vector to store ICC3 values
SEMvec <- NULL  # A vector to store SEM values
MDCvec <- NULL  # A vector to store MDC values

for(i in 1:12){
  SEM <- mean(sd(hip01[,i]) + sd(hip02[,i])) * (sqrt(1-ICCvec[i]))
  SEMvec[i] <- SEM
  MDC <- 1.96 * SEM * sqrt(2)
  MDCvec[i] <- MDC
}

round(SEMvec, 2)
round(MDCvec, 2)


# KNEE DATA
knee01 <- kneedata[1:12]  # Dataframe with knee paramenters from day 1
knee02 <- kneedata[13:24]  # Dataframe with knee paramenters from day 2
ICCknee <-c(0.98, 0.93, 0.61, 0.86, 0.89, 0.81, 0.31, 0.30, 0.68, 0.40, 0.37, 0.02)  # A vector to store ICC3 values
SEMknee <- NULL  # A vector to store SEM values
MDCknee <- NULL  # A vector to store MDC values

for(i in 1:12){
  SEM <- mean(sd(knee01[,i]) + sd(knee02[,i])) * (sqrt(1-ICCknee[i]))
  SEMknee[i] <- SEM
  MDC <- 1.96 * SEM * sqrt(2)
  MDCknee[i] <- MDC
}

round(SEMknee, 2)
round(MDCknee, 2)



# ANKLE DATA
ankle01 <- ankledata[1:9]  # Dataframe with ankle paramenters from day 1
ankle02 <- ankledata[10:18]  # Dataframe with ankle paramenters from day 2
ICCankle <-c(0.37, 0.14, 0.21, 0.30, 0.33, 0.45, 0.25, 0.29, 0.45)  # A vector to store ICC3 values
SEMankle <- NULL  # A vector to store SEM values
MDCankle <- NULL  # A vector to store MDC values

for(i in 1:9){
  SEM <- mean(sd(ankle01[,i]) + sd(ankle02[,i])) * (sqrt(1-ICCankle[i]))
  SEMankle[i] <- SEM
  MDC <- 1.96 * SEM * sqrt(2)
  MDCankle[i] <- MDC
}

round(SEMankle, 2)
round(MDCankle, 2)



## GRAPHICS

# Reorganizing the Hip dataset
hipdata2 <- hipdata[c("H1", "H1_2", "H2", "H2_2", "H3", "H3_2", "H4", "H4_2", "H5", "H5_2", "H6", "H6_2", "H7", "H7_2", "H8", "H8_2", "H9", "H9_2", "H10", "H10_2", "H11", "H11_2", "H12", "H12_2")]
hipdata2$Participants <- c(1:14)

hipdata.m <- melt(hipdata2, id.var = "Participants") # Changing dataset into "long" format
hipdata.m$days <- c(rep(c(rep("Day 01", 14), rep("Day 02", 14)), 12))



# Boxplot of the hip parameters
hip.graph <- ggplot(data = hipdata.m, aes(x=variable, y=value, fill = days)) + geom_boxplot() + ylab("") + xlab("")

pdf("hip_plot2.pdf")
hip.graph + scale_fill_brewer(palette = "Dark2") + guides(fill=guide_legend(title= "Test days")) + 
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8))
dev.off()
  
   

# KNEE 
# Reorganizing the Knee dataset
kneedata2 <- kneedata[c("K1", "K1_2", "K2", "K2_2", "K3", "K3_2", "K4", "K4_2", "K5", "K5_2", "K6", "K6_2", "K7", "K7_2", "K8", "K8_2", "K9", "K9_2", "K10", "K10_2", "K11", "K11_2", "K12", "K12_2")]
kneedata2$Participants <- c(1:14)

kneedata.m <- melt(kneedata2, id.var = "Participants") # Changing dataset into "long" format
kneedata.m$days <- c(rep(c(rep("Day 01", 14), rep("Day 02", 14)), 12))



# Boxplot of the knee parameters
knee.graph <- ggplot(data = kneedata.m, aes(x=variable, y=value, fill = days)) + geom_boxplot() + ylab("") + xlab("")

pdf("knee_plot2.pdf")
knee.graph + scale_fill_brewer(palette = "Dark2") + guides(fill=guide_legend(title= "Test days")) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8))
dev.off()
 
 
 
 # ANKLE
# Reorganizing the ankle dataset
ankledata2 <- ankledata[c("A1", "A1_2", "A2", "A2_2", "A3", "A3_2", "A4", "A4_2", "A5", "A5_2", "A6", "A6_2", "A7", "A7_2", "A8", "A8_2", "A9", "A9_2")]
ankledata2$Participants <- c(1:14)

ankledata.m <- melt(ankledata2, id.var = "Participants") # Changing dataset into "long" format
ankledata.m$days <- c(rep(c(rep("Day 01", 14), rep("Day 02", 14)), 9))



# Boxplot of the hip parameters
ankle.graph <- ggplot(data = ankledata.m, aes(x=variable, y=value, fill = days)) + geom_boxplot() + ylab("") + xlab("")

pdf("ankle_plot2.pdf")
ankle.graph + scale_fill_brewer(palette = "Dark2") + guides(fill=guide_legend(title= "Test days")) + 
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8))
dev.off()


# SAVING FIGURES IN .PNG FORMAT

# Figures were saved in .png format by using the 'export' option in the upper bar of the graphics panel. 
# Options: width = 1112; height = 997




 
 



