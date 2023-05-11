
 
library(readxl)

data_all <- read_excel("data.xlsx", sheet = "sheet1")

library(reshape2)

data_all_1 = melt(data_all) 
data_all_2 <- as.data.frame(data_all_1)
variable <- as.factor(data_all_2$variable)  
value <- data_all_2$value


#Test for homogeneity of variance in the data----
library(car)
library(carData) 

leveneTest(value, variable, center = "mean")




#ANOVA----
data.aov <- aov(value~variable, data = data_all)
summary(data.aov)  


#LSD----
library(DescTools)

PostHocTest(data.aov,method = "lsd")  






