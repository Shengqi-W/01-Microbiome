#For example: Fig 4A - Bacteria_summer


library(randomForest)

otu <- read.csv('B-S.csv',row.names = 1)

age <- read.csv('B-Group-S.csv', row.names = 1)
otu_1 <- data.frame(t(otu))
otu_1 <- otu_1[rownames(age), ]
otu_2 <- cbind(otu_1, age)






#Random Forest calculation----
set.seed(1234) #Set the seed for random number generation

otu_2$City <- factor(otu_2$City) 


otu.forest <- randomForest(City~.,data = otu_2,  
                           
                           ntree=500,
                           
                           importance = TRUE)

print(otu.forest)  


MeanDecreaseAccuracy_1 <- importance(otu.forest, type = 1)  

#Sort the table data in descending order of importance
MeanDecreaseAccuracy_2 <- MeanDecreaseAccuracy_1[order(MeanDecreaseAccuracy_1[,1],decreasing = T),]     

#Transform the data format
MeanDecreaseAccuracy_2 <- as.data.frame(MeanDecreaseAccuracy_2)  





MeanDecreaseAccuracy_2$otu <- rownames(MeanDecreaseAccuracy_2) 


colnames(MeanDecreaseAccuracy_2) <- c("MDA_value", "OTU_name")  
MDA_top_1 <- MeanDecreaseAccuracy_2[1:15,]  


#save the results as "csv" format
write.table(MDA_top_1, file = "B_S-MDA.csv", sep = ",", row.names = TRUE, col.names = TRUE, quote = FALSE)



#plot: Horizontal bar chart of OTU MDA----

library(ggplot2)




ggplot(MDA_top_1, aes(y = reorder(OTU_name, MDA_value), x = MDA_value))+  
  
  geom_bar(stat = "identity",
           aes(fill =  MDA_value))+   
  
  theme(panel.grid = element_line(color = 'white', linetype = 2,size = 0),
        
        panel.background = element_rect(color = 'black',fill = 'transparent'),
        
        legend.key = element_rect(fill = 'transparent'))   
  



#Ten-fold cross-validation----
#Cross-validation to determine the optimal number of important OTUs for model construction

otu_cv_1= replicate(5, rfcv(otu_2[-ncol(otu_2)], otu_2$City, cv.fold = 10, step = 1.5), simplify=FALSE)
otu_cv_2 <- data.frame(sapply(otu_cv_1, '[[', 'error.cv')) 

otu_cv_2$otus <- rownames(otu_cv_2)  

otu_cv_3 <- reshape2::melt(otu_cv_2, id = 'otus') 

otu_cv_3$otus <- as.numeric(as.character(otu_cv_3$otus))  

otu_cv.mean <- aggregate(otu_cv_3$value, by = list(otu_cv_3$otus), FUN = mean)  




#plot: Line plot. Aims: Select an appropriate number of OTUs ----

library(ggplot2)
library(ggalt)

p <- ggplot(otu_cv.mean, aes(Group.1, x))+
  
  scale_y_continuous(breaks = seq(0,0.45,0.05))+  
  
  scale_x_continuous(limits = c(0,800), breaks = c(1,15,50,100,200,400,600))+
  
  geom_xspline(spline_shape = 1)+
  
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(color = 'black', 
                                        fill = 'transparent')) +  
  
  labs(title = '',x = 'Number of genus', y = 'Error rate of Cross-validation')
p 










