library(vegan) 
library(ggplot2)
library(reshape2)

#load the data of mean relative abundance of microorganisms: "MDA.csv"
HP_TOP_01 <- read.csv("MDA.csv", header = T,
                      row.names = 1)

#Normalize the values of MDA
HP_TOP_02 <- decostand(HP_TOP_01[,1:5],"standardize",MARGIN = 1)  

HP_TOP_02$otu_name <- row.names(HP_TOP_02)   

#Convert the data format
HP_TOP_03 <- melt(data.frame(HP_TOP_02), id.vars = "otu_name")  


head(HP_TOP_02)

#Sort the taxa based on their MDA values
HP_TOP_03$otu_name <- factor(HP_TOP_03$otu_name, levels = c("taxa 1","taxa 2","taxa 3","taxa 4","taxa 5","taxa 6","taxa 7","taxa 8","taxa 9","taxa 10","taxa 11","taxa 12","taxa 13","taxa 14","taxa 15"))


#Rearrange the x-axis order (cities)
HP_TOP_03$variable <- factor(HP_TOP_03$variable, levels = c("Harbin","Beijing","Nanjing","Lhasa","Shenzhen"))




ggplot(HP_TOP_03, aes(x = variable, y = otu_name))+
  
  geom_tile(aes(fill = value),  
            colour = "white")+

  
  scale_fill_gradient2(name = "Relaive abundance",    
                      limit = c(-1.1, 1.8),            
                      breaks = c(-1, 0, 1.5),  
                      low = "steelblue",                 
                      mid = "#FFF7CB",
                      high = "red")+
  
  theme(panel.background = element_blank())+
  
  theme_grey(base_size = 9) + 
  
  labs(x = "", y = "") + 
  
  scale_x_discrete(expand = c(0, 0)) +
  
  scale_y_discrete(expand = c(0, 0))
  
 


