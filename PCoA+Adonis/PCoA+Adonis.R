#For example: Fig 3A “Beijing"

#load data: "Beijing.csv"
otu<-read.delim("Beijing.csv",   
                sep=",",     
                header = TRUE,  
                row.names = 1,   
                check.names = FALSE, 
                stringsAsFactors = FALSE)  

#Convert the data format
otu <- data.frame(t(otu))   



#Calculate the Bray distance----

library(vegan)
distance <- vegdist(otu, method = 'bray')  
pcoa <- cmdscale(distance, k = (nrow(otu) - 1), eig = T) 



sample_site <- data.frame(pcoa$point)[1:2]  
head(sample_site)

sample_site$Sample <- rownames(sample_site) 
head(sample_site)

names(sample_site)[1:2] <- c('PCoA1', 'PCoA2')  
head(sample_site)

#Percentage of explanation for the x-axis and y-axis in relation to the results
pcoa_eig <- (pcoa$eig)[1:2] / sum(pcoa$eig)  



group<-read.delim("Beijing-Group.csv",
                  header=TRUE,
                  sep=",",    
                  stringsAsFactors = FALSE,
                  check.names = FALSE)
head(group)



sample_site <- merge(sample_site, group, by = 'Sample', all.x = T) 
head(sample_site)


sample_site$Season <- factor(sample_site$Season, 
                           levels = c('Summer', 'Winter')) 



table(sample_site$Season)     



group_border <- plyr::ddply(sample_site, 'Season', 
                            function(df) df[chull(df[[2]], df[[3]]), ]) 


group_border

#ADONIS----
pcoa_adonis2 <- adonis2(otu~Season,group, permutations = 999)


#plot-----

library(ggplot2)

ggplot(sample_site, aes(PCoA1, PCoA2, group = Season))+  
  geom_vline(xintercept = 0, color = 'gray', size = 0.5,linetype = 2) + 
  geom_hline(yintercept = 0, color = 'gray', size = 0.5,linetype = 2)+
  
  
  
  geom_point(aes(fill = Season),shape = 21, 
             size = 1.5, alpha = 0.8,stroke =0.2)+     
  
  scale_fill_manual(values = c("#C34541","#687CC4"))+
  
  
  
  #Adding a confidence interval band of 0.95
  stat_ellipse(aes(colour = Season), size = 0.5, 
               level = 0.95,
               show.legend = F)+  
  
  
  
  scale_shape_manual(values = c(17, 16,15,12,10))+
  
 
  labs(x = paste('PCoA axis1 (', 
                 round(100 * pcoa_eig[1], 2), '%)'), 
       
       y = paste('PCoA axis2 (', 
                 round(100 * pcoa_eig[2], 2), '%)'))+
  
  
  theme(panel.grid = element_line(color = 'white',    
                                  linetype = 2, size = 0),
        panel.background = element_rect(color = 'black',
                                        fill = 'transparent'),
        legend.key = element_rect(fill = 'transparent'))   








