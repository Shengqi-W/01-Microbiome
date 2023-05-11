
library(vegan)
#load "otu" data
otu = read.table('different sequencing depth.txt', header=T, sep="\t", quote = "", row.names=1, comment.char="", stringsAsFactors = FALSE)


colSums(otu)
#Make the sample depths equal and assign the results to "otu_Flattening"
otu_Flattening =as.data.frame(t(rrarefy(t(otu),min(colSums(otu)))))

#Save the results as "otu_bacteria_even.csv"
write.table(otu_Flattening, file="otu_Fungi_even.csv",sep =",", quote=FALSE)