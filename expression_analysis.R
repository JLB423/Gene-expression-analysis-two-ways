### Midterm Part 2 ###

library(reshape2)
# Preprocessing of data - 
# read in the col file
col<- read.csv('Mnemiopsis_col_data.csv', header=T, row.names=1)
# check out the file
head(col) # this shows the factor variables and the condition (to be used for DESeq2)

# read in the data file
data<- read.csv('Mnemiopsis_count_data.csv', header=T)
# See what is in the file
head(data)

# Clean up the data- rename the col headers based on the data in the col file 
names(data)<- c('Gene', 'Aboral1','Aboral2','Aboral3','Aboral4','Oral1', 'Oral2', 'Oral3', 'Oral4' )
# Check to make sure the col names are now correct
data[1:4,]
head(data)
summary(data)

## 1. What are the top 5 genes with the highest average expression across experiments, in the set? 
# get the means by row 
data_mean <- rowMeans(data[,-1], 1)
data_mean<- round(data_mean, 2)

# add the means to the dataframe 
data['row means'] = data_mean

# get the order with the highest values being at the top
y<- order(data$`row means`, decreasing = T)
#get the indexes of the top 5 row means
y[1:5]

# pull up each gene to show the top 5 genes by expression
data[12714,]  # ML20395a 
data[14235,]  # ML426358a 
data[16420,] # ML46651a 
data[2612,] # ML020045a   
data[30,]  # ML00017a  


## 2. Are the top 5 genes different if they are done on a per col basis?
# get the top values by col
top_aboral1<-order(data$Aboral1, decreasing = T)
top_aboral1[1:5]  # 16420 , 12714, 2612, 11879, 14235
# print the top 5 genes in column aboral1
data[16420,]
data[12714,]
data[2612,]
data[11879,]
data[14235,]

top_aboral2<-order(data$Aboral2, decreasing = T)
top_aboral2[1:5] # 12714  16420  14235  1908 3788
# print the top 5 genes in column aboral2
data[12714,]
data[16420,]
data[14235,]
data[1908,]
data[3788,]

top_aboral3<-order(data$Aboral3, decreasing = T)
top_aboral3[1:5] # 12714  1908  14235 16420  3788 
# print the top 5 genes in column aboral3
data[12714,]
data[1908,]
data[14235,]
data[16420,]
data[3788,]

top_aboral4<-order(data$Aboral4, decreasing = T)
top_aboral4[1:5] # 1908  12714  3788  16420 3790
# print the top 5 genes in column aboral4
data[1908,]
data[12714,]
data[3788,]
data[16420,]
data[3790,]

top_oral1<-order(data$Oral1, decreasing = T)
top_oral1[1:5] # 12714 2612 4249 14235  30
# print the top 5 genes in column oral1
data[12714,]
data[2612,]
data[4249,]
data[14235,]
data[30,]

top_oral2<-order(data$Oral2, decreasing = T)
top_oral2[1:5] # 12714 2612 4249 14235  30
# print the top 5 genes in column oral2
data[12714,]
data[2612,]
data[4249,]
data[30,]
data[14235,]

top_oral3<-order(data$Oral3, decreasing = T)
top_oral3[1:5] # 12714 588 4249 14235  30
# print the top 5 genes in column oral3
data[12714,]
data[588,]
data[14325,]
data[30,]
data[4249,]

top_oral4<-order(data$Oral4, decreasing = T)
top_oral4[1:5] # 12714 588 16420 2612 30
# print the top 5 genes in column oral4
data[12714,]
data[588,]
data[16420,]
data[2612,]
data[30,]


which.max(data$Aboral1) # 125,638
data[16420,]  # gene ML46651a

which.max(data$Aboral2) # 131,017
data[12714,] # ML20395a

which.max(data$Aboral3) # 136,282
data[12714,] # ML20395a 

which.max(data$Aboral4) # 111,860
data[1908,] # ML01482a

which.max(data$Oral1) # 163,380
data[12714,] # ML20395a 

which.max(data$Oral2) # 101,792
data[12714,] # ML20395a 

which.max(data$Oral3) # 101,421
data[12714,] # ML20395a 

which.max(data$Oral4) # 109,944
data[12714,] # ML20395a 


## 3. Calc the mean and sd of each col
col_mean <- apply(data[2:9], 2, mean)
col_mean  # print to see the means
col_sd<- apply(data[,2:9], 2, sd)
col_sd  # print to see the sd's
                
# the means are not the same, so we will scale the cols to make the means equal
Ab1_scaled<- data$Aboral1 / 1.021633
mean(Ab1_scaled)
Ab2_scaled<- data$Aboral2 / 1.13162
mean(Ab2_scaled)
Ab3_scaled <- data$Aboral3 / 1.13308
mean(Ab3_scaled)
Ab4_scaled <- data$Aboral4 / 1.09179
mean(Ab4_scaled)
Or1_scaled <- data$Oral1 / 1.075322
mean(Or1_scaled)
Or2_scaled <- data$Oral2 / 0.836244
mean(Or2_scaled)
Or3_scaled <- data$Oral3 / 0.8179467
mean(Or3_scaled)
Or4_scaled <- data$Oral4 / 0.891679
mean(Or4_scaled)

# Now recreate the data frame with the new scaled cols
scaled_df <- data.frame(data$Gene, Ab1_scaled, Ab2_scaled, Ab3_scaled, Ab4_scaled, Or1_scaled, Or2_scaled, Or3_scaled, Or4_scaled)
head(scaled_df)
summary(Scaled_df)
colnames(scaled_df, )

## 4. use correlations between cols to find the samples that are closely related. 
## .. is this concordant with the col labels?
col_cor<- cor(scaled_df[,2:9])
# by looking at the output you can see which cols are the most closely correrlated
round(col_cor, 4)
# highest correlations between cols:
# Aboral1 = Aboral3
# Aboral2 = Aboral4
# Aboral3 = Aboral2
# Aboral4 = Aboral2
# Oral1 = Oral2
# Oral2 = Oral1
# Oral3 = Oral4
# Oral4 = Oral3


## 5. row correlations 
row_data<- read.csv('Mnemiopsis_count_data.csv', header=T, row.names = 1)
# See what is in the file
head(row_data)

# Clean up the data- rename the col headers based on the data in the col file 
names(row_data)<- c('Aboral1','Aboral2','Aboral3','Aboral4','Oral1', 'Oral2', 'Oral3', 'Oral4' )

y<- melt(row_data)
z<-cor(t(row_data))
z[1:10]

#get rid of the values 1 and < 0.5
z[z ==1]<- NA
z[abs(z)<0.5] <- NA
x<- na.omit(z)
x[1:10]


mat<- as.matrix(scaled_df)
row_cor<- cor(t(mat))
row_cor[1:100]
#order(row_cor)
row<-c(mat[2:16548,])
row_corr<- cor(t(row))

ordered<- order(row, decreasing= T)
ordered[1:5]


install.packages('qlcMatrix')

# 6. Divide the genes in each column into high medium and low count genes
library(qlcMatrix)
# first lets turn the data frame into a matrix
mat<- as.matrix(data[2:9])
head(mat)

# lets get the average of the
avg<- rowMeans(mat)
# print the summary
summary(avg)
# break the genes down by low, medium, and high
# since the highest value is 12,5638 but the means are all closer to 500, this an outlier
# so we will use low < 200, medium >200 < 750, high < 750
low<- which(avg>0, avg<100)
length(low)
medium <- which(avg>=200, avg<600)
length(medium)
high<- which(avg>= 600)
length(high)


col_mean <- apply(data[2:9], 2, mean)
summary(col_means)
summary(data)
col_mean

install.packages('gtools')
install.packages('tidyverse')

library(tidyverse)
library('gtools')
# 6. break the genes down by low, medium, and high
# since the highest value is 12,5638 but the means are all closer to 500, this an outlier
# so we will use low < 100, medium >200 < 650, high < 650
x<- quantcut(data$Aboral1, seq(0,1, by=.1))
plot(x)             
table(x)
# function to retrieve closest quantile for a given value.
get_quantile <- function(x)names(q)[which(abs(q-x)==min(abs(q-x)))]
# apply this function for all values in df$Amount_Spent
df_Quantile  <- sapply(data[,2:9], get_quantile)
df
y<- quantile(data$Aboral2)
table(y)
plot(y)

quantile(data$Aboral1)
quantile(data$Aboral2)
quantile(data$Aboral3)
quantile(data$Aboral4)
quantile(data$Oral1)
quantile(data$Oral2)
quantile(data$Oral3)
quantile(data$Oral4)

lowA1<- which(data$Aboral1 < 100) 
length(lowA1)
medA1<- which(data$Aboral1 >=100, data$Aboral1 < 650) 
length(medA1)
highA1<- which(data$Aboral1 >=650, data$Aboral1) 
length(highA1)
hist(highA1)

which.max(data$Aboral2) # 131,017
data[12714,] # ML20395a

which.max(data$Aboral3) # 136,282
data[12714,] # ML20395a 

which.max(data$Aboral4) # 111,860
data[1908,] # ML01482a

which.max(data$Oral1) # 163,380
data[12714,] # ML20395a 

which.max(data$Oral2) # 101,792
data[12714,] # ML20395a 

which.max(data$Oral3) # 101,421
data[12714,] # ML20395a 

which.max(data$Oral4) # 109,944
data[12714,] # ML20395a 


low<- which(avg>0, avg<100)
length(low)
medium <- which(avg>=200, avg<600)
length(medium)
high<- which(avg>= 600)
length(high)

install.packages('matrixStats')

# 7. Make a list of the top 5 genes with the most and least variablity 
library('matrixStats')
rs <- rowSums(data) #sums of genes
data['row sums'] = rs

good_data<- which(data$`row sums` > 5)
newdata<- data[good_data,]

# first get the std deviation for all rows
data_sd<- rowSds(as.matrix(newdata[,-1],1))
# add to the data frame
newdata['row sd'] = data_sd
high_variation<- order(data$`row sd`, decreasing =T) 
high_variation[1:5]
data[16420,]
data[1908,]
data[3788,]
data[3790,]
data[4015,]

low_variation<- order(newdata$`row sd`, decreasing = F)
low_variation[1:5]
newdata[5255,]
newdata[13588,]
newdata[2691,]
newdata[5909,]
newdata[6388,]



#----## DESeq2 analysis ##-----------#

library(energy)
library(dendextend)
library(pheatmap)
library(ggplot2)
library(reshape2)
library(plyr)
library(scales)
# read in the data file
count_data<- read.csv('Mnemiopsis_count_data.csv', header=T, row.names = 1)
# See what is in the file
head(count_data)
# Clean up the data- rename the col headers based on the data in the col file 
names(count_data)<- c('Aboral1','Aboral2','Aboral3','Aboral4','Oral1', 'Oral2', 'Oral3', 'Oral4' )
# get the means by row 
data_mean <- rowMeans(count_data[,-1], 1)
data_mean<- round(data_mean, 2)
# add the means to the dataframe 
count_data['row means'] = data_mean
head(count_data)

# 1. Build a heirarchical tree on cols , and rows (exclude low expressions)
# get the heirarchial clusters of genes based on all data - using pearson cor
# first get the subset of data 
data_subset<- as.data.frame(count_data[count_data$`row means`>12000,])
#head(data_subset)
dm<- as.dist((1-cor(t(data_subset), method = c('pearson')))/2) # plot by row
my_hclust_data<- hclust(dm, method = 'complete')
# plot the clusters
par(mar=c(5,5,5,12))
nPar<- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = 'blue')
ePar<- list(col = 2:3, lwd = 2:1)
# plot the heirarchial clusters by gene
plot(as.dendrogram(my_hclust_data), nodePar = nPar, edgePar = ePar, horiz = T)

# by cols
dm1<- as.dist((1-cor(data_subset, method = c('pearson')))/2) # plot by col
my_hclust_data<- hclust(dm1, method = 'complete')
#plot the clusters
par(mar=c(5,5,5,12))
nPar<- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = 'blue')
ePar<- list(col = 2:3, lwd = 2:1)
# plot the heirarchial clusters by player
plot(as.dendrogram(my_hclust_data), nodePar = nPar, edgePar = ePar, horiz = T)

# distance matrix based on euclidean distance
dm2<- dist(t(data_subset)) # get the distances
my_hclust_data<- hclust(dm2, method = 'complete') # cluster based on the distances
# plot the hierarachial clusters
par(mar=c(5,5,5,12))
nPar<- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = 'blue')
ePar<- list(col = 2:3, lwd = 2:1)
plot(as.dendrogram(my_hclust_data), nodePar = nPar, edgePar = ePar, horiz = T)

#get the subset 
data1_subset<- as.data.frame(data[data$`row means`>12000,])
# melt the data 
data_subset_m <- melt(data_subset)
# rescale the values
data_subset_m <- ddply(data_subset_m, .(variable), transform, rescale = rescale(value))
head(data_subset_m)

# plot
plot1<- ggplot(data_subset_m, aes(variable, Gene)) + 
  geom_tile(aes(fill = rescale), colour = 'white') + 
  scale_fill_gradient(low='white', high='dark blue')
plot1
# draw a heat map of the expression data
pheatmap(data_subset, cutree_rows = 10, cutree_cols = 5)

#select <- order(rowMeans(counts(dds,normalized=TRUE)),decreasing=TRUE)
#df <- as.data.frame(col(dds)[,c("condition","type")])
#pheatmap(assay(vsd)[select,], cluster_rows=FALSE, show_rownames=FALSE,
        # cluster_cols=FALSE, annotation_col=df)


## 3.	Use DESeq2 to analyse this data which are the most significantly changing genes in this dataset

library(calibrate)
library(DESeq2)
library(RColorBrewer)
library(gplots)
library(genefilter)
library(BiocManager)
install.packages('ashr')
library(ashr)

# read in the data file
count_data1<- read.csv('Mnemiopsis_count_data.csv', header=T, row.names = 1)
# See what is in the file
head(count_data1)
# Clean up the data- rename the col headers based on the data in the col file 
names(count_data1)<- c('Aboral1','Aboral2','Aboral3','Aboral4','Oral1', 'Oral2', 'Oral3', 'Oral4' )

# turn data into a matrix
countdata <- as.matrix(count_data1)
head(col)
# create a deseq object
deseq_obj <- DESeqDataSetFromMatrix(countData=countdata, colData=col, design=~condition)
# Run the DESeq pipeline
dds <- DESeq(deseq_obj)
resultsNames(dds) # lists the coefficients
results<- results(dds, name = 'condition_oral_vs_aboral')
summary(results)
res_sig<- subset(results, padj < 0.1)
res_sigre
res_ordered<- results[order(results$pvalue),]
res_ordered[1:10,] # top ten by lowest p-value (most different?)
log2_fold_ordered<- results[order(results$log2FoldChange, decreasing=T),]
log2_fold_ordered[1:10,]
# how many p-values are less than 0.05
sum(results$pvalue < 0.05, na.rm = T)
# or same thing..
results05<- results(dds, alpha=0.05)
summary(results05)
# or results to shrink log fold changes associated with condition
results1<- lfcShrink(dds, coef = 'condition_oral_vs_aboral', type='apeglm') 
# get results of the shrunken log2 fold change, which removes the noise associated with log2 fold changes from low count genes
resultsShrink<- lfcShrink(dds, coef = 2, type = 'ashr')
# LFC shrinkage plot 
plotMA(resultsNorm) # looking for large fold changes = more changes 

plotMA(results) #show the log2 fold changes attributable to a given variable over the mean for all the samples,  Points will be colored red if the adjusted p value is less than 0.1. Points which fall out of the window are plotted as open triangles pointing either up or down.
#After calling plotMA, use the function identify to interactively detect the row number of individual genes by clicking on the plot, then recover the gene identifiers by saving the resulting indices
#idx <- identify(results$baseMean, results$log2FoldChange)
#rownames(results)[idx]

plotCounts(dds, gene = which.min(results$padj), intgroup = 'condition')

# get the variance stabilized transformation data 
vsd<- vst(dds, blind = F)
#plot heatmap
sample_dists<- dist(t(assay(vsd)))
sample_dists_mx<- as.matrix(sample_dists)
rownames(sample_dists_mx)<- paste(vsd$condition, vsd$type, sep='-')
colnames(sample_dists)<- NULL
colors<- colorRampPalette(rev(brewer.pal(9,'Blues')) ) (255)
pheatmap(sample_dists_mx, clustering_distance_rows = sample_dists, clustering_distance_cols = sample_dists, col= colors)

# plot PCA
plotPCA(vsd, intgroup=c('condition', 'type'))

# check for any outliers
par(mar=c(8,5,2,2))
boxplot(log10(assays(dds)[["cooks"]]), range=0, las=2)

# dispersion plot - useful diagnostic
plotDispEsts(dds)
