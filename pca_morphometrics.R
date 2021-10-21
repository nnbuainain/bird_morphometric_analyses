## Written by Nelson Buainain, 2020
## nnbuainain@gmail.com
## 

### PCA FILE

"
This Script was used to process and analyze the morphometric data used in
Buainain, N., Maximiano, M. F. A., Ferreira, M., Aleixo, A., Faircloth, B. C., Brumfield, R. T., . . . Ribas, C. C. (2021). Multiple species and deep genomic divergences despite little phenotypic differentiation in an ancient Neotropical songbird, Tunchiornis ochraceiceps (Sclater, 1860) (Aves: Vireonidae). Molecular Phylogenetics and Evolution, 162, 107206. https://doi.org/10.1016/j.ympev.2021.107206
publication accessible at: https://www.sciencedirect.com/science/article/abs/pii/S1055790321001391?via%3Dihub

The purpose is to 

- Process the data, eliminating outliers, inputing missing data, deleting
samples with too much missing information, etc..

- Find out how many groups there are using Principal Component Analysis and
Machine Learning algorithms such as mcluster

- Plot boxplot graphs grouping samples by different category
"

################### Morphometric Analyses ########################

library("dplyr")
library("ggplot2")
library('RColorBrewer')
library('vegan')

################### DATA PREPROCESSING ########################

## Define a set of configuration to plot a very clean graph in ggplot2
## Simply add it as an argument when using ggplot2

cleanup = theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line=element_line(color="black"))

## Create a colorblind friendly color palette to use in graphs

dalt <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#000000")


# Load dataset

data<-read.csv("data/tunc_morph.csv",sep=";",dec=",",head=T)


### Cleaning the database
" The database contains several columns that are not essential for our purpose here,
lots of samples with missing data for crucial columns such as sex, etc..."

# Select only variables that matter

data

colnames(data)

# Delete columns with locality names, ossification information 
#and total length

data_clean<-data[,-c(6:8,13,14)] 

colnames(data_clean)

# Counting row number before start cleaning process to see how much we "loose"
nrow(data_clean) # 625 samples

########## Filter missing data and outliers

# Delete samples with no information on geography and sex, and also immature specimens
# CAUTION - remember to change genomics2 by the label you want to use 
# to color the PCA graph with 

data_clean <- with(data_clean, data_clean[!(long == "" | lat == "" | sex == "" | age == "J" | genomics2 == ""), ])
nrow(data_clean)

##########Filtrando os dados com missing data

# I will experiment with two datasets
# 1) a conservative approach where no NA is allowed
# 2) an inclusive approach filling the NA with the mean value of each column

# Aproach 1 - conservative
data_clean_cons<-data_clean[complete.cases(data_clean[11:15]),]
nrow(data_clean_cons)

data_clean_cons <- cbind(data_clean_cons[,-c(11:15)],
                         c(decostand(data_clean_cons[11:15],
                                     method="standardize",MARGIN=2)))

# Aproach 2 - mean filled values
## Delete samples with missing data for more than 3 variables
data_clean_mean<-data_clean[rowSums(is.na(data_clean[11:15])) < 3L,]
nrow(data_clean_mean)

## Some analyses are sensitive to outliers, if you would like to filter
# if you would like to remove them execute the lines below.
# In this case, it did not make a difference.

data_clean_mean =
  data_clean_mean %>% filter(!bill.length %in% boxplot.stats(data_clean_mean$bill.length)$out) %>%
  filter(!bill.width %in% boxplot.stats(data_clean_mean$bill.width)$out) %>%
  filter(!bill.height %in% boxplot.stats(data_clean_mean$bill.height)$out) %>%
  filter(!wing.length %in% boxplot.stats(data_clean_mean$wing.length)$out) %>%
  filter(!tail.length %in% boxplot.stats(data_clean_mean$tail.length)$out)

## Replacing NA by mean values
data_clean_mean$bill.length[which(is.na(data_clean_mean$bill.length))]<-mean(data_clean_mean$bill.length,na.rm=TRUE)
data_clean_mean$bill.height[which(is.na(data_clean_mean$bill.height))]<-mean(data_clean_mean$bill.height,na.rm=TRUE)
data_clean_mean$bill.width[which(is.na(data_clean_mean$bill.width))]<-mean(data_clean_mean$bill.width,na.rm=TRUE)
data_clean_mean$wing.length[which(is.na(data_clean_mean$wing.length))]<-mean(data_clean_mean$wing.length,na.rm=TRUE)
data_clean_mean$tail.length[which(is.na(data_clean_mean$tail.length))]<-mean(data_clean_mean$tail.length,na.rm=TRUE)

# Standardize variable to reduce difference in magnitude among them
data_clean_mean <- cbind(data_clean_mean[,-c(11:15)],
                         c(decostand(data_clean_mean[11:15],
                                     method="standardize",MARGIN=2)))

##################							##################
##################			PCA				##################
##################							##################

######## Mean Filled dataset

pca<-prcomp(data_clean_mean[,13:17],scale=TRUE)
pca

# Proportion of variance explained by each PC
summary(pca)

# Contribution of each variable to the PCs variance
loadings<-pca$rotation
loadings

# Biplot, this helps visualizing how each variable is contributing
# to the variation in the two axis

biplot(pca,scale=0)

# PC values for each sample
pca$x

data_clean_mean<-cbind(data_clean_mean,pca$x[,1:2])

############ PLOTING THE GRAPHS

pca.graph<-ggplot(data_clean_mean,
                  aes(x=PC1,y=PC2,fill=genomics2))+
  cleanup+geom_point(shape=21,colour="black",alpha=0.8,cex=2)+
  labs(x="PC1 (55%)",y=("PC2 (34%)"),
       title=("Vocal variables PCA"))+
  stat_ellipse(aes(x=PC1,y=PC2,color=genomics2,lty=factor(genomics2)),show.legend = FALSE)+
  scale_fill_manual(values=c("black","#F0E442","#E69F00","#D55E00","#0072B2","#CC79A7","white","#999999","#009E73"))+
  scale_color_manual(values=c("black","#F0E442","#E69F00","#D55E00","#0072B2","#CC79A7","black","#999999","#009E73"))+
  scale_linetype_manual(values=c(1,1,1,1,1,1,2,1,1))
pca.graph

## No trend for morphometric differentiation can be observed
## If you like to try to color point with other categories just 
## adjust the ggplot function and ideally the data filtering in 
## line 73 to make sure there is no samples with a blank value

# A simpler version easier to adapt to other variables:

ggplot(data_clean_mean,aes(x=PC1,y=PC2,color=plumage.group))+
  cleanup+geom_point(size=2)+
  scale_color_brewer(palette = 'Dark2')+
  labs(x="PC1 (28.9%)",y=("PC2 (24.3%)"),
  title=("Morphometrics - Plumage group"))+
  stat_ellipse()
