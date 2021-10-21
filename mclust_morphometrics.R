## Written by Nelson Buainain, 2020
## nnbuainain@gmail.com
## 

### MCLUST FILE

"
This Script was used to process and analyze the morphometric data used in
Buainain, N., Maximiano, M. F. A., Ferreira, M., Aleixo, A., Faircloth, B. C., Brumfield, R. T., . . . Ribas, C. C. (2021). Multiple species and deep genomic divergences despite little phenotypic differentiation in an ancient Neotropical songbird, Tunchiornis ochraceiceps (Sclater, 1860) (Aves: Vireonidae). Molecular Phylogenetics and Evolution, 162, 107206. https://doi.org/10.1016/j.ympev.2021.107206
publication accessible at: https://www.sciencedirect.com/science/article/abs/pii/S1055790321001391?via%3Dihub

The purpose is to 

- Process the data, eliminating outliers, inputing missing data, deleting
samples with too much missing information, etc..

- Find out how many groups there are using Principal Component Analysis and
Machine Learning algorithms such as mcluster

- Plot boxplot graphs grouping sampels by different category
"

################### Morphometric Analyses ########################

library("dplyr")
library("mclust")
library('RColorBrewer')


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
data_clean <- with(data_clean, data_clean[!(long == "" | lat == "" | sex == "" | age == "J"), ])
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

## Mcluster is sensitive to outliers, if you would like to filter
# if you would like to remove them execute the lines below.
# In this case, it did not make a difference.

# data_clean_mean = 
#   data_clean_mean %>% filter(!bill.length %in% boxplot.stats(data_clean_mean$bill.length)$out) %>% 
#   filter(!bill.width %in% boxplot.stats(data_clean_mean$bill.width)$out) %>% 
#   filter(!bill.height %in% boxplot.stats(data_clean_mean$bill.height)$out) %>% 
#   filter(!wing.length %in% boxplot.stats(data_clean_mean$wing.length)$out) %>% 
#   filter(!tail.length %in% boxplot.stats(data_clean_mean$tail.length)$out)

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

##################							  ##################
##################			Mclust	  ##################
##################							  ##################

______________________

# Get only the morphometric variables that will be used in the model

clean_mean_var <- data_clean_mean[,13:17];clean_mean_var
cons_var <- data_clean_cons[,13:17];cons_var

# Get labels
# mean filled model
genomics_mean <- data_clean_mean$genomics2
subspecies_mean <- data_clean_mean$subspecies
sex_mean <- data_clean_mean$sex

# conservative model
genomics_cons <- data_clean_cons$genomics2
subspecies_cons <- data_clean_cons$subspecies
sex_cons <- data_clean_cons$sex

#Fazer os plots, lembre-se aqui que cada matriz dessa idealmente deve ser filtrada
#de acordo com o que se planeja testar, para que não se descarte dados a toa
#dessa maneira caso vá testar tudo ao mesmo tempo crie um objeto pra cada hipotese

clPairs(cons_var,genomics_cons,colors = brewer.pal(n = 11, name = "RdYlBu"))
clPairs(cons_var,subspecies_cons,colors = brewer.pal(n = 11, name = "RdYlBu"))
clPairs(cons_var,sex_cons)

clPairs(clean_mean_var,genomics_mean,colors = brewer.pal(n = 11, name = "RdYlBu"))
clPairs(clean_mean_var,subspecies_mean,colors = brewer.pal(n = 11, name = "RdYlBu"))
clPairs(clean_mean_var,sex_mean)

# It seems that there is not much trend for grouping in the data

#_______________________

########   MCLUST with Mean filled model

## Run model selection

mod<-mclustBIC(clean_mean_var,control=emControl())

## Show summary result
summary(mod)

## plot graphs

plot(mod)

## run mclust with best model
res <- Mclust(clean_mean_var, x = mod)
summary(res,parameters = TRUE)
plot(res)

## See if mclust classification matches any of out expectation

table(data_clean_mean$genomics, res$classification)
table(data_clean_mean$subspecies, res$classification)
table(data_clean_mean$sex, res$classification)

# Classification looks random regarding to our expectations

#__________________________

########   MCLUST with Conservative model

## Run model selection

mod<-mclustBIC(cons_var,control=emControl())

## Show summary result
summary(mod)

## plot graphs

plot(mod)

## run mclust with best model
res <- Mclust(cons_var, x = mod)
summary(res,parameters = TRUE)
plot(res)

## See if mclust classification matches any of out expectation

table(data_clean_cons$genomics, res$classification)
table(data_clean_cons$subspecies, res$classification)
table(data_clean_cons$sex, res$classification)

# Classification looks random regarding to our expectations
