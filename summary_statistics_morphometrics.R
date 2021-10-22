## Written by Nelson Buainain, 2020
## nnbuainain@gmail.com
## 

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

- Produce summary statistics based on groups
"

###										
###
###		Summary Statistics / Descriptive 
###
###

#### Calculate summary statistics in a fast and efficient way with dplyr

## group_by vai ser os subgrupos no exemplo mostrado ele vai agrupar por subspecies e pra cada subspecie vai computar as estatisticas para machos e femeas separadamente
## depois vem o nome que voce quer, seguido de = e a estatistica que voce quer que ele faça, abre parenteses a coluna da variável e caso deseje desconsiderar os missing dat insira , na.rm =TRUE
#Só não consegui ainda evitar que não seja criada uma linha com os individuos que tem N.A. para a categoria escolhida

################### Morphometric Analyses ########################

library("dplyr")
library('tidyr')

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
# Delete blank occurences in category you want to group by with

# An example grouped by category genomics2 for males and females separately
# To do with other categories just replace genomics2 by the category you would
# like to 

data_clean <- with(data_clean, data_clean[!(long == "" | lat == "" | sex == "" | age == "J" | genomics2 == ''), ])
nrow(data_clean)

summary_stats_genomics <- 
  data_clean %>%
  drop_na(genomics2) %>% 
  group_by(genomics2,sex, na.rm=TRUE,) %>%
  summarise(n = n(),
            bill.length_mean = mean(bill.length, na.rm = TRUE),
            bill.length_sd = sd(bill.length, na.rm = TRUE),
            bill.length_min = min(bill.length, na.rm = TRUE),
            bill.length_max = max(bill.length, na.rm = TRUE),

            bill.height_mean = mean(bill.height, na.rm = TRUE),
            bill.height_sd = sd(bill.height, na.rm = TRUE),
            bill.height_min = min(bill.height, na.rm = TRUE),
            bill.height_max = max(bill.height, na.rm = TRUE),
            
            bill.width_mean = mean(bill.width, na.rm = TRUE),
            bill.width_sd = sd(bill.width, na.rm = TRUE),
            bill.width_min = min(bill.width, na.rm = TRUE),
            bill.width_max = max(bill.width, na.rm = TRUE),
            
            wing.length_mean = mean(wing.length, na.rm = TRUE),
            wing.length_sd = sd(wing.length, na.rm = TRUE),
            wing.length_min = min(wing.length, na.rm = TRUE),
            wing.length_max = max(wing.length, na.rm = TRUE),
            
            tail.length_mean = mean(tail.length, na.rm = TRUE),
            tail.length_sd = sd(tail.length, na.rm = TRUE),
            tail.length_min = min(tail.length, na.rm = TRUE),
            tail.length_max = max(tail.length, na.rm = TRUE))
            
summary_stats_genomics

# Save Results
write.csv(summary_stats_genomics,paste("results/sum_stats_genom2.csv"),row.names=FALSE)

