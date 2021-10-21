# bird_morphometric_analyses
>Analyses to differentiate birds species of the Tunchiornis genus with statistics and ML (clustering) 


** *Note that I'm currently working on this, I'll upload the script as I finish to clean them* **


In this project I try to explore the possible variations in measurements of birds such as measurements of bill, wing and tail to see if there was any difference between closest related species.

<p align="center"> <img src=morphometric_plot.jpg width="500" alt="bird_pic" class="center"></p>

## Goals

* Process raw morphometric data, deleting records with too much missing data, inputing data when possible, excluding outliers, and others.
* Test the number of morphometric groups involved
* See if these groups are congruent with bird taxa, genetic groups, geographic regions, etc...
* Test if variation is related to geographic distance (Isolation by distance, a correlation between geographic and morphologic distances)
* Generate boxplots and descriptive statistics for the differents bird groups

## Tools

I do that using R language and the following packages:
* *Vegan*
* *mcluster*
* *dplyr*
* *ggplot2*
* *other graphic packages * 

## Who is this for?

If you work with taxonomy and are trying to differentiate groups of organisms using any continuous variables this will be good for you.
However, the data processing and basic statistics used here could be used to any problem that tries to clean data and differentiate two or more groups.

## Source

Most of the codes and the data that I used for this project is published [here](https://www.sciencedirect.com/science/article/abs/pii/S1055790321001391?via%3Dihub).

Buainain, N., Maximiano, M. F. A., Ferreira, M., Aleixo, A., Faircloth, B. C., Brumfield, R. T., . . . Ribas, C. C. (2021). Multiple species and deep genomic divergences despite little phenotypic differentiation in an ancient Neotropical songbird, Tunchiornis ochraceiceps (Sclater, 1860) (Aves: Vireonidae). Molecular Phylogenetics and Evolution, 162, 107206. https://doi.org/10.1016/j.ympev.2021.107206
