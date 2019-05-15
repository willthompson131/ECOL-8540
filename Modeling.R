#Modeling Handout ECOL 8540 (R Studio workshop)

#---------------------------------------------------------------------------------------
#Description

#Modeling tools from second day of the R workshop, including tools for building functions,
#grouping data, using purrr, isolating data within a nested dataset only functional to step 14


#---------------------------------------------------------------------------------------
# Load packages

library(tidyverse)
library(magrittr)
library(GGally)
library(purrr)

#---------------------------------------------------------------------------------------
# Imprort Data
setwd('C:/Users/wat28052/Desktop/Lyme')
lyme<-read_csv('lymeData.csv')

#---------------------------------------------------------------------------------------
#Task 2
#Visualization with ggpair

ggpairs(lyme,columns=c("prcp","avtemp","size","cases"))

#Task 3
lyme$log10size<-log10(lyme$size)
lyme$log10cases<-log10((lyme$cases)+1)
ggpairs(lyme,columns=c("prcp","avtemp","log10size","log10cases"))

#Task 4
set.seed(222)
sublyme <-sample_n(lyme, 100)

#Using ggplot to make a graph
Myplot<-ggplot(data=sublyme)+geom_point(aes(x=prcp, y=avtemp))
Myplot

#Task 5 Fit a linear fit line to subset of data

Myplot<-ggplot(data=sublyme)+geom_point(aes(x=prcp, y=avtemp))+geom_smooth(mapping=aes(x=prcp, y=avtemp),method=lm )
Myplot

#Task 6, Create a general linear model

myModel <- lm(avtemp ~ prcp, data = sublyme)
summary(myModel)

#Task 7
summary(myModel)
# Slope = 0.00672, p value< 0.05 (3.19e-06) indicated the model is significantly different from the slope. 

#Task 8
x <- lyme %>% group_by(year) %>% summarize(TotalPop = sum(size)) %>% ggplot(.) +geom_point(aes (year, TotalPop))
x

#Task 9: Group Data by state

bystate <- lyme %% group_by(state)

#Task 10: Nest the grouping of data within state
bystate <-bystate %<>% nest

Task 11

#Task 11, Georgia data
bystate$residuals[[10]]

#Task 12

linearmodelfunction<-function(df){
task12 <- lm(size~year,data=df)
return(task12)
}

linearmodelfunction(lyme)

# Task 13 

library(purrr)

bystate %<>% mutate(model= purrr::map(data, linearmodelfunction))



#Task 14, Adding residuals to the model
bystate %<>% mutate(residuals = purrr::map2(data,model,add_residuals))

#Task 15, Absolute Residuals

sum_resids <- function(x){
sum(abs(x$residuals))
}
bystate %<>% mutate(totalResids = purrr::map(residuals,sum_resids))
