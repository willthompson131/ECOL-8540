#Project Management Script

#---------------------------------------------------------------------------------------------
#Description
#Simple script to make a graph from the mers data

#---------------------------------------------------------------------------------------------

# Packages necessary to make graphs
library("RColorBrewer", lib.loc="~/R/win-library/3.5")
library("ggplot2", lib.loc="~/R/win-library/3.5")
library(lubridate)
library(plotly)


#---------------------------------------------------------------------------------------------
# Import Data

mers<-read.csv('cases.csv')

#---------------------------------------------------------------------------------------------
#Cleaning & Formating Data

class(mers$onset)
mers$hospitalized[890]<-c('2015-02-20')
mers <-mers[-471,]
install.packages("lubridate")
mers$onset2<-ymd(mers$onset)
mers$hospitalized2<-ymd(mers$hospitalized)
class(mers$onset2)
day0<-min(na.omit(mers$onset2))
mers$epi.day<-as.numeric(mers$onset2-day0)


#---------------------------------------------------------------------------------------------
#Making a simple plot 


ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Making an Interactive plot

epi.curve <- ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplotly(epi.curve)