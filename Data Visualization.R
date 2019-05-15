# ECOL 8540 (R Studio workshop)

#-----------------------------------------------------------------------------------------------------------------------------
#Description

#Simple Scripts to practice data visualization for the first day of UGA's R workshop. Primarily consists of ggplot scripts 
#illustrating different ways to visualize data using ggplot2 including univariate plots(a variety of histograms) and bivariate plots
#(includes point plots, as well as how to make faceted figures)

#-----------------------------------------------------------------------------------------------------------------------------
# Packages
library(lubridate)
library("ggplot2", lib.loc="~/R/win-library/3.5")

#-----------------------------------------------------------------------------------------------------------------------------
#Setup and data cleanup

setwd('C:/Users/wat28052/Desktop/mers')
mers<-read.csv('cases.csv')
head(mers)
class(mers$onset)
mers$hospitalized[890]<-c('2015-02-20')
mers <-mers[-471,]
install.packages("lubridate")
mers$onset2<-ymd(mers$onset)
mers$hospitalized2<-ymd(mers$hospitalized)
class(mers$onset2)
day0<-min(na.omit(mers$onset2))
mers$epi.day<-as.numeric(mers$onset2-day0)


#-----------------------------------------------------------------------------------------------------------------------------
#ggplot, note mapping function in second ggplot figure

ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=gender)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=gender)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") +
  coord_flip()

ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=gender)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") +
  coord_polar()


#-----------------------------------------------------------------------------------------------------------------------------
# Univariate plots

mers$infectious.period <- mers$hospitalized2-mers$onset2 # calculate "raw" infectious period
class(mers$infectious.period) # these data are class "difftime"
mers$infectious.period <- as.numeric(mers$infectious.period, units = "days") # convert to days
ggplot(data=mers) +
geom_histogram(aes(x=infectious.period)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

mers$infectious.period2 <- ifelse(mers$infectious.period<0,0,mers$infectious.period)

ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")


#Probability Density
ggplot(data=mers) +
  geom_density(mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Probability density for MERS infectious period (positive values only)', caption="Data from:")

# Area Plot
ggplot(data=mers) +
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")


#Area Plot but with dots

ggplot(data=mers) +
  geom_dotplot(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Area Plot but with binned bar graph

ggplot(data=mers) +
  geom_bar(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")


#-----------------------------------------------------------------------------------------------------------------------------
# Bivariate plots

#My old janky way of making a plot
plot(mers$infectious.period2~mers$onset2, ylab='Length of Infection',xlab='Date of Onset', main="Length of Infection over the course of the Mers Epidemic", sub='Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv')


#THe better way to make a similar plot 

ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2))  +
  geom_point()
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period (positive values only) over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Adding a Trend Curve

  ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2))  +
    geom_point() + geom_smooth(mapping = aes(color=country),method="loess") +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period (positive values only) over time', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
  
#Faceting (Creating multiple graphs from the same dataset)
  
  ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
    geom_point(mapping = aes(color=country)) +
    facet_wrap(~ country) +
    scale_y_continuous(limits = c(0, 50)) +
    labs(x='Epidemic day', y='Infectious period',
         title='MERS infectious period (positive values only) over time',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

  
  # Country by Sex
  ggplot(data=subset(mers, gender %in% c('M', 'F') &
                       country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea','UAE')),
         mapping=aes(x=epi.day, y=infectious.period2)) +
    geom_point(mapping = aes(color=country)) +
    facet_grid(gender ~ country) +
    scale_y_continuous(limits = c(0, 50)) +
    labs(x='Epidemic day', y='Infectious period',
         title='MERS infectious period by gender and country',
         caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
  
  #Fatality Rate
  
  attach(mers)
  newmers <- mers[ which(outcome==c('fatal','recovered')),]
  detach(mers) 
  
  ggplot(data=subset(newmers, outcome %in% c('fatal', 'recovered') &
                       +                        country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea','UAE'),
                     +          mapping=aes(x=epi.day, y=c(count('fatal')/count('fatal'+'recovered'))))
                     

  