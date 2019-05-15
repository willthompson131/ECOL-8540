# Data Wrangling ECOL 8540 (R Studio workshop)
#-----------------------------------------------------------------------------------------------------------------------------
# Description 
#
# Data Wrangling techniques from the R workshop. Includes examples of how to convert data into the tidy format, combine data sets, 
# saving data frames, as well as incorporating georgraphic data (5/14/19)

#-----------------------------------------------------------------------------------------------------------------------------

#Library
library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)

#-----------------------------------------------------------------------------------------------------------------------------
# Import data
ld<-read_csv('lyme.csv')
pop<-read_csv('pop.csv')
prism<-read_csv('climate.csv')

#-----------------------------------------------------------------------------------------------------------------------------
#Task 2: Assessing how population data does or does not conform to the tidy data format

#One issue is that some of the data is summarized; for example the state of Alabama's summary data is counted
# in the state and county name columns. 
head(pop)

#-----------------------------------------------------------------------------------------------------------------------------
#Task 3

#Selects the FIPS locality ID, converts it to a new object (pop2)
pop %<>% select(fips,starts_with("pop2"))
#Culls data in pop2 to only starting year, size
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit
#Adds "pop" to the begining of the year
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))
#Converts year into an integer? 
pop %<>% mutate(year=as.integer(year))
#Removes all starting years 0s 
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))
# Creates an interger vector of specificed length fips
pop %<>% mutate(fips=as.integer(fips))

#-----------------------------------------------------------------------------------------------------------------------------
#Task 4: Correctly formating FIPS code data 

ld %<>% gather(starts_with("Cases"),key="str_year",value="cases")
ld %<>% mutate(year=str_replace_all(str_year, "Cases",""))
ld %<>% mutate(year=as.integer(year))
ld %<>% rename(state=STNAME,country=CTYNAME)

fips.builder<-function(st,ct){
  if(str_length(ct)==3){
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
}

ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE))
ld %<>% select(-c(STCODE,CTYCODE,str_year))

#Task 5: Combining the lyme disease data with climate datasets 
lyme.climate<-inner_join(ld,prism,by=c('year','fips'))

# Task 6: Combining outputs from task 6 with population data 
lyme.climate.pop<-inner_join(lyme.climate,pop,by=c('fips','year'))

# Task 7: How many cases of lyme per year
cases_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))

##The average number of cases in each state - averaged across county and
##year
lymeAvgState <- lyme.climate.pop %>% group_by(state) %>% summarize(avgLyme=mean(cases)) %>% print

#how many cases of lyme per year
cases_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))

##The average number of cases in each state - averaged across county and
##year
lymeAvgState <- lyme.climate.pop %>% group_by(state) %>% summarize(avgLyme=mean(cases)) %>% print

#What was the worst year? Which three states have been most impacted on average?
#Worst year is 2009
#3 states are Connecticut, Massachusetts, Delware

####Task 8: use save to create an Rda file of the data frame and use write_csv to create a csv file of the same
save(lyme.climate.pop, file='LymeData.Rda')
write_csv(lyme.climate.pop, path='lymeData.csv')


# Task 9: Make that map

county_map <- map_data("county")
state_map <- map_data("state")
ag.fips <- group_by(lyme.climate.pop,fips)
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
ld.16y<-left_join(select(lyme.climate.pop,c(state,county,fips)),ld.16y)
ld.16y<-distinct(ld.16y)
ld.16y %<>% rename(region=state,subregion=county)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
ld.16y$region<-tolower(ld.16y$region)
ld.16y$subregion<-tolower(ld.16y$subregion)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," parish","")
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
map.ld.16y<-left_join(county_map,ld.16y)
ggplot(map.ld.16y)+geom_polygon(aes(long,lat,group=group,fill=log10cases),color="gray",lwd=0.2) +
  scale_fill_gradientn(colours=rev(heat.colors(10)))

