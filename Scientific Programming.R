# Scientific Programming

#----------------------------------------------------------------------------
#Description
# Analysis of west nile virus data set that includes a variety of techniques for data manipulation in R
# Includes examples of modular arthimatic (5/13/19)


#----------------------------------------------------------------------------
#Setup
setwd('C:/Users/wat28052/Desktop/wnv')
wnv<-read.csv('wnv.csv')



#----------------------------------------------------------------------------
#West Nile Virus Script
#
library(ggplot2)

# Histogram of cases by State
ggplot(data=wnv, aes(x=reorder(wnv$Year, wnv$Total),y=wnv$Total)) + geom_histogram(stat="identity",position="stack") +facet_wrap(~wnv$State)


# Log Histogram Way #1

wnv$logtotal <- log(wnv$Total)

ggplot(data=wnv, aes(x=reorder(wnv$Year, wnv$logtotal),y=wnv$logtotal)) + geom_histogram(stat="identity",position="stack") +facet_wrap(~wnv$State)


# Log Histogram #2

x<- log(wnv$Total)

ggplot(data=wnv, aes(x=reorder(wnv$Year, x),y=x)) + geom_histogram(stat="identity",position="stack") +facet_wrap(~wnv$State)


# Raw Case Fatality Rate

wnv$CFR = wnv$Fatal/wnv$Total
ggplot(data=wnv, aes(x=reorder(wnv$Year, wnv$CFR),y=wnv$CFR)) + geom_histogram(stat="identity",position="stack") +facet_wrap(~wnv$State)

# Verification of Total as sum of other cases

sum(wnv$EncephMen+wnv$Fever+wnv$Other)==sum(wnv$Total)

# Modular Arthimatic
wnv$rounded<-wnv$Total - wnv$Total%%12
wnv$rounded
1-sum(wnv$rounded)/sum(wnv$Total)

# Overall Mean and Standard Error
mean(wnv$Fever/wnv$Total)
sd(wnv$Fever/wnv$Total)

#State Specific Rmean and Standard Error
attach(wnv)
calwnv <- wnv[ which(wnv$State==c('California')),]
colwnv <- wnv[ which(wnv$State==c('Colorado')),]
nywnv <- wnv[ which(wnv$State==c('New York')),]
detach(wnv) 

mean(calwnv$Fever/calwnv$Total)
sd(calwnv$Fever/calwnv$Total)
mean(colwnv$Fever/colwnv$Total)
sd(colwnv$Fever/colwnv$Total)
mean(nywnv$Fever/nywnv$Total)
sd(nywnv$Fever/nywnv$Total)

#ggplot2 to make a bargraph
mean <-c(mean(calwnv$Fever/calwnv$Total),mean(colwnv$Fever/colwnv$Total),mean(nywnv$Fever/nywnv$Total))
sd <- c(sd(calwnv$Fever/calwnv$Total),sd(colwnv$Fever/colwnv$Total),sd(nywnv$Fever/nywnv$Total))
df <- data.frame(State=c("California", "Colorado", "New York"),mean,sd)

ggplot(df, aes(x=State, y=Mean)) + 
                   geom_bar(stat="identity") +  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2,position=position_dodge(.9))
                
# Proper solution
ndr<-function(state='Colorado', years=1999:2007){
x<-wnv[wnv$State %in% state & wnv$Year %in% years,]
y<-data.frame(state = x$State, ndr = x$EncephMen/x$Total)
m<-aggregate(y$ndr, by=list(y$state), FUN= mean)
se<-aggregate(y$ndr, by=list(y$state), FUN= function(x) sd(x)/sqrt(length(x)) )
out<-merge(m, se, by='Group.1')
names(out)<-c('state','mean.ndr','sd.ndr')
return(out)
}

disease<- ndr(state=c("California", "Colorado", "New York"))
ggplot(disease,aes(x=state,y=mean.ndr,fill=state))+ geom_bar(stat='identity')