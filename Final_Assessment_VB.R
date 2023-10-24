# load the dataset
library(tidyverse)
library(dslabs)
library(ggplot2)
library(caret)
library(reshape2)
library(gridExtra)


setwd("C:/Users/vladi/OneDrive/Documents/R/Final_Assessment/Option 2")
dir()

bank_score = read.csv("bank.csv", sep=';')
head(bank_score)

############################
#PART 1: DATA ANALYSIS
############################
str(bank_score)
summary(bank_score)


for (j in 1:length(var_cat)) {
  cat("________________________ \n")
  cat(var_cat[j],"\n")
  print(table(bank_score[,var_cat[j]]))
}


#Empty columns
Empty <- for (j in 1:ncol(bank_score)) {
  sum(is.na(bank_score)[j])
}
Empty

sum(is.na(bank_score$age))
sum(is.na(bank_score$job))
sum(is.na(bank_score$marital))
sum(is.na(bank_score$education))
sum(is.na(bank_score$default))
sum(is.na(bank_score$balance))
sum(is.na(bank_score$housing))
sum(is.na(bank_score$loan))
sum(is.na(bank_score$contact))
sum(is.na(bank_score$day))
sum(is.na(bank_score$month))
sum(is.na(bank_score$duration))
sum(is.na(bank_score$campaign))
sum(is.na(bank_score$pdays))
sum(is.na(bank_score$previous))
sum(is.na(bank_score$poutcome))
sum(is.na(bank_score$y))


#This tells us the number of unique value??
for (j in 1:ncol(bank_score)) {
  cat(names(bank_score)[j], "\t", length(unique(bank_score[,j])),"\n")
}


#bank_score%>% lm(y ~ age, data = .)

#Linear regression, loess, knn
bank_score$y

# categorical
# check job type
Graph1 <- bank_score %>%
  group_by(job) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = job, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by Job Type",
    x = "Job",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# check of distribution of yes/no by job type

unique(bank_score$job)
m = table(bank_score$job, bank_score$y)
m
n = round(100*prop.table(m,2),2)
n
t(n) # column percentages

n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
job <- row.names(n)
job

df1 <- data.frame(no, yes, job)
df2 <- melt(df1, id.vars='job')
head(df2)

# Define custom colors for 'No' and 'Yes'
colors <- c("No" = "lightyellow", "Yes" = "lightblue")

Graph2 <-ggplot(df2, aes(x=job, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y distribution (in percent)", x = "Job type")+
  labs(fill = "titre legend") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90))
Graph2

grid.arrange(Graph1, Graph2, ncol = 2)
## blue colar significant more no and management, retired more yes, VARIABLE JOB KEPT

# check marital
Graph1 <- bank_score %>%
  group_by(marital) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = marital, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by marital Type",
    x = "marital",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# We see much lower rate for married

# check of distribution of yes/no by marriage
unique(bank_score$marital)

m = table(bank_score$marital, bank_score$y)
m
n = round(100*prop.table(m,2),2)
n
t(n) # column percentages

n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
marital <- row.names(n)
marital

df1 <- data.frame(no, yes, marital)
df2 <- melt(df1, id.vars='marital')
head(df2)

Graph2 <- ggplot(df2, aes(x=marital, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y distribution (in percent)", x = "x axis name")+
  labs(fill = "titre legend") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90))
grid.arrange(Graph1, Graph2, ncol = 2)
# More no for married, this can be kept VARIABLE MARITAL KEPT

# check education 
Graph1 <- bank_score %>%
  group_by(education) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = education, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by education Type",
    x = "education",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Graph1

# check of distribution of yes/no by education
unique(bank_score$education)

m = table(bank_score$education, bank_score$y)
m
n = round(100*prop.table(m,2),2)
n
t(n) # column percentages

n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
education <- row.names(n)
education

df1 <- data.frame(no, yes, education)
df2 <- melt(df1, id.vars='education')
head(df2)

Graph2 <-  ggplot(df2, aes(x=education, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y distribution (in percent)", x = "education")+
  labs(fill = "titre legend") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90))
grid.arrange(Graph1, Graph2, ncol = 2)

# tertiary seems confirmed as an indicator of yes, VARIABLE EDUCATION KEPT

# check default   
Graph1 <- bank_score %>%
  group_by(default) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = default, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by default  Type",
    x = "default",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Graph1

# check of distribution of yes/no by default
unique(bank_score$default)

m = table(bank_score$default, bank_score$y)
m
n = round(100*prop.table(m,2),2)
n
t(n) # column percentages

n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
default <- row.names(n)
default

df1 <- data.frame(no, yes, default)
df2 <- melt(df1, id.vars='default')
head(df2)

Graph2 <-  ggplot(df2, aes(x=default, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y repartition (in percent)", x = "Default")+
  labs(fill = "titre legend") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90))  
Graph2

grid.arrange(Graph1, Graph2, ncol = 2)

# check housing 
Graph1 <-bank_score %>%
  group_by(housing) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = housing, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by housing Type",
    x = "housing",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Graph1

# check of distribution of yes/no by housing type
m = table(bank_score$housing, bank_score$y)
n = round(100*prop.table(m,2),2)
t(n) # column percentages
n
n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
housing <- row.names(n)
housing

df1 <- data.frame(no, yes, housing)
df2 <- melt(df1, id.vars='housing')
head(df2)

Graph2 <- ggplot(df2, aes(x=housing, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y axis name (in percent)", x = "Housing")+
  labs(fill = "titre legend") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90)) 
Graph2
grid.arrange(Graph1, Graph2, ncol = 2)
# No housing is confirmed to be a good indicator of yes VARIABLE HOUSING KEPT

# check loan 
Graph1<- bank_score %>%
  group_by(loan) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = loan, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by loan Type",
    x = "loan",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# check of distribution of yes/no by loan type
m = table(bank_score$loan, bank_score$y)
n = round(100*prop.table(m,2),2)
t(n) # column percentages
n
n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
loan <- row.names(n)
loan

df1 <- data.frame(no, yes, loan)
df2 <- melt(df1, id.vars='loan')
head(df2)

Graph2 <- ggplot(df2, aes(x=loan, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y distribution (in percent)", x = "loan")+
  labs(fill = "titre legend")+
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90)) 
grid.arrange(Graph1, Graph2, ncol = 2)

# While we see most of loans are in no, there is still a difference between yes and no repartition so variable LOAN IS KEPT


# check contact 
Graph1 <- bank_score %>%
  group_by(contact) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = contact, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by contact Type",
    x = "contact",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Graph1

# check of distribution of yes/no by loan type
m = table(bank_score$contact, bank_score$y)
n = round(100*prop.table(m,2),2)
t(n) # column percentages
n
n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
contact <- row.names(n)
contact

df1 <- data.frame(no, yes, contact)
df2 <- melt(df1, id.vars='contact')
head(df2)

Graph2 <- ggplot(df2, aes(x=contact, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y distribution (in percent)", x = "contact")+
  labs(fill = "titre legend")+
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90)) 
Graph2
grid.arrange(Graph1, Graph2, ncol = 2)
# Cellular associated to yes, variable CONTACT KEPT

#check month 
#Check share of yes/no per month
Graph1 <- bank_score %>%
  group_by(month) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = month, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by month Type",
    x = "month",
    y = "month 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Graph1

# Analysis of distribution of yes and no across the months
m = table(bank_score$month, bank_score$y)
n = round(100*prop.table(m,2),2)
t(n) # column percentages
n
n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
month <- row.names(n)
month

df1 <- data.frame(no, yes, month)
df2 <- melt(df1, id.vars='month')
head(df2)

Graph2 <- ggplot(df2, aes(x=month, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y axis name (in percent)", x = "x axis name")+
  labs(fill = "titre legend") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90)) 
grid.arrange(Graph1, Graph2, ncol = 2)
# Strong difference between months VARIABLE MONTH KEPT

# check poutcome 
Graph1 <- bank_score %>%
  group_by(poutcome) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = poutcome, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by poutcome Type",
    x = "poutcome",
    y = "poutcome 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Graph1

# Analysis of distribution of yes and no across poutcome
m = table(bank_score$poutcome, bank_score$y)
n = round(100*prop.table(m,2),2)
t(n) # column percentages
n
n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
poutcome <- row.names(n)
poutcome

df1 <- data.frame(no, yes, poutcome)
df2 <- melt(df1, id.vars='poutcome')
head(df2)

Graph2 <- ggplot(df2, aes(x=poutcome, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y repartition (in percent)", x = "poutcome")+
  labs(fill = "titre legend") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90)) 
Graph2
grid.arrange(Graph1, Graph2, ncol = 2)
##unknown seems a predictor of no, while success a predictor of yes VARIABLE POUTCOME IS KEPT


# VARIABLE MORE CONTINUE

# check age 
Graph1 <- bank_score %>%
  group_by(age) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = age, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by age",
    x = "age",
    y = "age 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Analysis of distribution of yes and no across age
m = table(bank_score$age, bank_score$y)
n = round(100*prop.table(m,2),2)
t(n) # column percentages
n
n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
age <- row.names(n)
age

df1 <- data.frame(no, yes, age)
df2 <- melt(df1, id.vars='age')
head(df2)

Graph2 <- ggplot(df2, aes(x=age, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y repartition (in percent)", x = "age")+
  labs(fill = "y") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90)) 
Graph2
##HOW TO MAKE IT MORE LEDGIBLE
grid.arrange(Graph1, Graph2, ncol = 2)
## It seems there are more no around 30, VARIABLE AGE KEPT


# CHECK BALANCE 
# Given the spread of the continuous value, the boxplot analysis makes more sense here
# Analysis of distribution of yes and no across balance
m = table(bank_score$balance, bank_score$y)
n = round(100*prop.table(m,2),2)
t(n) # column percentages
n
n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
balance <- row.names(n)
balance

df1 <- data.frame(no, yes, balance)
df2 <- melt(df1, id.vars='balance')
head(df2)
class(df2$balance)
df2$balance <- as.numeric(as.character(df2$balance))
class(df2$balance)

ggplot(df2, aes(x = variable, y = balance, fill = value)) +
  geom_boxplot() +
  labs(y = "Balance", x = "Y") +
  scale_fill_manual(values = c("#CDC8B1", "#9FB6CD")) +
  theme(axis.text.x = element_text(angle = 90))
###Most data concentrated between 0 and less that 5,000, many outliers, no special difference between Yes and No observed. Variable balance not kept.
## WHY ARE YES AND NO THE SAME? HOW TO DEAL WITH OUTLIERS?
## no trend appears, VARIABLE BALANCE NOT KEPT

# check day 
#Graph1 <- bank_score %>%
#  group_by(day) %>%
#  summarize(avg = mean(y == "yes")) %>%
#  ggplot(aes(x = day, y = avg)) +
#  geom_point(stat = "identity", fill = "blue") +
#  labs(
#    title = "Average 'Yes' Responses by day",
#    x = "day",
#    y = "Average 'Yes' Responses"
#  ) +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#  geom_smooth()
Graph1 <- bank_score %>%
  group_by(day) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = day, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by day",
    x = "day",
    y = "day 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Graph1
# Analysis of distribution of yes and no across balance
m = table(bank_score$day, bank_score$y)
n = round(100*prop.table(m,2),2)
t(n) # column percentages
n
n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
day <- row.names(n)
day

df1 <- data.frame(no, yes, day)
df2 <- melt(df1, id.vars='day')
head(df2)
class(df2$day)
df2$day <- as.numeric(as.character(df2$day))
class(df2$day)

Graph2<- ggplot(df2, aes(x= day, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y repartition (in percent)", x = "day")+
  labs(fill = "y") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90)) 
Graph2

grid.arrange(Graph1, Graph2, ncol = 2)  
## it seems that from day 5 to 10, more no are present, We will test the model with and without the variable Day

# check duration  
Graph1 <- bank_score %>%
  group_by(duration) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = duration, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by day",
    x = "day",
    y = "day 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Graph1
# Analysis of distribution of yes and no across duration
m = table(bank_score$duration, bank_score$y)
n = round(100*prop.table(m,2),2)
t(n) # column percentages
n
n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
duration <- row.names(n)
duration

df1 <- data.frame(no, yes, duration)
df2 <- melt(df1, id.vars='duration')
head(df2)
class(df2$duration)
df2$duration <- as.numeric(as.character(df2$duration))
class(df2$duration)

Graph2<- ggplot(df2, aes(x= duration, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y repartition (in percent)", x = "duration")+
  labs(fill = "y") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90)) 

#fROM GRAPH 2, we can see the y=no are moslty on duration below 220, however the boxplot shows similar distribution, I am not sure why. Generally, it seems the boxplot always give me the same distribution
Graph2 <- ggplot(df2, aes(x = variable, y = duration, fill = value)) +
  geom_boxplot() +
  labs(y = "duration", x = "Y") +
  scale_fill_manual(values = c("#CDC8B1", "#9FB6CD")) +
  theme(axis.text.x = element_text(angle = 90))


grid.arrange(Graph1, Graph2, ncol = 2)  
###graphs to be improved
### Same issue with boxplot
#The trend is not confirmed, VARIABLE DURATION NOT KEPT

# Check campaign  
Graph1 <- bank_score %>%
  group_by(campaign) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = campaign, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by day",
    x = "campaign",
    y = "campaign 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Graph1
#Minor trend
# Analysis of distribution of yes and no across campaign
m = table(bank_score$campaign, bank_score$y)
n = round(100*prop.table(m,2),2)
t(n) # column percentages
n
n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
campaign <- row.names(n)
campaign

df1 <- data.frame(no, yes, campaign)
df2 <- melt(df1, id.vars='campaign')
head(df2)
class(df2$campaign)
df2$campaign <- as.numeric(as.character(df2$campaign))
class(df2$campaign)

Graph2<- ggplot(df2, aes(x= campaign, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y repartition (in percent)", x = "campaign")+
  labs(fill = "y") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90)) 
Graph2
grid.arrange(Graph1, Graph2, ncol = 2)  
# Seems more yes below 5, formula to be tested with and without CAMPAIGN

# check pdays  
Graph1 <- bank_score %>%
  group_by(pdays) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = pdays, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by day",
    x = "pdays",
    y = "pdays 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Graph1
#Seems there is a trend toward lowers yest around 250 (150-350)
# Analysis of distribution of yes and no across pdays
m = table(bank_score$pdays, bank_score$y)
n = round(100*prop.table(m,2),2)
t(n) # column percentages
n
n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
pdays <- row.names(n)
pdays

df1 <- data.frame(no, yes, pdays)
df2 <- melt(df1, id.vars='pdays')
head(df2)
class(df2$pdays)
df2$pdays <- as.numeric(as.character(df2$pdays))
class(df2$pdays)

Graph2<- ggplot(df2, aes(x= pdays, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y repartition (in percent)", x = "pdays")+
  labs(fill = "y") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90)) 
Graph2 <- ggplot(df2, aes(x = variable, y = pdays, fill = value)) +
  geom_boxplot() +
  labs(y = "pdays", x = "Y") +
  scale_fill_manual(values = c("#CDC8B1", "#9FB6CD")) +
  theme(axis.text.x = element_text(angle = 90))

grid.arrange(Graph1, Graph2, ncol = 2)  
###Same challenges wit boxplot of similar distribution and treatment of outliers


# Trend seems minor, VARIABLE PDAY NOT KEPT

# check previous  
Graph1 <- bank_score %>%
  group_by(previous) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = previous, y = avg)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by day",
    x = "previous",
    y = "previous 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
Graph1

#Seems there is a trend toward higher yest after 10
# Analysis of distribution of yes and no across previous
bank_score$previous
m = table(bank_score$previous, bank_score$y)
n = round(100*prop.table(m,2),2)
t(n) # column percentages
n
n[,1]
n[,2]
no <- n[,1]
no
yes <- n[,2]
yes
previous <- row.names(n)
previous

df1 <- data.frame(no, yes, previous)
df2 <- melt(df1, id.vars='previous')
head(df2)
class(df2$previous)
df2$previous <- as.numeric(as.character(df2$previous))
class(df2$previous)

Graph2<- ggplot(df2, aes(x= previous, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y repartition (in percent)", x = "previous")+
  labs(fill = "y") +
  scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
  theme(axis.text.x = element_text(angle = 90)) 
Graph2
grid.arrange(Graph1, Graph2, ncol = 2) 
# Seems No are concentrated at 0, VARIABLE PREVIOUS KEPT


#Next steps use lm, loess, knn
#Creation train set
bank_score$y
test_index <- createDataPartition(y= bank_score$y, times = 1, p=0.15, list = FALSE)
bank_score_train <- bank_score[-test_index,]
bank_score_test <- bank_score[test_index,]
nrow(bank_score)-nrow(bank_score_train)-nrow(bank_score_test)
?createDataPartition


###############
#Part 2 - Preparing the sample
#################
#sum(bank_score$y=="yes")/nrow(bank_score)
#0.111
#The share of yes is very low, therefore we want to increase the share of yes in the sample

###Create a train and test set making sure the proportion of yes remains the same
dataset <- bank_score
table(dataset$y)

dataset0 = dataset[dataset$y=="no",]
dataset1 = dataset[dataset$y=="yes",]
nrow(dataset0)+nrow(dataset1)-nrow(dataset)

#We split data set of no into bank_score_set and final holdout to test the final model
tt_index0 <- createDataPartition(dataset0$y,times=1,p=0.9,list=FALSE)
bank_score_set0 <- dataset0[tt_index0, ]
finalholdout_set0 <- dataset0[-tt_index0, ]

#We split data set of yes into train and test
tt_index1 <- createDataPartition(dataset1$y,times=1,p=0.9,list=FALSE)
bank_score_set1 <- dataset1[tt_index1, ]
finalholdout_set1 <- dataset1[-tt_index1, ]

#We link the train set of 0 and 1 into 1, same for the test
bank_score_set = rbind(bank_score_set0,bank_score_set1)
bank_score_set
final_holdout_set= rbind(finalholdout_set0,finalholdout_set1)
final_holdout_set
nrow(Bank_Score_set)+nrow(Final_Holdout_set)-nrow(dataset)

#We split Bank_Score_set between train and test set to assess the different models
dataset <- bank_score_set
table(dataset$y)

dataset0 = dataset[dataset$y=="no",]
dataset1 = dataset[dataset$y=="yes",]
nrow(dataset0)+nrow(dataset1)-nrow(dataset)

#We split data set of no into bank_score_set and final holdout to test the final model
tt_index0 <- createDataPartition(dataset0$y,times=1,p=0.9,list=FALSE)
train_set0 <- dataset0[tt_index0, ]
test_set0 <- dataset0[-tt_index0, ]

#We split data set of yes into train and test
tt_index1 <- createDataPartition(dataset1$y,times=1,p=0.9,list=FALSE)
train_set1 <- dataset1[tt_index1, ]
set_set1 <- dataset1[-tt_index1, ]

#We link the train set of 0 and 1 into 1, same for the test
train_set =rbind(train_set0,train_set1)
test_set  =rbind(test_set0,set_set1)

nrow(train_set)+nrow(test_set)-nrow(dataset)


##I DO NOT UNDERSTAND WHAT THE BELOW IS DOING,I UNDERSTOOD WE WANTED 50% YES AND NO IN BOTH TEST AND TRAIN
#train_set= train_set[sample(1:nrow(train_set)), ]
#test_set= test_set[sample(1:nrow(test_set)), ]

#nrow(train_set)+nrow(test_set)-nrow(dataset)

#table(train_set$y)
#table(test_set$y)

#table(train_set$y) / length(train_set$y)
#table(test_set$y) / length(test_set$y)


###Couleurs des graphs
### to do: similar approach as movielens with edX and holdhout
###glmnet
## trControl = trainControl("cv", number = 10)
## dealing with unbalanced dataset
##Ovesampling
## Confusion matrix



#Variable kept:job -  education -  marital -  loan - month - poutcome - age - previous

#Variable to test: day - yes

# Variable to no keep: balance, duration, pday


head(train_set)
na
train_glm <- train(y ~ job+marital+education+housing+month+loan+poutcome+age+previous, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)

train_glm <- train(y ~ job+marital+education+housing+month+loan+poutcome+age+previous+day, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)
#day is deteriorating the regression

train_glm <- train(y ~ job+marital+education+housing+month+loan+poutcome+age+previous+campaign, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)
#CAMPAIGN is deteriorating the regression


train_knn <- train(y ~job+marital+education+housing+month+loan+poutcome+age+previous, method="knn", data = train_set)
knn_pred <- predict(train_knn, train_set)
mean(knn_pred==train_set$y)

train_rf <- train(y ~job+marital+education+housing+month+loan+poutcome+age+previous, method="rf", data = train_set)
rf_pred <- predict(train_rf, train_set)
mean(rf_pred==train_set$y)

