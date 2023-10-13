# load the dataset
library(tidyverse)
library(dslabs)
library(ggplot2)
library(caret)
library(reshape2)

setwd("C:/Users/vladi/OneDrive/Documents/R/Final_Assessment/Option 2")
dir()

bank_score = read.csv("bank.csv", sep=';')
head(bank_score)

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

#categorical
# check job type
bank_score %>%
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
# We see outliers in terms of retired and student and unknown
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

ggplot(df2, aes(x=job, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y axis name (in percent)", x = "x axis name")+
  labs(fill = "titre legend") +
  scale_fill_discrete(labels=c('No', 'Yes'))
## blue colar significant more no and management, retired more yes, VARIABLE PREVIOUS KEPT

# check marital
bank_score %>%
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

ggplot(df2, aes(x=marital, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y axis name (in percent)", x = "x axis name")+
  labs(fill = "titre legend") +
  scale_fill_discrete(labels=c('No', 'Yes'))
# More no for married, this can be kept VARIABLE MARITAL KEPT

# check education 
bank_score %>%
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
# We see higher rate for tertiary
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

ggplot(df2, aes(x=education, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y axis name (in percent)", x = "x axis name")+
  labs(fill = "titre legend") +
  scale_fill_discrete(labels=c('No', 'Yes'))
# tertiary seems confirmed as an indicator of yes, VARIABLE EDUCATION KEPT

# check default   
bank_score %>%
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
# very small difference, will not explore further VARIABLE DEFAULT NOT KEPT

# check housing 
bank_score %>%
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
# We see higher rate for non housing
# check of distribution of yes/no by loan type
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

ggplot(df2, aes(x=housing, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y axis name (in percent)", x = "x axis name")+
  labs(fill = "titre legend") +
  scale_fill_discrete(labels=c('No', 'Yes'))
# No housing is confirmed to be a good indicator of yes VARIABLE HOUSING KEPT

# check loan 
bank_score %>%
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

# We see higher rate for non loan
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

ggplot(df2, aes(x=loan, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y axis name (in percent)", x = "x axis name")+
  labs(fill = "titre legend") +
  scale_fill_discrete(labels=c('No', 'Yes'))
# all considered, it seems the impact is not so big because most of value are anyhow concentrated on the no VARIABLE LOAN NOT KEPT

# check contact 
bank_score %>%
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
# Unknown has much lower rate, it does not seems to necessarily make sense and VARIABLE CONTACT NOT KEPT
#check month 
#Check share of yes/no per month
bank_score %>%
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

# December, March, October and September seem to be outliers

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

ggplot(df2, aes(x=month, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y axis name (in percent)", x = "x axis name")+
  labs(fill = "titre legend") +
  scale_fill_discrete(labels=c('No', 'Yes'))
# Strong difference between months VARIABLE MONTH KEPT

# check poutcome 
bank_score %>%
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
# Success has clearly a higher rate of Yes
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

ggplot(df2, aes(x=poutcome, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  labs(y= "y axis name (in percent)", x = "x axis name")+
  labs(fill = "titre legend") +
  scale_fill_discrete(labels=c('No', 'Yes'))
##unknown seems a predictor of no, while success a predictor of yes VARIABLE POUTCOME IS KEPT


# VARIABLE MORE CONTINUE
# check age 
bank_score %>%
  group_by(age) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = age, y = avg)) +
  geom_point(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by age Type",
    x = "age",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_smooth()
#between 30 and 50 score is better
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

ggplot(df2, aes(x = age, y = value, color = variable)) +
  geom_point() +
  labs(y = "y axis name (in percent)", x = "x axis name") +
  labs(color = "titre legend") +
  scale_color_discrete(labels = c('No', 'Yes'))+
  geom_smooth()
## It seems there are more no around 30, VARIABLE AGE KEPT


# check balance 
bank_score %>%
  group_by(balance) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = balance, y = avg)) +
  geom_point(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by balance",
    x = "balance",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_smooth()

#No clear trend, possible the lowest balance are slightly associated with Yes
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

ggplot(df2, aes(x = balance, y = value, color = variable)) +
  geom_point() +
  labs(y = "y axis name (in percent)", x = "x axis name") +
  labs(color = "titre legend") +
  scale_color_discrete(labels = c('No', 'Yes'))+
  geom_smooth()
## no trend appears, VARIABLE BALANCE NOT KEPT


# check day 
bank_score %>%
  group_by(day) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = day, y = avg)) +
  geom_point(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by day",
    x = "day",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_smooth()
# Possible trend toward higher yes early in the month but not strong
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

ggplot(df2, aes(x = day, y = value, color = variable)) +
  geom_point() +
  labs(y = "y axis name (in percent)", x = "x axis name") +
  labs(color = "titre legend") +
  scale_color_discrete(labels = c('No', 'Yes'))+
  geom_smooth()
## no trend appears VARIABLE DAY NOT KEPT


# check duration  
bank_score %>%
  group_by(duration) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = duration, y = avg)) +
  geom_point(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by duration",
    x = "duration",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_smooth()
# Clear trend longer duration higher yes
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

ggplot(df2, aes(x = duration, y = value, color = variable)) +
  geom_point() +
  labs(y = "y axis name (in percent)", x = "x axis name") +
  labs(color = "titre legend") +
  scale_color_discrete(labels = c('No', 'Yes'))+
  geom_smooth()
#The trend is not confirmed, VARIABLE DURATION NOT KEPT

# Check campaign  
bank_score %>%
  group_by(campaign) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = campaign, y = avg)) +
  geom_point(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by campaign",
    x = "campaign",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_smooth()
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

ggplot(df2, aes(x = campaign, y = value, color = variable)) +
  geom_point() +
  labs(y = "y axis name (in percent)", x = "x axis name") +
  labs(color = "titre legend") +
  scale_color_discrete(labels = c('No', 'Yes'))+
  geom_smooth()
# lack of trend confirmed VARIABLE CAMPAIGN NOT KEPT

# check pdays  
bank_score %>%
  group_by(pdays) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = pdays, y = avg)) +
  geom_point(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by duration",
    x = "pdays",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_smooth()

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

ggplot(df2, aes(x = pdays, y = value, color = variable)) +
  geom_point() +
  labs(y = "y axis name (in percent)", x = "x axis name") +
  labs(color = "titre legend") +
  scale_color_discrete(labels = c('No', 'Yes'))+
  geom_smooth()
# Trend seems minor, VARIABLE PDAY NOT KEPT

# check previous  
bank_score %>%
  group_by(previous) %>%
  summarize(avg = mean(y == "yes")) %>%
  ggplot(aes(x = previous, y = avg)) +
  geom_point(stat = "identity", fill = "blue") +
  labs(
    title = "Average 'Yes' Responses by previous",
    x = "previous",
    y = "Average 'Yes' Responses"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_smooth()
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

ggplot(df2, aes(x = previous, y = value, color = variable)) +
  geom_point() +
  labs(y = "y axis name (in percent)", x = "x axis name") +
  labs(color = "titre legend") +
  scale_color_discrete(labels = c('No', 'Yes'))+
  geom_smooth()
# a few spikes are noted, VARIABLE PREVIOUS KEPT

##Analysis of the distribution 

#Next steps use lm, loess, knn
#Creation train set
bank_score$y
test_index <- createDataPartition(y= bank_score$y, times = 1, p=0.15, list = FALSE)
bank_score_train <- bank_score[-test_index,]
bank_score_test <- bank_score[test_index,]
nrow(bank_score)-nrow(bank_score_train)-nrow(bank_score_test)
?createDataPartition


##########
Preparing the sample
#########
sum(bank_score$y=="yes")/nrow(bank_score)
#0.111
#The share of yes is very low, therefore we want to increase the share of yes in the sample

###Increasing the share of yes
dataset <- bank_score
table(dataset$y)

dataset0 = dataset[dataset$y=="no",]
dataset1 = dataset[dataset$y=="yes",]
nrow(dataset0)+nrow(dataset1)-nrow(dataset)

#We split data set of no into train and test
tt_index0 <- createDataPartition(dataset0$y,times=1,p=0.9,list=FALSE)
train_set0 <- dataset0[tt_index0, ]
test_set0 <- dataset0[-tt_index0, ]

#We split data set of yes into train and test
tt_index1 <- createDataPartition(dataset1$y,times=1,p=0.9,list=FALSE)
train_set1 <- dataset1[tt_index1, ]
test_set1 <- dataset1[-tt_index1, ]

#We link the train set of 0 and 1 into 1, same for the test
train_set =rbind(train_set0,train_set1)
test_set  =rbind(test_set0,test_set1)

nrow(train_set)+nrow(test_set)-nrow(dataset)

##I DO NOT UNDERSTAND WHAT THE BELOW IS DOING,I UNDERSTOOD WE WANTED 50% YES AND NO IN BOTH TEST AND TRAIN
train_set= train_set[sample(1:nrow(train_set)), ]
test_set= test_set[sample(1:nrow(test_set)), ]

nrow(train_set)+nrow(test_set)-nrow(dataset)

table(train_set$y)
table(test_set$y)

table(train_set$y) / length(train_set$y)
table(test_set$y) / length(test_set$y)





train_set
na
train_glm <- train(y ~ job+marital+education+housing+month+poutcome+age+previous, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)

train_knn <- train(y ~job+marital+education+housing+month+poutcome+age+previous, method="knn", data = train_set)
knn_pred <- predict(train_knn, train_set)
mean(knn_pred==train_set$y)

train_rf <- train(y ~job+marital+education+housing+month+poutcome+age+previous, method="rf", data = train_set)
knn_rf <- predict(train_rf, train_set)
mean(rf_pred==train_set$y)
