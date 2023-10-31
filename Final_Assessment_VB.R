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

###############CHECK NEW METHOD
######################################
########## categorical variables : barplot ####
######################################

#colors <- c("No" = "lightyellow", "Yes" = "lightblue")
#par(mfrow=c(1,2))

twograph <- function(namevar,labelvar,xfactors=NULL) {
  data_score=bank_score[,c(namevar,"y")]
  names(data_score) <- c("x","y")
#factor important for order of graphs  
  if (!is.null(xfactors)) {
    data_score$x =
      factor(data_score$x,levels=xfactors)
  }
  Graph1 <- data_score %>%
    group_by(x) %>%
    summarize(avg = mean(y == "yes")) %>%
    ggplot(aes(x = x, y = avg)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(
      title = paste("Frequency of y=yes by ",
                    labelvar,sep = ""),
      #x = libvar,
      x = " ",
      y = "Percent (%)"
      #y = "Average 'Yes' Responses"
    ) +
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(axis.text.x = element_text(angle = 90))+
    theme(axis.text.x = element_text(size = rel(0.8)))
 
  m = table(data_score$x, data_score$y)
  n = round(100*prop.table(m,2),2)
  
  no <- n[,1]
  yes <- n[,2]
  md <- row.names(n)
  
  df1 <- data.frame(no, yes, md)
  df2 <- melt(df1, id.vars='md')
  #print(df2)
  
  if (!is.null(xfactors)) {
    df2$md = factor(df2$md,levels=xfactors)
  }
  
  Graph2 <-ggplot(df2, aes(x=md, y=value, fill=variable)) +
    ggtitle(paste("Frequency of y by ", labelvar,sep = ""))+
    geom_bar(stat='identity', position='dodge') +
    #labs(y= "Percent of y", x = "Job type")+
    labs(y= "Percent (%)", x = " ")+
    labs(fill = " ") +
    scale_fill_manual(values=c("#CDC8B1","#9FB6CD"))+
    theme(axis.text.x = element_text(angle = 90))+
    theme(axis.text.x = element_text(size = rel(0.8)))

    plot(Graph1)
    plot(Graph2)
    cat("variable=",labelvar,"\n")
    print(t(n))
    grid.arrange(Graph1, Graph2, ncol = 2)
    
  # get in a list all the variables of the function
  return (list(graph1=Graph1,graph2=Graph2,
               m=m,n=n,yes=yes,no=no,md=md,
               df1=df1,df2=df2,namevar=namevar,
               labelvar=labelvar))
  
}

# check job
g1g2 = twograph("job","Job")
#Visual difference between yes and no: variable to keep

# check marital
g1g2 = twograph("marital","Marital")
#Visual difference between yes and no: variable to keep

# check education
g1g2 = twograph("education","Education")
#Visual difference between yes and no: variable to keep


# check default
g1g2 = twograph("default","Default")
#the variable does not seem to have an impact, it will be tested further

# check housing
g1g2 = twograph("housing","Housing")
#Visual difference between yes and no: variable to keep
  
# check loan
g1g2 = twograph("loan","Loan")
#Visual difference between yes and no: variable to keep

#check month
g1g2 = twograph("month","Month",
                c("jan","feb","mar","apr","may",
                  "jun", "jul", "aug", "sep","oct",
                  "nov", "dec"))
#Visual difference between yes and no: variable to keep

# check poutcome
g1g2 = twograph("poutcome","Poutcome")
#Visual difference between yes and no, however, interpretation is uncertain: variable to test

#check day
g1g2 = twograph("day","Day",
                as.character(seq(1:31)))
#Visual difference between yes and no, however, interpretation can be difficult because of granular data: variable to test

# check age
g1g2 = twograph("age","Age",
                as.character(seq(min(bank_score$age),
                                 max(bank_score$age),
                                 by=1)))
#Visual difference between yes and no, however, interpretation can be difficult because of granular data: variable to test

# Check campaign
g1g2 = twograph("campaign","Campaign",
                as.character(seq(min(bank_score$campaign),
                                 max(bank_score$campaign),
                                 by=1)))
#Visual difference between yes and no, however, interpretation uncertain because of granular data: variable to test

# check previous
g1g2 = twograph("previous","Previous",
                as.character(seq(min(bank_score$previous),
                                 max(bank_score$previous),
                                 by=1)))
#Visual difference between yes and no, however, interpretation uncertain because of granular data: variable to test

######################################
########## continuous variables : boxplot ###
######################################

onegraph_boxp <- function (namevar,labelvar, bank_score_) {
  data_score=bank_score_[,c(namevar,"y")]
  names(data_score) <- c("x","y")
  data_score$y <- factor(data_score$y,
                         levels=c("yes","no"))
  bp <- ggplot(data_score, aes(y, x))
  bp <- bp + geom_boxplot(fill = "#FFFFFF", color = "#FFFFFF")
  bp <- bp + geom_boxplot(aes(fill = y))
  bp <- bp + scale_fill_manual(values = c("#CDC8B1","#9FB6CD"))
  bp <- bp +labs(
    title = "Boxplots",
    x = " ",
    y = labelvar
  )
  bp <- bp + theme(legend.position = "right")
  # bp
  
  return (bp)
}

p <- onegraph_boxp("balance","Balance",bank_score)
plot(p)

##We see we could consider amounts beyond ZMW 10,000
p <- onegraph_boxp("balance","Balance",bank_score[bank_score$balance<=10000,])
plot(p)
#seems to be a difference: variable to test taking about outliers above 20,000

#Age
p <- onegraph_boxp("age","Age",bank_score)
plot(p)
# seems to be a slight difference, variable to test

# check duration
p <- onegraph_boxp("duration","Duration", bank_score)
plot(p)
#We can remove outliers beyond 1000
p <- onegraph_boxp("duration","Duration", bank_score[bank_score$duration<=1000,])
plot(p)
# to be tested without outliers

# check pdays
p <- onegraph_boxp("pdays","Pdays", bank_score)
plot(p)
#We can remove outliers beyond 250
p <- onegraph_boxp("pdays","Pdays", bank_score[bank_score$pdays<=250,])
plot(p)
#to be tested without outliers

# check previous
p <- onegraph_boxp("previous","Previous", bank_score)
plot(p)
#Remove outliers above 10
p <- onegraph_boxp("previous","Previous", bank_score[bank_score$previous<=10,])
plot(p)


#Next steps use lm, loess, knn
#Creation train set
#bank_score$y
#test_index <- createDataPartition(y= bank_score$y, times = 1, p=0.15, list = FALSE)
#bank_score_train <- bank_score[-test_index,]
#bank_score_test <- bank_score[test_index,]
#nrow(bank_score)-nrow(bank_score_train)-nrow(bank_score_test)
#?createDataPartition


###############
#Part 2 - Preparing the sample
#################

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
nrow(bank_score_set)+nrow(final_holdout_set)-nrow(dataset)

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
train_set0
train_set1
train_set =rbind(train_set0,train_set1)
train_set

#?levels
#unique(train_set$y)
test_set  =rbind(test_set0,set_set1)
test_set

nrow(train_set)+nrow(test_set)-nrow(dataset)


##I DO NOT UNDERSTAND WHAT THE BELOW IS DOING,I UNDERSTOOD WE WANTED 50% YES AND NO IN BOTH TEST AND TRAIN
#train_set= train_set[sample(1:nrow(train_set)), ]
#test_set= test_set[sample(1:nrow(test_set)), ]
#nrow(train_set)+nrow(test_set)-nrow(dataset)
#table(train_set$y)
#table(test_set$y)
#table(train_set$y) / length(train_set$y)
#table(test_set$y) / length(test_set$y)

## Confusion matrix


#Variable kept:job -  education -  marital -  loan - month - poutcome - age - previous
#Variable to test: day - yes
# Variable to no keep: balance, duration, pday


head(train_set)
na

###PART 3 TRAIN THE MODEL
#######################
#We will for sure keep the variables job -  education -  marital - housing-  loan – month  
#and test the variables default day – poutcome – age - campaign- previous (removing outliers beyond 10) -  balance (taking out outliers beyond 10,000), duration (taking out outliers beyond 1,000), pdays(without outliers beyond 250
# let us remove the outliers

train_set <- train_set[train_set$previous<=10,]
train_set <- train_set[train_set$balance<=10000,]
train_set <- train_set[train_set$duration<=1000,]
train_set <- train_set[train_set$pdays<=250,]

                                                                                                                                                                                                          
#only with variable job -  education -  marital - housing-  loan – month
train_glm <- train(y ~ job + education + marital + housing + loan + month, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)
#0.8954

#test day
train_glm <- train(y ~ job + education + marital + housing + loan + month+ day, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)
#Accuracy unchanged, it does not matter if we keep day

#test poutcome
train_glm <- train(y ~ job + education + marital + housing + loan + month+ day+ poutcome, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)
#Accuracy increases to 0.9062, poutcome to be kept

#test age
train_glm <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)
#Accuracy increases, age to be kept

#test campaign
train_glm <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ campaign, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)
#Accuracy decreases, campaign is not to be kept

#test previous
train_glm <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)
#Accuracy unchanged, we can keep it or not

#test default
train_glm <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)
#Accuracy decreases we do not keep default

#test balance
train_glm <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous+ balance, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)
#Accuracy decreases we do not keep balance

#test duration
train_glm <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous+ duration, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)
#Accuracy improves, we keep duration

#test pdays
train_glm <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous+ duration+pdays, method="glm", data = train_set)
glm_pred <- predict(train_glm, train_set)
mean(glm_pred==train_set$y)
#Accuracy decreases, we do not keep pdays
##I WOULD NEED TO ADD A CONFUSION MATRIX HERE


#Perform knn method
train_knn <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous+ duration, method="knn", data = train_set)
knn_pred <- predict(train_knn, train_set)
knn_pred
mean(knn_pred==train_set$y)

#Perform knn method with best fit for k
train_knn2 <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous+ duration, method="knn", data = train_set, tuneGrid = data.frame(k=seq(1,50,2)))
train_knn2
ggplot(train_knn2)

knn_pred <- predict(train_knn2, train_set)
knn_pred
mean(knn_pred==train_set$y)

#Perform knn method with cross validation and identification of best k
train_knn2_cv <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous+ duration, method="knn", data = train_set, tuneGrid = data.frame(k=seq(1,50,2)), trControl = trainControl(method="cv", number = 10, p =0.9))
train_knn2_cv$bestTune
ggplot(train_knn2_cv)
#The best value are 16 and 31
best_k <- 16
train_knn <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous+ duration, method="knn", data = train_set, tuneGrid = data.frame(k = best_k), trControl = trainControl(method="cv", number = 10, p =0.9))
knn_pred <- predict(train_knn, train_set)
mean(knn_pred==train_set$y)
#best prediction using kbb is 0.90465

##I WOULD NEED TO ADD A CONFUSION MATRIX HERE

#I FACE LEVEL ISSUES AND DO NOT MANAGE TO PRODUCE A CONFUSION MATRIX
#levels(knn_pred$y)
#levels(train_set$y)
#knn_pred$y <- factor(knn_pred$y, levels = levels(train_set$y))
#train_set$y <- factor(knn_pred$y, levels = levels(train_set$y))
#unique(train_set$y)
#confusionMatrix(knn_pred, train_set$y)$overall[["Accuracy"]]

##prediction is not as good as prediction for the glm
#rf
train_rf <- train(y ~ job+marital+education+housing+month+loan+age, method="rf", data = train_set)
rf_pred <- predict(train_rf, train_set)
mean(rf_pred==train_set$y)

train_rf <- train(y ~job+marital+education+housing+month+loan+poutcome+age+previous, method="rf", data = train_set)
rf_pred <- predict(train_rf, train_set)
mean(rf_pred==train_set$y)
#not as good as glm
confusionMatrix(rf_pred, train_set$y)$overall[["Accuracy"]]

#Let's try rf with all values
train_rf <- train(y ~., method="rf", data = train_set)
rf_pred <- predict(train_rf, train_set)
mean(rf_pred==train_set$y)

# use cross validation to choose parameter
mtry= seq(1,7)
train_rpart_rf <- train(y ~ .,
                    method = "rf",
                    ntree=100,
                    tuneGrid = data.frame(mtry= seq(1,7)),
                    data = train_set)
train_rpart_rf
train_rpart_rf$bestTune

#not too sure how to interpret the data
#confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# IS IT POSSIBLE TO CREATE A LOOP WHICH ASSESSES VARIABLES?
#train_set
#pvals <- rep(0, ncol(train_set))
#pvals
#for (i in 1:ncol(train_set)) {
#  pvals[i] <- t.test(train_set[,i][y==0], train_set[,i][y==1], var.equal=TRUE)$p.value
#}


##TO DO: knn using cross validation and 
