# load the dataset
library(tidyverse)
library(dslabs)
library(ggplot2)
library(caret)
library(reshape2)
library(gridExtra)
library(ROSE)
library(randomForest)
library(caret)
library(e1071)
library(mlbench)
library(pROC)



setwd("C:/Users/vladi/OneDrive/Documents/R/Own-project")
dir()

bank_score = read.csv("bank.csv", sep=';')
head(bank_score)

############################
#PART 1: DATA ANALYSIS
############################
str(bank_score)
summary(bank_score)


#verification line by line
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
#no empty cell 


#Let us check how many unique value per variable
for (j in 1:ncol(bank_score)) {
  #cat(names(bank_score)[j], "\t", length(unique(bank_score[,j])),"\n")
  cat(format(names(bank_score)[j],
         width = 10,
         justify = "left"), length(unique(bank_score[,j])),"\n")
  }

###############DATA VISUALIZATION
###############DISCRETE VARIABLE

#Function to produce graphs
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

########## continuous variables : boxplot ###

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

###############
#Part 2 - Preparing the sample
#################

num_seed <- 321
###Create a train and final holdout making sure the proportion of yes remains the same
dataset <- bank_score
table(dataset$y)

dataset0 = dataset[dataset$y=="no",]
dataset1 = dataset[dataset$y=="yes",]
nrow(dataset0)+nrow(dataset1)-nrow(dataset)

#We split data set of no into bank_score_set and final holdout to test the final model
set.seed(num_seed)
tt_index0 <- createDataPartition(dataset0$y,times=1,p=0.9,list=FALSE)
bank_score_set0 <- dataset0[tt_index0, ]
finalholdout_set0 <- dataset0[-tt_index0, ]

#We split data set of yes into train and test
set.seed(num_seed)
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
set.seed(num_seed)
tt_index0 <- createDataPartition(dataset0$y,times=1,p=0.9,list=FALSE)
train_set0 <- dataset0[tt_index0, ]
test_set0 <- dataset0[-tt_index0, ]

#We split data set of yes into train and test
set.seed(num_seed)
tt_index1 <- createDataPartition(dataset1$y,times=1,p=0.9,list=FALSE)
train_set1 <- dataset1[tt_index1, ]
set_set1 <- dataset1[-tt_index1, ]

#We link the train set of 0 and 1 into 1, same for the test
train_set0
train_set1
train_set =rbind(train_set0,train_set1)
train_set

#unique(train_set$y)
test_set  =rbind(test_set0,set_set1)
test_set

nrow(train_set)+nrow(test_set)-nrow(dataset)

#Build a balanced sample instead
sum(train_set$y == "yes")
sum(train_set$y == "no")

set.seed(num_seed)
bank_score_set_over <- ovun.sample(y~., data = train_set,
                                   method = "under", N = sum(train_set$y=="yes")*2)$data
table(bank_score_set_over$y)
train_balanced = bank_score_set_over


###PART 3 TRAIN THE MODEL
#######################
#We will for sure keep the variables job -  education -  marital - housing-  loan – month  
#and test the variables default day – poutcome – age - campaign- previous (removing outliers beyond 10) -  balance (taking out outliers beyond 10,000), duration (taking out outliers beyond 1,000), pdays(without outliers beyond 250

# Removing of the outliers
nrow(train_set)
train_set <- train_set[train_set$previous<=10,]
train_set <- train_set[train_set$balance<=10000,]
train_set <- train_set[train_set$duration<=1000,]
train_set <- train_set[train_set$pdays<=250,]

nrow(train_balanced)
train_balanced <- train_balanced[train_balanced$previous<=10,]
train_balanced <- train_balanced[train_balanced$balance<=10000,]
train_balanced <- train_balanced[train_balanced$duration<=1000,]
train_balanced <- train_balanced[train_balanced$pdays<=250,]

nrow(train_set)
nrow(train_balanced)

#Function to automatically identify accuracy, error and AUC
functionerror <- function(train_glm,train_set,test_set, ifcat=TRUE) {
  glm_pred_train <- predict(train_glm, train_set)
  glm_pred_test  <- predict(train_glm, test_set)
  if (ifcat) {
  cat("______________________________________\n")
  cat(paste(" ",train_glm$method,"  ",
            as.character(train_glm$call)[2],sep=""),"\n",
      paste(train_glm$method,"-> accuracy train = ",sep=""),
      round(mean(glm_pred_train==train_set$y),4),"\n",
      paste(train_glm$method,"-> accuracy test  = ",sep=""),
      round(mean(glm_pred_test==test_set$y),4),"\n")
  }
  tables2 = cbind(table(glm_pred_train,train_set$y),
                  table(glm_pred_test,test_set$y))
  if (ifcat) {
    cat("--------------------------------------\n")
    cat(paste(" ",train_glm$method,"-> confusion matrices
(train|test)\n",sep=""))
    print(tables2)
  }
  roc.out <- roc( as.integer(test_set$y=="yes"),
                  as.integer(as.character(glm_pred_test)=="yes"))
  auc.out = auc(roc.out)
  if (ifcat) {
    cat("AUC =",as.numeric(auc.out),"\n")
    cat("______________________________________\n")
  }
  return (list(glm_pred_train=glm_pred_train,glm_pred_test=glm_pred_test,
               train_glm=train_glm,train_set=train_set,test_set=test_set,
               auc.out=auc.out))
}

#only with variable job -  education -  marital - housing-  loan – month
train_glm <- train(y ~ job + education + marital + housing + loan + month, method="glm", 
                   data = train_set,
                   preProcess = c("center","scale"))
glm1 = functionerror(train_glm,train_set,test_set) 
# AUC 0.5134, Accuracy 0.8768 - however, yes yes only 2/46 identified
#Test with balanced set
train_glm_balanced <- train(y ~ job + education + marital + housing + loan + month,
                            method="glm",
                            data = train_balanced,
                            preProcess = c("center","scale"))
glm1_bal = functionerror(train_glm_balanced,train_balanced,test_set) 
# AUC 0.604 Accuracy 0.6847 - good yes 23/26 - lower accuracy but better prediction of yes 
# we will work only on the balanced_sample

#test day
#train_glm <- train(y ~ job + education + marital + housing + loan + month+ day, method="glm", data = train_set)
#glm2 = functionerror(train_glm,train_set,test_set) 
# Accuracy 0.8842 - good yes 2/46
train_glm_balanced <- train(y ~ job + education + marital + housing + loan + month+ day,
                            method="glm",
                            data = train_balanced,
                            preProcess = c("center","scale"))
glm2_bal = functionerror(train_glm_balanced,train_set,test_set) 
# AUC decreased, day is not to be kept, we can try it with other method though

#glm_pred <- predict(train_glm, train_set)
#mean(glm_pred==train_set$y)
#Accuracy unchanged, it does not matter if we keep day

#test poutcome
#train_glm <- train(y ~ job + education + marital + housing + loan + month+ day+ poutcome, method="glm", data = train_set)
#glm3 = functionerror(train_glm,train_set,test_set) 
#Accuracy 0.0.8916 and yesyes 8/46
train_glm_balanced <- train(y ~ job + education + marital + housing + loan + month + poutcome, 
                            method="glm", 
                            data = train_balanced,
                            preProcess = c("center","scale"))
glm3_bal = functionerror(train_glm_balanced,train_set,test_set) 
#AUC increased 0.6402778: kept

#glm_pred <- predict(train_glm, train_set)
#mean(glm_pred==train_set$y)
#Accuracy increases to 0.9062, poutcome to be kept

#test age
#train_glm <- train(y ~ job + education + marital + housing + loan + month+ day+ poutcome+ age, method="glm", data = train_set)
#glm4 = functionerror(train_glm,train_set,test_set) 
#Accuracy 0.8916 and yesyes 8/46
train_glm_balanced <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age,
                            method="glm",
                            data = train_balanced,
                            preProcess = c("center","scale"))
glm4_bal = functionerror(train_glm_balanced,train_set,test_set) 
#AUC increases 0.6469807, and yes yes prediction increases

#test campaign
#train_glm <- train(y ~ job + education + marital + housing + loan + month+ day+ poutcome+ campaign, method="glm", data = train_set)
#glm5 = functionerror(train_glm,train_set,test_set) 
# Accuracy 0.8916 and yesyes 8/46
train_glm_balanced <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ campaign, 
                            method="glm",
                            data = train_balanced,
                            preProcess = c("center","scale"))
glm5_bal = functionerror(train_glm_balanced,train_set,test_set) 
#AUC and yes/yes prediction decreases 0.6397947 , campaign not kept 

#test previous
#train_glm <- train(y ~ job + education + marital + housing + loan + month+ day+ poutcome+ age + previous, method="glm", data = train_set)
#glm6 = functionerror(train_glm,train_set,test_set) 
# Accuracy 0.8892 and yesyes 8/46
train_glm_balanced <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous, 
                            method="glm",
                            data = train_balanced,
                            preProcess = c("center","scale"))
glm6_bal = functionerror(train_glm_balanced,train_set,test_set) 
#Accuracy increased at 0.6603 and yes/yes prediction increased  at 26/46, will  keep previous

#test default
#train_glm <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ default, method="glm", data = train_set)
#glm7 = functionerror(train_glm,train_set,test_set) 
# Accuracy 0.8892  and yesyes 8/46
train_glm_balanced <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default,
                            method="glm",
                            data = train_balanced,
                            preProcess = c("center","scale"))
glm7_bal = functionerror(train_glm_balanced,train_set,test_set) 
#AUC increased to 0.6617754 

#test balance
#train_glm <- train(y ~ job + education + marital + housing + loan + month+ day+ poutcome+ age + balance, method="glm", data = train_set)
#glm8 = functionerror(train_glm,train_set,test_set) 
# Accuracy 0.8892 and yesyes 8/46
train_glm_balanced <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + balance,
                            method="glm",
                            data = train_balanced,
                            preProcess = c("center","scale"))
glm8_bal = functionerror(train_glm_balanced,train_set,test_set) 

#AUC decreases  and yes/yes prediction decreases 
#balance not kept

#test duration
#train_glm <- train(y ~ job + education + marital + housing + loan + month+ day+ poutcome+ age + balance + duration, method="glm", data = train_set)
#glm9 = functionerror(train_glm,train_set,test_set) 
# Accuracy 0.9039 and yesyes 121
train_glm_balanced <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration, 
                            method="glm", 
                            data = train_balanced,
                            preProcess = c("center","scale"))
glm9_bal = functionerror(train_glm_balanced,train_set,test_set) 
print(summary(train_glm_balanced$finalModel))

#summary(train_glm_balanced$finalModel)$adj.r.squared

#AUC increases 0.8308575   and yes/yes prediction increases 39/46

#glm_pred <- predict(train_glm, train_set)
#mean(glm_pred==train_set$y)
#Accuracy improves, we keep duration

#test pdays
#train_glm <- train(y ~ job + education + marital + housing + loan + month+ day+ poutcome+ age + duration+pdays, method="glm", data = train_set)
#glm10 = functionerror(train_glm,train_set,test_set) 
# Accuracy 0.917 and yesyes 23/46
train_glm_balanced <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration + pdays, method="glm", data = train_balanced)
glm10_bal = functionerror(train_glm_balanced,train_set,test_set) 
#decreases

#the best set of variable has been identified as job + education + marital + housing + loan + month+ day+ poutcome+ age + duration with
#AUC 0.8417271
#YES/YES 40/46

######knn


 
#Perform knn method without any adjustement
#given the previous results, we will perform straight with the balanced_sample
train_knn1_bal <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration, method="knn", data = train_balanced)
knn1_bal = functionerror(train_knn1_bal,train_set,test_set) 
#accuracy 0.7471014 and YES/YES 35/46
#Check all variable do not do better than pre selection of variables
train_knn2_bal <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration + day, method="knn", data = train_balanced)
knn2_bal = functionerror(train_knn2_bal,train_set,test_set)
#day decreases AUC
train_knn3_bal <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration+campaign, method="knn", data = train_balanced)
knn3_bal = functionerror(train_knn3_bal,train_set,test_set)
#AUC also decreases AUC
train_knn4_bal <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration+balance, method="knn", data = train_balanced)
knn4_bal = functionerror(train_knn4_bal,train_set,test_set)
#balance decreases AUC
train_knn5_bal <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration+pdays, method="knn", data = train_balanced)
knn5_bal = functionerror(train_knn5_bal,train_set,test_set)
#with knn, pdays really increases AUC to 0.7788


#job + education + marital + housing + loan + month+ day+ poutcome+ campaign+ previous + duration 0.7633 31/46
train_knn_cv_bal <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration+pdays,
                           method="knn",
                           data = train_balanced,
                           tuneGrid = data.frame(k=seq(5,50,2)),
                           trControl = trainControl(method="repeatedcv", 
                                                    number = 15,
                                                    repeats = 5),
                           preProcess = c("center","scale"),
                           metric="Accuracy")

train_knn_cv_bal$bestTune
train_knn_cv_bal$bestTune[1,]

ggplot(train_knn_cv_bal)
knn_bal_CV = functionerror(train_knn_cv_bal,train_set,test_set) 
#AUC 0.8091184 yy 37/46

##########rf


##prediction is not as good as prediction for the glm
# rf1 with unbalanced data set does not predict any yes
# job + education + marital + housing + loan + month+ day+ poutcome+ campaign+ previous + duration 41/46 AUC 0.8456522 
#Which variables are good with rf
set.seed(num_seed)
train_rf_bal <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration, method="rf", data = train_balanced)
rf1 = functionerror(train_rf_bal,train_balanced,test_set) 
# rf1 with balanced data set has an AUC of 0.8155797  and predicts 39/46 yes/yes
train_rf_bal_2 <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration + day, method="rf", data = train_balanced)
rf2 = functionerror(train_rf_bal_2,train_balanced,test_set) 
#AUC 0.8428744 , y/y 4. With rf, day improves AUC

train_rf_bal_3 <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration + day+ campaign, method="rf", data = train_balanced)
rf3 = functionerror(train_rf_bal_3,train_balanced,test_set) 
# AUC 0.8320048  , campaign decreases AUC

train_rf_bal_4 <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration + day+ balance, method="rf", data = train_balanced)
rf4 = functionerror(train_rf_bal_4,train_balanced,test_set)
# AUC 0.8169686 , balance decreases AUC

train_rf_bal_5 <- train(y ~ job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration + day+ pdays, method="rf", data = train_balanced)
rf5 = functionerror(train_rf_bal_5,train_balanced,test_set)
#AUC 0.8456522, y/y 41, pdays increases AUC

# use cross validation to choose parameter
# Define a range of values for ntree to tune over

# set.seed(num_seed)
# #validation croisee avec 5 traine et test
# cv_folds <- createFolds(train_set$y, k = 5, returnTrain = TRUE)
# cv_folds

######### BETTER TUNING RF #########

#10 folds repeat 3 times
control <- trainControl(method='repeatedcv',
                        number=10,
                        repeats=10)
#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(num_seed)

nodesize.val.all  = c(1:5) 
mtry.val.all      = c(1:7) #1:7
num.trees.val.all = c(50,100,150,200,250)#c(50,100,150,200,250)
nb.models         = length(mtry.val.all)*
  length(num.trees.val.all)*
  length(nodesize.val.all)

AUC.all = matrix(0,nrow = nb.models, ncol=4)
colnames(AUC.all) <- c("mtry","num.trees","nodesize","auc")


model_all= list()

m= 0
sink("rf_boucle_ouput.txt")

for (nodesize.val in nodesize.val.all) {
  for (mtry.val in mtry.val.all) {
    for (num.trees.val in num.trees.val.all) {
      m = m+1
      cat("m=",m,"/",nb.models,"\n")
      cat("running random forest with mlty =",mtry.val,
          "  num.trees=",num.trees.val,"nodesize.val =", nodesize.val,"\n")
      tune.grid <- expand.grid(mtry=c(mtry.val))
      train_rf_try <- train(y~job + education + marital + housing + loan + month+ poutcome+ age+ previous + default + duration + day+ pdays,
                            data=train_balanced,#[,c(list_vars,"y")],
                            method='rf',
                            metric='Accuracy',
                            tunegrid=tune.grid,
                            #num.trees = num.trees.val,
                            ntree = num.trees.val,
                            nodesize = nodesize.val,
                            trControl=control)
      model_all[[m]]=train_rf_try
      rf_try = functionerror(train_rf_try,train_balanced,test_set,
                             ifcat = TRUE)
      roc.out <- roc( as.integer(test_set$y=="yes"),
                      as.integer(as.character(rf_try$glm_pred_test)=="yes"))
      auc.out = auc(roc.out)
      cat("mlty =",mtry.val,"  num.trees =",num.trees.val,
          "nodesize.val =", nodesize.val,
          "  ", "AUC =",as.numeric(auc.out),"\n")
      #cat("--------------------n")
      cat("\n")
      
      AUC.all[m,] = c(mtry.val,num.trees.val,nodesize.val,as.numeric(auc.out))
      
    }
  }
}

#close
sink()
#from the file analysis, the best model is 117
model_best_rf <- model_all[[117]]
model_best_rf
rf_best = functionerror(model_best_rf,train_balanced,test_set, ifcat = TRUE)
# AUC = 0.8579106  

#what is the score on the holdout
rf_best_holdout = functionerror(model_best_rf,train_balanced,final_holdout_set, ifcat = TRUE)
#AUC = 0.7804808  
