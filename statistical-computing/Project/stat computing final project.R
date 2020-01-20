
library(dplyr)
library(ggplot2)
install.packages('GGally')
#install.packages('broom')
library(GGally)

#rm(list = ls())

wine = read.csv('winequality-red.csv')
dim(wine)
str(wine)

################## a. Data visualization ##################

ggpairs(data=wine)

#ggcorr(wine, label = TRUE)

install.packages("ggcorrplot")
library(ggcorrplot)
# ggcorrplot(cor(wine[c(1:11)]))
ggcorrplot(cor(wine[c(1:11)]), type = "lower",lab = TRUE)

#cor(wine)

#Nothing appears to be highly correlated (>0.70 correlation value); 
#however, there are some pairs of variables that decently correlated. 
#For example, fixed acidity is decently correlated (>0.5 or <-0.5) with 
#citric acidity, density and pH(fixed.acidity and PH are negatively correlated), 
#volatile acidity is decently correlated with citric acid. 
#Also, total sulfur dioxide is decently correlated with free sulfur dioxide

# histogram
ggplot(data=wine, aes(quality)) + 
  geom_histogram(aes(y =..count..), 
                 breaks=seq(2, 8),
                 alpha = .2) +
  labs(title="Histogram of Quality Wine") +
  labs(x="Quality", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=14)) + scale_fill_brewer(palette="Set2")

# convert quality into categorical variable
wine$quality[wine$quality >= 6]<-"good"
wine$quality[wine$quality<6]<-"bad"
wine[,12]=as.factor(wine[,12])

attach(wine)
names(wine)
summary(wine[12])
contrasts(wine$quality)

ggpairs(data=wine, 
        mapping=ggplot2::aes(colour = quality))

# optional
ggplot(wine, aes(x = quality, y = alcohol, fill = quality)) +
  geom_boxplot() + 
  xlab("Quality") + 
  ylab("Alcohol") +
  ggtitle("Alcohol vs Quality") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text=element_text(size=14))+ scale_fill_brewer(palette="Set2")

# we could see that alcohol looks like an important factor
# when deciding quality, the alcohol level, sulphates level 
# and citric acidity of "good quality" red wine
# are higher than "bad quality";
# Total sulfur dioxide and volatile acidity of "good quality" 
# red wine are lower than "bad quality".


################## b. statistical model building ##################

# I. logistic regression 

# M1 - using all variables available to build the model
glm.fit1=glm(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol,family="binomial",data=wine)
summary(glm.fit1)
summary(glm.fit1)$coef

# we can see from the P-value that volatile.acidity, total.sulfur.dioxide
# sulphates, alcohol are important variables given the significant value 0.001
# volatile.acidity, citric.acid, chlorides, free.sulfur.dioxide, total.sulfur.dioxide
# sulphates, alcohol are important variables given the significant value 0.05

AIC1=AIC(glm.fit1)
BIC1=BIC(glm.fit1)
# A low AIC/BIC is desirable

# evaluate the perfomance of the model using the test set 
# based on hold-out method

# split the data into training and testing set using holdout method
set.seed(2)

train=sample(nrow(wine),nrow(wine)*0.8)
wine.test=wine[-train, ]

test.truevalue=quality[-train]
train.truevalue=quality[train]

dim(wine.test)

glm.probs1=predict(glm.fit1,wine.test,type="response")
glm.pred1=rep("bad",320)
glm.pred1[glm.probs1 > 0.5]="good"

# confusion matrix
table(glm.pred1,test.truevalue)
mean(glm.pred1==test.truevalue)*100
# the prediction accuracy is 73.125

# M2 - using only those variables that are important
glm.fit2=glm(quality~volatile.acidity+citric.acid+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+sulphates+alcohol,data=wine,family="binomial")
summary(glm.fit2)
exp(coef(glm.fit2))

AIC2=AIC(glm.fit2)
BIC2=BIC(glm.fit2)
AIC2
BIC2

# evaluate the perfomance of the model using the test set 
set.seed(2)
glm.probs2=predict(glm.fit2,wine.test,type="response")
glm.pred2=rep("bad",320)
glm.pred2[glm.probs2 > 0.5]="good"
table(glm.pred2,test.truevalue)
mean(glm.pred2==test.truevalue)*100 
# the prediction accuracy is 72.5

# M3 - delete citric.acid
glm.fit3=glm(quality~volatile.acidity+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+sulphates+alcohol,data=wine,family="binomial")
summary(glm.fit3)
exp(coef(glm.fit3))

AIC3=AIC(glm.fit3)
BIC3=BIC(glm.fit3)
AIC3
BIC3

# evaluate the perfomance of the model using the test set 
glm.probs3=predict(glm.fit3,wine.test,type="response")
glm.pred3=rep("bad",320)
glm.pred3[glm.probs3 > 0.5]="good"
table(glm.pred3,test.truevalue)
mean(glm.pred3==test.truevalue)*100 
# the prediction accuracy is 73.4375

# check for multicollinearity level
install.packages("fmsb")
library(fmsb)

fit1<-lm(fixed.acidity~volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol)
summary(fit1)
VIF(fit1)

fit2<-lm(volatile.acidity~citric.acid+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+sulphates+alcohol)
summary(fit2)
VIF(fit2)

fit3<-lm(volatile.acidity~chlorides+free.sulfur.dioxide+total.sulfur.dioxide+sulphates+alcohol)
summary(fit3)
VIF(fit3)

# II. Classification
library(tree)

# best = 4
set.seed(2)

train=sample(nrow(wine),nrow(wine)*0.8)
tree.model=tree(quality~.,wine,subset =train)
wine.test=wine[-train,]
quality.test=quality[-train]
cv.model=cv.tree(tree.model,K=10,FUN=prune.misclass)  
cv.model

train=sample(nrow(wine),nrow(wine)*0.8)
wine.test=wine[-train, ]

prune.model=prune.tree(tree.model,best=4)
plot(prune.model)
text(prune.model,pretty=0)
prunetree.pred=predict(prune.model,wine.test,type="class")
table(prunetree.pred,quality.test)
mean(prunetree.pred==wine.test$quality)*100
# prediction accuracy is 70%

# best = 8
set.seed(2)

train=sample(nrow(wine),nrow(wine)*0.8)
tree.model=tree(quality~.,wine,subset =train)
wine.test=wine[-train,]
quality.test=quality[-train]
cv.model=cv.tree(tree.model,K=10,FUN=prune.misclass)  
cv.model

train=sample(nrow(wine),nrow(wine)*0.8)
wine.test=wine[-train, ]

prune.model=prune.tree(tree.model,best=8)
plot(prune.model)
text(prune.model,pretty=0)
prunetree.pred=predict(prune.model,wine.test,type="class")
table(prunetree.pred,quality.test)
mean(prunetree.pred==wine.test$quality)*100
# prediction accuracy is 73.4375

# III. Random Forest
library(randomForest)                          

set.seed(2) 
rf.wine=randomForest(quality~.,data=wine,subset=train,mtry=3,importance=TRUE)
rf.wine 

pred.test.rf = predict(rf.wine,newdata=wine[-train,],type="class")
table(pred.test.rf,wine.test$quality)

mean(pred.test.rf==wine.test$quality)*100
# prediction accuracy is 82.1875

importance(rf.wine) 
varImpPlot(rf.wine)

detach(wine)