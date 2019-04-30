
# Load Data
rm(list=ls())
Exam=read.csv(file.choose(),header=T)
dim(Exam)
str(Exam)

attach(Exam)
names(Exam)
summary(Exam)

# The First Question
glm.fit=glm(deny~hir+lvr+pbcr+self+single+uria+condo,family="binomial",data=Exam)
summary(glm.fit)    
coef(glm.fit)      
summary(glm.fit)$coef
exp(coef(glm.fit))

# The Second Question
k=10
folds=sample(1:k,nrow(Exam),replace=TRUE)

accuracy=rep(0,k)

for(i in 1:k)
{
  glm.fit2=glm(deny~hir+lvr+pbcr+self+single+uria+condo,family="binomial",data=Exam[folds!=i,])
  Exam.test=Exam[folds==i, ]
  glm.probs=predict(glm.fit2,Exam.test, type="response")
  glm.pred=rep("No",nrow(Exam[folds==i,]))
  glm.pred[glm.probs>.5]="yes"
  
  test.truevalue=deny[folds==i]
  table(test.truevalue,glm.pred)       
  accuracy[i]=mean(glm.pred==test.truevalue)
}
mean(accuracy)
