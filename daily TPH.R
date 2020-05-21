rm(list=ls())

library(readxl)
df<-as.data.frame(read_excel("D:\\TPH Daily\\daily data fillna 02042020.xlsx"))
names(df)

df<-df[,c("RP_power","SFbyS","RP_Bin_Level","Clinker","PA_prop","Ball_Mill","BM_Frate","S_ByDamper","TPH")]

newdf01<-as.data.frame(sapply(df, as.numeric))
#df2<-df1[,c(4,21,22,32)]
sum_na<-as.data.frame(sapply(newdf01,function(x) sum(is.na(x))))
sum_na

# [1] "RP_power"            "RP_Bin_Level"        "Ball_Mill"           "S_ByDamper"          "S_kW"               
#[6] "C_Residue"           "Clinker"             "BM Feed Rate (TPH)"  "SF_Kw"               "Gypsum Proportion %"
#[11] "TPH"





library(psych)
library(e1071)
cor.plot(newdf01)

library(car)

#######################

hist(newdf01$RP_power)
skewness(newdf01$RP_power)


############


hist(newdf01$RP_Bin_Level)
skewness(newdf01$RP_Bin_Level)
newdf01$RP_Bin_Level_x<-sqrt(max(newdf01$RP_Bin_Level+1)-newdf01$RP_Bin_Level)
skewness(newdf01$RP_Bin_Level_x)
hist(newdf01$RP_Bin_Level_x)

##################

hist(newdf01$Ball_Mill)
skewness(newdf01$Ball_Mill)
#newdf01$Ball_Mill_x<-sqrt(max(newdf01$Ball_Mill+1)-newdf01$Ball_Mill)
#skewness(newdf01$Ball_Mill_x)


#########################

hist(newdf01$S_ByDamper)
skewness(newdf01$S_ByDamper)
summary(newdf01$S_ByDamper)
table(newdf01$S_ByDamper)
newdf01$S_ByDamper_x<-sqrt(max(newdf01$S_ByDamper+1)-newdf01$S_ByDamper)
skewness(newdf01$S_ByDamper_x)
hist(newdf01$S_ByDamper_x)


##########################

hist(newdf01$SFbyS)
skewness(newdf01$SFbyS)



##########as it is#####

hist(newdf01$Clinker)
skewness(newdf01$Clinker)


##################

hist(newdf01$BM_Frate)
skewness(newdf01$BM_Frate)


############################



hist(newdf01$PA_prop)
skewness(newdf01$PA_prop)
newdf01$PA_prop_x<-sqrt(newdf01$PA_prop)
hist(newdf01$PA_prop_x)
skewness(newdf01$PA_prop_x)

#####################3

hist(newdf01$TPH)
skewness(newdf01$TPH)

library(writexl)
#write_xlsx(newdf01,"D:\\work\\holtec\\13042020\\refined.xlsx")
names(newdf01)

newdf001<-newdf01[,c("RP_power","SFbyS","RP_Bin_Level_x","Clinker","PA_prop_x","Ball_Mill","BM_Frate","S_ByDamper_x","TPH")]

boxplot(newdf001)
names(newdf001)
standardize = function(x){return((x-mean(x))/sd(x))}
newdf12=as.data.frame(lapply(newdf001[,c(1:9)], standardize))


dbscan::kNNdistplot(newdf12, k =  4)
abline(h = 2.1, lty = 2)


set.seed(123)
# fpc package
res.fpc <- fpc::dbscan(newdf12, eps = 2.1, MinPts = 4)
# dbscan package
res.db <- dbscan::dbscan(newdf12,2.1, 4)

all(res.fpc$cluster == res.db)
library(factoextra)
fviz_cluster(res.fpc, newdf12, geom = "point")
fviz_cluster(res.db, newdf12, geom = "point")
which(res.db$cluster==0)
which(res.fpc$cluster==0)


lmm<-list()
lms<-list()
for(i in 1:9){
  lmm[i]=mean(newdf001[,i])
  lms[i]=sd(newdf001[,i])
}
gh<-unlist(lmm)
gh1<-unlist(lms)

gh1




er<-rep(NA,8647)
for(i in 1:9){
  
  newdf21=newdf12[,i]*gh1[i]+gh[i]
  er<-cbind(er,newdf21)
  
}
head(newdf21)
er<-er[,-1]
head(er)
head(newdf001)
trans_newdf11<-as.data.frame(er)
head(trans_newdf11)
colnames(trans_newdf11)<-names(newdf12)
names(trans_newdf11)
head(trans_newdf11)
trans_newdf11$cluster<-res.fpc$cluster
#write_xlsx(trans_newdf11,"D:\\work\\holtec\\13042020\\refined data new.xlsx")
# [1]  11  63  73 190 285 288 291 292 302 303 407 408 409 410 430 441 444
trans_newdf11<-trans_newdf11[-which(res.fpc$cluster==0),]

head(trans_newdf11)
library(writexl)
#write_xlsx(trans_newdf11,"D:\\work\\holtec\\13042020\\rand fr data.xlsx")


####################################


standardize = function(x){return(x-mean(x))}
newdf122=as.data.frame(lapply(trans_newdf11[,c(1:8)], standardize))
newdf11<-as.data.frame(cbind(newdf122,TPH=trans_newdf11$TPH))


#write_xlsx(asdf,"D:\\work\\holtec\\13042020\\final raw1.xlsx")

cor.plot(newdf11)
names(newdf11)

#standardize = function(x){return(x-mean(x))}
n#ewdf122=as.data.frame(lapply(asdf[,c(1:10)], standardize))
#newdf11<-as.data.frame(cbind(newdf122,TPH=asdf$TPH))

lh<-lm(TPH~.,data=newdf11)
summary(lh)
step(lh)
vif(lh)

library(olsrr)

ols_plot_cooksd_bar(lh)
plot(lh$residuals)
hist(lh$residuals)
skewness(lh$residuals)
trans_newdf11$da<-cooks.distance(lh)
head(trans_newdf11)

head(asdf)
ols_plot_dfbetas(model)
ols_plot_dffits(model)
ols_plot_resid_stud(model)
ols_plot_resid_stand(model)
ols_plot_resid_lev(model)
ols_plot_resid_stud_fit(model)
ols_plot_hadi(model)
ols_plot_resid_pot(model)
asdf=subset(trans_newdf11,trans_newdf11$da<0.001)
quantile(trans_newdf11$da,probs = c(.91,.92,.93,.94,.95,.96,.97,.98,.985,.99,1))
hist(newdf1$C_Residue)



lh<-lm(TPH~.,data = asdf[,-c(10,11)])
summary(lh)
step(lh)
vif(lh)

#lg<-lm(TPH~(Clinker+Gypsum+PA_prop_x+BM+RP_Power+SF_Kw+S_KW+C_Residue+GA_Dosage+BM_vfd_speed+RP_Condition+Cl_C3s+SliderMR_m+RP_bin_m_x)^2,data=asdf)
#summary(lg)
#vif(lg)
#step(lg)
#plot(lh)
library(car)
vif(lh)

n=8379
ntrain <- round(n*0.80) 
set.seed(123) 
tindex <- sample(n, ntrain) 
train <- asdf[tindex,-c(10,11)] 

test <- asdf[-tindex,-c(10,11)] 
names(train)





custom<-trainControl(method = "repeatedcv",number = 10,repeats = 5,verboseIter = T)
set.seed(1234)
lm<-train(TPH~.,train,method="lm",trControl=custom)
lm$results
lm
summary(lm$finalModel)

library(car)
vif(lm$finalModel)
varImp(lm$finalModel)

p<-predict(lm$finalModel,test)
a<-test$TPH
rss <- sum((p - a) ^ 2)
tss <- sum((a - mean(a)) ^ 2)
rsq <- 1 - rss/tss
rsq
names(test)


pr<-predict(lm$finalModel,test)
resi=pr-test$TPH
summary(resi)

R2=1 - sum((test$TPH-pr)^2)/sum((test$TPH-mean(test$TPH))^2)
R2
N=length(test$TPH)
p=8
adjusted_r2= 1-(((1-R2)*(N-1))/(N-p-1))
adjusted_r2


sqrt(mean((test$TPH-pr)^2))

vif(lh)
varImp(lm$finalModel)


#write_xlsx(newdf11,"D:\\work\\holtec\\13042020\\da.xlsx")

####################33
newdf001<-newdf01[,c("RP_power","SFbyS","RP_Bin_Level","Clinker","PA_prop","Ball_Mill","BM_Frate","S_ByDamper","TPH")]

boxplot(newdf001)
names(newdf001)
standardize = function(x){return((x-mean(x))/sd(x))}
newdf12=as.data.frame(lapply(newdf001[,c(1:9)], standardize))


dbscan::kNNdistplot(newdf12, k =  4)
abline(h = 2.9, lty = 2)


set.seed(123)
# fpc package
res.fpc <- fpc::dbscan(newdf12, eps = 2.9, MinPts = 4)
# dbscan package
res.db <- dbscan::dbscan(newdf12,2.9, 4)

all(res.fpc$cluster == res.db)
library(factoextra)
fviz_cluster(res.fpc, newdf12, geom = "point")
fviz_cluster(res.db, newdf12, geom = "point")
which(res.db$cluster==0)
which(res.fpc$cluster==0)


lmm<-list()
lms<-list()
for(i in 1:9){
  lmm[i]=mean(newdf001[,i])
  lms[i]=sd(newdf001[,i])
}
gh<-unlist(lmm)
gh1<-unlist(lms)

gh1




er<-rep(NA,488)
for(i in 1:9){
  
  newdf21=newdf12[,i]*gh1[i]+gh[i]
  er<-cbind(er,newdf21)
  
}
head(newdf21)
er<-er[,-1]
head(er)
head(newdf001)
trans_newdf11<-as.data.frame(er)
head(trans_newdf11)
colnames(trans_newdf11)<-names(newdf12)
names(trans_newdf11)
head(trans_newdf11)
trans_newdf11$cluster<-res.fpc$cluster
#write_xlsx(trans_newdf11,"D:\\work\\holtec\\13042020\\refined data new.xlsx")
# [1]  11  63  73 190 285 288 291 292 302 303 407 408 409 410 430 441 444
trans_newdf11<-trans_newdf11[-which(res.fpc$cluster==0),]

head(trans_newdf11)
#############random forest################################


n=482

ntrain <- round(n*0.80) 
set.seed(123) 
tindex <- sample(n, ntrain) 
train.data <- trans_newdf11[tindex,-10] 
head(train.data)

test.data <- trans_newdf11[-tindex,-10]


library(randomForest)

set.seed(123)
rf<-randomForest(TPH~.,data=train.data,mtry=3,ntree=500)
p<-predict(rf)
a<-train.data$TPH
rss <- sum((p - a) ^ 2)
tss <- sum((a - mean(a)) ^ 2)
rsq <- 1 - rss/tss
rsq
p1<-predict(rf,test.data)
a1<-test.data$TPH
rss1 <- sum((p1 - a1) ^ 2)
tss1 <- sum((a1 - mean(a1)) ^ 2)
rsq1 <- 1 - rss1/tss1
rsq1


max(p1)
sqrt(mean((test.data$TPH-p1)^2))

accuracy=1-abs(mean((test.data$TPH-p1)/test.data$TPH))
accuracy
library(caret)
varImp(rf)
varImpPlot(rf)



###############################
pre<-as.data.frame(read_excel("D:\\work\\holtec\\18052020\\data for prediction.xlsx"))
names(pre)
pre<-pre[,-c(9,10,11)]
names(pre)
p1<-predict(rf,pre)
pre<-as.data.frame(cbind(pre,p1))
write_xlsx(pre,"D:\\work\\holtec\\18052020\\data for prediction3.xlsx")

