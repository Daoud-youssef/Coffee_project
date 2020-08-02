## https://github.com/jldbc/coffee-quality-database
### Libraries
library(tidyverse)
library(measurements)
library(corrgram)
library(corrplot)
library(reshape2)
library(caret)
library(caTools)
library(knitr)
library(e1071)
library(randomForest)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


## loading data
arabica.data<-read.csv("arabica_data_cleaned.csv")
robusta.data<-read.csv("robusta_data_cleaned.csv")
### selecting columns 
arabica.data<-arabica.data %>% select(Species,Country.of.Origin, unit_of_measurement,altitude_high_meters,   altitude_mean_meters,Aroma,Flavor,Aftertaste,Acidity,Body,Balance,Uniformity,Clean.Cup,Sweetness,Cupper.Points,Total.Cup.Points) 
robusta.data<-robusta.data %>% select(Species,Country.of.Origin, unit_of_measurement,altitude_mean_meters,altitude_high_meters,
                                 Fragrance...Aroma,Flavor,Aftertaste,Salt...Acid,Mouthfeel,Balance,Uniform.Cup,Clean.Cup,Bitter...Sweet,Cupper.Points,Total.Cup.Points)
### rename columns to match
robusta.data<-robusta.data%>%rename( Aroma= Fragrance...Aroma,
  Acidity= Salt...Acid,
  Body= Mouthfeel,
  Uniformity= Uniform.Cup,
  Sweetness= Bitter...Sweet
)
coffee.data<-rbind(arabica.data,robusta.data)
## remove rows that contain NAs values
coffee.data<-coffee.data%>%na.omit()
### Data is tidy now
anyNA(coffee.data)

### Data exploration
head(coffee.data)

### structure of the data
str(coffee.data)
coffee.data$Species<-as.factor(coffee.data$Species)
coffee.data$Country.of.Origin<-as.factor(coffee.data$Country.of.Origin)
coffee.data$unit_of_measurement<-as.factor(coffee.data$unit_of_measurement)

### One missing country name, il replace it by missing country
levels(coffee.data$Country.of.Origin)[levels(coffee.data$Country.of.Origin)==""]<-"Missing country"

## convert feet to meters
coffee.data<-within(coffee.data,{
  i<-unit_of_measurement=="ft"
  unit_of_measurement[i]<-"m"
  altitude_high_meters[i]<-conv_unit( altitude_high_meters[i],"ft","m")
  altitude_mean_meters[i]<-conv_unit( altitude_mean_meters[i],"ft","m")
   })

### we choose farms with altitude above 100 m and below 5000
##  i removed above 5 000 m cause they are outliers and seems error in data entry.
coffee.data<-coffee.data%>%filter(Aroma>4 & Clean.Cup>4 & altitude_mean_meters>100 & altitude_high_meters<5000)

###### adding two columns score and quality
coffee.data$score<-round(coffee.data$Total.Cup.Points/10)
coffee.data$quality<-ifelse(coffee.data$score<8,"low","high")
coffee.data$quality[coffee.data$score==8]<-"medium"
coffee.data$quality<-as.factor(coffee.data$quality) 

### remove unecessary columns
coffee.data<-subset(coffee.data,select=-c(unit_of_measurement, altitude_high_meters,i))
### statistical summary
summary(coffee.data)

# #####
# coffee.data%>%group_by(quality)%>% summarize(n())
# coffee.data%>%ggplot(aes(score))+geom_bar()


### histogram of distribution of species

coffee.data%>% group_by(Species)%>%ggplot(aes(Species))+geom_bar(aes(col=Species))
coffee.data%>%group_by(Species)%>%summarize(n=n())

#### Histogram of mean altitude
## Arabica 1300~1500
## Robusta 900 ~1000

### altitude and species

coffee.data%>%group_by(altitude_mean_meters)%>% ggplot(aes(altitude_mean_meters))+geom_bar(aes(col=Species))

#### countries and species
coffee.data%>%group_by(Species,Country.of.Origin)%>%summarize(n=n())%>%ggplot(aes(Country.of.Origin,n,colour=Species))+geom_point()+theme(axis.text.x = element_text(angle=90, vjust = 0.5))+labs(Title="Distribution of Species from origin country ", x="Country of Origin", y=" Number of Samples")

### best coffee most rated
p<-coffee.data%>%group_by(Country.of.Origin)%>%summarize(avg_quality=mean(Total.Cup.Points))%>%arrange(desc(avg_quality))
p%>%ggplot(aes(x=reorder(Country.of.Origin,avg_quality),y=avg_quality))+geom_bar(stat = "identity")+coord_flip()+ labs(Title="Rating of coffee quality  ", x="Country of Origin", y=" Total points")


#### Correlation between variables
coffee.data%>% select(Aroma,Flavor,Aftertaste,Acidity,Body,Balance,Uniformity,Clean.Cup,Sweetness,Cupper.Points)%>%corrgram(order = TRUE,lower.panel = panel.shade, upper.panel = panel.pts, main=" Correlation between variables ")

var.cor<-coffee.data%>% select(Aroma,Flavor,Aftertaste,Acidity,Body,Balance,Uniformity,Clean.Cup,Sweetness,Cupper.Points)%>% cor()
corrplot(var.cor)
### Correlation between variables adding mean altitude
coffee.data%>% select(Aroma,Flavor,Aftertaste,Acidity,Body,Balance,Uniformity,Clean.Cup,Sweetness,Cupper.Points,altitude_mean_meters)%>%corrgram(order = TRUE,lower.panel = panel.shade, upper.panel = panel.pts, main=" Correlation between Beans ")

##### density comparison
ttt<-melt(coffee.data,id.vars = "Species",measure.vars = c("altitude_mean_meters", "Aroma","Flavor","Aftertaste","Acidity","Body","Balance","Uniformity","Clean.Cup","Sweetness","Cupper.Points","Total.Cup.Points"))
ggplot(ttt,aes(x=value,colour=Species))+stat_density()+facet_wrap(variable~.,scales = "free")

#### count  comparison
ggplot(ttt,aes(x=value,colour=Species))+geom_area(stat = "bin",bins=100)+facet_wrap(variable~.,scales = "free")

####importance of variable in predicting species
df<-subset(coffee.data,select = -c(Country.of.Origin,quality))
var_imp<-filterVarImp(df[,-1],df[,1])
var_imp

#### variable importance for predicting coffee quality 
df_1<-subset(coffee.data,select = -c(Species,Country.of.Origin))
var_imp_1<-filterVarImp(df_1[,-14],df_1[,14])
var_imp_1

#### 

tt<-melt(coffee.data,id.vars = "quality",measure.vars = c("altitude_mean_meters", "Aroma","Flavor","Aftertaste","Acidity","Body","Balance","Uniformity","Clean.Cup","Sweetness","Cupper.Points"))
ggplot(tt,aes(x=value,colour=quality))+stat_density()+facet_wrap(variable~.,scales = "free")

#####
## quality distribution 

coffee.data%>%group_by(score)%>%summarize(n=n())%>%ggplot(aes(score,n,colour=score))+geom_bar(stat = "identity")+labs(Title="Distribution of Score ", x="Score", y=" Number of Observations")
coffee.data %>%group_by(quality)%>%summarize(n=n())%>%mutate(quality=fct_relevel(quality,"low","medium","high"))%>%ggplot(aes(quality,n,colour=quality))+geom_bar(stat = "identity")+labs(Title="Distribution of quality", x="Quality", y=" Number of Observations")




coffee.data%>% select(Aroma,Flavor,Aftertaste,Acidity,Body,Balance,Uniformity,Clean.Cup,Sweetness,Cupper.Points,altitude_mean_meters,Total.Cup.Points)%>%corrgram(order = TRUE,lower.panel = panel.shade, upper.panel = panel.pts, main=" Correlation between all variables ")



##################### modeling 
set.seed(123)
options(scipen=4) ## remove scientific notation
#### test set will be 30% fo the data
### Reduce columns
coffee_data<-subset(coffee.data,select = -c(Country.of.Origin,Total.Cup.Points))
test_index<-createDataPartition(coffee_data$Species,times = 1,p=0.3,list = FALSE)
train_coffee<-coffee_data%>%slice(-test_index)
test_coffee<-coffee_data%>%slice(test_index)

### RMSE

RMSE <- function (true_ratings, predicted_ratings) {
  sqrt(mean ((true_ratings-predicted_ratings)^2))
}
#### partitioning arabica data to predict quality score
### raw data


### standardize  data using preprocess function from caret package
s<-preProcess(train_coffee)
train_coffee_s<-predict(s,train_coffee)
test_coffee_s<-predict(s,test_coffee)

### Normalized data using preprocess function from caret package

n<-preProcess(train_coffee,method = "range")
train_coffee_n<-predict(n,train_coffee)
test_coffee_n<-predict(n,test_coffee)

######## Species prediction with standardized data 
################################## model 1  linear regression without altitude ###################################
## 1 for arabica and 0 for robusta
##convert outcome to numeric
train_coffee_s<-train_coffee_s%>%mutate(Species=ifelse(Species=="Arabica",1,0))
### linear regression
data.train_1<-subset(train_coffee_s,select = -c(score,altitude_mean_meters,quality))
data.test_1<-subset(test_coffee_s,select = -c(score,altitude_mean_meters,quality))
species_lm_1<-lm(Species~. , data = data.train_1)
species_lm_hat_1<-predict(species_lm_1,data.test_1)
# convert the outcome to factor
pred_species_1<-factor(ifelse(species_lm_hat_1>0.5,"Arabica","Robusta"))
### Evaluating the results
result_1<-caret::confusionMatrix(pred_species_1,data.test_1$Species)
rmse_model_1<-RMSE(as.numeric(data.test_1$Species),as.numeric(pred_species_1))
RMSE_results<-data.frame(Method=" Predicting species based on quality measures without altitude ",Data="Standardized", RMSE = rmse_model_1,r.squared = summary(species_lm_1)$r.squared, Adjusted.r.squared=summary(species_lm_1)$adj.r.squared)

################################# model 2 :  linear regression with altitude
data.train_2<-subset(train_coffee_s,select = -c(score,quality))
data.test_2<-subset(test_coffee_s,select = -c(score,quality))
species_lm_2<-lm(Species~. ,data = data.train_2)
species_lm_hat_2<-predict(species_lm_2,data.test_2)
#summary(species_lm_2)
# convert the outcome to factor
pred_species_2<-factor(ifelse(species_lm_hat_2>0.5,"Arabica","Robusta"))
### Evaluating the results
result_2<-caret::confusionMatrix(pred_species_2,data.test_2$Species)
rmse_model_2<-RMSE(as.numeric(data.test_2$Species),as.numeric(pred_species_2))
RMSE_results<-rbind(RMSE_results,data.frame(Method=" Predicting species based on quality measures with altitude ",Data="Standardized", RMSE = rmse_model_2, r.squared=summary(species_lm_2)$r.squared, Adjusted.r.squared = summary(species_lm_2)$adj.r.squared))


############################# model 3 :   linear regression with score
data.train_3<-subset(train_coffee_s,select = -c(quality))
data.test_3<-subset(test_coffee_s,select = -c(quality))
species_lm_3<-lm(Species~. ,data = data.train_3)
species_lm_hat_3<-predict(species_lm_3,data.test_3)
#summary(species_lm_3)
# convert the outcome to factor
pred_species_3<-factor(ifelse(species_lm_hat_3>0.5,"Arabica","Robusta"))
### Evaluating the results
result_3<-caret::confusionMatrix(pred_species_3,data.test_3$Species)
rmse_model_3<-RMSE(as.numeric(data.test_3$Species),as.numeric(pred_species_3))
RMSE_results<-rbind(RMSE_results, data.frame(Method=" Predicting species based on quality measures with altitude and score ",Data="Standardized", RMSE = rmse_model_3, r.squared=summary(species_lm_3)$r.squared, Adjusted.r.squared = summary(species_lm_3)$adj.r.squared))
#####################################################################################



######## Species prediction with normalized data 
################################## model 1.n  linear regression without altitude ###################################
## 1 for arabica and 0 for robusta
##convert outcome to numeric
train_coffee_n<-train_coffee_n%>%mutate(Species=ifelse(Species=="Arabica",1,0))
### linear regression
data.train_1n<-subset(train_coffee_n,select = -c(score,altitude_mean_meters,quality))
data.test_1n<-subset(test_coffee_n,select = -c(score,altitude_mean_meters,quality))
species_lm_1n<-lm(Species~. , data = data.train_1n)
species_lm_hat_1n<-predict(species_lm_1n,data.test_1n)
# convert the outcome to factor
pred_species_1n<-factor(ifelse(species_lm_hat_1n>0.5,"Arabica","Robusta"))
### Evaluating the results
result_1n<-caret::confusionMatrix(pred_species_1n,data.test_1n$Species)
rmse_model_1n<-RMSE(as.numeric(data.test_1n$Species),as.numeric(pred_species_1n))
RMSE_results<-rbind(RMSE_results, data.frame(Method=" Predicting species based on quality measures without altitude ",Data="Normalized", RMSE = rmse_model_1n,r.squared = summary(species_lm_1n)$r.squared, Adjusted.r.squared=summary(species_lm_1n)$adj.r.squared))

################################# model 2.n  linear regression with altitude
data.train_2n<-subset(train_coffee_n,select = -c(score,quality))
data.test_2n<-subset(test_coffee_n,select = -c(score,quality))
species_lm_2n<-lm(Species~. ,data = data.train_2n)
species_lm_hat_2n<-predict(species_lm_2n,data.test_2n)
#summary(species_lm_2)
# convert the outcome to factor
pred_species_2n<-factor(ifelse(species_lm_hat_2n>0.5,"Arabica","Robusta"))
### Evaluating the results
result_2n<-caret::confusionMatrix(pred_species_2n,data.test_2n$Species)
rmse_model_2n<-RMSE(as.numeric(data.test_2n$Species),as.numeric(pred_species_2n))
RMSE_results<-rbind(RMSE_results,data.frame(Method=" Predicting species based on quality measures with altitude ",Data="Normalized", RMSE = rmse_model_2n, r.squared=summary(species_lm_2n)$r.squared, Adjusted.r.squared = summary(species_lm_2n)$adj.r.squared))

############################# model 3.n linear regression with score
data.train_3n<-subset(train_coffee_n,select = -c(quality))
data.test_3n<-subset(test_coffee_n,select = -c(quality))
species_lm_3n<-lm(Species~. ,data = data.train_3n)
species_lm_hat_3n<-predict(species_lm_3n,data.test_3n)
#summary(species_lm_3)
# convert the outcome to factor
pred_species_3n<-factor(ifelse(species_lm_hat_3n>0.5,"Arabica","Robusta"))
### Evaluating the results
result_3n<-caret::confusionMatrix(pred_species_3n,data.test_3n$Species)
rmse_model_3n<-RMSE(as.numeric(data.test_3n$Species),as.numeric(pred_species_3n))
RMSE_results<-rbind(RMSE_results, data.frame(Method=" Predicting species based on quality measures with altitude and score ", Data="Normalized", RMSE = rmse_model_3n, r.squared=summary(species_lm_3n)$r.squared, Adjusted.r.squared = summary(species_lm_3n)$adj.r.squared))
#####################################################################################



###### Predicting the quality rate  based on variables and altitude for the Arabica coffee
###### for linear regression i will use score as outcome. 
set.seed(123)
options(scipen=4) ## remove scientific notation
#### test set will be 30% fo the data
### Reduce columns

arabica_coffee_data<-subset(coffee.data,select = -c(Country.of.Origin,Total.Cup.Points))
###removing Robusta coffee and species column
arabica_coffee_data<-arabica_coffee_data[arabica_coffee_data$Species=="Arabica",-1]

#### Data partitioning 
test_index<-createDataPartition(arabica_coffee_data$quality,times = 1,p=0.3,list = FALSE)
train_set<-arabica_coffee_data%>%slice(-test_index)
test_set<-arabica_coffee_data%>%slice(test_index)


### standardize  data using preprocess function from caret package
s<-preProcess(train_set)
train_s<-predict(s,train_set)
test_s<-predict(s,test_set)

### Normalized data using preprocess function from caret package
n<-preProcess(train_set,method = "range")
train_n<-predict(n,train_set)
test_n<-predict(n,test_set)

################################# model  4  linear regression with variable altitude only 

###train_s$quality<-as.integer(train_s$quality)
score_lm_4<-lm(score~altitude_mean_meters,data = train_s)
score_lm_hat_4<-predict(score_lm_4,test_s)
#summary(score_lm_4)
### Evaluating the results
rmse_model_4<-RMSE(score_lm_hat_4,test_s$score)

results<-data.frame(Method=" Predicting score based on altitude ",Data="Standardized", RMSE = rmse_model_4,r.squared = summary(score_lm_4)$r.squared, Adjusted.r.squared=summary(score_lm_4)$adj.r.squared)
#####################################################################################

#################################   model 5 linear regression with quality measures  only 
train_s_5<-subset(train_s,select = -c(altitude_mean_meters,quality))
test_s_5<-subset(test_s,select = -c(altitude_mean_meters,quality))
score_lm_5<-lm(score~.,data = train_s_5)
score_lm_hat_5<-predict(score_lm_5,test_s_5)
#summary(score_lm_5)
### Evaluating the results
rmse_model_5<-RMSE(score_lm_hat_5,test_s_5$score)
results<-cbind(results,data.frame(Method=" Predicting score based on quality measures",Data="Standardized", RMSE = rmse_model_5,r.squared = summary(score_lm_5)$r.squared, Adjusted.r.squared=summary(score_lm_5)$adj.r.squared))
#####################################################################################


################################### model 6 linear regression with quality measures and altitude.

### linear regression with all variable
score_lm_6<-lm(score~.-quality,data = train_s)
score_lm_hat_6<-predict(score_lm_6,test_s)
#summary(score_lm-6)
### Evaluating the results
rmse_model_6<-RMSE(score_lm_hat_6,test_s$score)
results<-cbind(results,data.frame(Method=" Predicting score based on quality measures and altitude",Data="Standardized", RMSE = rmse_model_6,r.squared = summary(score_lm_6)$r.squared, Adjusted.r.squared=summary(score_lm_6)$adj.r.squared))
#####################################################################################
#### normalized data
###################################     model 7  linear regression with variable ##altitude only 

score_lm_7<-lm(score~altitude_mean_meters-quality,data = train_n)
score_lm_hat_7<-predict(score_lm_7,test_n)
#summary(score_lm_7)

### Evaluating the results
rmse_model_7<-RMSE(score_lm_hat_7,test_n$score)
results<-cbind(results,data.frame(Method=" Predicting score based on altitude",Data="Normalized", RMSE = rmse_model_7,r.squared = summary(score_lm_7)$r.squared, Adjusted.r.squared=summary(score_lm_7)$adj.r.squared))
#####################################################################################

###################################### model 8  linear regression with variable  only 
score_lm_8<-lm(score~.-altitude_mean_meters-quality,data = train_n)
score_lm_hat_8<-predict(score_lm_8,test_n)
#summary(score_lm_8)
### Evaluating the results
rmse_model_8<-RMSE(score_lm_hat_8,test_n$score)
results<-cbind(results,data.frame(Method=" Predicting score based on quality measures",Data="Normalized", RMSE = rmse_model_8,r.squared = summary(score_lm_8)$r.squared, Adjusted.r.squared=summary(score_lm_8)$adj.r.squared))
#####################################################################################

######################################## model 9  linear regression with variables

score_lm_9<-lm(score~.,data = train_n)
score_lm_hat_9<-predict(score_lm_9,test_n)
#summary(score_lm_9)

### Evaluating the results
rmse_model_9<-RMSE(score_lm_hat_9,test_n$score)
results<-cbind(results,data.frame(Method=" Predicting score based on quality measures and alitude",Data="Normalized", RMSE = rmse_model_9,r.squared = summary(score_lm_9)$r.squared, Adjusted.r.squared=summary(score_lm_9)$adj.r.squared))
#####################################################################################


####################################### model 10  knn
#### knn works with discrete variables and recommend standardize data
###3 with classification models i will quality variable instead of score

train_s<-subset(train_s,select = -c(score))
test_s<-subset(test_s,select = -c(score))

control<- trainControl(method="cv",number=10,p=.9)
train_knn<-train(quality~., method="knn",
                 data = train_s,
                 tuneGrid= data.frame(k=seq(3,71,2)),
                 trControl=control)
##### choosing the best k
ks<-train_knn$bestTune
#### ploting accuracy
knn_plot<-ggplot(train_knn,highlight = TRUE)

pred_knn<-predict(train_knn,test_s,k=ks)
cm1<-confusionMatrix(pred_knn,test_s$quality)

#########################################  model 11     tree partition 
train_set<-subset(train_set,select=-c(score))
test_set<-subset(test_set,select=-c(score))
train_rpart<-train(quality~.,
                   method="rpart",
                   tuneGrid=data.frame(cp=seq(0,0.05,len=10)),
                   data = train_set)
ggplot(train_rpart)

cp<-train_rpart$bestTune
fancyRpartPlot(train_rpart$finalModel)
pred_rpart<-predict(train_rpart,test_set)
cm2<-confusionMatrix(pred_rpart,test_set$quality)
cm2$overall

####################################### model 12 random forest

#### choose the best nodesize 
nodesize<-seq(1,150,10)
accuracy<-sapply(nodesize,function(ns){
  train(quality~.,
        method="rf",
        tuneGrid=data.frame(mtry=2),
        data = train_set)$results$Accuracy
})
qplot(nodesize,accuracy)
#### run the model for the best nodesize
model_forest<-randomForest(quality~., data = train_set,nodesize=nodesize[which.max(accuracy)],importance=TRUE)
pred_model_forest<-predict(model_forest,test_set)
confusionMatrix(pred_model_forest,test_set$quality)

plot(model_forest)
legend("top",colnames(model_forest$err.rate),col=1:4,cex=0.5,fill=1:4)


