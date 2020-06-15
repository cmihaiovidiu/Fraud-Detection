#Incarcare librarii
library(tidyverse)
library(tidymodels)
library(grid)
library(ggplot2)
library(doParallel)
library(e1071)
library(pROC)
library(h2o)
library(DMwR)
library(ROSE)
library(caret)
suppressMessages(library(reshape2))
suppressMessages(library(tidyverse))
suppressMessages(library(DataExplorer))
suppressMessages(library(ggplot2))
suppressMessages(library("gridExtra"))


#Incarcare datecsv
datecsv = read_csv("creditcard.csv")

introduce(datecsv)
plot_intro(datecsv)

#Verificare valori lipsa
for (Var in names(datecsv)) {
  valori_lipsa <- sum(is.na(datecsv[,Var]))
  if (valori_lipsa > 0) {
    print(c(Var,valori_lipsa))
  }
}

#Verificare valori constante
flag <- FALSE
for (f in names(datecsv)) {
  if (length(unique(datecsv[[f]])) == 1) {
    cat(f, "\n")
    flag <- TRUE
  }
}
if (flag == FALSE){
  print ("nu am gasit nici o variabila constanta")
}

#Verificare coloane egale
flag <- FALSE
features_pair <- combn(names(datecsv), 2, simplify = F)
for(pair in features_pair) {
  fp1 <- pair[1]
  fp2 <- pair[2]
  
  if (all(datecsv[[fp1]] == datecsv[[fp2]])) {
    cat(fp1, "si", fp2, "sunt egale.\n")
    flag <- TRUE
  }
}

if (flag == FALSE){
  print ("Nu s-a gasit nici o variabile egala")
}

datecsv=na.omit((datecsv))

summary(datecsv)

# Crearea Vectorului
datecsv <- datecsv %>% 
  mutate(Class = as.factor(Class)) 
levels(datecsv$Class) <- c('Normal', 'Fraud')

#Plotare
table(datecsv$Class)
ggplot(datecsv)+
  geom_bar(aes(x = Class, fill = Class))+ggtitle("Tranzactii Normal vs Fraud")

#Crearea a doua csv-uri tranzactii Fraud si Normal
 
dateFraudate <- subset(datecsv,Class=='Fraud')
dateNonFraudate <- subset(datecsv,Class=='Normal')


#Verificare tranzactii daca apar la un moment specific
#Creare variabila night bazata pe variabila de timp 
new_data<-na.omit(read_csv("creditcard.csv"))
new_data$Night <- as.factor((floor(new_data$Time/60/60)%%24 <= 9)*1)

#Tranzactile pe zile
datePlotare <- new_data
datePlotare$factClass <- as.factor(new_data$Class)
datePlotare <- table(datePlotare$Night, datePlotare$factClass)
datePlotare <- melt(datePlotare)
datePlotare$value[datePlotare$Var2==0] = datePlotare$value[datePlotare$Var2==0]/sum(datePlotare$value[datePlotare$Var2==0])
datePlotare$value[datePlotare$Var2==1] = datePlotare$value[datePlotare$Var2==1]/sum(datePlotare$value[datePlotare$Var2==1])
names(datePlotare) <- c("Noapte", "Fraud", "Procente")
datePlotare$Fraud <-as.factor(datePlotare$Fraud)
ggplot(datePlotare, aes(x=Fraud, y=Procente, fill=Fraud))+geom_bar(stat="identity")+
  facet_grid(~Noapte)+
  ggtitle("Tranzactiile impartite zi vs noapte")+
  scale_fill_discrete(name="Normal (0) | Fraud (1)")

# Variatia tranzactiilor Normal vs Fraud in variatie cu timpul

p1 <- ggplot(dateNonFraudate, aes(x = Time)) + geom_histogram(fill="light blue", colour = "black") + labs(title=expression("Tranzactii Normal") + stat_count())

p2 <- ggplot(dateFraudate, aes(x = Time)) + geom_histogram(fill="light green", colour = "black") + labs(title=expression("Tranzactii Fraud") + stat_count())

p3 <- ggplot(data = datecsv,aes(x=Time,fill=factor(Class)))+geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=mean(Time[Class=="Normal"])),color="red",linetype="dashed",lwd=0.5)+
  geom_vline(aes(xintercept=mean(Time[Class=="Fraud"])),color="blue",linetype="dashed",lwd=0.5)+ 
  scale_x_continuous(breaks = seq(50000,100000,150000))+
  xlab(label = "Time") +
  ggtitle("Releventa timpului in tranzactiilor normale vs fraudate")+
  theme_classic()

grid.arrange(p3, p1, p2)


#Verificare daca tranzactiile fraudate au legatura cu suma

p3 <- ggplot(dateFraudate, aes(x = Amount), binwidth = 2) + geom_histogram(fill="light blue", colour = "black") + labs(title=expression("Fraud Transactions")) + stat_count()

p4 <- ggplot(dateNonFraudate, aes(x = Amount), binwidth = 2) + geom_histogram(fill="light green", colour = "black") + labs(title=expression("Normal Transactions")) + stat_count()

grid.arrange(p3, p4)

#Quantilele Cumultative

quantile(dateNonFraudate$Amount, seq(0, 1, by=.2))

quantile(dateFraudate$Amount, seq(0, 1, by=.2))

#Graficele pentru quantile

quan <- melt(table(datecsv$Amount[datecsv$Class=='Normal']))
quan$CummulativePercentage <- cumsum(quan$value) / sum(quan$value) # cumulative Frequency
names(quan)[1] <- "Amount"
p5 <- ggplot(quan[quan$Amount<50,], aes(x=Amount, y=CummulativePercentage, color=CummulativePercentage))+
  geom_line()+ggtitle("Tranzactii Normale")

quan <- melt(table(datecsv$Amount[datecsv$Class=='Fraud']))
quan$CummulativePercentage <- cumsum(quan$value) / sum(quan$value)
names(quan)[1] <- "Amount"
p6 <- ggplot(quan[quan$Amount<50,], aes(x=Amount, y=CummulativePercentage, color=CummulativePercentage))+
  geom_line()+ggtitle("Tranzactii Fraudate")

grid.arrange(p5, p6)


#Verificare daca tranzactiile au legatura cu timpul si suma

p7<-ggplot(dateFraudate, aes(x=Time, y=Amount)) + 
  geom_point(colour="light blue") + ggtitle("Tranzactii fraudate") 

p8<-ggplot(dateNonFraudate, aes(x=Time, y=Amount)) + 
  geom_point(colour="light green")+ggtitle("Tranzactii Normale")

grid.arrange(p7, p8)

#Matricea de corelatie

plot_correlation(datecsv, maxcat = 8L)

# Care dintre variabile explica tranzactiile fraudate?
# Comparare intre tranzactiile normale vs cele fraudate

skew <- sum(as.numeric(datecsv$Class))/nrow(datecsv)
mugood <- apply(dateNonFraudate[sample(rownames(dateNonFraudate), size = as.integer(skew *nrow(datecsv)), replace = T), -c(1, 30, 31)], 2, mean)
muanom <- apply(dateFraudate[, -c(1, 30, 31)], 2, mean)
plot(muanom, col = "blue", xlab = "Features", ylab = "Mean")
lines(muanom, col = "blue", lwd = 2)
points(mugood, col = "green")
lines(mugood, col = "green", lwd = 2)
legend("topright", legend = c("Normal", "Fraud"), lty = c(1,1), col = c("green", "blue"), lwd = c(2,2))

#Boxplot
boxplot(datecsv[2:29])

# V1 

p1 <- ggplot(datecsv, aes(x = V1, fill = Class)) + 
   geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_density(geom = "line", aes(colour = Class), adjust = 10) + xlim(-20,20 ) + labs(title=expression("Distribution"))

p2 <- ggplot(datecsv, aes(y=V1, x=Class)) +  geom_boxplot(aes(fill = Class)) + labs(title=expression("Box Plot")) + xlab("Type of Transaction")

grid.arrange(p1, p2, top="V1 - Fraud vs Normal Transactions", ncol = 2)                                                                                



#Analog V2-V28

p1 <- ggplot(datecsv, aes(x = V2, fill = Class)) + 
   geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity') + stat_density(geom = "line", aes(colour = Class), adjust = 10) + xlim(-20,20 ) + labs(title=expression("Distribution"))

p2 <- ggplot(datecsv, aes(y=V2, x=Class)) +  geom_boxplot(aes(fill = Class)) + labs(title=expression("Box Plot")) + xlab("Type of Transaction")

grid.arrange(p1, p2, top="V2 - Fraud vs Normal Transactions", ncol = 2)  

# GBM

h2o.init()
gbm_date <- h2o.importFile("creditcard.csv")
response <- "Class"
gbm_date[[response]] <- as.factor(gbm_date[[response]])
predictors <- setdiff(names(gbm_date),response)

gbm_date_splits <- h2o.splitFrame( data = gbm_date, ratios = 0.7, seed = 1)

train_gbm <- gbm_date_splits[[1]]
test_gbm <- gbm_date_splits[[2]]

# Modelul de antrenare

gbm <- h2o.gbm(
  # Parametrii standar pentru acest model
  x = predictors, y = response, 
  training_frame = train_gbm, 
  validation_frame = test_gbm,
  # Cross validation 5 folds
  nfolds = 5,
  fold_assignment = "Stratified",
  balance_classes = TRUE,
  distribution = "bernoulli",
  # Cu cat avem mai multi "arbori" este mai bine daca si rata de invatare este mica 
  ntrees = 1000,
  learn_rate=0.01,                                                         
  # Oprirea mai devreme odata ce validarea auc nu se imbunatateste cu cel pu??in 0,01% pentru 5 evenimente consecutive 
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC",
  # sample 80% din randuri per arbore per tree
  sample_rate = 0.8,                                              
  # sample 80% din coloane per impartire
  col_sample_rate = 0.8,                                           
  seed = 1234,                                                      
  score_tree_interval = 10                                                 
  )

# Performanta modelului

gbm@model$cross_validation_metrics_summary
h2o.confusionMatrix(gbm, valid = TRUE)
h2o.varimp(gbm)
prediction = h2o.predict(gbm, newdata=test_gbm)
head(prediction)
pred = as.data.frame(prediction)
perf = h2o.performance(gbm, newdata=test_gbm)
h2o.auc(perf)
plot(perf, type = "roc")

# XGB
# Modelul de antrenare

xgb <- h2o.xgboost(
  x = predictors, y = response, 
  training_frame = train_gbm, 
  validation_frame = test_gbm,
  nfolds = 5,
  fold_assignment = "Stratified",
  distribution = "bernoulli",
  ntrees = 1000,
  learn_rate=0.01,                                                         
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC",
  sample_rate = 0.8,                                              
  col_sample_rate = 0.8,                                           
  seed = 1234,                                                      
  score_tree_interval = 10                                                 
)

# Performanta modelului

xgb@model$cross_validation_metrics_summary
h2o.confusionMatrix(xgb, valid = TRUE)
h2o.varimp(xgb)
prediction = h2o.predict(xgb, newdata=test_xgb)
head(prediction)
pred = as.data.frame(prediction)
perf = h2o.performance(xgb, newdata=test_xgb)
h2o.auc(perf)
plot(perf, type = "roc")

# Random Forest

# Prepararea datelor

train<- as.data.frame(train_gbm)
levels(train$Class) <- c("NotFraud", "Fraud")
test <- as.data.frame(test_gbm)
levels(test$Class) <- c("NotFraud", "Fraud")

basic_rec <- recipe(Class ~ ., data = train) %>% step_zv(all_predictors()) 

ctrl <- trainControl(
  # folosim cross validation 10 fold pentru resample
  method = "cv",
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = 'final',
  # facem si downspampling inloc de resampling
  sampling = 'down',
  verboseIter = FALSE)
rf_grid <- expand.grid(
  .mtry = c(6)
)

# Modelul de antrenare

cl <- makeCluster(8)
registerDoParallel(cl)

rf_mod <- train(basic_rec,
                data = train,
                method = 'rf',
                metric = "ROC",
                tuneGrid = rf_grid,
                trControl = ctrl,
                # setare cresterea pana la 500 de arbori
                ntree = 500,
                verbose = FALSE,
                importance = TRUE)

stopCluster(cl)

# Performanta modelului

imp <- varImp(rf_mod, scale = FALSE, 
              surrogates = FALSE, 
              competes = FALSE)
imp1 <- ggplot(imp, aes(x=reorder(rownames(imp)), y = importance)) +
  geom_bar(stat="identity", fill="lightblue", colour="black") +
  coord_flip() + 
  labs(title="Predictia folosind Random Forest", x="Variabila", y="Importanta variabilei")
imp1

draw_confusion_matrix <- function(test, title) {
  tst <- data.frame(test$predicted, test$Class)
  opts <-  c("Prediction", "True")
  names(tst) <- opts
  cf <- plyr::count(tst)
  cf[opts][cf[opts]==0] <- "Not Fraud"
  cf[opts][cf[opts]==1] <- "Fraud"
  
  ggplot(data =  cf, mapping = aes(x = True, y = Prediction)) +
    labs(title = "Confusion Matrix", subtitle = title) +
    geom_tile(aes(fill = freq), colour = "black") +
    geom_text(aes(label = sprintf("%1.0f", freq)), vjust = 1) +
    scale_fill_gradient(low = "lightgreen", high = "green") +
    theme_bw() + theme(legend.position = "none")}

test$predicted <- predict(rf_mod ,test)
confusionMatrix(rf_mod)
draw_confusion_matrix(test,"Random Forest")

print(rf_mod)
plot_roc <- function(x, ...) {
  roc_obj <- roc(
    response = x[["obs"]], 
    predictor = x[["Fraud"]], 
    levels = rev(levels(x$obs))
  )
  plot(roc_obj, ...)
  matplot(datecsv.frame(roc_obj$sensitivities, roc_obj$specificities), x = roc_obj$thresholds, 
      type='l', 
  xlab = 'threshold', ylab='TPR, TNR')
  legend('bottomright', legend=c('TPR', 'TNR'))
   # include the area under curve
  return(auc(roc_obj))
  }

plot_roc(rf_mod$pred,col = "lightblue")

# Modelul KNN

# Prepararea datelor
data_both <- ovun.sample(Class ~ ., data = train, method = "both", p=0.5, N=10000, seed = 123)$data
table(data_both$Class)

cl <- makeCluster(8)
registerDoParallel(cl)
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(222)
knn_both_mod <- train(Class~., data = data_both, trControl=control, method = "knn")

stopCluster(cl)

# Modelul de predictie pe setul de date
pred_both <- predict(knn_both_mod, newdata = test)

# Matricea de confuzie
confusionMatrix(test$Class, pred_both, positive = "Fraud")

# Concluzii
gbm_pred_df <- cbind(test,pred)

gbm_pred_df <- gbm_pred_df %>% 
  mutate(fraud = if_else(predict == 0, 1-p0, p0))

plot(roc(rf_mod$pred$obs, rf_mod$pred$Fraud, thresholds = 0.3), col = 'blue')
plot(roc(gbm_pred_df$Class, gbm_pred_df$fraud), add = TRUE, col = 'orange')


