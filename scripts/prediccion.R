#librer√≠as utilizadas
library (dplyr)
library (MASS) #stepwise
library (rpart)# √rbol
library(rpart.plot)
library(caret)

############################ STEPWISE #########################################

#Establecemos los valores inferior y superior para aplicar el modelo
install.packages("dplyr")
minimo <- glm(Churn.Value~1,data=datos,family = binomial(link="logit"))
completo <-glm(Churn.Value ~.,data=datos,family = binomial(link="logit"))
#Aplicamos el modelo stepwise ambas direcciones
var_sele <-stepAIC(minimo,scope=list(upper=completo),direction="both",
                   trace=FALSE,family = binomial(link="logit"))
summary(var_sele)
formula(var_sele)


############################ LOGIT ############################################## 

#se cogen las variables predictoras selccionadas con el modelo stepwise

variablesreg <- dplyr::select (balanced.datosclean, -Gender, -Phone.Service, -Churn.Value,
                               -CLTV, -Married, -Tenure.in.Months , -Avg.Monthly.Long.Distance.Charges, 
                               -Multiple.Lines, -Internet.Service, -Internet.Type, -Avg.Monthly.GB.Download, 
                               -Online.Backup , -Device.Protection.Plan , -Streaming.Movies, -Unlimited.Data, 
                               -Total.Refunds, -Total.Extra.Data.Charges, - Satisfaction.Score)

#para porbar si las que salen con un puntito mejoran o no el modelo (opcional)
variablesreg <- dplyr::select (balanced.datosclean, -Gender, -Phone.Service, -Churn.Value,
                               -CLTV, -Married, -Tenure.in.Months , -Avg.Monthly.Long.Distance.Charges, 
                               -Multiple.Lines, -Internet.Service, -Internet.Type, -Avg.Monthly.GB.Download, 
                               -Online.Backup , -Device.Protection.Plan , -Streaming.Movies, -Unlimited.Data, 
                               -Total.Refunds, -Total.Extra.Data.Charges, - Satisfaction.Score, "-Streaming.TV, -Monthly.charge.Cat")

#comprobamos y anadimos la variable objetivo en factor
str(variablesreg)
variablesreg$Churn.Value <- balanced.datosclean$Churn.Value

#creamos las dummies
datosfinalesdummies <- dummy_cols ( 
  variablesreg , 
  select_columns  =  NULL , 
  remove_first_dummy  = TRUE , 
  remove_most_frequent_dummy  =  FALSE , 
  ignore_na  =  FALSE , 
  split  =  NULL , 
  remove_selected_columns  =  TRUE
)

datosfinalesdummies$Churn.Value <- balanced.datosclean$Churn.Value

#hemos decidido que no escalamos los datos

#entrenamiento y test

split <- sample.split(datosfinalesdummies$Churn.Value, SplitRatio = 0.80)
train <- subset(datosfinalesdummies, split == TRUE)
test <- subset(datosfinalesdummies, split == FALSE)

#Comprobamos que los conjuntos de entrenamiento y validaci√≥n est√°n balanceados
prop.table(table(train$Churn.Value))
prop.table(table(test$Churn.Value))
prop.table(table(datosfinalesdummies$Churn.Value))

#Creamos modelo logit con las variables obtenidas de stepwise y predecimos
modelo.logit_step <- glm(Churn.Value ~ ., data = train, family = "binomial")
str(train)
#eliminamos depentes 7 y 8 por valores nulos al balancear
train <- train[,-8]
test <- test[,-8]

summary(modelo.logit_step)
prediccion <- predict(modelo.logit_step, test, type="response")
prediccion_01 <- ifelse(prediccion >0.5,1,0)

summary(modelo.logit_step)
modelo.logit_step$

#Aplicamos logit con todas las variables para ver las m√°s significativas 

modelo.logit_full <- glm(Churn.Value ~., data = train, family = "binomial")
summary(modelo.logit_full)


#install.packages("caret")
# Calculo las m√©tricas
confusionMatrix(as.factor(prediccion_01),as.factor(test$Churn.Value))

#curva roc
pred1 <- prediction(as.numeric(prediccion_01), as.numeric(test$Churn.Value))
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1, main="ROC RegresiÛn logÌstica")

############################ √ÅRBOL ############################################## 

modelo.arbol <- rpart (Churn.Value ~.,data = train, method = "class")
summary(modelo.arbol)

# Pinto arboll
rpart.plot(modelo.arbol2)
str(test)

# Hacemos predicci√≥n
prediccion_arbol <- predict(modelo.arbol,test, type="class")
summary (prediccion_arbol)
summary(as.factor(test$Churn.Value))
confusionMatrix(as.factor(prediccion_arbol),as.factor(test$Churn.Value))

#curva roc
pred2 <- prediction(as.numeric(prediccion_arbol), as.numeric(test$Churn.Value))
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2, main="ROC ¡rbol de decisiÛn")

importanciaarbol <- as.data.frame(modelo.arbol$variable.importance)
importanciaarbol <- rownames_to_column(importanciaarbol,var = "variable")

#######################Random Forest ###########################################

splitRandom <- sample.split(variablesreg$Churn.Value, SplitRatio = 0.80)
trainR <- subset(variablesreg, split == TRUE)
testR <- subset(variablesreg, split == FALSE)
str(train)
modelo.rf <- randomForest(Churn.Value ~.,data = trainR, importance = TRUE, 
                          ntree = 250)
pred_test_RF <- predict(modelo.rf, newdata = testR)
confusionMatrix(as.factor(pred_test_RF),as.factor(testR$Churn.Value))
summary(pred_test_RF)
summary(modelo.rf)
modelo.rf$importance

#curva roc
pred3 <- prediction(as.numeric(pred_test_RF), as.numeric(test$Churn.Value))
perf3 <- performance(pred3, "tpr", "fpr")
plot(perf3, main="ROC Random Forest")

#importancia con accuracy y gini en R.Forest

importancia <- as.data.frame(modelo.rf$importance)
importancia <- rownames_to_column(importancia,var = "variable")

p1 <- ggplot(data = importancia, aes(x = reorder(variable, MeanDecreaseAccuracy),
                                     y = MeanDecreaseAccuracy,
                                     fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "ReducciÛn de Accuracy") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia, aes(x = reorder(variable, MeanDecreaseGini),
                                     y = MeanDecreaseGini,
                                     fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "ReducciÛn de pureza (Gini)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)

#para arbol la importancia
library(dplyr)
imp_arbol =data.frame(varImp(modelo.arbol, scale=T)) %>%
  dplyr::mutate(variable=rownames(.)) %>% dplyr::rename(importance_modelo_arbol=Overall) %>%
  dplyr::arrange(-importance_modelo_arbol) %>%
  dplyr::mutate(rank_arbol=seq(1:nrow(.)))

#para regresion la importancia
library(dplyr)
imp_regresion =data.frame(varImp(modelo.logit_step, scale=T)) %>%
  dplyr::mutate(variable=rownames(.)) %>% dplyr::rename(importance_modelo_logit=Overall) %>%
  dplyr::arrange(-importance_modelo_logit) %>%
  dplyr::mutate(rank_logit=seq(1:nrow(.)))

#para random forest la importancia
library(dplyr)
imp_random =data.frame(varImp(modelo.rf, scale=T)) %>%
  dplyr::mutate(variable=rownames(.)) %>% dplyr::rename(importance_modelo_RandomForest=Overall) %>%
  dplyr::arrange(-importance_modelo_RandomForest) %>%
  dplyr::mutate(rank_RForest=seq(1:nrow(.)))
