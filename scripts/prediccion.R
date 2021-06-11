#librerías utilizadas
library (dplyr)
library (MASS) #stepwise
library (rpart)# árbol
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
#Creamos modelo logit con las variables obtenidas de stepwise y predecimos
modelo.logit_step <- glm(Churn.Value ~ Online.Security  + Contract +
                        Internet.Type + Number.of.Referrals + Married + 
                        Offer + Number.of.Dependents + Tenure.in.Months + Payment.Method +
                        Phone.Service + Multiple.Lines, data = train, family = "binomial")
summary(modelo.logit_step)
prediccion <- predict(modelo.logit_step, test, type="response")
prediccion_01 <- ifelse(prediccion >0.5,1,0)

#Aplicamos logit con todas las variables para ver las más significativas 

modelo.logit_full <- glm(Churn.Value ~., data = train, family = "binomial")
summary(modelo.logit_full)

#install.packages("caret")
# Calculo las métricas
confusionMatrix(as.factor(prediccion_01),as.factor(test$Churn.Value))

############################ ÁRBOL ############################################## 

modelo.arbol <- rpart (Churn.Value ~ Online.Security  + Contract +
                         Internet.Type + Number.of.Referrals + Married + 
                         Offer + Number.of.Dependents + Tenure.in.Months + Payment.Method +
                         Phone.Service + Multiple.Lines,data = train, method = "class")
summary(modelo.arbol)

# Pinto árbol
rpart.plot(modelo.arbol)

# Hacemos predicción
prediccion_arbol <- predict(modelo.arbol,test, type="class")
summary (prediccion_arbol)
summary(as.factor(test$Abandono))
confusionMatrix(as.factor(prediccion_arbol),as.factor(test$Churn.Value))
