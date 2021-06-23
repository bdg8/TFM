#librerias
library(fastDummies)
library(psych)
install.packages("MASS")
library (MASS) 
library(dplyr)


#correlacion grafiquito nuevo

#estandarizamos/normalizamos los datos numéricos
datosnumericos <- datosnumericos[,-12]
datos_scaled <- scale(datosnumericos)
datos_scaled <- data.frame(datos_scaled)

ggcorr(datos_scaled, hjust = 1, size = 4, label = T)
ggcorr(datos_scaled, label = T)
str(datosclean)
datos_scaled <- datos_scaled[,-9]

str(datosclean)
datosclean  <- datosclean[,-6]
datosclean$Edad.Grupos <- df_final_prueba$EdadGrupos

#grafiquitas para ver la proporcion del churn en los datos (antes del balanceo y post balanceo)

datosclean %>% 
  group_by(Churn.Value) %>% 
  summarise(Number = n()) %>%
  mutate(Porcentaje = prop.table(Number)*100) %>% 
  ggplot(aes(Churn.Value, Porcentaje)) + 
  geom_col(aes(fill = Churn.Value)) +
  labs(title = "Churn Porcentaje") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = sprintf("%.2f%%", Porcentaje)), hjust = 0.01,vjust = -0.5, size = 4) +
  theme_minimal()

balanced.datosclean %>% 
  group_by(Churn.Value) %>% 
  summarise(Number = n()) %>%
  mutate(Porcentaje = prop.table(Number)*100) %>% 
  ggplot(aes(Churn.Value, Porcentaje)) + 
  geom_col(aes(fill = Churn.Value)) +
  labs(title = "Churn Porcentaje") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = sprintf("%.2f%%", Porcentaje)), hjust = 0.01,vjust = -0.5, size = 4) +
  theme_minimal()

######################################################################################################################
#balanceo de los datos 
install.packages("ROSE")
library(ROSE)

#balanceo tipo under
balanced.datosclean <- ovun.sample(Churn.Value ~.,
                                   data= datosclean,
                                   seed = 123,
                                   method = "under")$data
table(balanced.datosclean$Churn.Value)

#me doy cuenta de que estan metidas las numericas que no deben estar y no estan aplicados algunas transformaciones 
#en el satisfactionscore y numberofreferrals

#creo las variables dummies para las categoricas 
#cambio el datosnuericos por datosclean
datosdummiesbalanced <- dummy_cols ( 
  balanced.datosclean , 
  select_columns  =  NULL , 
  remove_first_dummy  =  FALSE , 
  remove_most_frequent_dummy  =  FALSE , 
  ignore_na  =  FALSE , 
  split  =  NULL , 
  remove_selected_columns  =  TRUE
)

str(balanced.datosclean)
datosdummiesbalanced <- scale(datosdummiesbalanced)
datosdummiesbalanced <- data.frame(datosdummiesbalanced)

KMO(variableskmo)
cor.gor=round(cor(variableskmo),3)
det(cor.gor)
KMO(cor.gor)

#el primer modelito te da un output con la variable redundante
abc <- Hmisc::redun(~ .,data=datosdummiesbalanced, nk=0)
abc$Out

variablessw <- dplyr::select (datosdummiesbalanced, -"Number.of.Referrals_8", -"Number.of.Dependents_4", -"Satisfaction.Score_4" ,-           "Offer_Offer.A" ,                 
                       - "Contract_Two.Year"  ,-             "Internet.Service_No" ,-            "Internet.Type_None"    ,-          "Internet.Service_Yes"  ,         
                       -"Edad.Grupos_51.a.60" ,-            "Churn.Value_0"  ,- "Churn.Value_1"  ,-              "Premium.Tech.Support_No"  ,-       "Married_Yes"  ,                  
                       - "Payment.Method_Mailed.Check" ,-    "Streaming.Movies_No" ,-            "Streaming.Music_No"  ,-            "Monthly.charge.Cat_100...118.25",
                       - "Streaming.TV_No"       ,-          "Multiple.Lines_No"     ,-          "Online.Security_No"   ,-           "Gender_Female" ,                 
                       - "Online.Backup_No"    ,-            "Paperless.Billing_No"  ,-          "Device.Protection.Plan_No"  ,-     "Phone.Service_No" ,              
                       - "Unlimited.Data_No" )             
                       
#probamos si seguimos teniendo variables redundantes                      
                  
str(variablessw)   
abcd <- Hmisc::redun(~ .,data=variablessw, nk=0)
abcd$Out
                       
#salen algunas pero preferimos pasar al modelo con todas las variables dejando fuera unicamente una dummy

minimo <- glm(Churn.Value~1,data=variablessw,family = binomial(link="logit"))
completo <-glm(Churn.Value ~.,data=variablessw,family = binomial(link="logit"))

#Aplicamos el modelo stepwise ambas direcciones
modelo_stepwise <-stepAIC(minimo,scope=list(upper=completo),direction="both",
                         trace=FALSE,family = binomial(link="logit"))

summary(modelo_stepwise)
formula(modelo_stepwise)

str(variablessw) #para crear entrenamiento y val

#segumos realizando transformaciones (no recuerdo bien lo que era aqui)

variableskmo <- select (variableskmo, -"Number.of.Referrals_0", -"Churn.Value_0", -"Satisfaction.Score_3" )

variableskmo <- select (variableskmo, -"Monthly.Charge")

str(variableskmo)

#creo dataset para variables del stepwise
variablesmodelo <- variableskmo
#le meto la variable churn normal 
variablesmodelo$Churn.Value <- datosnumericos$Churn.Value

#le quito la variable churn dummie que quedba, y le anado el monthly.charge(escalada)
variablesmodelo <- select(variablesmodelo, -"Churn.Value_1")
variablesmodelo$Monthly.Charge <- datosdummiesnormal2$Monthly.Charge
#compruebo

str(variablesmodelo)
ab <- Hmisc::redun(~ .,data=variablesmodelo, nk=0)
ab$Out
vif(ab[,57])

#me dice que saque a monthly charge :(

############################ STEPWISE ######################################### 
#voy a prbar quitando el monthly charge
variablesmodelo <- variablesmodelo[,-58]
#Establecemos los valores inferior y superior para aplicar el modelo
install.packages("dplyr")
library(dplyr)

str(variablesmodelo)

minimo <- glm(Churn.Value~1,data=variablesmodelo,family = binomial(link="logit"))
completo <-glm(Churn.Value ~.,data=variablesmodelo,family = binomial(link="logit"))

#Aplicamos el modelo stepwise ambas direcciones
variables_sele <-stepAIC(minimo,scope=list(upper=completo),direction="both",
                   trace=FALSE,family = binomial(link="logit"))

summary(variables_sele)
formula(variables_sele)

warnings()
alias( lm( Churn.Score ~.-Churn.Value , data= variablesmodelo) )

lm2<-lm(Churn.Score ~.-Churn.Value,data=variablesmodelo)
vif(lm2)
car::vif(lm2)

#sacamos el internet service yes
variablesmodelo <- variablesmodelo[,-8]

str (datosexportar)

datosexportar <- datosclean
datosexportar$Churn.Category <- dfdata3$Churn.Category
datosexportar$Churn.Reason <- dfdata3$Churn.Reason
datosexportar$cluster <- df_final_prueba$clustering
datosexportar$City <- dfdata4$City
datosexportar$Churn.Label <- dfdata3$Churn.Label
datosexportar$Edad.Grupos <- df_final_prueba$EdadGrupos
datosexportar$Monthly.Charge.Cat <- df_final_prueba$Monthly.Charge.Cat

datosexportar$Churn.Label <- as.factor(datosexportar$Churn.Label)
datosexportar$Churn.Category <- as.factor(datosexportar$Churn.Category)
datosexportar$Churn.Reason <- as.factor(datosexportar$Churn.Reason)