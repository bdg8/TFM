#librerías utilizadas
library(readxl)
library (dplyr)
library(Hmisc)#describe
library(PerformanceAnalytics)
library(tidyverse)
library(corrplot)
library(fastDummies)


#Seteamos el directorio de trabajo a la ruta de este fichero 
setwd(dirname(rstudioapi::getSourceEditorContext()$path));
Tcc_demographics <- read_excel("./data/Telco_customer_churn_demographics.xlsx");
Telco_customer_churn_services <- read_excel("./data/Telco_customer_churn_services.xlsx");
Telco_customer_churn_status <- read_excel("./data/Telco_customer_churn_status.xlsx");
Telco_customer_churn <- read_excel("./data/Telco_customer_churn.xlsx");

#Renombro la variable Customer ID para quitarle el espacio en todas las bases de
#datos que lo contengan
names(Tcc_demographics)[names(Tcc_demographics) == "Customer ID"] <- "CustomerID"
names(Telco_customer_churn_services)[names(Telco_customer_churn_services) == "Customer ID"] <- "CustomerID"
names(Telco_customer_churn_status)[names(Telco_customer_churn_status) == "Customer ID"] <- "CustomerID"

#Ordenamos los registros en orden alfabético en función del CustomerID
Tcc_demographics = Tcc_demographics[order(Tcc_demographics$CustomerID),]
Telco_customer_churn_services = Telco_customer_churn_services[order(Telco_customer_churn_services$CustomerID),]
Telco_customer_churn_status = Telco_customer_churn_status[order(Telco_customer_churn_status$CustomerID),]
Telco_customer_churn = Telco_customer_churn[order(Telco_customer_churn$CustomerID),]

#Convierto a data frame mis datos para poder usar después la función merge
dfdata1 = data.frame(Tcc_demographics)
dfdata2 = data.frame(Telco_customer_churn_services)
dfdata3 = data.frame(Telco_customer_churn_status)
dfdata4 = data.frame(Telco_customer_churn) 

#Quitamos las columnas repetidas ya que generan conflicto con el resto de datos
drop <- c("Multiple.Lines","Internet.Service", "Online.Security", "Online.Backup",
          "Streaming.TV", "Streaming.Movies", "Contract", "Payment.Method", 
          "Total.Charges", "Churn.Score", "Churn.Reason")
dfdata4 = dfdata4[,!(names(dfdata4) %in% drop)]

#Hago el merge para juntar todas las tablas
datos <- merge(dfdata1, dfdata2)
datos <- merge(datos, dfdata3)
datos <- merge(datos, dfdata4)

#Quitamos las columnas que no son útiles para el análisis
datos <-  select (datos, -Count, -Senior.Citizen, -Churn.Label, -Country, -Zip.Code, -Lat.Long, 
                 -Latitude, -Longitude, -Partner, -Tenure.Months, 
                 -Device.Protection, -Tech.Support, -Monthly.Charges, -Quarter,
                 -Dependents, -Referred.a.Friend, -Under.30, -State, -Customer.Status)

#Convierto la variable Churn a factor
datos$Churn.Value<-as.factor(datos$Churn.Value)
#Frecuencia de la tasa de abandono
table(datos$Churn.Value)
#Miramos si hay valores nulos
table(is.na(datos))
#Vemos las variables para buscar valores faltantes
apply(is.na(datos), 2, which)
#Miramos cuántos valores nulos hay en Churn Category
table(is.na(datos$Churn.Category))
#Miramos cuántos valores nulos hay en Churn Reason
table(is.na(datos$Churn.Reason))
#Quitamos estas últimas columnas que no son útiles para el análisis y CustomerID que tampoco aporta 
datos <- select(datos, -Churn.Category, -Churn.Reason, -CustomerID)

#observamos los datos
str(datos)
summary(datos)
# el comando describe nos muestra un panorama m�s completo de las variables. En el caso de las num�ricas 
#nos muestra los estad�sticos descriptivos, la cantidad de observaciones, los valores perdidos. Para las 
#variables categ�ricas, muestra la frecuencia, las proporciones y los valores perdidos.
#install.packages("Hmisc")
describe(datos)

###############################################################################
###############################################################################

#vamos a crear una copia de los datos solo con las variables que necesitamos de momento para iniciarnos con la
#segmentacion de los clientes, para ello, deben ser todas numericas para estudiar la correlacion. 

datosnumericos <-  select (datos, - Gender, -Phone.Service, -Paperless.Billing, -Married, 
                           -Offer, -Multiple.Lines, -Internet.Service, -Internet.Type, -Online.Security,
                           -Online.Backup, -Device.Protection.Plan, -Premium.Tech.Support, -Streaming.TV,
                           -Streaming.Music, -Streaming.Movies, -Unlimited.Data,-Contract, -Payment.Method,
                           -City)

###############################################################################
###############################################################################
datosfactor <- datos

datosfactor$Gender <- as.factor(datosfactor$Gender)
datosfactor$Phone.Service <- as.factor(datosfactor$Phone.Service)
datosfactor$Paperless.Billing <- as.factor(datosfactor$Paperless.Billing)
datosfactor$Married <- as.factor(datosfactor$Married)
datosfactor$Offer <- as.factor(datosfactor$Offer)
datosfactor$Multiple.Lines <- as.factor(datosfactor$Multiple.Lines)
datosfactor$Internet.Service <- as.factor(datosfactor$Internet.Service)
datosfactor$Internet.Type <- as.factor(datosfactor$Internet.Type)
datosfactor$Online.Security <- as.factor(datosfactor$Online.Security)
datosfactor$Online.Backup <- as.factor(datosfactor$Online.Backup)
datosfactor$Device.Protection.Plan <- as.factor(datosfactor$Device.Protection.Plan)
datosfactor$Premium.Tech.Support <- as.factor(datosfactor$Premium.Tech.Support)
datosfactor$Streaming.TV <- as.factor(datosfactor$Streaming.TV)
datosfactor$Streaming.Movies <- as.factor(datosfactor$Streaming.Movies)
datosfactor$Streaming.Music <- as.factor(datosfactor$Streaming.Music)
datosfactor$Unlimited.Data <- as.factor(datosfactor$Unlimited.Data)
datosfactor$Contract <- as.factor(datosfactor$Contract)
datosfactor$Payment.Method <- as.factor(datosfactor$Payment.Method)
datosfactor$City <- as.factor(datosfactor$City)

#elimino city porque tendria 1129 levels en factor 
datosfactor <- datosfactor[,-35]

###############################################################################
###############################################################################

var_sele_step <- c("Churn.Value","Churn.Score", "Online.Security", "Contract", "Internet.Type",
                   "Number.of.Referrals", "Married", "Offer", "Number.of.Dependents",
                   "Tenure.in.Months","Payment.Method", "Phone.Service", "Multiple.Lines")
var_num_step <- c("Churn.Score", "Number.of.Referrals","Number.of.Dependents",
                  "Tenure.in.Months")
datos_sel_step<-datosfactor[,var_sele_step] 
datos_scaled_sw <- scale(datos_sel_step[,var_num_step])
datos_scaled_sw <- data.frame(datos_scaled_sw, datos_sel_step[,!(names(datos_sel_step) %in% var_num_step)])

###############################################################################
###############################################################################

#estandarizamos/normalizamos los datos numericos
datos_scaled <- scale(datosnumericos[,-1])
datos_scaled <- data.frame(datos_scaled)

#realizamos analisis preliminar de los datos
correlacion<-round(cor(datos_scaled), 1)

#matriz de correlacion
corrplot(correlacion, method="number", type="upper")

#para ver si la correlacion es estadisticamente significativa con un nivel de significancia 
#del 5% segun el pvalue
rcorr(as.matrix(datos_scaled))

#con este comando lo vemos todo, la correlacion, la significancia y la dispersion 
#install.packages("PerformanceAnalytics")
chart.Correlation(datos_scaled, histogram = F, pch = 19)

#a�ado la columna de customerID a la tabla normalizada
datos_scaled$CustomerID <- datos$CustomerID

###############################################################################
###############################################################################

#creo las variables dummies para las categoricas 
datosdummies <- dummy_cols ( 
  datosnumericos , 
  select_columns  =  NULL , 
  remove_first_dummy  =  FALSE , 
  remove_most_frequent_dummy  =  FALSE , 
  ignore_na  =  FALSE , 
  split  =  NULL , 
  remove_selected_columns  =  TRUE
)

###############################################################################
###############################################################################

#Para seguir con el PCA tenemos que normalizar los datos 
#El proceso de PCA identifica aquellas direcciones en las que la varianza es mayor. 
#Como la varianza de una variable se mide en su misma escala elevada al cuadrado, 
#si antes de calcular las componentes no se estandarizan todas las variables para 
#que tengan media 0 y desviaci?n est?ndar 1, aquellas variables cuya escala sea mayor 
#dominar?n al resto. De ah? que sea recomendable estandarizar siempre los datos.

datosdummiesnormal <- scale(datosdummies)
datosdummiesnormal <- data.frame(datosdummiesnormal)

###############################################################################
###############################################################################

#Seleccionar variables a utilizar en el modelo de cluster con PAM
#nos basamos en el PCA

datosPAM = datosfactor %>% select(Gender, Phone.Service, Paperless.Billing, Churn.Value, CLTV,
                                   Age, Married, Number.of.Referrals, Tenure.in.Months, Offer, 
                                   Device.Protection.Plan, Streaming.Music, Contract, Payment.Method, 
                                   Monthly.Charge, Churn.Score, Satisfaction.Score, Total.Extra.Data.Charges)

