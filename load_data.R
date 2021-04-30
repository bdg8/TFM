#librerías utilizadas
library(readxl)
library (dplyr)

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
datoscopia <-  select (datos, -Count, -Senior.Citizen, -Churn.Label, -Country, -Zip.Code, -Lat.Long, 
                 -Latitude, -Longitude, -Partner, -Tenure.Months, 
                 -Device.Protection, -Tech.Support, -Monthly.Charges, -Quarter,
                 -Dependents, -Referred.a.Friend, -Under.30, -State, -Customer.Status)

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
datoscopia <- select(datoscopia, -Churn.Category, -Churn.Reason, -CustomerID)

#observamos los datos
str(datoscopia)
summary(datoscopia)
# el comando describe nos muestra un panorama m�s completo de las variables. En el caso de las num�ricas 
#nos muestra los estad�sticos descriptivos, la cantidad de observaciones, los valores perdidos. Para las 
#variables categ�ricas, muestra la frecuencia, las proporciones y los valores perdidos.
install.packages("Hmisc")
library(Hmisc)
describe(datoscopia)

#vamos a crear una copia de los datos solo con las variables que necesitamos de momento para iniciarnos con la
#segmentacion de los clientes, para ello, deben ser todas numericas para estudiar la correlacion. 

datosnumericos <-  select (datoscopia, - Gender, -Phone.Service, -Paperless.Billing, -Married, 
                           -Offer, -Multiple.Lines, -Internet.Service, -Internet.Type, -Online.Security,
                           -Online.Backup, -Device.Protection.Plan, -Premium.Tech.Support, -Streaming.TV,
                           -Streaming.Music, -Streaming.Movies, -Unlimited.Data,-Contract, -Payment.Method,
                           -City)

# esto lo dejo aqui por si necesitamos en algun momento convertir las variables que no son numericas a factor, pero 
#no le eches cuenta ahora mismo

datosnumericos$Gender <- as.factor(datosnumericos$Gender)
datosnumericos$Phone.Service <- as.factor(datosnumericos$Phone.Service)
datosnumericos$Paperless.Billing <- as.factor(datosnumericos$Paperless.Billing)
datosnumericos$Married <- as.factor(datosnumericos$Married)
datosnumericos$Offer <- as.factor(datosnumericos$offer)
datosnumericos$Multiple.Lines <- as.factor(datosnumericos$Multiple.Lines)
datosnumericos$Internet.Service <- as.factor(datosnumericos$Internet.Service)
datosnumericos$Internet.Type <- as.factor(datosnumericos$Internet.Type)
datosnumericos$Online.Security <- as.factor(datosnumericos$Online.Security)
datosnumericos$Online.Backup <- as.factor(datosnumericos$Online.Backup)
datosnumericos$Device.Protection.Plan <- as.factor(datosnumericos$Device.Protection.Plan)
datosnumericos$Premium.Tech.Support <- as.factor(datosnumericos$Premium.Tech.Support)
datosnumericos$Streaming.TV <- as.factor(datosnumericos$Streaming.TV)
datosnumericos$Streaming.Movies <- as.factor(datosnumericos$Streaming.Movies)
datosnumericos$Streaming.Music <- as.factor(datosnumericos$Streaming.Music)
datosnumericos$Unlimited.Data <- as.factor(datosnumericos$Unlimited.Data)
datosnumericos$Contract <- as.factor(datosnumericos$Contract)
datosnumericos$Payment.Method <- as.factor(datosnumericos$Payment.Method)

#estandarizamos/normalizamos los datos numericos
datos_scaled <- scale(datosnumericos[,-1])
datos_scaled <- data.frame(datos_scaled)

#realizamos analisis preliminar de los datos
correlacion<-round(cor(datos_scaled), 1)
library(tidyverse)
library(corrplot)

#matriz de correlacion
corrplot(correlacion, method="number", type="upper")

#para ver si la correlacion es estadisticamente significativa con un nivel de significancia 
#del 5% segun el pvalue
rcorr(as.matrix(datos_scaled))

#con este comando lo vemos todo, la correlacion, la significancia y la dispersion 
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(datos_scaled, histogram = F, pch = 19)

#a�ado la columna de customerID a la tabla normalizada
datos_scaled$CustomerID <- datoscopia$CustomerID

#empezamos a analizar el numero optimo de clusters
#probamos primero con factoextra
install.packages("factoextra")
library(factoextra)
fviz_nbclust(datos_scaled[,-16],kmeans)
#nos dice que el optimo seria 2 o 4 

#Probamos con la metrica de Within cluster Sum of Squares (wss)
fviz_nbclust(datos_scaled[,-16],kmeans, method="wss")
#diria que es entre 4 o 5 pero no lo tengo claro (es el del codo)

#probamos ahora con la libreria NbClust (tarda un buen rato)
library(NbClust)

NbClust(datos_scaled[,-16], min.nc = 2, max.nc=8, method="kmeans")

#Aplicamos el método stepwise para seleccionar las variables
#Instalamos el paquete MASS para ello
#install.packages("MASS") 
library (MASS)

#Convierto la variable Churn a factor
datoscopia$Churn.Value<-as.factor(datoscopia$Churn.Value)
#Establecemos los valores inferior y superior para aplicar el modelo
inferior <- glm(Churn.Value~1,data=datoscopia,family = binomial(link="logit"))
full<-glm(Churn.Value ~.,data=datoscopia,family = binomial(link="logit"))
#Aplicamos el modelo stepwise
var_sele <-stepAIC(inferior,scope=list(upper=full),direction="both",
                trace=FALSE,family = binomial(link="logit"))
summary(var_sele)
formula(var_sele)
