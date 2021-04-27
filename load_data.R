#columnas con las que me quedo (totales)
# Telco_customer_churn$CustomerID
# Telco_customer_churn$Count
# Telco_customer_churn$Country
# Telco_customer_churn$State
# Telco_customer_churn$City
# Telco_customer_churn$`Zip Code`
# Telco_customer_churn$`Lat Long`
# Telco_customer_churn$Latitude
# Telco_customer_churn$Longitude
# Telco_customer_churn$Gender
# Telco_customer_churn$`Senior Citizen`
# Telco_customer_churn$Partner
#   Telco_customer_churn$Dependents
#   Telco_customer_churn$`Tenure Months`
#   Telco_customer_churn$`Phone Service`
#   Telco_customer_churn$`Multiple Lines`
#   Telco_customer_churn$`Internet Service`
#   Telco_customer_churn$`Online Security`
#   Telco_customer_churn$`Online Backup`
#   Telco_customer_churn$`Device Protection`
#   Telco_customer_churn$`Tech Support`
#   Telco_customer_churn$`Streaming TV`
#   Telco_customer_churn$`Streaming Movies`
#   Telco_customer_churn$Contract
#   Telco_customer_churn$`Paperless Billing`
#   Telco_customer_churn$`Payment Method`
#   Telco_customer_churn$`Monthly Charges`
#   Telco_customer_churn$`Total Charges`
#   Telco_customer_churn$`Churn Label`
#   Telco_customer_churn$`Churn Value`
#   Telco_customer_churn$`Churn Score`
#   Telco_customer_churn$CLTV
#   Telco_customer_churn$`Churn Reason`
#   Telco_customer_churn_demographics$Age
#   Telco_customer_churn_demographics$`Under 30`
#   Telco_customer_churn_demographics$Married
#   Telco_customer_churn_demographics$`Number of Dependents`
#   Telco_customer_churn_services$Quarter
#   Telco_customer_churn_services$`Referred a Friend`
#   Telco_customer_churn_services$`Number of Referrals`
#   Telco_customer_churn_services$`Tenure in Months`
#   Telco_customer_churn_services$Offer
#   Telco_customer_churn_services$`Avg Monthly Long Distance Charges`
#   Telco_customer_churn_services$`Internet Type`
#   Telco_customer_churn_services$`Avg Monthly GB Download`
#   Telco_customer_churn_services$`Device Protection Plan`
#   Telco_customer_churn_services$`Premium Tech Support`
#   Telco_customer_churn_services$`Streaming Music`
#   Telco_customer_churn_services$`Unlimited Data`
#   Telco_customer_churn_services$`Total Refunds`
#   Telco_customer_churn_services$`Total Extra Data Charges`
#   Telco_customer_churn_services$`Total Long Distance Charges`
#   Telco_customer_churn_services$`Total Revenue`
#  Telco_customer_churn_status$`Satisfaction Score`
#  Telco_customer_churn_status$`Customer Status`
#  Telco_customer_churn_status$`Churn Category`

library(readxl)

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

#Para utilizar la función select es necesaria la librería dplyr
#install.packages("dplyr") 
library (dplyr)

#Quitamos las columnas que no son útiles para el análisis
datos <- select (datos, -Count, -Churn.Label, -Country, -Zip.Code, -Lat.Long, 
                 -Latitude, -Longitude, -Partner, -Tenure.Months, 
                 -Device.Protection, -Tech.Support, -Monthly.Charges, -Quarter,
                 -Dependents, -Referred.a.Friend)
