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

#analisis exploratorio de los datos 
#para poder reducir la dimension de los datos sin perder informacion lo mas adecaudo es el PCA
#analisis de componentes princpales
#Recordar que el PCA solo puede aplicarse a datos num�ricos. 
#Si los datos contienen variables categ�ricas, deben ser convertidas a num�ricas.
str(datoscopia)
datosnumericos <- datoscopia

datosnumericos$Gender <- as.factor(datosnumericos$Gender)
datosnumericos$Phone.Service <- as.factor(datosnumericos$Phone.Service)
datosnumericos$Paperless.Billing <- as.factor(datosnumericos$Paperless.Billing)
datosnumericos$Married <- as.factor(datosnumericos$Married)
datosnumericos$Offer <- as.factor(datosnumericos$Offer)
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
datosnumericos$City <- as.factor(datosnumericos$City)

#elimino city porque tendria 1129 levels en factor 
datosnumericos <- datosnumericos[,-35]

#creo las variables dummies para las categoricas 
library(fastDummies)
datosdummies <- dummy_cols ( 
  datosnumericos , 
  select_columns  =  NULL , 
  remove_first_dummy  =  FALSE , 
  remove_most_frequent_dummy  =  FALSE , 
  ignore_na  =  FALSE , 
  split  =  NULL , 
  remove_selected_columns  =  TRUE
)

#Para seguir con el PCA tenemos que normalizar los datos 
#El proceso de PCA identifica aquellas direcciones en las que la varianza es mayor. 
#Como la varianza de una variable se mide en su misma escala elevada al cuadrado, 
#si antes de calcular las componentes no se estandarizan todas las variables para 
#que tengan media 0 y desviaci�n est�ndar 1, aquellas variables cuya escala sea mayor 
#dominar�n al resto. De ah� que sea recomendable estandarizar siempre los datos.

datosdummiesnormal <- scale(datosdummies)
datosdummiesnormal <- data.frame(datosdummiesnormal)

library(FactoMineR)
#creamos el objeto del Analisis PCA
AnalisisPCA<- PCA(X = datosdummiesnormal, scale.unit = FALSE, ncp = 65, graph = TRUE)

#el mismo pero dejamos que la funcion escale los datos en vez de escarlo nosotras #
#(se puede elegir una u otra)
AnalisisPCA2<- PCA(X = datosdummies, scale.unit = TRUE, ncp = 65, graph = TRUE)

#esto te da uno resultados que estan recogidos en el objeto AnalisisPCA y usando el $ puedes trastear.

# ver la calidad de representacion ene l mapa de factores
library(factoextra)
fviz_pca_var(AnalisisPCA2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE )

#para ver la contribuccion de las variables en cada dimension (matriz)
corrplot(AnalisisPCA$var$contrib, is.corr=FALSE)  
 

#cantidad de variacion retenida por cada componente principal (varianza explicada)
get_eigenvalue(AnalisisPCA2)

#como necesitamos muchos componentes principales para explicar la varianza total 
#de nuestros datos, vamos a buscar el punto de inflexion de cuantos serian nuestros 
#componentes optimos, el porcentaje debemos pensar nosotras cual queremos
fviz_eig(AnalisisPCA2, addlabels = TRUE, ylim = c(0, 50))

library(corrplot)
corrplot(AnalisisPCA$var$cos2, is.corr = FALSE)

# Contributions of variables to PC1
fviz_contrib(AnalisisPCA, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(AnalisisPCA, choice = "var", axes = 2, top = 10)
get_eigenvalue(AnalisisPCA)

#contribuccion total
dim1 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 1, top = 5)
dim2 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 2, top = 5)
dim3 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 3, top = 5)
dim4 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 4, top = 5)
dim5 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 5, top = 5)
dim6 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 6, top = 5)
dim7 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 7, top = 5)
dim8 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 8, top = 5)
dim9 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 9, top = 5)
dim10 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 10, top = 5)
dim11 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 11, top = 5)
dim12 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 12, top = 5)
dim13 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 13, top = 5)
dim14 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 14, top = 5)
dim15 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 15, top = 5)
dim16 <- fviz_contrib(AnalisisPCA, choice = "var", axes = 16, top = 5)

library(ggpubr)

ggarrange(dim1, dim2,dim3, dim4, dim5, dim6, dim7, dim8, dim9, dim10, dim11, dim12, dim13, dim14,
          dim15, dim16,
          labels = c(""),
          ncol = 4, nrow = 4)
fviz_contrib(AnalisisPCA, choice = "var", axes = 1:16, top = 16)

#vamos a crear una variable que nos ayude a agrupar los datos con la ayuda de kmeans
#esta agrupando variables, no individuos

kmeansPCAvar <- kmeans(AnalisisPCA$var$coord, centers = 4, nstart = 50)
gruposvar <- as.factor(kmeansPCAvar$cluster)
# Color variables by groups
fviz_pca_var(AnalisisPCA, col.var = gruposvar, 
             palette = c("#0073C2FF", "#EFC000FF", "#FC4E07", "#00AFBB"),
             legend.title = "Cluster")

#vamos a obserar los individuos
fviz_pca_ind(AnalisisPCA, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE )

#creamos el nuevo dataframe con las variables seleccionadas y las respectivas dummies
datosvarselec <- select (datosdummies, Monthly.Charge, Churn.Value_0, Churn.Value_1, Phone.Service_Yes,
                         Phone.Service_No, Streaming.Music_Yes, Streaming.Music_No, Married_Yes, Married_No,
                         Gender_Male, Gender_Female, Age, Offer_None, "Offer_Offer A", "Offer_Offer B",
                         "Offer_Offer C", "Offer_Offer D", "Offer_Offer E", "Payment.Method_Bank Withdrawal",
                         "Payment.Method_Credit Card","Payment.Method_Mailed Check", Satisfaction.Score_1, 
                         Satisfaction.Score_2, Satisfaction.Score_3, Satisfaction.Score_4, Satisfaction.Score_5,
                         "Contract_Month-to-Month", "Contract_One Year", "Contract_Two Year",
                         Device.Protection.Plan_No, Device.Protection.Plan_Yes ,Number.of.Referrals, 
                         Paperless.Billing_No, Paperless.Billing_Yes, Churn.Score, Total.Extra.Data.Charges)
str(datosvarselec)

#empezamos con kmeans
#primero factoextra
library(factoextra)
fviz_nbclust(datosvarselec,kmeans)

#Probamos con la metrica de Within cluster Sum of Squares (wss)
fviz_nbclust(datosvarselec,kmeans, method="wss")

#escalamos los datos
datosvarselecnormal <- scale(datosvarselec)
datosvarselecnormal <- data.frame(datosvarselecnormal)
#probamos ahora con la libreria NbClust (tarda un buen rato)
library(NbClust)

NbClust(datosvarselecnormal, min.nc = 2, max.nc=8, method="kmeans")

#PROCEDEMOS AHORA A APLICAR EL ALGORITMO KMEAS, CON DIFERENTES K Y VER LAS METRICAS QUE OBTENEMOS

kmeans(datosvarselec, 4, iter.max = 20, nstart = 50)
kmeans(datos_scaled[,-17], 4, iter.max = 10, nstart = 10)
kmeans(datos_scaled[,-17], 5, iter.max = 10, nstart = 10)
kmeans(datos_scaled[,-17], 2, iter.max = 10, nstart = 10)

#creamos el objeto y lo representamos 
agrupamiento <- kmeans(datosvarselec, 4, iter.max = 20, nstart = 50)
table(agrupamiento$cluster)

#########################################################################################
#########################################################################################
########################################_______________MODELO PAM
#str(datosnumericos)
#en datos numericos ya estan todas las variables categ en factor

#vamos a aplicar el modelo PAM, y determinaremos el numero de k con siluette
#carga librerías
library(dplyr)
library(cluster)
library(ggplot2)
install.packages("Rtsne")
library(Rtsne)
library(ggpubr)

#Seleccionar variables a utilizar en el modelo de cluster con PAM
#nos basamos en el PCA

datos2 = datosnumericos %>% select(Gender, Phone.Service, Paperless.Billing, Churn.Value, CLTV,
                                   Age, Married, Number.of.Referrals, Tenure.in.Months, Offer, 
                                   Device.Protection.Plan, Streaming.Music, Contract, Payment.Method, 
                                   Monthly.Charge, Churn.Score, Satisfaction.Score, Total.Extra.Data.Charges)

#calcular distancias tipo gower

gower_datos2 <- daisy(datos2,
                      metric = "gower")

summary(gower_datos2)

#Calcular silhouette widht para decidir numero de clusters

silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gower_datos2),
                     diss = TRUE, k = i)
  silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
}
plot(1:10, silhouette, xlab = "Clusters",
     ylab = "Silhouette Width") 

lines(1:10, silhouette)

kp <- pam(datos2, 4 )
clusplot( datos2, kp$cluster, color = TRUE, 
          shade = FALSE, labels = 4, lines = 1,
          col.p = "#FC4E07" )
#empezamos a analizar el numero optimo de clusters con otros metodos
#dendograma
matriz_distancias <- dist(x = datoscopia, method = "euclidean")
hc_metodo_ward  <- hclust(d = gower_datos2, method = "ward.D")
plot(x = hc_metodo_ward, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Metodo ward(jerarquico)")

#############CLUSTER k=2 #####################

#Estimar clusters y añadir el cluster a cada tipo

pam_ijimai_2 = pam(gower_datos2, diss = TRUE, k = 2) 
datos2[pam_ijimai_2$medoids, ]

#Para resumir la información de cada cluster

pam_summary_2 <- datos2 %>%
  mutate(cluster = pam_ijimai_2$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))


pam_summary_2$cluster_summary[[2]]


#Para representarlo gráficamente

tsne_object2 <- Rtsne(gower_datos2, is_distance = TRUE)

tsne_df2 <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_ijimai_2$clustering))

ggplot(aes(x = X, y = Y), data = tsne_df2) +
  geom_point(aes(color = cluster)) +
  scale_color_manual(values=c("#0098cd","#55225f", "#7fb433"))

#Para añadir el cluster en el dataset inicial

df_final_2<-bind_cols(datos2, pam_ijimai_2['clustering'])
df_final_2$clustering <- as.factor(df_final_2$clustering)

#Para saber la propensión de cada cluster

prop.table(table(df_final_2$clustering, df_final_2$Churn.Value),1)


#############CLUSTER k=3 #####################

#Estimar clusters y añadir el cluster a cada tipo

pam_ijimai_3 = pam(gower_datos2, diss = TRUE, k = 3) 
datos2[pam_ijimai_3$medoids, ]

#Para resumir la información de cada cluster

pam_summary_3 <- datos2 %>%
  mutate(cluster = pam_ijimai_3$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))


pam_summary_3$cluster_summary[[2]]


#Para representarlo gráficamente

tsne_object <- Rtsne(gower_datos2, is_distance = TRUE)

tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_ijimai_3$clustering))

ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster)) +
  scale_color_manual(values=c("#0098cd","#55225f", "#7fb433"))

#Para añadir el cluster en el dataset inicial

df_final_3<-bind_cols(datos2, pam_ijimai_3['clustering'])
df_final_3$clustering <- as.factor(df_final_3$clustering)

#Para saber la propensión a comprar bicicletas de cada cluster

prop.table(table(df_final_3$clustering, df_final_3$Churn.Value),1)

#############CLUSTER k=4 #####################

#Estimar clusters y añadir el cluster a cada tipo

pam_ijimai_4 = pam(gower_datos2, diss = TRUE, k = 4) 
datos2[pam_ijimai_4$medoids, ]

#Para resumir la información de cada cluster

pam_summary_4 <- datos2 %>%
  mutate(cluster = pam_ijimai_4$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))


pam_summary_4$cluster_summary[[2]]


#Para representarlo gráficamente

tsne_object4 <- Rtsne(gower_datos2, is_distance = TRUE)

tsne_df4 <- tsne_object4$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_ijimai_4$clustering))

ggplot(aes(x = X, y = Y), data = tsne_df4) +
  geom_point(aes(color = cluster)) +
  scale_color_manual(values=c("#0098cd","#55225f", "#7fb433", "#FC4E07"))

#Para añadir el cluster en el dataset inicial

df_final_4<-bind_cols(datos2, pam_ijimai_4['clustering'])
df_final_4$clustering <- as.factor(df_final_4$clustering)

#Para saber la propensión a comprar bicicletas de cada cluster

prop.table(table(df_final_4$clustering, df_final_4$Churn.Value),1)

####################################################################################
####################################################################################






