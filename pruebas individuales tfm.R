

install.packages("writexl") 
library(writexl) 
write_xlsx(x = datoscopia, path = "datostfm.xlsx", col_names = TRUE)


#numero optimo de cluster por dendograma

matriz_distancias <- dist(x = datoscopia, method = "euclidean")
hc_euclidea_ward  <- 
hclust(d = matriz_distancias, method = "ward.D")
plot(x = hc_euclidea_ward, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage Ward")
km_clusters <- kmeans(x = datoscopia, centers = 4, nstart = 50)

#analisis exploratorio
#Para dibujar distribuciÃ³n de las variables
library(ggplot2)
Gender <- ggplot(datoscopia, aes(Gender)) + geom_bar(fill = "#0098cd") + labs(y="", x = "Gender")

Age <- ggplot(datoscopia, aes(Age)) + geom_bar(fill = "#0098cd") + labs(y="", x = "Age")

Married <- 
  
  ggplot(datoscopia, aes(x= Married, y=Age)) + geom_bar(fill = "#0098cd") + labs(y="Age", x = "Married")

occupation <- ggplot(df, aes(Occupation)) + geom_bar(fill = "#0098cd") + labs(y="", x = "Occupation")

marital <- ggplot(df, aes(MaritalStatus)) + geom_bar(fill = "#0098cd") + labs(y="", x = "Marital Status")

owner <- ggplot(df, aes(HomeOwnerFlag)) + geom_bar(fill = "#0098cd") + labs(y="", x = "Home Owner Flag")

cars <- ggplot(df, aes(NumberCarsOwned)) + geom_bar(fill = "#0098cd") + labs(y="", x = "Number Cars Owned")

childrenhome <- ggplot(df, aes(NumberChildrenAtHome)) + geom_bar(fill = "#0098cd") + labs(y="", x = "Number Children At Home")

childrentotal <- ggplot(df, aes(TotalChildren)) + geom_bar(fill = "#0098cd") + labs(y="", x = "Total Children")

boxplot(df$YearlyIncome)

ggarrange(age, country, education, occupation, marital, owner, cars, childrenhome, childrentotal,
          labels = c(""),
          ncol = 3, nrow = 3)

library(tidyverse)
datoscopia %>%
  ggplot(aes(x = Tenure.in.Months, y = Total.Revenue, color = Churn.Value))+
  geom_point()+
  labs(x = "Meses siendo cliente",
       y = "Total Ingresos",
       title = "Churn value ~ Tenure in monts y Total Revenue",
       subtitle = "Relacion entre los meses siendo cliente y los beneficios",
       caption = "Datos: datoscopia")

datoscopia$Satisfaction.Score<-as.factor(datoscopia$Satisfaction.Score)
datoscopia %>%
  ggplot(aes(x = Tenure.in.Months, y = Total.Revenue, color = Satisfaction.Score))+
  geom_point()+
  labs(x = "Meses siendo cliente",
       y = "Total Ingresos",
       title = "Satisfaction Score ~ Tenure in monts y Total Revenue",
       subtitle = "Relacion entre los meses siendo cliente y los beneficios",
       caption = "Datos: datoscopia")


str(datosnumericos)
#en datos numericos ya estan todas las variables categ en factor

#vamos a aplicar el modelo PAM, y determinaremos el numero de k con siluette
#carga librerÃ­as
library(dplyr)
library(cluster)
library(ggplot2)
install.packages("Rtsne")
library(Rtsne)
library(ggpubr)

#Seleccionar variables a utilizar en el cluster
#nos basamos en el PCA

datos2 = datosnumericos %>% select(Gender, Phone.Service, Paperless.Billing, Churn.Value, CLTV,
                                   Age, Married, Number.of.Referrals, Tenure.in.Months, Offer, 
                                   Device.Protection.Plan, Streaming.Music, Contract, Payment.Method, 
                                   Monthly.Charge, Churn.Score, Total.Extra.Data.Charges)

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

#############CLUSTER k=3 #####################

#Estimar clusters y aÃ±adir el cluster a cada tipo

pam_ijimai_3 = pam(gower_datos2, diss = TRUE, k = 3) 
datos2[pam_ijimai_3$medoids, ]

#Para resumir la informaciÃ³n de cada cluster

pam_summary_3 <- datos2 %>%
  mutate(cluster = pam_ijimai_3$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))


pam_summary_3$cluster_summary[[2]]


#Para representarlo grÃ¡ficamente

tsne_object <- Rtsne(gower_datos2, is_distance = TRUE)

tsne_df <- tsne_object$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_ijimai_3$clustering))

ggplot(aes(x = X, y = Y), data = tsne_df) +
  geom_point(aes(color = cluster)) +
  scale_color_manual(values=c("#0098cd","#55225f", "#7fb433"))

#Para aÃ±adir el cluster en el dataset inicial

df_final_3<-bind_cols(datos2, pam_ijimai_3['clustering'])
df_final_3$clustering <- as.factor(df_final_3$clustering)

#Para saber la propensiÃ³n a comprar bicicletas de cada cluster

prop.table(table(df_final_3$clustering, df_final_3$Churn.Value),1)

#############CLUSTER k=4 #####################

#Estimar clusters y aÃ±adir el cluster a cada tipo

pam_ijimai_4 = pam(gower_datos2, diss = TRUE, k = 4) 
datos2[pam_ijimai_4$medoids, ]

#Para resumir la informaciÃ³n de cada cluster

pam_summary_4 <- datos2 %>%
  mutate(cluster = pam_ijimai_4$clustering) %>%
  group_by(cluster) %>%
  do(cluster_summary = summary(.))


pam_summary_4$cluster_summary[[2]]


#Para representarlo grÃ¡ficamente

tsne_object4 <- Rtsne(gower_datos2, is_distance = TRUE)

tsne_df4 <- tsne_object4$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_ijimai_4$clustering))

ggplot(aes(x = X, y = Y), data = tsne_df4) +
  geom_point(aes(color = cluster)) +
  scale_color_manual(values=c("#0098cd","#55225f", "#7fb433", "#FC4E07"))

#Para aÃ±adir el cluster en el dataset inicial

df_final_4<-bind_cols(datos2, pam_ijimai_4['clustering'])
df_final_4$clustering <- as.factor(df_final_4$clustering)

#Para saber la propensiÃ³n a comprar bicicletas de cada cluster

prop.table(table(df_final_4$clustering, df_final_4$Number.of.Referrals),1)


#otras cositas

pam.res <- pam(datos2, 4)
# Visualize
fviz_cluster(pam.res)









