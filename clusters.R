#librerías utilizadas
library (dplyr)
library (MASS) #stepwise
library(FactoMineR) #PCA
library(factoextra)
library(corrplot)
library(ggpubr)#ggarrange
library(cluster)
library(ggplot2)
library(Rtsne) #PAM
library(ggpubr)


############################ STEPWISE ######################################### 
#Establecemos los valores inferior y superior para aplicar el modelo
minimo <- glm(Churn.Value~1,data=datos,family = binomial(link="logit"))
completo <-glm(Churn.Value ~.,data=datos,family = binomial(link="logit"))
#Aplicamos el modelo stepwise
var_sele <-stepAIC(minimo,scope=list(upper=completo),direction="both",
                   trace=FALSE,family = binomial(link="logit"))
summary(var_sele)
formula(var_sele)


############################ PCA ############################################## 

#El PCA solo puede aplicarse a datos numéricos. 
#Si los datos contienen variables categóricas, deben ser convertidas a numéricas.
#Creamos el objeto del Analisis PCA
AnalisisPCA <- PCA(X = datosdummiesnormal, scale.unit = FALSE, ncp = 65, graph = TRUE)

#El mismo pero dejamos que la funcion escale los datos en vez de escarlo nosotras
AnalisisPCA2<- PCA(X = datosdummies, scale.unit = TRUE, ncp = 65, graph = TRUE)

#Calidad de representación en el mapa de factores
fviz_pca_var(AnalisisPCA2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE )

#Contribución de las variables en cada dimensión (matriz)
corrplot(AnalisisPCA$var$contrib, is.corr=FALSE)  

#Cantidad de variación retenida por cada componente principal (varianza explicada)
get_eigenvalue(AnalisisPCA2)

#como necesitamos muchos componentes principales para explicar la varianza total 
#de nuestros datos, vamos a buscar el punto de inflexion de cuantos serian nuestros 
#componentes optimos, el porcentaje debemos pensar nosotras cual queremos
fviz_eig(AnalisisPCA2, addlabels = TRUE, ylim = c(0, 50))
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


############################# MODELO PAM ######################################


#Determinamos el numero de k con silhouette
#calcular distancias tipo gower
#pam_fn <- function(dataFn){
  dataFn<-ddatosPAM
  gower_datosPAM <- daisy(dataFn,
                        metric = "gower")
  
  summary(gower_datosPAM)
  
  #Calcular silhouette widht para decidir numero de clusters
  
  silhouette <- c()
  silhouette = c(silhouette, NA)
  for(i in 2:10){
    pam_clusters = pam(as.matrix(gower_datosPAM),
                       diss = TRUE, k = i)
    silhouette = c(silhouette ,pam_clusters$silinfo$avg.width)
  }
  plot(1:10, silhouette, xlab = "Clusters",
       ylab = "Silhouette Width") 
  
  lines(1:10, silhouette)
  
  kp <- pam(dataFn, 4 )
  clusplot( dataFn, kp$cluster, color = TRUE, 
            shade = FALSE, labels = 4, lines = 1,
            col.p = "#FC4E07" )
  
  #empezamos a analizar el numero optimo de clusters con otros metodos
  #dendograma
  matriz_distancias <- dist(x = datos, method = "euclidean")
  hc_metodo_ward  <- hclust(d = gower_datosPAM, method = "ward.D")
  plot(x = hc_metodo_ward, cex = 0.6, xlab = "", ylab = "", sub = "",
       main = "Metodo ward(jerarquico)")
  
  #############CLUSTER k=2 #####################
  
  #Estimar clusters y añadir el cluster a cada tipo
  
  pam_ijimai_2 = pam(gower_datosPAM, diss = TRUE, k = 2) 
  dataFn[pam_ijimai_2$medoids, ]
  
  #Para resumir la información de cada cluster
  
  pam_summary_2 <- dataFn %>%
    mutate(cluster = pam_ijimai_2$clustering) %>%
    group_by(cluster) %>%
    do(cluster_summary = summary(.))
  
  
  pam_summary_2$cluster_summary[[2]]
  
  
  #Para representarlo gráficamente
  
  tsne_object <- Rtsne(gower_datosPAM, is_distance = TRUE)
  
  tsne_df2 <- tsne_object$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_ijimai_2$clustering))
  
  ggplot(aes(x = X, y = Y), data = tsne_df2) +
    geom_point(aes(color = cluster)) +
    scale_color_manual(values=c("#0098cd","#55225f", "#7fb433"))
  
  #Para añadir el cluster en el dataset inicial
  
  df_final_2<-bind_cols(dataFn, pam_ijimai_2['clustering'])
  df_final_2$clustering <- as.factor(df_final_2$clustering)
  
  #Para saber la propensión de cada cluster
  
  prop.table(table(df_final_2$clustering, df_final_2$Churn.Value),1)
  
  
  #############CLUSTER k=3 #####################
  
  #Estimar clusters y añadir el cluster a cada tipo
  
  pam_ijimai_3 = pam(gower_datosPAM, diss = TRUE, k = 3) 
  dataFn[pam_ijimai_3$medoids, ]
  
  #Para resumir la información de cada cluster
  
  pam_summary_3 <- dataFn %>%
    mutate(cluster = pam_ijimai_3$clustering) %>%
    group_by(cluster) %>%
    do(cluster_summary = summary(.))
  
  
  pam_summary_3$cluster_summary[[2]]
  
  
  #Para representarlo gráficamente
  
  tsne_object <- Rtsne(gower_datosPAM, is_distance = TRUE)
  
  tsne_df <- tsne_object$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_ijimai_3$clustering))
  
  ggplot(aes(x = X, y = Y), data = tsne_df) +
    geom_point(aes(color = cluster)) +
    scale_color_manual(values=c("#0098cd","#55225f", "#7fb433"))
  
  #Para añadir el cluster en el dataset inicial
  
  df_final_3<-bind_cols(dataFn, pam_ijimai_3['clustering'])
  df_final_3$clustering <- as.factor(df_final_3$clustering)
  
  #Para saber la propensión a comprar bicicletas de cada cluster
  
  prop.table(table(df_final_3$clustering, df_final_3$Churn.Value),1)
  
  #############CLUSTER k=4 #####################
  
  #Estimar clusters y añadir el cluster a cada tipo
  
  pam_ijimai_4 = pam(gower_datosPAM, diss = TRUE, k = 4) 
  dataFn[pam_ijimai_4$medoids, ]
  
  #Para resumir la información de cada cluster
  
  pam_summary_4 <- dataFn %>%
    mutate(cluster = pam_ijimai_4$clustering) %>%
    group_by(cluster) %>%
    do(cluster_summary = summary(.))
  
  
  pam_summary_4$cluster_summary[[2]]
  
  
  #Para representarlo gráficamente
  
  tsne_object4 <- Rtsne(gower_datosPAM, is_distance = TRUE)
  
  tsne_df4 <- tsne_object4$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(pam_ijimai_4$clustering))
  
  ggplot(aes(x = X, y = Y), data = tsne_df4) +
    geom_point(aes(color = cluster)) +
    scale_color_manual(values=c("#0098cd","#55225f", "#7fb433", "#FC4E07"))
  
  #Para añadir el cluster en el dataset inicial
  
  df_final_4<-bind_cols(dataFn, pam_ijimai_4['clustering'])
  df_final_4$clustering <- as.factor(df_final_4$clustering)
  
  #Para saber la propensión a comprar bicicletas de cada cluster
  
  prop.table(table(df_final_4$clustering, df_final_4$Churn.Value),1)
#}

#pam_fn(datosPAM)
#pam_fn(datos_scaled_sw)

