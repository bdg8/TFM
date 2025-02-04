#librerias usadas

library(ggplot2)
install.packages("cowplot")
library(cowplot)
install.packages("stringr")
library(stringr)
library(carData)
library(car)
library(GGally)
install.packages("olsrr")
library(olsrr)
library(dplyr)

#visualizaciones descriptivas de los datos categoricos

#como algunas variables no estan incluidas en el set de datos por previa division, incluyo el churn value 
#en las que se necesita para las visualizaciones

df_final_prueba$Churn.Value <- datosnumericos$Churn.Value
datos2$Churn.Value <- datosnumericos$Churn.Value

#empezamos con las visualizaciones
# ATENCION! como hemos ido cambiando de nombre el data set para las diferentes transformaciones
#hay que asegurarse de que se usa el dataset correcto para las visualizaciones

#genero, edad, married y dependents (porcentual barras apiladas 100%)
plot_grid(ggplot(datosnumericos, aes(x=Gender,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Porcentaje"), 
          ggplot(df_final_prueba, aes(x=EdadGrupos,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Porcentaje"),
          ggplot(df_final_prueba, aes(x=Married,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Porcentaje"),
          ggplot(datos2, aes(x=Number.of.Dependents,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Porcentaje") +
            theme_minimal() +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")
#barras apiladas (count)
#Gender plot
p1 <- ggplot(datosexportar, aes(x = Gender)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) 

#Edad.Grupos plot
p2 <- ggplot(datosexportar, aes(x = Edad.Grupos)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Married plot
p3 <- ggplot(datosexportar, aes(x = Married)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Dependents plot
p4 <- ggplot(datosexportar, aes(x = Number.of.Dependents)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)+ theme_minimal()

#Plot demographic data within a grid
grid.arrange(p1, p2, p3, p4, ncol=2)

#variables relacionadas con services (barras apiladas 100%)
plot_grid(ggplot(datosnumericos, aes(x= Internet.Service ,fill = Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"), 
          ggplot(datosnumericos, aes(x= Phone.Service, fill = Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x= Paperless.Billing, fill = Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x= Offer, fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x=Multiple.Lines,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x=Internet.Type,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x= Online.Security, fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x= Online.Backup ,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x= Device.Protection.Plan ,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage")
          + 
            theme_minimal() +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

#barras apiladas (count)
#Internet service plot
p5 <- ggplot(datosexportar, aes(x = Internet.Service)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Phone.Service plot
p6 <- ggplot(datosexportar, aes(x = Phone.Service)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

# PAperless billing plot
p7 <- ggplot(datosexportar, aes(x = Paperless.Billing)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Offer plot
p8 <- ggplot(datosexportar, aes(x = Offer)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Multiple lines plot
p9 <- ggplot(datosexportar, aes(x = Multiple.Lines)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Internet Type plot
p10 <- ggplot(datosexportar, aes(x = Internet.Type)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Online Securuty plot
p11 <- ggplot(datosexportar, aes(x = Online.Security)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Online Backup plot
p12 <- ggplot(datosexportar, aes(x = Online.Backup)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Device Protection Plan plot
p13 <- ggplot(datosexportar, aes(x = Device.Protection.Plan)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Plot service data within a grid
grid.arrange(p5, p6, p7,
             p8, p9, p10,
             p11, p12, p13,
             ncol=3)

#mas services
plot_grid(ggplot(datosnumericos, aes(x= Premium.Tech.Support ,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"), 
          ggplot(datosnumericos, aes(x= Streaming.TV  ,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x= Streaming.Movies  ,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x= Streaming.Music  ,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x= Unlimited.Data  ,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x= Contract  ,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x= Payment.Method  ,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosnumericos, aes(x= Satisfaction.Score  ,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage"),
          ggplot(datosexportar, aes(x= Monthly.Charge.Cat  ,fill=Churn.Value))+ geom_bar(position = 'fill') + labs(y = "Percentage")
          + 
            theme_minimal() +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

#Premium Tech support plot
p14 <- ggplot(datosexportar, aes(x = Premium.Tech.Support)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#streaming TV plot
p15 <- ggplot(datosexportar, aes(x = Streaming.TV)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

# Streaming Movies plot
p16 <- ggplot(datosexportar, aes(x = Streaming.Movies)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Streaming music plot
p17 <- ggplot(datosexportar, aes(x = Streaming.Music)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Unlimited Data plot
p18 <- ggplot(datosexportar, aes(x = Unlimited.Data)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Contract plot
p19 <- ggplot(datosexportar, aes(x = Contract)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#PAyment method plot
p20 <- ggplot(datosexportar, aes(x = Payment.Method)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Satisfaction score plot
p21 <- ggplot(datosexportar, aes(x = Satisfaction.Score)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Monthly charge plot
p22 <- ggplot(datosexportar, aes(x =Monthly.Charge.Cat)) +
  geom_bar(aes(fill = Churn.Value)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
grid.arrange(p14, p15, p16,
             p17, p18, p19,
             p20, p21, p22,
             ncol=3)
str(datosexportar)



#correlacion, multiconealidad y redundancia
#partimos del dataset datosnumericos

#estandarizamos/normalizamos los datos numÃ©ricos
datos_scaled <- scale(datosnumericos[,-1])
datos_scaled <- data.frame(datos_scaled)

#correlacion 

ggcorr(datos_scaled, label = T)

datos_scaled <- datos_scaled[,-9] #aqui es donde voy eliminando las variables que son redundantes

#el primer modelito te da un output con la variable redundante
a <- Hmisc::redun(~ .,data=datos_scaled, nk=0)
a$Out

##estudiamos el vif y la tolerancia de las variables, pero primero hay que crear una regresion lineal, 
#la creo con el churn.score como ejemplo 

lm1<-lm(Churn.Score ~.,data=datos_scaled)
vif(lm1)
car::vif(lm1) #esto es otro codigo que tb sale lo mismo

#segun los resultados que me va dando voy eliminado esa variable en datos_scaled

#este codigo sirve para ver aquellas variables que son "alias" 
#estuve probando con diferentes codigos 

alias( lm( Churn.Score ~. , data= datos_scaled) )

ols_vif_tol(lm1) #otra forma de ver el vif basado en el modelo lm1

#las que se eliminan son Total.Revenue, Total.charges y Total.Long.Distance.Charges, 
#queda solucionado el problema de multiconealidad

###################################################################
# representaciones variables numericas

#probe primero este codigo que es bastante mas completo, pero al probar con otras variabls no me
#salian bien las visualizaciones, entonces probe con los que pongo mas abajo.

tenurehist <-
datoscopia %>% 
  group_by(Tenure.in.Months, Churn.Value) %>% 
  summarise(Number = n()) %>% 
  ggplot(aes(Tenure.in.Months, Number)) +
  geom_line(aes(col = Churn.Value)) +
  labs(x = "Tenure / Antigüedad (meses)",
       y = "Número de clientes",
       title = "Abandono basado en Antigüedad ") +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  theme_minimal()

monthlychargehist <-
  datoscopia %>% 
  group_by(Total.Refunds, Churn.Value) %>% 
  summarise(Number = n()) %>% 
  ggplot(aes(Total.Refunds, Number)) +
  geom_line(aes(col = Churn.Value))  +
  scale_x_continuous(breaks = seq(0, 70, 10)) +
  theme_minimal()

##### estos son los graficos que estan metidos en el documento de las variables numericas

#Tenure histogram
p17 <- ggplot(data = datoscopia, aes(Tenure.in.Months, color = Churn.Value))+
  geom_freqpoly(binwidth = 1, size = 1) +
  theme_minimal()

#cltv histogram
p18 <- ggplot(data = datoscopia, aes(CLTV, color = Churn.Value))+
  geom_freqpoly(binwidth = 100, size = 1)+
  theme_minimal()

#Avg.Monthly.Long.Distance.Charges histogram
p19 <- ggplot(data = datoscopia, aes(Avg.Monthly.Long.Distance.Charges, color = Churn.Value))+
  geom_freqpoly(binwidth = 3, size = 1)+
  theme_minimal()

#Avg.Monthly.GB.Download histogram
p20 <- ggplot(data = datoscopia, aes(Avg.Monthly.GB.Download, color = Churn.Value))+
  geom_freqpoly(binwidth = 10, size = 1)+
  theme_minimal()

#Monthly.Charge histogram
p21 <- ggplot(data = datoscopia, aes(Monthly.Charge, color = Churn.Value))+
  geom_freqpoly(binwidth = 5, size = 1)+
  theme_minimal()

#Churn.Score histogram
p23 <- ggplot(data = datoscopia, aes(Churn.Score, color = Churn.Value))+
  geom_freqpoly(binwidth = 10, size = 1)+
  theme_minimal()

#Total.Extra.Data.Charges histogram
p22 <- ggplot(data = datoscopia, aes(Total.Extra.Data.Charges, color = Churn.Value))+
  geom_freqpoly(binwidth = 25, size = 1)+
  theme_minimal()


#limpiamos nuestros datos numericos (no es escalados) quitando tb las variables que son eliminadas
str(datosnumericos)

#crear un dataset completo de las variables numericas finales y las categoricas pasadas previamente a factor.
#ese dataset ya esta creado aunque hay que limpiarlo un poco
#es el de datos numericos (cuidado con este dataset porque al principio son solo los numericos, luego es el q se
#usa para datos_scaled, y luego se le anaden los factores, pero el nombre es el mismo, lo cambiare ahora)

datosclean <- select (datosnumericos, -Total.Charges, -Total.Long.Distance.Charges, -Total.Revenue)

# en datosclean se quitan las 3 eliminadas por multiconealidad y faltaria ciudad (que no esta incluida aqui)
#ahora hay que convertir a factor 3 variables que no estan convertidas y dejar edad como numerica o cambiarla 
#por la de rangos que ya esta creada asi como tambien la de monthly charge, cree rangos tb para esa variable.

datosclean$Number.of.Dependents <- as.factor(datosclean$Number.of.Dependents)
datosclean$Number.of.Referrals <- as.factor(datosclean$Number.of.Referrals)

#en el caso de querer incluir edadgrupos y monthly charge cat:
datosclean$EdadGrupos <- df_final_prueba$EdadGrupos
datosclean$Monthly.Charge.Cat <- df_final_prueba$Monthly.Charge.Cat

#eliminariamos la numerica ene l caso de que incluyamos la categorica 
datosclean <- datosclean[,-xxx] #poner el numero de columna que sea
