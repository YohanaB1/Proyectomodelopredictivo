library(readxl)
library(dplyr)
library(visdat) # visualización de datos
library(naniar)
library(corrr)
library(psych)
Proyecto_Autos_usados <- read_excel("Modelos predictivos/Proyecto-Autos usados.xlsx", 
                                    sheet = "used_cars_UK")
View(Proyecto_Autos_usados)

Proyecto_categorias_usados <- read_excel("Modelos predictivos/Proyecto-Autos usados.xlsx", 
                                    sheet = "Categorias")

Df_Data_usados<-Proyecto_Autos_usados
Df_cat_usados<-Proyecto_categorias_usados

summary(Df_Data_usados)
#Histogramas
par(mfrow = c(1, 2)) 
hist(x = Df_Data_usados$Previous_Owners, main = "Histograma de Previous owners", 
     xlab = "Previous owners", ylab = "Frecuencia",
     col = "lightblue")
abline(v=mean(Df_Data_usados$Previous_Owners), col='purple',lwd=3)

h2<-hist(x = Df_Data_usados$Price, main = "Histograma de Price", 
     xlab = "Price", ylab = "Frecuencia",
     col = "lightblue")
abline(v=mean(Df_Data_usados$Price), col='red',lwd=3)
#text(h2$mids,h2$counts,labels=h2$counts,adj=c(0.5,-0.5))



str(Df_Data_usados)

#muestra la cantidad de valores faltantes en los datos

val_na<-Df_Data_usados %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))

#Gráfico Muestra datos faltantes en los datos
vis_dat(Df_Data_usados)

#Muestra matriz de correlacion
library(GGally)
ggcorr(Df_cat_usados, label=TRUE,
       color = "grey60")

ggcorr(Df_cat_usados, method = c("pairwise", "pearson"),
       nbreaks = NULL, digits = 2, low = "#3B9AB2",
       mid = "#EEEEEE", high = "#F21A00",
       geom = "tile", label = TRUE,
       label_alpha = TRUE)

#muestra matriz de correlacion con gráficos
Df_Data_usadosnum <- subset(Df_Data_usados, select = c(Price, Mileage,Registration_Year,Antiguedad_auto,Previous_Owners,Doors,Seats))
pairs(Df_Data_usadosnum)

pairs.panels(Df_Data_usadosnum)