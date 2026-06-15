
# Limpiar el área de trabajo
rm(list = ls())

#install.packages("VIM")
#install.packages("readr")
#install.packages("Hmisc")    #Análisis de datos
#install.packages("gtsummary")    #Análisis de datos
#install.packages("lmtest") # prueba de homogeneidad de varianzas
#install.packages("car") # prueba de multicolinealidad
#install.packages("randtests") # prueba de independencia
#install.packages("MASS") 
# Librerías
library(openxlsx)
library(dplyr)             
library(ggplot2)
library(tidyr)
library(VIM)
library(readr)
library(moments)
library(corrplot)
library(Hmisc)
library(gtsummary)
library(lmtest)
library(car)
library(randtests)
library(MASS)
library(naniar)



# 1. Datos
setwd("C:/Users/Admin/Downloads/Proyecto_est")
datos1 <- read.xlsx("Datos_MP1.xlsx", sheet = 3)

#Tomamos muestra
n_muestra <- round(0.7 * nrow(datos1))

set.seed(123)

# Seleccionar muestra aleatoria de n_muestra filas
muestra_3 <- datos1[sample(1:nrow(datos1), n_muestra), ]

# Guardar la muestra en un archivo Excel
write.xlsx(muestra_3, "muestra_3.xlsx")

#######################################################################################

#Queremos hacer imputacion, pero hay valores faltantes por lo tanto decidimos:

#1.1 Analizar patron de datos faltantes

summary(muestra_3)  # Ver cuántos NA por variable

#1. Guardar datos antes de imputación (copia de seguridad)
muestra_antes_imputacion <- muestra_3  # Datos CON NAs


#1.2 Analizo los valores existentes sin los faltantes

plot(density(muestra_3$asistencia_miles, na.rm = TRUE), 
     col = "red", lwd = 2, 
     main = "Densidad de Asistencia ") 
plot(density(muestra_3$num_arrestos, na.rm = TRUE), 
     col = "red", lwd = 2, 
     main = "Numero de arrestos")
plot(density(muestra_3$inv_social_millones, na.rm = TRUE), 
     col = "red", lwd = 2, 
     main = "Inversion social")

#Asistencia y arrestos sin los valores faltantes tienen una dist.sesgada a la derecha
#y= inversion social sin los valores faltantes tiene una dist. normal

resumen <- muestra_3 %>%
  summarise(
    # Arrestos
    arrestos_min    = min(num_arrestos, na.rm = TRUE),
    arrestos_max    = max(num_arrestos, na.rm = TRUE),
    arrestos_sd     = sd(num_arrestos, na.rm = TRUE),
    arrestos_mean   = mean(num_arrestos, na.rm = TRUE),
    arrestos_median = median(num_arrestos, na.rm = TRUE),
    arrestos_skew   = skewness(num_arrestos, na.rm = TRUE),
    arrestos_kurt   = kurtosis(num_arrestos, na.rm = TRUE),
    arrestos_CV     = (sd(num_arrestos, na.rm = TRUE)/mean(num_arrestos, na.rm = TRUE))*100,
    
    # Inversión social
    inv_min    = min(inv_social_millones, na.rm = TRUE),
    inv_max    = max(inv_social_millones, na.rm = TRUE),
    inv_sd     = sd(inv_social_millones, na.rm = TRUE),
    inv_mean   = mean(inv_social_millones, na.rm = TRUE),
    inv_median = median(inv_social_millones, na.rm = TRUE),
    inv_skew   = skewness(inv_social_millones, na.rm = TRUE),
    inv_kurt   = kurtosis(inv_social_millones, na.rm = TRUE),
    inv_CV     = (sd(inv_social_millones, na.rm = TRUE)/mean(inv_social_millones, na.rm = TRUE))*100,
    
    # Asistencia
    asistencia_min    = min(asistencia_miles, na.rm = TRUE),
    asistencia_max    = max(asistencia_miles, na.rm = TRUE),
    asistencia_sd     = sd(asistencia_miles, na.rm = TRUE),
    asistencia_mean   = mean(asistencia_miles, na.rm = TRUE),
    asistencia_median = median(asistencia_miles, na.rm = TRUE),
    asistencia_skew   = skewness(asistencia_miles, na.rm = TRUE),
    asistencia_kurt   = kurtosis(asistencia_miles, na.rm = TRUE),
    asistencia_CV     = (sd(asistencia_miles, na.rm = TRUE)/mean(asistencia_miles, na.rm = TRUE))*100
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Estadistico"),
    names_pattern = "(.*)_(min|max|sd|mean|median|skew|kurt|CV)"
  ) %>%
  pivot_wider(
    names_from = Estadistico,
    values_from = value
  )

print(resumen)

#Notamos que:
#variable arrestos: la media es mayor que la mediana por tanto hay sesgo a la derecha,hay una ligera asimetria y ligera planitud
#variable inversion: la media y la mediana son practicamentye iguales por tanto es  practicamente dist.simetrica, su skew tamb muesta simetria, es ligeramente plana
#variable asistencia: la media es mayor que la mediana por tanto hay sesgo a la derecha,la asimetria muestra que esta altamente sesgada a la derecha y es picuda hay que tener en cuenta la curtosis ya que indica colas pesadas 

#Revisamos outliers

names(muestra_3)
head(muestra_3)


datos_largos1 <- muestra_3 %>%
  dplyr::select(num_arrestos,inv_social_millones,asistencia_miles ) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Valor")

# Boxplots en un mismo gráfico
ggplot(datos_largos1, aes(x = Variable, y = Valor, fill = Variable)) +
  geom_boxplot(alpha = 0.6, outlier.colour = "red", outlier.shape = 16) +
  labs(title = "Detección de outliers en las 3 variables antes de imputacion",
       x = "Variable",
       y = "Valor") +
  theme_minimal()

#Notamos que hay 2 outliers para la variable asistencia y 1 para inversion social

#_----------------------------------

# test_little
little_test <- naniar::mcar_test(muestra_antes_imputacion)

# Resultados
cat("Test de Little para MCAR\n")
cat("------------------------\n")
cat("Chi-cuadrado:", round(little_test$statistic, 4), "\n")
cat("P-valor:", round(little_test$p.value, 6), "\n")

# Conclusión 
if(little_test$p.value > 0.05) {
  cat("RESULTADO: MCAR (p > 0.05)\n")
} else {
  cat("RESULTADO: NO MCAR (p <= 0.05)\n")
}
#Se muestra que los datos son MCAR (p-valor = 0.367972 > 0.05) por tanto podemos usar metodos como la media o mediana

# grafica datos faltantes 

library(naniar)

# Solo el gráfico de datos faltantes
vis_miss(muestra_antes_imputacion, sort_miss = TRUE)

#----------------------------------------
#1.3 Imputacion 

#Para arrestos usamos mediana
#Para Inversion social usamos media
#Para Asistencia usamos mediana

# Asistencia y arrestos: 
muestra_3$asistencia_miles[is.na(muestra_3$asistencia_miles)] <- median(muestra_3$asistencia_miles, na.rm = TRUE)
muestra_3$num_arrestos[is.na(muestra_3$num_arrestos)] <- median(muestra_3$num_arrestos, na.rm = TRUE)

# Redondear al entero más cercano
muestra_3$num_arrestos <- round(muestra_3$num_arrestos)

# Inversión social: 
muestra_3$inv_social_millones[is.na(muestra_3$inv_social_millones)] <- mean(muestra_3$inv_social_millones, na.rm = TRUE)

# Confirmar que no quedan NAs
summary(muestra_3)

###############################################################################
# Análisis Exploratorio de Datos
#############################################

#luego de imputacion

# Analizamos de nuevo la distribuciones 

# -----------------------------------------------

# Asistencia
plot(density(muestra_antes_imputacion$asistencia_miles, na.rm = TRUE), 
     col = "red", lwd = 2, 
     main = "Comparación Densidades: Asistencia",
     xlab = "Asistencia (miles)",
     ylab = "Densidad")
lines(density(muestra_3$asistencia_miles), 
      col = "blue", lwd = 2)
legend("topright", 
       legend = c("Antes imputación", "Después imputación"), 
       col = c("red", "blue"), 
       lwd = 2)

# Arrestos
plot(density(muestra_antes_imputacion$num_arrestos, na.rm = TRUE), 
     col = "red", lwd = 2, 
     main = "Comparación Densidades: Arrestos",
     xlab = "Número de arrestos",
     ylab = "Densidad")
lines(density(muestra_3$num_arrestos), 
      col = "blue", lwd = 2)
legend("topright", 
       legend = c("Antes imputación", "Después imputación"), 
       col = c("red", "blue"), 
       lwd = 2)

# Inversión Social
plot(density(muestra_antes_imputacion$inv_social_millones, na.rm = TRUE), 
     col = "red", lwd = 2, 
     main = "Comparación Densidades: Inversión Social",
     xlab = "Inversión social (millones)",
     ylab = "Densidad")
lines(density(muestra_3$inv_social_millones), 
      col = "blue", lwd = 2)
legend("topright", 
       legend = c("Antes imputación", "Después imputación"), 
       col = c("red", "blue"), 
       lwd = 2)


#calculamos min max desv.est media mediana

# Resumen en formato ancho con nombres consistentes
library(dplyr)
library(tidyr)
library(moments) # para skewness y kurtosis

resumen <- muestra_3 %>%
  summarise(
    # Arrestos
    arrestos_min    = min(num_arrestos, na.rm = TRUE),
    arrestos_max    = max(num_arrestos, na.rm = TRUE),
    arrestos_sd     = sd(num_arrestos, na.rm = TRUE),
    arrestos_mean   = mean(num_arrestos, na.rm = TRUE),
    arrestos_median = median(num_arrestos, na.rm = TRUE),
    arrestos_skew   = skewness(num_arrestos, na.rm = TRUE),
    arrestos_kurt   = kurtosis(num_arrestos, na.rm = TRUE),
    arrestos_CV     = (sd(num_arrestos, na.rm = TRUE)/mean(num_arrestos, na.rm = TRUE))*100,
    
    # Inversión social
    inv_min    = min(inv_social_millones, na.rm = TRUE),
    inv_max    = max(inv_social_millones, na.rm = TRUE),
    inv_sd     = sd(inv_social_millones, na.rm = TRUE),
    inv_mean   = mean(inv_social_millones, na.rm = TRUE),
    inv_median = median(inv_social_millones, na.rm = TRUE),
    inv_skew   = skewness(inv_social_millones, na.rm = TRUE),
    inv_kurt   = kurtosis(inv_social_millones, na.rm = TRUE),
    inv_CV     = (sd(inv_social_millones, na.rm = TRUE)/mean(inv_social_millones, na.rm = TRUE))*100,
    
    # Asistencia
    asistencia_min    = min(asistencia_miles, na.rm = TRUE),
    asistencia_max    = max(asistencia_miles, na.rm = TRUE),
    asistencia_sd     = sd(asistencia_miles, na.rm = TRUE),
    asistencia_mean   = mean(asistencia_miles, na.rm = TRUE),
    asistencia_median = median(asistencia_miles, na.rm = TRUE),
    asistencia_skew   = skewness(asistencia_miles, na.rm = TRUE),
    asistencia_kurt   = kurtosis(asistencia_miles, na.rm = TRUE),
    asistencia_CV     = (sd(asistencia_miles, na.rm = TRUE)/mean(asistencia_miles, na.rm = TRUE))*100
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Estadistico"),
    names_pattern = "(.*)_(min|max|sd|mean|median|skew|kurt|CV)"
  ) %>%
  pivot_wider(
    names_from = Estadistico,
    values_from = value
  )

print(resumen)

#Podemos ver que inv e inversion tienen unas distribuciones mas estables pero arrestos es la mas variable

#Revisamos outliers
datos_largos2 <- muestra_3 %>%
  dplyr::select(num_arrestos, inv_social_millones, asistencia_miles) %>%
  pivot_longer(cols = everything(),
               names_to = "Variable",
               values_to = "Valor")

# Boxplots en un mismo gráfico
ggplot(datos_largos2, aes(x = Variable, y = Valor, fill = Variable)) +
  geom_boxplot(alpha = 0.6, outlier.colour = "red", outlier.shape = 16) +
  labs(title = "Detección de outliers en las 3 variables",
       x = "Variable",
       y = "Valor") +
  theme_minimal()

# Puede que estos outliers de asistencia sean porque es una variable sesgada a la derecha
#En 2 de las variables hay outliers  y 1 de ellos es alto pero estos corresponden a la asimetría natural de la distribución por tanto decidimos no hacer transformaciones.

#Resultados del analisis exploratorio



#variable:
#Arrestos:  su media es mayor a la mediana, hay por tanto un sesgo a la derecha, segun su desviacion estandar los datos estan bastante dispersos.
#Inv social: su media y mediana son iguales, por tanto es simetrica la dist, su desv estandar es moderada por tanto hay una ligera variacion solamente.
#Asitencia: su media es mayor a la mediana, hay sesgo a la derecha, su desv estandar es algo grande, hay alta variabilidad.
#----------------------------------------

#Modelos---------------------------------------------------------

#Matriz de correlaciones

df_numerico = muestra_3 %>%
  dplyr::select(asistencia_miles, inv_social_millones,num_arrestos)

#Verificación de correlaciones significativas


library(ggcorrplot)
matriz_cor <- cor(df_numerico)
ggcorrplot(matriz_cor, 
           method = "circle", 
           lab = TRUE,     
           lab_size = 5)  

matriz_cor <- cor(df_numerico)

# Imprime en la terminal
print(matriz_cor)

#Vemos que no hay multicolinealidad r= 0.19 muy bajo, nos da una idea sin embargo verificamos luego con VIF
#asistencia y arrestos muestra una correlacion muy debil y negativa r= -0.13
#inv_social y arrestos muestra una correlacion moderada y negativa r= -0.5

#----------------------------------------------------------------------------
#Modelo de regresion lineal multiple

modelo_lineal1 <- lm(num_arrestos ~ asistencia_miles + inv_social_millones, data = df_numerico)
summary(modelo_lineal1 )

###########################################################################
# Validación de supuestos del modelo_lineal1 
##################################################

errores = modelo_lineal1$residuals
errores

#1. Supuesto de media cero o linealidad
t.test(errores)

#2. Varianza constante - Homocedasticidad: Breusch-Pagan
lmtest::bptest(modelo_lineal1)


#3. Independencia - Durbin-Watson
dwtest(modelo_lineal1)

#4. Multicolinalidad
vif_values <- vif(modelo_lineal1)
print(vif_values)
barplot(vif(modelo_lineal1))

# Identificar variables con VIF > 10
high_vif <- names(vif_values[vif_values > 10])
print(paste("Variables con alta multicolinealidad:", paste(high_vif, collapse = ", ")))

#5. Normalidad
shapiro.test(resid(modelo_lineal1))

#----------------
#Se cumple linealidad,homocedasticidad,se cumple independencia,no hay multicolinealidad,se cumple normalidad

#Metricas de evaluacion de rendimiento del modelo
#R²,RECM,EAM, MAPE

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))      #RSME
mae  <- function(y, yhat) mean(abs(y - yhat))           #MAE
mape <- function(y, yhat) mean(abs((y - yhat)/y))*100   #MAPE

rmse(df_numerico$num_arrestos, modelo_lineal1$fitted.values)
mae(df_numerico$num_arrestos, modelo_lineal1$fitted.values)
mape(df_numerico$num_arrestos, modelo_lineal1$fitted.values)

y    <- df_numerico$num_arrestos
yhat <- fitted(modelo_lineal1)
RMSE = rmse(y, yhat)
MAE  = mae(y, yhat)
MAPE = mape(y, yhat)

RMSE
MAE
MAPE
BIC(modelo_lineal1) 
AIC(modelo_lineal1) 

#revisamos ademas Coeficientes del modelo, Error estándar, R2, Esta-F, p-value

summary(modelo_lineal1)
#RESULTADOS MODELO LINEAL

#Se cumplen bien todos los supuestos, sin embargo hay una posible heterocedasticidad

# aistencia no tiene un efecto significativo, inversion si
#El modelo explica un 34% de la variabilidad de los arrestos
#p-value: El efecto del modelo es debil

#RMSE el modelo se equivoca en 39 arrestos en promedio
#MAE= el error absoluto medio es 33 arrestos
#MAPE: El modelo se equivoca en promedio un 42% respecto a los valores reales
#BIC es de 173.5

#El modelo en su mayoria cumple bien con los supuestos sin embargo en cuanto a su capacidad de prediccion presenta valores bajos (34%) y errores altos (42%)
#----------------------------------------------------------------------------
#Modelo poisson

modelo_poisson <- glm(num_arrestos ~ asistencia_miles + inv_social_millones, family = poisson(link = "log"),data = df_numerico)
summary(modelo_poisson)

###########################################################################
# Validación de supuestos del modelo poisson
##################################################

# 1. Test de sobredispersión
pearson_residuals <- residuals(modelo_poisson, type = "pearson")
dispersion_manual <- sum(pearson_residuals^2) / modelo_poisson$df.residual
cat("Estadístico de dispersión (manual):", dispersion_manual, "\n")

#phi

phi <- sum(pearson_residuals^2) / modelo_poisson$df.residual
p_value <- pchisq(phi * modelo_poisson$df.residual, df = modelo_poisson$df.residual, lower.tail = FALSE)

cat("Phi (dispersión):", phi, "\n")
cat("P-valor sobredispersión:", p_value, "\n")


# 2. Calcular residuos de Pearson
residuos_pearson <- residuals(modelo_poisson, type = "pearson")
print(residuos_pearson)

# 3. INDEPENDENCIA 

dw_test <- dwtest(modelo_poisson)
print(dw_test)

# 4. MULTICOLINEALIDAD 

vif_values <- vif(modelo_poisson)
print("Valores de VIF:")
print(vif_values)

#----------------
#Metricas de evaluacion de rendimiento del modelo

#pseudo R²,RECM,EAM, MAPE

pseudo_r2 <- function(modelo) {
  1 - (modelo$deviance / modelo$null.deviance)
}
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))      #RSME
mae  <- function(y, yhat) mean(abs(y - yhat))           #MAE
mape <- function(y, yhat) {
  indices_no_cero <- y != 0
  if(sum(indices_no_cero) == 0) return(NA)
  mean(abs((y[indices_no_cero] - yhat[indices_no_cero]) / y[indices_no_cero])) * 100
}
rmse(df_numerico$num_arrestos, modelo_poisson$fitted.values)
mae(df_numerico$num_arrestos, modelo_poisson$fitted.values)
mape(df_numerico$num_arrestos, modelo_poisson$fitted.values)
y    <- df_numerico$num_arrestos      
yhat <- fitted(modelo_poisson) 
RMSE = rmse(y, yhat)
MAE  = mae(y, yhat)
MAPE = mape(y, yhat)
PSEUDO_R2 <- pseudo_r2(modelo_poisson)
RMSE
MAE
MAPE
PSEUDO_R2
BIC(modelo_poisson) 

#Revisamos modelo poisson
summary(modelo_poisson)
#-----------------------------------------
#RESULTADOS MODELO POISSON

#Hay sobredispersion muy alta,considerar binomial negativa
#Los residuos muestran un buen ajuste del modelo
#Tanto el test DW como su p value muestran que No hay autocorrelación significativa
#No hay problema de multicolinealidad
#Casi todos los supueston se cumplen correctamente, excepto test de sobredispersion

#Pseudo r2 muestra un ajuste ligeramente bueno del modelo, MAPE nos muestra una precision baja

#BIC de 338.8
#Asistencia muestra un efecto no significativo en arrestos
#Debido a la sobredispersion procedemos a evaluar con binomial negativo

#----------------------------------------------------


# Modelo binomial negativo
modelo_nb <- glm.nb(num_arrestos ~ asistencia_miles + inv_social_millones, 
                    data = df_numerico)

summary(modelo_nb)

###########################################################################
# Validación de supuestos del modelo binomial negativo
##################################################

# 1. Residuos de Pearson
residuos_pearson <- residuals(modelo_nb, type = "pearson")
print(residuos_pearson)

# 2. INDEPENDENCIA (Durbin-Watson)
dw_test <- dwtest(modelo_nb)
print(dw_test)

# 3. MULTICOLINEALIDAD
vif_values <- vif(modelo_nb)
print("Valores de VIF:")
print(vif_values)

#----------------
# Métricas de evaluación de rendimiento del modelo

# pseudo R², RMSE, MAE, MAPE
pseudo_r2 <- function(modelo) {
  1 - (modelo$deviance / modelo$null.deviance)
}
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))      # RMSE
mae  <- function(y, yhat) mean(abs(y - yhat))           # MAE
mape <- function(y, yhat) {
  indices_no_cero <- y != 0
  if(sum(indices_no_cero) == 0) return(NA)
  mean(abs((y[indices_no_cero] - yhat[indices_no_cero]) / y[indices_no_cero])) * 100
}

rmse(df_numerico$num_arrestos, modelo_nb$fitted.values)
mae(df_numerico$num_arrestos, modelo_nb$fitted.values)
mape(df_numerico$num_arrestos, modelo_nb$fitted.values)

# Guardar métricas
y    <- df_numerico$num_arrestos      
yhat <- fitted(modelo_nb) 
RMSE = rmse(y, yhat)
MAE  = mae(y, yhat)
MAPE = mape(y, yhat)
PSEUDO_R2 <- pseudo_r2(modelo_nb)

RMSE
MAE
MAPE
PSEUDO_R2
BIC(modelo_nb) 

# Revisamos modelo NB
summary(modelo_nb)

#-----------------------------------------
#RESULTADOS MODELO Binomial negativo

#Asitencia sigue sin mostrar un efecto significativo
#Los residuos de pearson muestran un ajuste aceptable, notamos que mejoraron considerablemente respecto a poisson
#El test de independencia muestra que no hay autoccorrelacion tanto en DW como en el p-avlue
#En VIF se evidencia que no hay problema de multicolinealidad
#Pseudo2 muestra un ajuste ligeramente bueno del modelo , con un 39% de la varianza explicada, pero podria mejorar
#MAPE muestra una precision razonable 
#AIC es de 165.64
#BIC de 168.7

#----------------------------------------------------


# Modelo binomial negativo sin asistencia

modelo_nb2<- glm.nb(num_arrestos ~  inv_social_millones, 
                    data = df_numerico)

summary(modelo_nb2)

###########################################################################
# Validación de supuestos del modelo binomial negativo sin asistencia
##################################################

# 1. Residuos de Pearson
residuos_pearson <- residuals(modelo_nb2, type = "pearson")
print(residuos_pearson)

# 2. INDEPENDENCIA (Durbin-Watson)
dw_test <- dwtest(modelo_nb2)
print(dw_test)


#----------------
# Métricas de evaluación de rendimiento del modelo

# pseudo R², RMSE, MAE, MAPE
pseudo_r2 <- function(modelo) {
  1 - (modelo$deviance / modelo$null.deviance)
}
rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))      # RMSE
mae  <- function(y, yhat) mean(abs(y - yhat))           # MAE
mape <- function(y, yhat) {
  indices_no_cero <- y != 0
  if(sum(indices_no_cero) == 0) return(NA)
  mean(abs((y[indices_no_cero] - yhat[indices_no_cero]) / y[indices_no_cero])) * 100
}

rmse(df_numerico$num_arrestos, modelo_nb2$fitted.values)
mae(df_numerico$num_arrestos, modelo_nb2$fitted.values)
mape(df_numerico$num_arrestos, modelo_nb2$fitted.values)

# Guardar métricas
y    <- df_numerico$num_arrestos      
yhat <- fitted(modelo_nb2) 
RMSE = rmse(y, yhat)
MAE  = mae(y, yhat)
MAPE = mape(y, yhat)
PSEUDO_R2 <- pseudo_r2(modelo_nb2)

RMSE
MAE
MAPE
PSEUDO_R2
BIC(modelo_nb2) 

# Revisamos modelo NB
summary(modelo_nb2)

#-----------------------------------------
#RESULTADOS MODELO Binomial negativo sin asistencia

#AIC 164.26 
#BIC de 166.5
#Residuos de pearson muestran un buen ajuste en el modelo
#El test DW nos muestra que no hay autocorrelacion en los residuos
#MAPE nos muestra una presicion del modelo ligeramnete buena
#Pseudo r2 nos indica que se justifica un 36% de la variablididad
#----------------------------------------------------


#Se elige el modelo Binomial Negativo
#Obtuvo el menor AIC y BIC: 164,26 y 166,58 respectivamente
#Resuelve el problema de sobredispersion que se tenia con el modelo Poisson 
#Si bien tanto el modelo BN reducido (nb2) como el completo (nb) obtiene resultados similares,
#se elige el modelo nb2 debido al principio de parsimonia.

#A partir de la matriz de coeficientes se encontro lo siguiente:
#La inversion social tiene un efecto negativo y significativo sobre los arrestos
#Podemos interpretarlo como: Por cada millon en inversion social los arrestos disminuyen en aproximadamente un 1,34%
#