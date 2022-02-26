install.packages("CORElearn")   #v 1.56.0
install.packages("RWeka")       #v 0.4-44
install.packages("FSelector")   #v 0.33
install.packages("caret")       #v 6.0-90
install.packages("corrplot")    #v 0.92
install.packages("tidyverse")   #v 1.3.1
install.packages("dplyr")       #v 1.0.7
install.packages("readr")       #v 2.1.1
install.packages("Hmisc")       #v 4.6-0
install.packages("naniar")      #v 0.6.1
install.packages("foreign")     #v 0.8.82
library(RWeka)
library(FSelector)
library(CORElearn)
library(caret)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(dplyr)
library(readr)
library(Hmisc)
library(naniar)
library(foreign)


# 1. PREPROCESAMIENTO DE DATOS

## Importación y Resumen de la base de datos
### Importando datos
AOS <- data.frame(Proyecto_HRP_v01)           # Guardar en una variable como dataframe

### Resumen
dim(AOS)               #Dimesiones del dataset
names(AOS)             #Nombre de cada variables
str(AOS)               #Identificar estructura de los datos
sum(is.na(AOS))        #Total datos faltantes
colSums(is.na(AOS))    #Datos faltantes por variable


## Eliminando predictores 
AOS <- AOS[!is.na(AOS$n_horas_dia_A),]  #Elimina los NA del outcome
AOS$idpac <- NULL                       #Elimina variable idpac
AOS = select(AOS, -ends_with("_B"))     #Elimina variables registradas en la visita 2
AOS = select(AOS, -ends_with("_C"))     #Elimina variables registradas en la visita 3
AOS = select(AOS, -ends_with("_D"))     #Elimina variables registradas en la visita 4

## Split de los datos PSG y HRP
AOS$n_horas_dia_A <- ifelse(AOS$n_horas_dia_A>=4,'0','1')   #condicional para estandarizar el outcome
AOS_PSG = subset(AOS,grupo2=="PSG")                         #extrae del dataframe pacientes a quienes le hicieron PSG
AOS_HRP = subset(AOS,grupo2=="HRP")                         #extrae del dataframe pacientes a quienes le hicieron HRP

AOS_PSG_N = dplyr::select_if(AOS_PSG, is.numeric)  #extrae variables tipo numericas de PSG
AOS_HRP_N = dplyr::select_if(AOS_HRP, is.numeric)  #extrae variables tipo numericas de HRP

## Eliminar datos faltantes
### Graficos porcentajes de datos faltantes HRP
gg_miss_var(AOS_HRP_N[,1:40], show_pct = TRUE)     #predictores del 1 al 40
gg_miss_var(AOS_HRP_N[,41:80], show_pct = TRUE)    #predictores del 41 al 80
gg_miss_var(AOS_HRP_N[,81:120], show_pct = TRUE)   #predictores del 81 al 120
gg_miss_var(AOS_HRP_N[,121:159], show_pct = TRUE)  #predictores del 121 al 159

### Graficos resumen de datos faltantes HRP
gg_miss_var(AOS_PSG_N[,1:40], show_pct = TRUE)     #predictores del 1 al 40
gg_miss_var(AOS_PSG_N[,41:80], show_pct = TRUE)    #predictores del 41 al 80
gg_miss_var(AOS_PSG_N[,81:120], show_pct = TRUE)   #predictores del 81 al 120
gg_miss_var(AOS_PSG_N[,121:159], show_pct = TRUE)  #predictores del 121 al 159

### Elimando predictores con mas del 20% de datos nulos (dataset PSG)
for (i in 1:ncol(AOS_PSG_N)) {
  if (sum(is.na(AOS_PSG_N[,i]))>25){
    colnames(AOS_PSG_N)[i] <- "Nuevo"
  }
}

for (i in 1:ncol(AOS_PSG_N)) {
  AOS_PSG_N$Nuevo <- NULL
}

### Elimando predictores con mas del 20% de datos nulos (dataset HRP)
for (i in 1:ncol(AOS_HRP_N)) {
  if (sum(is.na(AOS_HRP_N[,i]))>22){
    colnames(AOS_HRP_N)[i] <- "Nuevo"
  }
}

for (i in 1:ncol(AOS_HRP_N)) {
  AOS_HRP_N$Nuevo <- NULL
}

### Imputando la media de cada predictor en sus datos faltantes
for (i in 1:ncol(AOS_PSG_N)) {
  AOS_PSG_N[is.na(AOS_PSG_N[,i]),i] <- mean((AOS_PSG_N[,i]),na.rm = TRUE)
}

for (i in 1:ncol(AOS_HRP_N)) {
  AOS_HRP_N[is.na(AOS_HRP_N[,i]),i] <- mean((AOS_HRP_N[,i]),na.rm = TRUE)
}

### Agregando el outcome al dataframe numérico
AOS_PSG_N$outcome = AOS_PSG$n_horas_dia_A
AOS_HRP_N$outcome = AOS_HRP$n_horas_dia_A

### Graficando outcome (Dataset PSG)
ggplot(AOS_PSG_N, aes(x = outcome)) +
  geom_bar(width = 0.4,  fill=rgb(0.1,0.5,0.8,0.7)) +
  scale_x_discrete("0 (Apto)  1 (No Apto)") +     # configuración eje X (etiqueta del eje)
  scale_y_continuous("Pacientes") +
  labs(title = "Outcome (AOS_PSG_N)")

### Graficando outcome (Dataset HRP)
ggplot(AOS_HRP_N, aes(x = outcome)) +
  geom_bar(width = 0.4,  fill=rgb(0.1,0.5,0.8,0.7)) +
  scale_x_discrete("0 (Apto)  1 (No Apto)") +     # configuración eje X (etiqueta del eje)
  scale_y_continuous("Pacientes") +
  labs(title = "Outcome (AOS_HRP_N)")

### Balanceando el outcome
set.seed(123456)
n <- filter(AOS_PSG_N, outcome == "1")
m <- filter(AOS_HRP_N, outcome == "1")
muestreo1 <- sample_n(n, size= 30)     #Datos aleatorios con outcome 1 (PSG)
muestreo2 <- sample_n(m, size= 32)     #Datos aleatorios con outcome 1 (HRP)

AOS_PSG_N1<-rbind(AOS_PSG_N,muestreo1) 
AOS_PSG_N1<-rbind(AOS_PSG_N1,muestreo1)               #Imputando observaciones (PSG)

AOS_HRP_N1<-rbind(AOS_HRP_N,muestreo2)
AOS_HRP_N1<-rbind(AOS_HRP_N1,(sample_n(m, size=15)))  #Imputando observaciones (HRP)

### Graficando outcome balanceado (Dataset PSG)
ggplot(AOS_PSG_N1, aes(x = outcome)) +
  geom_bar(width = 0.4,  fill=rgb(0.1,0.5,0.8,0.7)) +
  scale_x_discrete("0 (Apto)  1 (No Apto)") +     # configuración eje X (etiqueta del eje)
  scale_y_continuous("Pacientes") +
  labs(title = "Outcome (AOS_PSG_N1)")

### Graficando outcome balanceado (Dataset HRP)
ggplot(AOS_HRP_N1, aes(x = outcome)) +
  geom_bar(width = 0.4,  fill=rgb(0.1,0.5,0.8,0.7)) +
  scale_x_discrete("0 (Apto)  1 (No Apto)") +     # configuración eje X (etiqueta del eje)
  scale_y_continuous("Pacientes") +
  labs(title = "Outcome (AOS_HRP_N1)")




# 2. FEATURE SELECTION

# Packages
install.packages("leaps")       #v 3.1
library(leaps)      # model selection functions

datos <- AOS_HRP_N    # Realizar analisis de Feature Selection con un primer dataset (PSG o)
datos <- AOS_PSG_N    # Al finalizar realizar analisis con el segundo dataset (PSG o HRP)

## Aplicando método FORWARD
forward <- regsubsets(outcome ~ ., datos, nvmax = , method = "forward")
summary(forward)

## Aplicando método BACKWARD
backward <- regsubsets(outcome ~ ., datos, nvmax = , method = "backward")
summary(backward)


##LASSO
###packages
install.packages("rsample")       #v 0.1.0
install.packages("glmnet")        #v 4.6-14
install.packages("ggplot2")       #v 3.3.5
install.packages("AmesHousing")   #v 0.0.4
library(rsample) 
library(glmnet)
library(ggplot2)
library(AmesHousing)

### Split para implementar método del LASSO
set.seed(123)
AOS_split <- initial_split(datos, prop = .7, strata = "outcome")
AOS_train <- training(AOS_split)
AOS_test  <- testing(AOS_split)

AOS_train_x <- model.matrix(outcome ~ ., AOS_train)[, -1]
AOS_train_y <- (AOS_train$outcome)

AOS_test_x <- model.matrix(outcome ~ ., AOS_test)[, -1]
AOS_test_y <- (AOS_test$outcome)

# Apply CV Ridge regression to ames data
AOS_lasso <- glmnet(
  x = AOS_train_x,
  y = AOS_train_y,
  alpha = 0
)
# plot results
plot(AOS_lasso, xvar = "lambda")

AOS_lasso <- cv.glmnet(
  x = AOS_train_x,
  y = AOS_train_y,
  alpha = 0
)
# plot results
plot(AOS_lasso)

coef(AOS_lasso, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  ggplot(aes(value, reorder(row, value), color = value > 0)) +
  geom_point(show.legend = FALSE) +
  ggtitle("Influential variables") +
  xlab("Coefficient") +
  ylab(NULL)


##RANDOM FOREST
install.packages("randomForest")    #v 4.6-14
install.packages("ranger")          #v 0.12.1
library(randomForest)
library(ranger)

set.seed(123)

# default RF model
m1 <- randomForest(
  formula = outcome ~ .,
  data    = AOS_train
)
m1
plot(m1)

# number of trees with lowest MSE
which.min(m1$mse)  

# RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)]) 

features <- setdiff(names(AOS_train), "outcome")

set.seed(123)

OOB_RMSE <- vector(mode = "numeric", length = 100)

for(i in seq_along(OOB_RMSE)) {
  
  optimal_ranger <- ranger(
    formula         = outcome ~ ., 
    data            = AOS_train, 
    num.trees       = 500,
    mtry            = 31,
    min.node.size   = 5,
    sample.fraction = .7,
    importance      = 'impurity'
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}

hist(OOB_RMSE, breaks = 20)

optimal_ranger$variable.importance %>% 
  tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(10) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 10 important variables")