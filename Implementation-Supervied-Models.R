# 3. IMPLEMENTACIÓN DE MODELOS

###packages
install.packages('gridExtra')     #v 4.0.2
install.packages('grid')          #v 2.3
library(gridExtra)
library(grid)

## Dataset HRP
datos <- AOS_HRP_N1

###subset Forward
datos <- datos[,c("quick", "tad1","hora_despertar","c10_A",
                  "epworth_final","gr_mapa_1","outcome")]

###subset Backward
datos <- datos[,c("quick","tas2","tas_pico","hora_despertar",
                  "tiempo_valido_pgr","tc90_pgr","c5","outcome")]

###subset LASSO
datos <- datos[,c("desviacion","somno_conducir","c1","c6_bi_F","c2","nveces_pr",
                  "repe_pr","tipo_mascara","escala_asda","c7_A","c9_A","tsh",
                  "aih30","c6","c5","c10_A","creatinina","gr_mapa_3","gr_mapa_2",
                  "c6_bi","eq50_items_dif","repe_mapa","c7_bi_F",
                  "gr_mapa_1","outcome")]

###subset Random Forest
datos <- datos[,c("ta_media_nocturna","colesterol","tas2","ta_media_24",
                  "ta_diastolica_24","sf36_fm","urico","ta_media_diurna",
                  "tad1","trigliceridos","outcome")]


## Dataset PSG
datos <- AOS_PSG_N1

###subset Forward
datos <- datos[,c("talla","tiempo_reg_psg","ind_arousal_psg","c10_A",
                  "hospital_num","intubaciones_num","outcome")]

###subset Backward
datos <- datos[,c("talla","urico","trigliceridos","ta_media_24",
                  "autocpap_valida","gr_mapa_2","ta_sistolica_diurna_dif",
                  "outcome")]

###subset LASSO
datos <- datos[,c("ta_sistolica_diurna_dif","ta_media_24_dif","c7_bi",
                  "ta_media_diurna_dif","ta_diastolica_24_dif","c8_A",
                  "desviacion","ta_diastolica_diurna_dif","tsh","c9_A","c7_A",
                  "urico","hospital_dias","c6","c7_bi_F","gr_mapa_1","hematies",
                  "c10_A","eq50_items_dif","gr_mapa_2","c6_bi","etapa","creatinina",
                  "hospital_num","autocpap_valida","outcome")]

###subset Random Forest
datos <- datos[,c("p_rem_psg","hemoglobina","ta_diastolica_24_dif","alt",
                  "urico","ta_diastolica_nocturna_dif","ta_media_diurna_dif",
                  "ta_diastolica_diurna_dif","trigliceridos","ind_arousal_psg",
                  "outcome")]   #nueva base de datos


for (i in 1:ncol(datos)) {
  datos[,i] <- as.numeric(datos[,i])
}

datos$outcome <- as.factor(datos$outcome)    #se cambia variable grupo2 a factor

inTrain <- createDataPartition(y = datos$outcome, p = 0.70, list =FALSE)  # Particion con caret
train   <- datos[inTrain,]
test    <- datos[-inTrain,]

# Data para validación
x_test  <- subset(test, select =-c(outcome)) # Se eliminó la variable target
y_test  <- subset(test, select = c(outcome)) # Regresa solo la varible outcome

control <- trainControl(method = 'repeatedcv',number = 10,repeats = 3) #Cross validation
set.seed(2000)  #semilla para forward

##MODELO LOGISTICO
glmfit <- train(outcome ~.,
                data = train,
                method = 'glm',
                preProc = c("center", "scale"),
                trControl = control)              #MODELO LOGISTICO
#Modelo logistico:
predictionglm <- predict(glmfit, newdata = x_test, type="prob")[,2]  # Probabilidad de que sea 1(no cumple)
predglm01     <- ifelse(predictionglm > 0.5, 1,0)
predglm01     <- as.factor(predglm01)
#test$diagnosis <- ifelse(datos$outcome=="B",'0','1')
datos1 <- as.factor(test$outcome)
matriz1 <- confusionMatrix(predglm01,datos1)
matriz1

sum(is.na(predictiongKNN))


##MODELO KNN
metric  <- "Accuracy"
set.seed(2013)

fit.knn <- train(outcome ~., data=train,
                 method="knn",
                 metric=metric,
                 trControl=control)             #MODELO KNN

#Modelo KNN:
predictiongKNN <- predict(fit.knn, newdata = x_test, type="prob")[,2]  # Probabilidad de que sea 1(no cumple)
predglkNN01     <- ifelse(predictiongKNN > 0.5, 1,0)
predglkNN01     <- as.factor(predglkNN01)
matriz2 <- confusionMatrix(predglkNN01,datos1)
matriz2


##MODELO REDES NEURONALES
install.packages("neuralnet")       #v 1.44.2
install.packages("h2o")             #v 3.34.0.3
library(neuralnet)
library(h2o)

h2o.init(nthreads = -1)
set.seed(2011)


#train$outcome <- factor(train$outcome, levels = c("0", "1"), labels = c("apto", "no apto"))
#test$outcome <- factor(test$outcome, levels = c("0", "1"), labels = c("apto", "no apto"))
classifier = h2o.deeplearning(y = 'outcome',
                              training_frame = as.h2o(train),
                              activation = 'Rectifier',
                              hidden = c(10, 10),
                              epochs = 100,
                              train_samples_per_iteration = -2)

plot(classifier)
prob_pred <- h2o.predict(classifier, newdata = as.h2o(x_test))

y_pred <- as.vector(ifelse(prob_pred$predict == "1", 1, 0))

predh2o     <- as.factor(y_pred)
matriz3 <- confusionMatrix(predh2o,datos1)
matriz3


##MODELO SUPPORT VECTOR MACHINE

install.packages("kernlab")        #v 0.9-29
install.packages("e1071")          #v 1.7-9
install.packages("ISLR")           #v 1.4
install.packages("RColorBrewer")   #v 1.1-2
library(kernlab)
library(e1071)
library(ISLR)
library(RColorBrewer)

set.seed(2012)

train$outcome <- ifelse(train$outcome=="apto",0,1)
test$outcome <- ifelse(test$outcome=="apto",0,1)

mod <- svm(outcome ~ ., data = train)
#table(datos[t.ids,"outcome"], fitted(mod), dnn = c("Actual" , "Predicho"))
pred <- predict(mod, test)
pred <- ifelse(pred > 0.5, 1,0)
pred <- as.factor(pred)
matriz4 <- confusionMatrix(pred,datos1)
matriz4

##PREPARANDO PREDICCIONES
test01 <- as.numeric(test$outcome)
predglm01 <- as.numeric(predglm01)
predglkNN01 <- as.numeric(predglkNN01)
predh2o <- as.numeric(predh2o)
pred <- as.numeric(pred)

#CURVA ROC.
install.packages("pROC")
library("pROC")

par(pty="s")
plot.roc(roc(test01, predglm01), col = "blue", lwd = 3, 
         main = "AUC for the Logistic model", xlab = "1 - Specificity", legacy.axes = TRUE)
plot.roc(roc(test01, predglkNN01), col = "blue", lwd = 3, 
         main = "AUC for the KNN model", xlab = "1 - Specificity", legacy.axes = TRUE)
plot.roc(roc(test01, y_pred), col = "blue", lwd = 3, 
         main = "AUC for the Neural Networks model", xlab = "1 - Specificity", legacy.axes = TRUE)
plot.roc(roc(test01, pred), col = "blue", lwd = 3, 
         main = "AUC for the Super Vector Machine model", xlab = "1 - Specificity", legacy.axes = TRUE)

par(pty="s")
plot.roc(roc(test01, predglm01), col = "blue", lwd = 3, 
         main = "Comparison of AUC for the different models", xlab = "1 - Specificity", legacy.axes = TRUE)
plot.roc(roc(test01, predglkNN01), add = TRUE, 
         col = "aquamarine4", lwd = 3)
plot.roc(roc(test01, y_pred), add = TRUE, 
         col = "purple", lwd = 3)
plot.roc(roc(test01, pred), add = TRUE, 
         col = "orange",lty =1, lwd = 3)

legend(x = "bottomright", legend = c("Logistic", "KNN", "Neural Networks", "SVM"), 
       lty = c(1, 1, 1, 1), lwd = c(3, 3, 3, 3),
       col = c("blue", "aquamarine4", "purple", "orange"))