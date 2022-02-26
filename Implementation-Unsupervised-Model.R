install.packages('cluster')         #v 2.1.2
install.packages('factoextra')      #v 1.0.7
install.packages('NbClust')         #v 3.0
library(cluster)
library(factoextra)
library(NbClust)

datos <- AOS_PSG_N
datos <- datos[,c("ta_diastolica_diurna","tas","tad","ta_media_diurna","aih30",
                  "ta_sistolica_diurna","tas1","tad1","ta_media_24","ta_diastolica_24",
                  "ta_diastolica_nocturna","iah_sup_psg","gr_mapa_1","ta_media_nocturna",
                  "ta_sistolica_24","tad_valle","desviacion_noche","hematies",
                  "tas_valle","escala_asda","somno_conducir","peso","p_cuello",                    
                  "glucosa")]

datos <- AOS_HRP_N
datos <- datos[,c("epworth_final","trigliceridos","ta_sistolica_diurna_dif",
                  "fcard_noche","imc_final","tiempo_reg_pgr","ta_media_diurna_dif",
                  "c8_bi","fcard_dia","iah_pgr","imc_nuevo","ta_sistolica_24_dif",
                  "hba1c","peso","ta_media_24_dif","outcome","tiempo_valido_pgr","c4",
                  "dif_epworth","ta_sistolica_nocturna_dif","centro_num","tas",
                  "tad_valle","ia_pgr")]


for (i in 1:ncol(datos)) {
  datos[,i] <- as.numeric(datos[,i])
}

datoss <- scale(datos)  #Normalización de los datos
summary(datoss)
sum(is.na(datoss))        #Total datos faltantes
colSums(is.na(datoss))    #Datos faltantes por variable

for (i in 1:ncol(datoss)) {
  if (sum(is.na(datoss[,i]))>25){
    colnames(datoss)[i] <- "Nuevo"
  }
}

datoss <- datoss[,c(-4,-66,-70,-71,-72,-74,-112)]
sum(is.na(datoss))

datoss <- datoss[,c(-4,-62,-66,-68,-69,-70,-107)]
sum(is.na(datoss))

fviz_nbclust(datoss, kmeans, method = "wss")
fviz_nbclust(datoss, kmeans, method = "silhouette")

resnumclust <- NbClust(datoss[,1:60], distance = "euclidean", min.nc = 2, max.nc = 6,
                       metho = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

resnumclust <- NbClust(datoss[,55:95], distance = "euclidean", min.nc = 2, max.nc = 6,
                       metho = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

resnumclust <- NbClust(datos2[,90:128], distance = "euclidean", min.nc = 2, max.nc = 6,
                       metho = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

install.packages("psych")         #v 2.1.9
library(psych)

#set.seed(1234)
set.seed(9745)
#set.seed(15145)
k2 <- kmeans(datoss, centers = 2)

k2$cluster
k2$centers
k2$size
str(k2)

aggregate(datos, by=list(k2$cluster), mean)

fviz_cluster(k2, data = datoss)
fviz_cluster(k2, data = datoss, ellipse.type = "euclid", repel = TRUE, star.plot = TRUE)
fviz_cluster(k2, data = datoss, ellipse.type = "norm")

res2 <- hcut(datoss, k = 2 , stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5, k_colors = c("red", "blue"))

install.packages("FeatureImpCluster")     #v 0.1.5
install.packages("flexclust")             #v 1.4-0
library(FeatureImpCluster)
library(flexclust)

set.seed(6584)  #PSG FINAL
res <- kcca(datoss,k=2)
FeatureImp_res <- FeatureImpCluster(res,as.data.table(datoss))
plot(FeatureImp_res)

set.seed(3564)
res2 <- kcca(datoss,k=2)
FeatureImp_res2 <- FeatureImpCluster(res,as.data.table(datoss2))
plot(FeatureImp_res2)