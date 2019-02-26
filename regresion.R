###TAREA 1 (REGRESIÓN LINEAL SIMPLE Y MÚLTIPLE)

#En primer lugar cargamos los datos
setwd("C:/Users/Jose/Downloads/california")
xtra <- read.csv("california.dat", comment.char="@")

#Le damos nombre a las variables de forma automática para facilitar 
#el acceso a las variables
n <- length(names(xtra)) - 1
names(xtra)[1:n] <- paste ("X", 1:n, sep="")
names(xtra)[n+1] <- "Y"

#Comprobamos que se han asignado bien los nombres
names(xtra)

#Para trabajar más cómodamente con el dataframe
attach(xtra)

#Visualizamos todas las variables para ver si hay alguna relación que podamos
#ver a simple vista
temp <- xtra
plotY <- function(x,y){
  plot(temp[,y]~temp[,x], xlab=names(temp)[x], ylab = names(temp)[y]) 
}

par(mfrow=c(3,3))
x <- sapply(c(1:(dim(temp)[2]-1)), plotY, dim(temp)[2])
par(mfrow=c(1,1))

#A simple vista la única variable que parece tener alguna realación lineal con 
#Y es X8. Aún así vamos a ver qué nos dice el R2 sobre la relación lineal
#que pueda existir entre las variables.

fitx1 <- lm(Y ~ X1)
fitx2 <- lm(Y ~ X2)
fitx3 <- lm(Y ~ X3)
fitx4 <- lm(Y ~ X4)
fitx5 <- lm(Y ~ X5)
fitx6 <- lm(Y ~ X6)
fitx7 <- lm(Y ~ X7)
fitx8 <- lm(Y ~ X8)

summary(fitx1) #R2: 0.002111
summary(fitx2) #R2: 0.02079
summary(fitx3) #R2: 0.01115
summary(fitx4) #R2: 0.018
summary(fitx5) #R2: 0.002564
summary(fitx6) #R2: 0.0006067
summary(fitx7) #R2: 0.004342
summary(fitx8) #R2: 0.4734

#Como sospechábamos la que más relación tiene es X8, seguido de X2. Vamos a intentar
#hacer un modelo lineal con ambas variables para ver si mejora el R2 significativamente.
fit2_8 <- lm(Y ~ X2 + X8)
summary(fit2_8) 

#Las mejoras obtenidas son mínimas

#Vamos a seguir ahora la estrategia de probar con todas las variables e ir eliminando en función
#del p-value asociado
fitall <- lm(Y ~ ., xtra) #Adjusted R-squared: 0.637
summary(fitall) 
#El R2 sigue siendo bajo para nuestro gusto, aunque la interpretabilidad del modelo es alta.

#Vamos a probar eliminando la que peor p-value tiene 
fitnox7 <- lm(Y ~ . - X7, xtra) #Adjusted R-squared: 0.6363
summary(fitnox7)

#Al quitar esa variable empeora muy levemente el R2 ajustado
#Vamos a quitar otra más a ver qué tal

fitnox7_nox4<-lm(Y ~.-X7-X4,xtra) #Adjusted R-squared: 0.6343
summary(fitnox7_nox4)

#Hemos eliminado X4 también porque era la variable que tenia el t value más pequeño
#en valor absoluto y por lo tanto su p value iba a ser mayor. Hemos empeorado un poco
#el R2 pero no significativamente.

#Ahora vamos a seguir otra estrategia que será la de buscar interacciones algo más
#complejas entre las varibles. Perderemos interpretabilidad, pero buscamos ahora
#obtener un R2 ajustado mayor


#Primero vamos a intentar mejorarlo añadiendo términos que dependan de X8
fitx8_cuadrado <- lm(Y ~ X8 + I(X8^2)) #Adjusted R-squared: 0.478
summary(fitx8_cuadrado)#Adjusted R-squared: 0.478
#Se obtiene mejor resultado que con la variable lineal pero peor que usando todas
fitx8_cubo <- lm(Y ~ X8 + I(X8^2) + I(X8^3))
summary(fitx8_cubo) #Adjusted R-squared: 0.4861

#De nuevo vemos que perdemos interpretabilidad y no ganamos demasiado en lo que refiere al 
#R2 ajustado. Vamos a probar utilizando todas las variables más estas que hemos añadido.

fitall_x8_2 <- lm(Y ~ . +I(X8^2),xtra) 
summary(fitall_x8_2) #Adjusted R-squared: 0.6397

fitall_x8_3 <- lm(Y ~ .+I(X8^2) + I(X8^3),xtra) 
summary(fitall_x8_3) #Adjusted R-squared: 0.6532

#Los resultados mejoran en algo el R2 ajustado, pero muy poco. Además vemos que los p-value
#de algunos coeficientes son bastante altos. Con lo cual si quisiéramos seguir con este
#modelo quizás deberíamos quitar algunas variables.

#Vamos a probar ahora algunas interacciones sugeridas en la prática de laboratorio 2
fit_sug_1 <- lm(Y~ X8+I(X8^2)+I(X8^3)+I(log(X3))+I(log(X4/X6))
                +I(log(X5/X6))+I(log(X6/X7))+I(log(X7)),xtra)
summary(fit_sug_1) #Adjusted R-squared: 0.6244 
#No mejora demasiado el modelo

fit_sug_2 <- lm(Y~ .+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)+I(X8^4)
                +X7*X8*X4*X5*X6,xtra)
summary(fit_sug_2) #Adjusted R-squared: 0.6862
#Aquí sí se mejora algo más el R2. Perdemos interpretabilidad porque metemos muchas
#interacciones, pero todavía podríamos sacar algo más.

#A partir de aqui vamos a intentar mejorar el R2 ajustado sin preocuparnos por
#la interpretabilidad. 

fit_r2_1 <- lm(Y~ X1*X2*X3*X4*X5*X6*X7*X8)

summary(fit_r2_1) #Adjusted R-squared: 0.7143


fit_r2_2 <- lm(Y~ (X1*X2*X3*X4*X5*X6*X7*X8) + I(X1^2)*I(X2^2)*I(X3^2)*I(X4^2)*I(X5^2)*I(X6^2)*I(X7^2)*I(X8^2) )

summary(fit_r2_2) #Adjusted R-squared: 0.7492


fit_r2_3 <- lm(Y~ (X1*X2*X3*X4*X5*X6*X7*X8) + I(X1^2)*I(X2^2)*I(X3^2)*I(X4^2)*I(X5^2)*I(X6^2)*I(X7^2)*I(X8^2)+I(log(X3))+I(log(X4/X6))
               +I(log(X5/X6))+I(log(X6/X7))+I(log(X7)) )

summary(fit_r2_3) #Adjusted R-squared: 0.7515

#El último modelo es el que mejor R2 ajustado da, pero tiene demasiadas interacciones.
#De hecho muy probablemente sobreajustaría los datos de training si hiciéramos un kfcv
#para comprobar lo bueno que es.


#COMENTARIOS: como hemos visto los métodos lineales más sencillos utilizando pocas variables
#daban resultados muy pobres. Al hacer regresión múltiple y eliminar algunas variables hemos
#conseguido un resultado mejor, complicando mínimamente el modelo.
#En cuanto hemos querido complicar el modelo para mejorar el R2 ajustado, las mejoras han sido
#mínimas y el intercambio entre interpretabilidad y mejora del modelo era muy desfavorable.
#Finalmente hemos probado interacciones más complicadas entre las variables para mejorar el R2
#y hemos conseguido mejorar bastante el R2 ajustado con respecto al inicial. Pero la mejora
#nuevamente no compensa la complejidad que hemos añadido al modelo.

#Cargamos los paquetes que vamos a necesitar

require(MASS)
require(kknn)
#Utilizamos el modelo knn con todas las variables
#Por defecto k=7 y el kernell es 'optimal'

fitknn1<-kknn(Y~.,xtra,xtra)
names(fitknn1)

#Hacemos un plot para comprobar gráficamente cómo se ajusta el modelo 
#a los datos reales

plot(Y~X8)
points(X8,fitknn1$fitted.values,col="blue",pch=20)

#Calculamos el error cuadrático medio

yprime = fitknn1$fitted.values
sqrt(sum((Y-yprime)^2)/length(yprime))

#El RMSE es 39132.79
#A continuación vamos a utilizar el método knn con otras variables y calcular
#el RMSE para ver si podemos reducir el error con una elección más apropiada
#de las variables

fitknn2 <- kknn(Y ~ X1*X2*X3*X4*X5*X6*X7*X8, xtra, xtra)
yprime = fitknn2$fitted.values; sqrt(sum((Y-yprime)^2)/length(yprime))

#El RMSE es 38333.7

fitknn3 <- kknn(Y ~ X8+I(X8^2)+I(X8^3)+I(log(X3))+I(log(X4/X6))
                +I(log(X5/X6))+I(log(X6/X7))+I(log(X7)), xtra, xtra)
yprime = fitknn3$fitted.values; sqrt(sum((Y-yprime)^2)/length(yprime))

#El RMSE es de 43654,97

fitknn4 <- kknn(Y ~ .+X4*X7*X8, xtra, xtra)
yprime = fitknn4$fitted.values; sqrt(sum((Y-yprime)^2)/length(yprime))

#El RMSE es de 37821.25

fitknn5 <- kknn(Y ~ .+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)+I(X8^4)
                +X7*X8*X4*X5*X6, xtra, xtra)
yprime = fitknn5$fitted.values; sqrt(sum((Y-yprime)^2)/length(yprime))

#El RMSE es de 36062.18

#A continuación vamos a comparar la media de los errores cuadráticos medios
#de los distintos métodos (knn, lm y lm con interacciones no lineales) para 
#las cinco particiones que hemos hecho del conjunto de datos california

nombre <- "california"
run_lminter_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep=""); x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep=""); x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep=""); names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep=""); names(x_tst)[In+1] <- "Y"
  if (tt == "train") { test <- x_tra }
  else { test <- x_tst }
  fitMulti=lm(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)
              +I(X8^4)+X7*X8*X4*X5*X6,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lminterMSEtrain<-mean(sapply(1:5,run_lminter_fold,nombre,"train"))
lminterMSEtest<-mean(sapply(1:5,run_lminter_fold,nombre,"test"))
#En este caso la media del error cuadrático medio sobre los conjunto
#de entrenamiento es 4151294153 y sobre los conjuntos test 5319480881

#Vamos a hacer el mismo procedimiento para knn y para la regresión lineal
#múltiple sin interacciones para comparar. Esto es simplemente para ver cómo
#funcionan los métodos sobre el conjunto de datos california. Para hacer una
#comparación general de los modelos knn y lm más tarde usaremos resultados
#generales sobre más conjuntos de datos.

run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep=""); x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep=""); x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep=""); names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep=""); names(x_tst)[In+1] <- "Y"
  if (tt == "train") { test <- x_tra }
  else { test <- x_tst }
  fitMulti=lm(Y~.,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

#En este caso los resultados sobre los conjuntos test son mejores con un MSE de
#4844365688, pero sin embargo sobre los conjuntos de entrenamiento son peores con
#un MSE de 4826189710. De donde podemos inferir que el anterior modelo estaba 
#haciendo un poco de overfitting con los conjuntos de entrenamiento.

run_knn_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep=""); x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep=""); x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep=""); names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep=""); names(x_tst)[In+1] <- "Y"
  if (tt == "train") { test <- x_tra }
  else { test <- x_tst }
  fitMulti=kknn(Y~.,x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))

#En este caso los resultados sobre los conjuntos de entrenamiento y sobre los
#conjuntos test son mejores con un MSE de 1560868807 y 3845914481
#respectivamente.

#Vamos a probar a hace kknn con las variables que mejor resultado global me han dado, para
#ver si realmente hay una mejora en los conjuntos test o hay overfitting

run_knninter_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep=""); x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep=""); x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep=""); names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep=""); names(x_tst)[In+1] <- "Y"
  if (tt == "train") { test <- x_tra }
  else { test <- x_tst }
  fitMulti=kknn(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)+I(X8^4)
                +X7*X8*X4*X5*X6,x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knninterMSEtrain<-mean(sapply(1:5,run_knninter_fold,nombre,"train"))
knninterMSEtest<-mean(sapply(1:5,run_knninter_fold,nombre,"test"))

#El MSE baja en los conjuntos de training y sube en los test, con lo cual al meter estas variables
#estamos teniendo un modelo más sofisticado que hace overfitting. Eso no quiere decir que siempre que
#añadamos más variable vamos a tener peores resultados. Pero en este caso la elección es mala.

run_knninter2_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep=""); x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep=""); x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep=""); names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep=""); names(x_tst)[In+1] <- "Y"
  if (tt == "train") { test <- x_tra }
  else { test <- x_tst }
  fitMulti=kknn(Y~.+I(X8^2)+I(X8^3)+I(log(X3))+I(log(X4/X6))
                +I(log(X5/X6))+I(log(X6/X7))+I(log(X7)),x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knninter2MSEtrain<-mean(sapply(1:5,run_knninter2_fold,nombre,"train"))
knninter2MSEtest<-mean(sapply(1:5,run_knninter2_fold,nombre,"test"))

#Este modelo knn funciona un poco mejor que el anterior, pero sigue siendo peor que el 
#obtenido utilizando todas las variables sin interacciones ni transformaciones sobre ellas.

#Ahora vamos a comparar los métodos knn y lm utilizando los resultados de ambos métodos
#sobre distintos datasets, obtenidos del fichero regr_test_alumnos.csv


#Leemos la tabla de resultados en los conjuntos test que hemos descargado de prado
resultados <- read.csv("regr_test_alumnos.csv")
tablatst <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatst) <- resultados[,1]
#Leemos la tabla con los errores medios de entrenamiento
resultados <- read.csv("regr_train_alumnos.csv")
tablatra <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatra) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatra) <- resultados[,1]

#Comparamos lm con knn utilizando el test de wilcoxon y normalizando los errores porque
#estamos haciendo regresión. Utilizamos la normalización sugerida en los apuntes.
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

#Aplicamos el test
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas #78
Rmenos #93
pvalue #0.7660
#El p-value nos dice que la probabilidad de que sean distintos los resultados
#de ambos modelos es de 1-0.7660=0.2340. Lo cual quiere decir que dan prácticamente
#los mismos resultados. O más bien que no hay evidencias significativas para decir que 
#uno es mejor que otro.


#Ahora aplicamos el test de friedman para ver si entre LM KNN y M5' hay algún modelo que 
#sea mejor o peor que el resto.
test_friedman <- friedman.test(as.matrix(tablatst))
test_friedman
#El p-value es 0.01467 lo cual es lo suficientemente bajo como para decir que hay un modelo
#distinto al resto. Hacemos un post-hoc Holm para ver cuál es distinto.
tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)

#Como vemos existen diferencias significativas para afirmas que M5' es mejor que LM y KNN
#sin embargo LM y KNN parece que son bastante similares.

#Vamos a repetir el mismo proceso para los resultados de training

difs_tra <- (tablatra[,1] - tablatra[,2]) / tablatra[,1]
wilc_1_2_tra <- cbind(ifelse (difs_tra<0, abs(difs_tra)+0.1, 0+0.1), ifelse (difs_tra>0, abs(difs_tra)+0.1, 0+0.1))
colnames(wilc_1_2_tra) <- c(colnames(tablatra)[1], colnames(tablatra)[2])
head(wilc_1_2_tra)

#Aplicamos el test
LMvsKNNtst_tra <- wilcox.test(wilc_1_2_tra[,1], wilc_1_2_tra[,2], alternative = "two.sided", paired=TRUE)
Rmas_tra <- LMvsKNNtst_tra$statistic
pvalue_tra <- LMvsKNNtst_tra$p.value
LMvsKNNtst_tra <- wilcox.test(wilc_1_2_tra[,2], wilc_1_2_tra[,1], alternative = "two.sided", paired=TRUE)
Rmenos_tra <- LMvsKNNtst_tra$statistic
Rmas_tra #10
Rmenos_tra #161
pvalue_tra #0.000328064
#El p-value en este caso indica que sí existe diferencia entre los modelos. Claro que esto es
#para los conjuntos de entrenamiento, pero no deja de ser sorprendente y nos indica que knn
#funciona mucho mejor sobre los conjuntos de training.


#Ahora aplicamos el test de friedman 
test_friedman_tra <- friedman.test(as.matrix(tablatra))
test_friedman_tra
#El p-value es 3.843e-050.01467 con lo cual parece claro que uno de los métodos funciona 
#mejor o peor que los demás sobre los conjuntos de training

tam_tra <- dim(tablatra)
groups_tra <- rep(1:tam_tra[2], each=tam_tra[1])
pairwise.wilcox.test(as.matrix(tablatra), groups_tra, p.adjust = "holm", paired = TRUE)

#  1      2     
#2 0.0031 -     
#3  3 0.0032 0.0032
#Lo cual quiere decir que con respecto a los conjuntos de training hay diferencias significativas
#entre todos los metodos.

