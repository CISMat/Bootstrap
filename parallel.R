library(parallel)
detectCores()

library(snow)
set.seed(1237)#semilla de aleatoriedad. Permite reproducibilidad
sleep = sample(1:10,10) # secuencia en la cual se interumpira el procesamiento
sleep
cl = makeCluster(4, type="SOCK")
clusterSplit(cl, sleep) #split de las interrupciones por nodo

set.seed(1237)
#cl = makeCluster(4, type="SOCK")
View(airquality)
newairquality <- airquality[sample(1:nrow(airquality), 10000000,
                                   replace = TRUE), 1:4]
colMeans(newairquality,na.rm = TRUE)
clusterApply(cl, newairquality, mean, na.rm = TRUE)
st <- snow.time(clusterApply(cl, newairquality, mean, na.rm = TRUE))
stLB <- snow.time(clusterApplyLB(cl, newairquality, mean, na.rm = TRUE))
stPL <- snow.time(parLapply(cl, newairquality, mean, na.rm = TRUE))

plot(st, title="clusterApply")
plot(stLB, title="clusterApplyLB")
plot(stPL, title="parLapply")
