library(FNN)
library(fda.usc)
library(readxl)
library(dbscan)

# View the contents of the wine data
wine <- readxl::read_excel("wine.xlsx",col_names = T)
str(wine)

# Scatterplot of wine pH against alcohol
plot(pH ~ alcohol, data = wine)

# Calculate the 5 nearest neighbors distance
wine_nn <- get.knn(wine, k = 5)

# View the distance matrix
head(wine_nn$nn.dist)

# Create score by averaging distances
wine_nnd <- rowMeans(wine_nn$nn.dist)
hist(wine_nnd)
boxplot(wine_nnd)
which.max(wine_nnd)

#Distribucion bootstrap
n<- dim(wine)[1]
B <- 1000
dist_boot_index <- matrix(NA,nrow = B,ncol=n)

# Eliminar datos atipicos
md <- mdepth.MhD(wine)$dep
LI <- quantile(md,probs = 0.1)
t_out <- sum(md < LI)
out <- which(md < LI)
hist(md,main = "Profundidad")
hist(md[-out])
muestra <- wine[-out,]
m <- dim(muestra)[1]

for(i in 1:B){
  ind <- sample(1:m,size = n,replace = TRUE)
  boot_wine <- muestra[ind,]
  boot_wine_nn <- get.knn(boot_wine, k = 5)
  boot_wine_nnd <- rowMeans(boot_wine_nn$nn.dist)
  dist_boot_index[i,] <- boot_wine_nnd 
}
hist(wine_nnd)
hist(dist_boot_index)

LI_boot <- quantile(dist_boot_index,probs = 0.99)
t_out <- sum(wine_nnd >= LI_boot);
t_out
boot_out <- which(wine_nnd >= LI_boot)

attach(wine)
plot(pH ~ alcohol, data = wine)
points(x = alcohol, y=pH, cex = (wine_nnd)*10, pch = 1, col="red")
text(x = alcohol[wine_nnd>=LI_boot][1:5],y = pH[wine_nnd>=LI_boot][1:5], 
     labels = round(wine_nnd, 2)[wine_nnd>=LI_boot][1:5], pos = 3)

plot(pH[boot_out] ~ alcohol[boot_out], data = wine)
plot(pH[-boot_out] ~ alcohol[-boot_out], data = wine)

######## Mismo analisis con lof

# Calculate the 5 nearest neighbors distance
wine_nn_lof <- lof(scale(wine), k = 5)

# lof score

hist(wine_nn_lof)
boxplot(wine_nn_lof)
which.max(wine_nn_lof)

dist_boot_index_lof<-matrix(NA,nrow = B,ncol=n)

for(i in 1:B){
  ind <- sample(1:m,size = n,replace = TRUE)
  boot_wine <- muestra[ind,]
  boot_wine_lof <- lof(scale(boot_wine), k = 5)
  dist_boot_index_lof[i,] <- boot_wine_lof 
}
hist(wine_nn_lof)
hist(dist_boot_index_lof)


LI_boot <- quantile(dist_boot_index_lof,probs = 0.99)
t_out <- sum(wine_nn_lof >= LI_boot)
t_out
boot_out <- which(wine_nn_lof >= LI_boot)

attach(wine)
plot(pH ~ alcohol, data = wine)
points(x = alcohol, y=pH, cex = (wine_nn_lof), pch = 1, col="red")
text(x = alcohol[wine_nn_lof>=LI_boot][1:5],y = pH[wine_nn_lof>=LI_boot][1:5], 
     labels = round(wine_nn_lof, 2)[wine_nn_lof>=LI_boot][1:5], pos = 3)

plot(pH[boot_out] ~ alcohol[boot_out], data = wine)
plot(pH[-boot_out] ~ alcohol[-boot_out], data = wine)
