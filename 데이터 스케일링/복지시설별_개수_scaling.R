library(cluster)
library(factoextra)

setwd("C:/R/BigDataCampus data")

data <- read.csv('merge.csv', header = T, row.names = 1)
View(data)

summary(data)
summary(scale(data))

fviz_nbclust(scale(data), pam, method = "wss")
fviz_nbclust(scale(data), pam, method = "gap_stat")
fviz_nbclust(scale(data), pam, method = "silhouette") # 2

pam.res <- pam(data, 2)

# 1. Extract cluster medoids 
pam.res$medoids

# 2. Extract clustering vectors 
head(pam.res$cluster)
clusplot(pam.res, main = "Cluster plot, k = 2", color = TRUE)
fviz_cluster(pam.res)
plot(silhouette(pam.res), col = 2:5) # col = color 
fviz_silhouette(silhouette(pam.res))
sil <- silhouette(pam.res)[, 1:3]
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, drop = FALSE]

aggregate(data=data, .~pam.res$cluster, mean)
data_cluster <- aggregate(data=data, .~pam.res$cluster, mean)
View(data_cluster)
write.csv(data_cluster, file = 'data_cluster.csv')
