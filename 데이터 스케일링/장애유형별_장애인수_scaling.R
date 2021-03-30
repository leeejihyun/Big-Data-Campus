library(cluster)
library(factoextra)

setwd("C:/R/BigDataCampus data")

type_gu_total <- read.csv('type_gu_total.csv', header = T, row.names = 1)
str(type_gu_total)
View(type_gu_total)

summary(type_gu_total)
summary(scale(type_gu_total))

fviz_nbclust(scale(type_gu_total), pam, method = "wss") # 4
fviz_nbclust(scale(type_gu_total), pam, method = "gap_stat")
fviz_nbclust(scale(type_gu_total), pam, method = "silhouette") # 3

pam.res <- pam(type_gu_total, 4)

# 1. Extract cluster medoids 
pam.res$medoids

# 2. Extract clustering vectors 
head(pam.res$cluster)
clusplot(pam.res, main = "Cluster plot, k = 3", color = TRUE)
fviz_cluster(pam.res)
plot(silhouette(pam.res), col = 2:5) # col = color 
fviz_silhouette(silhouette(pam.res))
sil <- silhouette(pam.res)[, 1:3]
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, drop = FALSE]

aggregate(data=type_gu_total, .~pam.res$cluster, mean)
type_gu_total_cluster <- aggregate(data=type_gu_total, .~pam.res$cluster, mean)
View(type_gu_total_cluster)
write.csv(type_gu_total_cluster, file = 'type_gu_total_cluster4.csv')
