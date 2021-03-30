library(cluster)
library(factoextra)
library(dplyr)

setwd("C:/R/BigDataCampus data")

densitydong <- read.csv('densitydong.csv', header = T, stringsAsFactors = F)
str(densitydong)
View(densitydong)
seongbukgu <- densitydong %>%
  filter((자치구 == '성북구') & (동 != '소계')) %>% 
  select(-자치구)
str(seongbukgu)
row.names(seongbukgu) <- seongbukgu$동
seongbukgu <- seongbukgu[-1]
str(seongbukgu)
View(seongbukgu)

# 정규화
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

summary(seongbukgu)
summary(normalize(seongbukgu))

fviz_nbclust(normalize(seongbukgu), pam, method = "wss") # 3
fviz_nbclust(normalize(seongbukgu), pam, method = "gap_stat")
fviz_nbclust(normalize(seongbukgu), pam, method = "silhouette") # 2

pam.res <- pam(normalize(seongbukgu), 2)

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

pam.res$cluster
aggregate(data=seongbukgu, .~pam.res$cluster, mean)
seongbukgu_density_cluster <- aggregate(data=seongbukgu, .~pam.res$cluster, mean)
View(seongbukgu_density_cluster)
write.csv(seongbukgu_density_cluster, file = 'seongbukgu_density_cluster.csv')