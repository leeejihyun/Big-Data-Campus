library(cluster)
library(factoextra)
library(dplyr)

setwd("C:/R/BigDataCampus data")

densitydong <- read.csv('densitydong.csv', header = T, stringsAsFactors = F)
str(densitydong)
View(densitydong)
jungnanggu <- densitydong %>%
  filter((자치구 == '중랑구') & (동 != '소계')) %>% 
  select(-자치구)
str(jungnanggu)
row.names(jungnanggu) <- jungnanggu$동
jungnanggu <- jungnanggu[-1]
str(jungnanggu)
View(jungnanggu)

# 정규화
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

summary(jungnanggu)
summary(normalize(jungnanggu))

fviz_nbclust(normalize(jungnanggu), pam, method = "wss") # 3
fviz_nbclust(normalize(jungnanggu), pam, method = "gap_stat")
fviz_nbclust(normalize(jungnanggu), pam, method = "silhouette") # 3

pam.res <- pam(normalize(jungnanggu), 3)

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

pam.res$cluster
aggregate(data=jungnanggu, .~pam.res$cluster, mean)
jungnanggu_density_cluster <- aggregate(data=jungnanggu, .~pam.res$cluster, mean)
View(jungnanggu_density_cluster)
write.csv(jungnanggu_density_cluster, file = 'jungnanggu_density_cluster.csv')