library(cluster)
library(factoextra)
library(dplyr)

setwd("C:/R/BigDataCampus data")

densitydong <- read.csv('densitydong.csv', header = T, stringsAsFactors = F)
str(densitydong)
View(densitydong)
junggu <- densitydong %>%
  filter((자치구 == '중구') & (동 != '소계')) %>% 
  select(-자치구)
str(junggu)
row.names(junggu) <- junggu$동
junggu <- junggu[-1]
str(junggu)
View(junggu)

# 정규화
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

summary(junggu)
summary(normalize(junggu))

fviz_nbclust(normalize(junggu), pam, method = "wss") # 2
fviz_nbclust(normalize(junggu), pam, method = "gap_stat") # 2
fviz_nbclust(normalize(junggu), pam, method = "silhouette") # 2

pam.res <- pam(normalize(junggu), 2)

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
aggregate(data=junggu, .~pam.res$cluster, mean)
junggu_density_cluster <- aggregate(data=junggu, .~pam.res$cluster, mean)
View(junggu_density_cluster)
write.csv(junggu_density_cluster, file = 'junggu_density_cluster.csv')