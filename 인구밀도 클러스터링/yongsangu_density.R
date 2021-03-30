library(cluster)
library(factoextra)
library(dplyr)

setwd("C:/R/BigDataCampus data")

densitydong <- read.csv('densitydong.csv', header = T, stringsAsFactors = F)
str(densitydong)
View(densitydong)
yongsangu <- densitydong %>%
  filter((자치구 == '용산구') & (동 != '소계')) %>% 
  select(-자치구)
str(yongsangu)
View(yongsangu)
row.names(yongsangu) <- yongsangu$동
yongsangu <- yongsangu[-1]

# 정규화
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

summary(yongsangu)
summary(normalize(yongsangu))

fviz_nbclust(normalize(yongsangu), pam, method = "wss") # 3
fviz_nbclust(normalize(yongsangu), pam, method = "gap_stat")
fviz_nbclust(normalize(yongsangu), pam, method = "silhouette") # 3

pam.res <- pam(normalize(yongsangu), 3)

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

aggregate(data=yongsangu, .~pam.res$cluster, mean)
yongsangu_density_cluster <- aggregate(data=yongsangu, .~pam.res$cluster, mean)
View(yongsangu_density_cluster)
write.csv(yongsangu_density_cluster, file = 'yongsangu_density_cluster.csv')
