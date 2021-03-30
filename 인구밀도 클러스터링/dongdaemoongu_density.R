library(cluster)
library(factoextra)
library(dplyr)

setwd("C:/R/BigDataCampus data")

densitydong <- read.csv('densitydong.csv', header = T, stringsAsFactors = F)
str(densitydong)
View(densitydong)
dongdaemoongu <- densitydong %>%
  filter((자치구 == '동대문구') & (동 != '소계')) %>% 
  select(-자치구)
str(dongdaemoongu)
View(dongdaemoongu)
row.names(dongdaemoongu) <- dongdaemoongu$동
dongdaemoongu <- dongdaemoongu[-1]

# 정규화
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

summary(dongdaemoongu)
summary(normalize(dongdaemoongu))

fviz_nbclust(normalize(dongdaemoongu), pam, method = "wss") # 2, 4
fviz_nbclust(normalize(dongdaemoongu), pam, method = "gap_stat")
fviz_nbclust(normalize(dongdaemoongu), pam, method = "silhouette") # 2

pam.res <- pam(normalize(dongdaemoongu), 2)

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

aggregate(data=dongdaemoongu, .~pam.res$cluster, mean)
dongdaemoongu_density_cluster <- aggregate(data=dongdaemoongu, .~pam.res$cluster, mean)
View(dongdaemoongu_density_cluster)
write.csv(dongdaemoongu_density_cluster, file = 'dongdaemoongu_density_cluster.csv')
