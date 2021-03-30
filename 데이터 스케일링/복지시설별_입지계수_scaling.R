library(cluster)
library(factoextra)

setwd("C:/R/BigDataCampus data")

enterratio <- read.csv('enterratio.csv', header = T, row.names = 1)
View(enterratio)
str(enterratio)
summary(enterratio)

# 표준화(standardization)

standardize <- function(x){
  return( (x - mean(x)) / sd(x) )
}

names(enterratio)
#시각장애인시설, 장애영유아, 장애인.공동생활가정, 장애인.단기거주시설, 중증장애인, 지적장애인시설, 지체장애인시설, 청각.및.언어장애인시설, 도서.및.출판시설, 수련시설, 수화통역센터, 이동지원센터, 장애인.주간보호시설, 장애인.체육시설, 장애인복지관, 점자도서관, 근로사업장, 보호작업장, 생산품

library(dplyr)
enterratio_std <- enterratio %>% 
  mutate(시각장애인시설 = standardize(시각장애인시설),
                장애영유아 = standardize(장애영유아),
                장애인.공동생활가정 = standardize(장애인.공동생활가정),
                장애인.단기거주시설 = standardize(장애인.단기거주시설),
                중증장애인 = standardize(중증장애인),
                지적장애인시설 = standardize(지적장애인시설),
                지체장애인시설 = standardize(지체장애인시설),
                청각.및.언어장애인시설 = standardize(청각.및.언어장애인시설),
                도서.및.출판시설 = standardize(도서.및.출판시설),
                수련시설 = standardize(수련시설),
                수화통역센터 = standardize(수화통역센터),
                이동지원센터 = standardize(이동지원센터),
                장애인.주간보호시설 = standardize(장애인.주간보호시설),
                장애인.체육시설 = standardize(장애인.체육시설),
                장애인복지관 = standardize(장애인복지관),
                점자도서관 = standardize(점자도서관),
                근로사업장 = standardize(근로사업장),
                보호작업장 = standardize(보호작업장),
                생산품 = standardize(생산품))

View(enterratio_std)
summary(enterratio_std)
summary(scale(enterratio))

fviz_nbclust(scale(enterratio), pam, method = "wss")
fviz_nbclust(scale(enterratio), pam, method = "gap_stat")
fviz_nbclust(scale(enterratio), pam, method = "silhouette") # 2
pam.res <- pam(scale(enterratio), 2)

# 중앙값을 이용한 표준화(standardization)

standardize_med <- function(x){
  return( (x - median(x)) / sd(x) )
}

enterratio_std_med <- enterratio %>% 
  mutate(시각장애인시설 = standardize_med(시각장애인시설),
                장애영유아 = standardize_med(장애영유아),
                장애인.공동생활가정 = standardize_med(장애인.공동생활가정),
                장애인.단기거주시설 = standardize_med(장애인.단기거주시설),
                중증장애인 = standardize_med(중증장애인),
                지적장애인시설 = standardize_med(지적장애인시설),
                지체장애인시설 = standardize_med(지체장애인시설),
                청각.및.언어장애인시설 = standardize_med(청각.및.언어장애인시설),
                도서.및.출판시설 = standardize_med(도서.및.출판시설),
                수련시설 = standardize_med(수련시설),
                수화통역센터 = standardize_med(수화통역센터),
                이동지원센터 = standardize_med(이동지원센터),
                장애인.주간보호시설 = standardize_med(장애인.주간보호시설),
                장애인.체육시설 = standardize_med(장애인.체육시설),
                장애인복지관 = standardize_med(장애인복지관),
                점자도서관 = standardize_med(점자도서관),
                근로사업장 = standardize_med(근로사업장),
                보호작업장 = standardize_med(보호작업장),
                생산품 = standardize_med(생산품))


summary(enterratio_std_med)

fviz_nbclust(enterratio_std_med, pam, method = "wss")
fviz_nbclust(enterratio_std_med, pam, method = "gap_stat")
fviz_nbclust(enterratio_std_med, pam, method = "silhouette") # 2
pam.res <- pam(enterratio_std_med, 2)


# 정규화(normalization)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

summary(normalize(enterratio))

fviz_nbclust(normalize(enterratio), pam, method = "wss") # 2
fviz_nbclust(normalize(enterratio), pam, method = "gap_stat")
fviz_nbclust(normalize(enterratio), pam, method = "silhouette") # 2
pam.res <- pam(normalize(enterratio), 2)

# 1. Extract cluster medoids 
pam.res$medoids

# 2. Extract clustering vectors
pam.res$cluster
data.frame(pam.res$cluster)
clusplot(pam.res, main = "Cluster plot, k = 2", color = TRUE)
fviz_cluster(pam.res)
plot(silhouette(pam.res), col = ncol(silhouette(pam.res))) # col = color 
fviz_silhouette(silhouette(pam.res))
sil <- silhouette(pam.res)[, 1:3]
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, drop = FALSE]

enterratio_cluster <- data.frame(aggregate(data=enterratio, .~pam.res$cluster, mean))
View(enterratio_cluster)
write.csv(enterratio_cluster, file = "enterratio_cluster.csv")
