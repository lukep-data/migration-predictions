
library(readstata13)
# library(cluster.datasets)
library(factoextra)

ab18 <- read.dta13("C:/Users/plutowl/Desktop/data/all_2018_rm_spa-eng_s_v1-0.dta", convert.factors = FALSE)
table(ab18$q14)
table(ab18$q14f)

mo <- ab18[ab18$q14 == 1, ]
mo$unemp <- ifelse(mo$ocup4a == 3 | mo$ocup4a == 7, 1,
                                 ifelse(mo$ocup4a < 3 | mo$ocup4a == 4 | mo$ocup4a == 5 | mo$ocup4a == 6, 0, NA))
mo$single <- ifelse(mo$q11n == 1 | mo$q11n == 4 | mo$q11n == 5 | mo$q11n == 6, 1,
                    ifelse(is.na(mo$q11n), NA, 0))

table(mo$q10new)

mo$pais_c <- as.character(mo$pais)

# q10a - remittances 
# q5a - religious attendance
dem_vars <- c("q1", "q2" , "ur", "single", "ed", "q10new", "unemp")
moc <- mo[, dem_vars]
moc <- moc[rowSums(is.na(moc)) < 5,]
colSums(is.na(moc))

all_column_median <- apply(moc, 2, median, na.rm=TRUE)

# imputing median value with NA 
for(i in colnames(moc)){
  moc[,i][is.na(moc[,i])] = all_column_median[i]
}

moc_std <- apply(moc, 2, scale, center = TRUE, scale = TRUE)

set.seed(615)
WSS = NULL
for (K in 2:10) {
  kms = kmeans(moc_std, centers = K, nstart = 10)
  wssk = sum(kms$withinss)
  WSS = c(WSS, wssk)
}

plot(c(2:10), WSS, xlab = "K", ylab = "WSS(K)")

# fviz_nbclust(moc, kmeans, method='silhouette')

mig_clust <- kmeans(moc_std, centers = 5, nstart = 10)
mig_clust$size
mig_clust$centers


#cluster 1: mostly men, younger, single,  
# 2: like cluster 1 but more women
# 3: older, rural adults, less educated
# 4: married people
# 5: Unemployed
# 6: rural

mig_clust <- kmeans(moc_std, centers = 2, nstart = 10)
mig_clust$centers

# clust 1: female, younger, urban, single, more highly educated, higher income, employed



clust = hclust(dist(moc2_std), method = "complete")
fusion = clust$height
n = nrow(moc2_std)
clusters = 20:1
plot(clusters, fusion)


six_clust = cutree(clust, k = 6)
means_mtx = matrix(0, nrow = 6, ncol = ncol(moc2_std))

for (i in 1:6){
  means_mtx[i,] = colMeans(moc2_std[six_clust == i,])
}

means_df <- as.data.frame(means_mtx)
colnames(means_df) <- colnames(moc2_std[, ])
means_df


library(mclust)
BIC=mclustBIC(moc2)
plot(BIC)
summary(BIC)


mod1=Mclust(moc2,G=2,modelNames=c("EEV"))
summary(mod1,parameters=TRUE)

# plot(mod1,what="classification")

