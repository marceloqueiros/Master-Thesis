#https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/
rm(list=ls())
library("cluster")
library("factoextra")
library("magrittr")
library("oce")
library("ggplot2")

#library("plotrix")
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\dataset\\"
all_metrics = readRDS(paste(path,'generated_files\\all_metrics_out_NEW.rds', sep = "")) #RDS
#all_metrics$cik = as.numeric(all_metrics$cik)
rownames(all_metrics) <- seq(length=nrow(all_metrics)) # to numeric where it is
#SCALE
#complex_met = apply(all_metrics[5:52], 2, rescale) #Scale variables
complex_met = scale(all_metrics[,5:52])              #Scale variables
#unscale = t(apply(complex_met, 1, function(r)r*attr(complex_met,'scaled:scale') + attr(complex_met, 'scaled:center')))
#(v2 - attr(v1_scl, 'scaled:center')) / attr(v1_scl, 'scaled:scale')

#################Partitioning clustering
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\"
fviz_nbclust(complex_met, kmeans, method = "gap_stat", k.max = 40) #Determining the optimal number of clusters
ggsave(paste(path,"images\\number_clusters.png", sep = ""), scale = 3)


#5 and 6 are good numbers of clusters

number_clusters = 6

set.seed(123)
km.res <- kmeans(complex_met, centers = number_clusters, nstart = 200, iter.max = 100)
# Visualize

fviz_cluster(km.res, data = complex_met,
             ellipse.type = "convex",
             palette = "ucscgb",
             ggtheme = theme_minimal())

ggsave(paste(path,"images\\clusters.png", sep = ""), scale = 3)



###################extern validation
#create a vector with rows = number of clusters
#centers_mean of cluster (to know the distance from each other), 3 metricas 
#centers_unscale = t(apply(km.res$centers, 1, function(r)r*attr(complex_met,'scaled:scale') + attr(complex_met, 'scaled:center')))

evaluate_metrics = data.frame("centers_mean"= rowMeans(km.res$centers), "size_clusters"= km.res$size, "NCSKEW_mean"=-999,"DUVOL_mean"=-999, "CRASH_COUNT_mean"=-999,
                              "NCSKEW_sd"=-999,"DUVOL_sd"=-999, "CRASH_COUNT_sd"=-999)

#get indices of each cluster
#mean of all metrics of indices
for(cluster in 1:number_clusters){
  indexs = which(km.res$cluster == cluster)
  evaluate_metrics$NCSKEW_mean[cluster] = mean(all_metrics$NCSKEW[indexs])
  evaluate_metrics$DUVOL_mean[cluster] = mean(all_metrics$DUVOL[indexs])
  evaluate_metrics$CRASH_COUNT_mean[cluster] = mean(all_metrics$CRASH_COUNT[indexs])
  
  evaluate_metrics$NCSKEW_sd[cluster] = sd(all_metrics$NCSKEW[indexs])
  evaluate_metrics$DUVOL_sd[cluster] = sd(all_metrics$DUVOL[indexs])
  evaluate_metrics$CRASH_COUNT_sd[cluster] = sd(all_metrics$CRASH_COUNT[indexs])
}
##different values how pretended. differente metrics are equals for same cluster


barplot_data = data.matrix(evaluate_metrics[,c("centers_mean", "NCSKEW_mean", "DUVOL_mean", "CRASH_COUNT_mean")])
barplot_data[,"centers_mean"] = barplot_data[,"centers_mean"]
colnames(barplot_data)[1] = "centers_mean"

myrange <- c(min(barplot_data),max(barplot_data))
text_x = 0.8
#colors = c("darkblue", "red", "green", "darkgreen" 
colors = rainbow(number_clusters)#colors()[sample(c(1:length(colors())), size=number_clusters, replace=F) ]
barplot(barplot_data,main="Crash Risk metrics by cluster - Means", beside = TRUE, cex.names=text_x, ylim = myrange,col=colors)#, ylim=c(-40,150), yaxt = "n"
#axis(2,at=seq(-50,270,20))

legend("topright", 
       legend = sprintf("Cluster %d",seq(1:number_clusters)), 
       fill = colors)
#legend("topright", 
#       legend = "Each color is a cluster")
ggsave(paste(path,"imagens\\means.png", sep = ""), scale = 3)


##sd

barplot_data = data.matrix(evaluate_metrics[,c("centers_mean", "NCSKEW_sd", "DUVOL_sd", "CRASH_COUNT_sd")])
barplot_data[,"centers_mean"] = barplot_data[,"centers_mean"]
colnames(barplot_data)[1] = "centers_mean"

myrange <- c(-1.1,max(barplot_data))
#colors = c("darkblue", "red", "green", "darkgreen" 
#colors = rainbow(number_clusters)#colors()[sample(c(1:length(colors())), size=number_clusters, replace=F) ]

barplot(barplot_data,main="Crash Risk metrics by cluster - sd", beside = TRUE, cex.names=text_x, ylim = myrange,col=colors)#, ylim=c(-40,150), yaxt = "n"
#axis(2,at=seq(-50,270,20))

legend("bottomright", 
       legend = sprintf("Cluster %d",seq(1:number_clusters)), 
       fill = colors)
#legend("topright", 
#       legend = "Each color is a cluster")

saveRDS(evaluate_metrics, paste(path, "evaluate_cluster_",number_clusters,".rds", sep = ""))

##############################################################################################

#random test to see if the metrics tend to look the same in random indexs
# for(cluster in 1:number_clusters){
#   print(cluster)
#   indexs = sample(c(1:935), size=200, replace=F) 
#   #evaluate_metrics$centers_mean[cluster] = mean(all_metrics$NCSKEW[indexs])
# 
#   evaluate_metrics$NCSKEW_mean[cluster] = mean(all_metrics$NCSKEW[indexs])
#   evaluate_metrics$DUVOL_mean[cluster] = mean(all_metrics$DUVOL[indexs])
#   evaluate_metrics$CRASH_COUNT_mean[cluster] = mean(all_metrics$CRASH_COUNT[indexs])
#   
#   evaluate_metrics$NCSKEW_sd[cluster] = sd(all_metrics$NCSKEW[indexs])
#   evaluate_metrics$DUVOL_sd[cluster] = sd(all_metrics$DUVOL[indexs])
#   evaluate_metrics$CRASH_COUNT_sd[cluster] = sd(all_metrics$CRASH_COUNT[indexs])
# }