round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}




#Pearson Correlation 
correlation = data.frame("Pearson Correlation"=-999)
correlation[1,1] = cor(all_metrics[,"NCSKEW"], all_metrics[,"DUVOL"], method = c("pearson"))
correlation[2,1] = cor(all_metrics[,"NCSKEW"], all_metrics[,"CRASH_COUNT"], method = c("pearson"))
correlation[3,1] = cor(all_metrics[,"DUVOL"], all_metrics[,"CRASH_COUNT"], method = c("pearson"))
rownames(correlation) = c("NCSKEW and DUVOL", "NCSKEW and CRASH_COUNT", "DUVOL and CRASH_COUNT")
correlation = round_df(correlation, 5)


#Pearson Correlation BY CLUSTER

#Pearson Correlation 
correlation_cluster = data.frame("NCSKEW and DUVOL"=-999,"NCSKEW and CRASH_COUNT"=-999, "DUVOL and CRASH_COUNT"=-999)
number_clusters= 6
for(cluster in 1:number_clusters){
  indexs = which(km.res$cluster == cluster)
  correlation_cluster[cluster,1] = cor(all_metrics$NCSKEW[indexs], all_metrics$DUVOL[indexs], method = c("pearson"))
  correlation_cluster[cluster,2] = cor(all_metrics$NCSKEW[indexs], all_metrics$CRASH_COUNT[indexs], method = c("pearson"))
  correlation_cluster[cluster,3] = cor(all_metrics$DUVOL[indexs], all_metrics$CRASH_COUNT[indexs], method = c("pearson"))
}
rownames(correlation_cluster) = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6")
correlation_cluster = round_df(correlation_cluster, 5)




library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = correlation
png(paste(path, "correlation.png"), height = 500, width = 1000, units = "px", res=200, pointsize = 12)
grid.table(df)
dev.off()


library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = correlation_cluster
png(paste(path, "correlation_cluster.png"), height = 500, width = 1000, units = "px", res=100, pointsize = 12)
grid.table(df)
dev.off()




valid_test <- clValid(complex_met, 2:20,
                      clMethods = c("kmeans"),
                      validation = c("internal", "stability"), maxitems = 1000)

summary(valid_test)

comp = matrix(as.numeric( valid_test@measures),nrow=nrow( valid_test@measures))
colnames(comp) = colnames(valid_test@measures)
rownames(comp) = rownames(valid_test@measures)
comp = round_df(comp, 5)

library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = comp
png(paste(path, "5_valid_test@measures.png"), height = 500, width = 10000, units = "px", res=100, pointsize = 12)
grid.table(df)
dev.off()



evaluate_metrics_6 = round_df(readRDS(paste(path,'evaluate_cluster_6.rds', sep = "")),5) #RDS
evaluate_metrics_5 = round_df(readRDS(paste(path,'evaluate_cluster_5.rds', sep = "")),5) #RDS

drop <- c("center_mean")
evaluate_metrics_6 = evaluate_metrics_6[,!(names(evaluate_metrics_6) %in% drop)]

drop <- c("center_mean")
evaluate_metrics_5 = evaluate_metrics_5[,!(names(evaluate_metrics_5) %in% drop)]

library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = evaluate_metrics_6[c("size_clusters", "NCSKEW_mean", "DUVOL_mean", "CRASH_COUNT_mean", "mean_all_CR_metrics")]
png(paste(path, "size_evaluate_metrics_6.png"), height = 500, width = 10000, units = "px", res=100, pointsize = 12)
grid.table(df)
dev.off()

library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = evaluate_metrics_5[c("size_clusters", "NCSKEW_mean", "DUVOL_mean", "CRASH_COUNT_mean", "mean_all_CR_metrics")]
png(paste(path, "size_evaluate_metrics_5.png"), height = 500, width = 10000, units = "px", res=100, pointsize = 12)
grid.table(df)
dev.off()

number_clusters=5
for(cluster in 1:number_clusters){
  evaluate_metrics_5$mean_all_CR_metrics[cluster] = mean(c(evaluate_metrics_5$NCSKEW_mean[cluster], evaluate_metrics_5$DUVOL_mean[cluster], evaluate_metrics_5$CRASH_COUNT_mean[cluster]))
  
  }

number_clusters=6
for(cluster in 1:number_clusters){
  evaluate_metrics_6$mean_all_CR_metrics[cluster] = mean(c(evaluate_metrics_6$NCSKEW_mean[cluster], evaluate_metrics_6$DUVOL_mean[cluster], evaluate_metrics_6$CRASH_COUNT_mean[cluster]))
}

# 6 clusters


todas_scale = t(apply(km.res$centers, 1, function(r)r ))
grupo1_6 = centers_unscale[,c("ARI", "ARI.simple", "ARI.NRI", "Bormuth.MC", "Bormuth.GP",
"Dale.Chall", "Dale.Chall.old", "Danielson.Bryan", "Dickes.Steiwer", "DRP", "ELF", "Flesch.PSK",
"Flesch.Kincaid", "FOG", "FOG.NRI", "FOG.PSK", "Farr.Jenkins.Paterson", "Fucks", "Linsear.Write",
"LIW", "nWS.3", "nWS.4", "RIX", "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de", "Spache", "Spache.old", "Strain",
"Traenkle.Bailer", "Wheeler.Smith", "meanSentenceLength")]
grupo1_6_scale = todas_scale[,c("ARI", "ARI.simple", "ARI.NRI", "Bormuth.MC", "Bormuth.GP",
                              "Dale.Chall", "Dale.Chall.old", "Danielson.Bryan", "Dickes.Steiwer", "DRP", "ELF", "Flesch.PSK",
                              "Flesch.Kincaid", "FOG", "FOG.NRI", "FOG.PSK", "Farr.Jenkins.Paterson", "Fucks", "Linsear.Write",
                              "LIW", "nWS.3", "nWS.4", "RIX", "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de", "Spache", "Spache.old", "Strain",
                              "Traenkle.Bailer", "Wheeler.Smith", "meanSentenceLength")]


grupo2_6 = centers_unscale[,c("Dale.Chall.PSK", "Flesch", "nWS", "nWS.2")] 
grupo2_6_scale = todas_scale[,c("Dale.Chall.PSK", "Flesch", "nWS", "nWS.2")] 
grupo3_6 =centers_unscale[,c("Coleman", "Coleman.C2", "Coleman.Liau.ECP", "Coleman.Liau.grade", "Coleman.Liau.short", "Danielson.Bryan.2", "FORCAST", "FORCAST.RGL","Scrabble","Traenkle.Bailer.2","meanWordSyllables")]
grupo3_6_scale =todas_scale[,c("Coleman", "Coleman.C2", "Coleman.Liau.ECP", "Coleman.Liau.grade", "Coleman.Liau.short", "Danielson.Bryan.2", "FORCAST", "FORCAST.RGL","Scrabble","Traenkle.Bailer.2","meanWordSyllables")]


mean_All = data.frame("mean_all_scale"= 1:6)

number_clusters=6
for(cluster in 1:number_clusters){
  mean_All$mean_all_scale[cluster] = mean(grupo1_6_scale[cluster])
}

grupo1_6 = cbind(mean_All, grupo1_6)

grupo1_6 = grupo1_6[order(grupo1_6[, "mean_all_scale"], decreasing = TRUE),]


mean_All = data.frame("mean_all_scale"= 1:6)

for(cluster in 1:number_clusters){
  mean_All$mean_all_scale[cluster] = mean(grupo2_6_scale[cluster])
}

grupo2_6 = cbind(mean_All, grupo2_6)
grupo2_6 = grupo2_6[order(grupo2_6[, "mean_all_scale"], decreasing = TRUE),]


mean_All = data.frame("mean_all_scale"= 1:6)

for(cluster in 1:number_clusters){
  mean_All$mean_all_scale[cluster] = mean(grupo3_6_scale[cluster])
}

grupo3_6 = cbind(mean_All, grupo3_6)
grupo3_6 = grupo3_6[order(grupo3_6[, "mean_all_scale"], decreasing = TRUE),]


library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = grupo1_6
png(paste(path, "grupo1_6.png"), height = 500, width = 10000, units = "px", res=100, pointsize = 12)
grid.table(df)
dev.off()

library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = grupo2_6
png(paste(path, "grupo2_6.png"), height = 500, width = 10000, units = "px", res=100, pointsize = 12)
grid.table(df)
dev.off()

library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = grupo3_6
png(paste(path, "grupo3_6.png"), height = 500, width = 10000, units = "px", res=100, pointsize = 12)
grid.table(df)
dev.off()





# 5 clusters

todas_scale = t(apply(km.res$centers, 1, function(r)r ))
#37
grupo1_5 = centers_unscale[,c("ARI", "ARI.simple", "ARI.NRI", "Bormuth.MC", "Bormuth.GP", "Coleman.C2", "Coleman.Liau.ECP", "Coleman.Liau.grade", "Coleman.Liau.short",
                             "Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK", "Danielson.Bryan", "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson", "Flesch", "Flesch.PSK", "Flesch.Kincaid","Fucks", "Linsear.Write",
                             "LIW","nWS", "nWS.2", "nWS.3", "nWS.4", "Scrabble", "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de", "Spache", "Spache.old", "Strain",
                             "Traenkle.Bailer", "Wheeler.Smith")]
grupo1_5_scale = todas_scale[,c("ARI", "ARI.simple", "ARI.NRI", "Bormuth.MC", "Bormuth.GP", "Coleman.C2", "Coleman.Liau.ECP", "Coleman.Liau.grade", "Coleman.Liau.short",
                              "Dale.Chall", "Dale.Chall.old", "Dale.Chall.PSK", "Danielson.Bryan", "Dickes.Steiwer", "DRP", "ELF", "Farr.Jenkins.Paterson", "Flesch", "Flesch.PSK", "Flesch.Kincaid","Fucks", "Linsear.Write",
                              "LIW","nWS", "nWS.2", "nWS.3", "nWS.4", "Scrabble", "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de", "Spache", "Spache.old", "Strain",
                              "Traenkle.Bailer", "Wheeler.Smith")]

#4
grupo2_5 = centers_unscale[,c("Coleman", "Coleman.C2", "Coleman.Liau.ECP", "Coleman.Liau.grade", "Coleman.Liau.short", "Danielson.Bryan.2", "FORCAST", "FORCAST.RGL","Scrabble","Traenkle.Bailer.2")] 
grupo2_5_scale = todas_scale[,c("Coleman", "Coleman.C2", "Coleman.Liau.ECP", "Coleman.Liau.grade", "Coleman.Liau.short", "Danielson.Bryan.2", "FORCAST", "FORCAST.RGL","Scrabble","Traenkle.Bailer.2")] 

#5
grupo3_5 =centers_unscale[,c("Bormuth.MC", "Bormuth.GP", "DRP", "Dickes.Steiwer", "Farr.Jenkins.Paterson")]
grupo3_5_scale =todas_scale[,c("Bormuth.MC", "Bormuth.GP", "DRP", "Dickes.Steiwer", "Farr.Jenkins.Paterson")]




mean_All = data.frame("mean_all_scale"= 1:5)

number_clusters=5
for(cluster in 1:number_clusters){
  mean_All$mean_all_scale[cluster] = mean(grupo1_5_scale[cluster])
}

grupo1_5 = cbind(mean_All, grupo1_5)
grupo1_5 = grupo1_5[order(grupo1_5[, "mean_all_scale"], decreasing = TRUE),]



mean_All = data.frame("mean_all_scale"= 1:5)

number_clusters=5
for(cluster in 1:number_clusters){
  mean_All$mean_all_scale[cluster] = mean(grupo2_5_scale[cluster])
}

grupo2_5 = cbind(mean_All, grupo2_5)
grupo2_5 = grupo2_5[order(grupo2_5[, "mean_all_scale"], decreasing = TRUE),]


mean_All = data.frame("mean_all_scale"= 1:5)

number_clusters=5
for(cluster in 1:number_clusters){
  mean_All$mean_all_scale[cluster] = mean(grupo3_5_scale[cluster])
}

grupo3_5 = cbind(mean_All, grupo3_5)
grupo3_5 = grupo3_5[order(grupo3_5[, "mean_all_scale"], decreasing = TRUE),]

library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = grupo1_5
png(paste(path, "grupo1_5.png"), height = 500, width = 10000, units = "px", res=100, pointsize = 12)
grid.table(df)
dev.off()

library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = grupo2_5
png(paste(path, "grupo2_5.png"), height = 500, width = 10000, units = "px", res=100, pointsize = 12)
grid.table(df)
dev.off()

library(gridExtra)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\escrever_tese\\images\\"
df = grupo3_5
png(paste(path, "grupo3_5.png"), height = 500, width = 10000, units = "px", res=100, pointsize = 12)
grid.table(df)
dev.off()



library(tidyverse)


