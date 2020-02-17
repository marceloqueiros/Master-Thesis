
########plot x y economic metrics in the end

# radarchart
# Library
library(fmsb)
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\dataset\\"
evaluate_metrics = readRDS(paste(path,'evaluate_cluster_6 - tese.rds', sep = "")) #RDS
# Create data: note in High school for several students
set.seed(99)
number=4
n=12
centers_unscale = t(apply(km.res$centers, 1, function(r)r*attr(complex_met,'scaled:scale') + attr(complex_met, 'scaled:center')))
data = centers_unscale[,(n*(number-1)+1):(n*number)]

coln = colnames(data)
data = as.data.frame(matrix( data , ncol=n))
colnames(data) = coln

data = rbind(apply(data,2,max) , apply(data,2,min), data)
rownames(data)=c("max", "min", "1", "2", "3", "4", "5", "6")


# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
#data=rbind(rep(20,5) , rep(0,5) , data)

#min_column = apply(centers_unscale[,0:5],2,min)
#max_column = apply(a,2,max)

#data = rbind(apply(centers_unscale[,0:5],2,max) , apply(centers_unscale[,0:5],2,min) , data)
#=================
# Plot3: If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
#colors_border=c( rgb(1,0,0,0.8), rgb(1,0.5,0,0.8), rgb(0,1,0,0.8) , rgb(173/255,216/255,230/255,0.8), rgb(0,0,1,0.8), rgb(160/255,32/255,240/255,0.8) )
#colors_in=c(     rgb(1,0,0,0.2), rgb(1,0.5,0,0.2), rgb(0,1,0,0.2) , rgb(173/255,216/255,230/255,0.2), rgb(0,0,1,0.2), rgb(160/255,32/255,240/255,0.2) )

colors_border=c( rgb(1,0,0,0.8), rgb(1,153/255,0,0.8), rgb(1,1,0,0.9), rgb(0,1,0,0.8), rgb(102/255,153/255,1,0.8) , rgb(204/255,51/255,1,0.8) )
colors_in=c(     rgb(1,0,0,0.1), rgb(1,153/255,0,0.1), rgb(1,1,0,0.1), rgb(0,1,0,0.1), rgb(102/255,153/255,1,0.1) , rgb(204/255,51/255,1,0.1) )
radarchart( data[-c(1,2),]  , axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)
legend(x=1.5, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)







########plot x y economic metrics DUVOL vs
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\dataset\\imagens\\financeiras\\novas\\"

png(paste(path,"duvol_vs_ncskew.png"), units = "px", width=2500, height=1700, res=200)
plot(evaluate_metrics$NCSKEW_mean, evaluate_metrics$DUVOL_mean,cex = evaluate_metrics$size_clusters/14, xlim = c(-0.3,0.06), ylim = c(-0.2,-0.03))
text(evaluate_metrics$NCSKEW_mean, evaluate_metrics$DUVOL_mean,label=c("1", " ", "3", "4", "5", "6"))
dev.off()

png(paste(path,"ncskew_vs_crashcount.png"), units = "px", width=2900, height=1500, res=200)
plot(evaluate_metrics$NCSKEW_mean, evaluate_metrics$CRASH_COUNT_mean,cex = evaluate_metrics$size_clusters/14, xlim = c(-0.29,0.055), ylim = c(-1.18,-0.05))
text(evaluate_metrics$NCSKEW_mean, evaluate_metrics$CRASH_COUNT_mean,label=c("1", " ", "3", "4", "5", "6"))
dev.off()

png(paste(path,"crashcount_vs_duvol.png"), units = "px", width=2500, height=1700, res=200)
plot(evaluate_metrics$DUVOL_mean, evaluate_metrics$CRASH_COUNT_mean,cex = evaluate_metrics$size_clusters/14, xlim = c(-0.2,-0.03), ylim = c(-1.18,-0.05))
text(evaluate_metrics$DUVOL_mean, evaluate_metrics$CRASH_COUNT_mean,label=c("1", " ", "3", "4", "5", "6"))
dev.off()

##GEOM
library(ggplot2)
library(dplyr)
library(knitr)

ggplot(aes(year, NCSKEW), data = all_metrics) +
  geom_point()

ggplot(aes(year, CRASH_COUNT), data = all_metrics) +
  geom_point()

ggplot(aes(year, DUVOL), data = all_metrics) +
  geom_point()