rm(list=ls())

library("readtext")
library("quanteda")
library("lubridate")
library("tidyverse")
library(dplyr)
library(tibble)

theme_set(theme_bw())

path_dir = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\dataset\\SP100_10K_2008-2017_company\\"
companies <- dir(path_dir)

all_metrics = data.frame()
#per company because reduces memory ram
for (c in 1:length(companies)){
    print(companies[c])
      nomes_ficheiros <- dir(paste(path_dir,companies[c], "\\", sep = ""), recursive = TRUE)#chapter_[0-9]{2}, pattern = ".txt"
    if(length(nomes_ficheiros)>0){  
      daten.files <- readtext(paste(path_dir, companies[c], "\\", nomes_ficheiros, sep = "/"))
      #daten.files$doc_id <- sprintf("%02d", 1:length(nomes_ficheiros)) #More practical naming of chapters
      
      #remove before </Header>
      for (i in 1:length(nomes_ficheiros)){
        daten.files$text[i] = strsplit(daten.files$text[i],"</Header>")[[1]][2]
      }
      #remove before </Header>##
      
      korpus <- corpus(daten.files, docid_field = "doc_id")
      #korpus.stats <- summary(korpus, n = 1000000)
      
      #korpus # Corpus consisting of X documents and 0 docvars.
      
      #ndoc(korpus) # number of documents
      
      #ntoken(korpus) # number of tokens 
      
      #ntype(korpus) # number of types
      
      #nsentence(korpus) # number of sentences
      
      #korpus.stats ## all the information
    
      metrics = textstat_readability(corpus(korpus), measure = "all")
      
      #add permno, company, year and date
      
      names(metrics)[1] = "cik"
      metrics$company <- 0
      metrics = metrics %>% select(1, length(metrics), everything())
      metrics$year <- 0
      metrics = metrics %>% select(1,2, length(metrics), everything())
      metrics$date <- 0
      metrics = metrics %>% select(1,2,3, length(metrics), everything())
      
      
      for (i in 1:length(nomes_ficheiros)){
        metrics[i,1] = strsplit(nomes_ficheiros[i], "_")[[1]][5] #cik
        metrics[i,2] = companies[c]#strsplit(nomes_ficheiros[i], "/")[[1]][1]  #company
        metrics[i,3] = paste("20",strsplit(strsplit(nomes_ficheiros[i], "_")[[1]][6], "-")[[1]][2], sep = "") #ano
        metrics[i,4] = strsplit(nomes_ficheiros[i], "_")[[1]][1] #date
        }
      #add company and year##
      
      all_metrics = rbind(all_metrics, metrics)
    }
}

#write in csv
path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\dataset\\"
write.csv2(all_metrics,file=paste0(path,"generated_files\\Companies.csv"), row.names=FALSE)#,,col.names=TRUE
write_rds(all_metrics, paste(path, "generated_files\\all_metrics.rds", sep = ""))

lower_metrics = c("ARI",	"ARI.simple", "ARI.NRI", "Bormuth.GP", "Coleman.Liau.grade", "Coleman.Liau.short", "Dale.Chall.old", "Dale.Chall.PSK", "Danielson.Bryan", "DRP", "ELF", "Flesch.PSK", "Flesch.Kincaid", "FOG", "FOG.PSK", "FOG.NRI", "FORCAST", "FORCAST.RGL", "Fucks", "Linsear.Write", "LIW", "nWS", "nWS.2", "nWS.3", "nWS.4", "RIX", "SMOG", "SMOG.C", "SMOG.simple", "SMOG.de", "Spache", "Spache.old", "Strain", "Wheeler.Smith", "meanSentenceLength", "meanWordSyllables")


all_metrics = readRDS(paste(path,'generated_files\\all_metrics_out.rds', sep = "")) #RDS
for (i in 1:length(lower_metrics)){
  
  all_metrics[,lower_metrics[i]] = -all_metrics[,lower_metrics[i]]
}

write_rds(all_metrics, paste(path, "generated_files\\all_metrics_out_NEW.rds", sep = ""))

write.csv2(all_metrics,file=paste0(path,"generated_files\\all_metrics_out_NEW.csv"), row.names=FALSE)#,,col.names=TRUE

