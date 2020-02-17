#rm(list = ls())

library(XBRL)
library(koRpus)
library(koRpus.lang.en)

#Path

path = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\R TESE\\files_xml\\"

#list text files 
ll.files <- list.files(path = path, pattern = "xml",  full.names = TRUE);
length(ll.files)

#set vectors
SMOG.score.vec        = rep(0.,length(ll.files))
FleshKincaid.score.vec= rep(0.,length(ll.files))
FOG.score.vec         = rep(0.,length(ll.files))

#loop through each file
for (i in 1:length(ll.files)){
    
      #read xbrl file
      xbrl.vars <- xbrlDoAll(ll.files[i], cache.dir="XBRLcache", prefix.out="out", verbose=TRUE, delete.cached.inst = TRUE)
      
      #tokenize
      tagged.text <- koRpus::tokenize(ll.files[i], lang="en")
      #hyphen the word for some of the packages that require it
      hyph.txt.en <- koRpus::hyphen(tagged.text)
      #Readability wrapper
      readbl.txt <- koRpus::readability(tagged.text, hyphen=hyph.txt.en, index="all")
      #Pull scores, convert to numeric, and update the vectors
      SMOG.score.vec[i]=as.numeric(summary(readbl.txt)$raw[36]) #SMOG Score
      FleshKincaid.score.vec[i]=as.numeric(summary(readbl.txt)$raw[11]) #Flesch Reading Ease Score 
      FOG.score.vec[i]=as.numeric(summary(readbl.txt)$raw[22]) #FOG score
      #if (i%%10==0)
      #  cat("finished",i,"\n")
  }
#to do just one
df=cbind(FOG.score.vec,FleshKincaid.score.vec,SMOG.score.vec)
colnames(df)=c("FOG", "Flesch Kincaid", "SMOG")
write.csv(df,file=paste0(path,"Combo.csv"),row.names=FALSE,col.names=TRUE)

#to write seperate csvs
#write.csv(SMOG.score.vec,file =  paste0(path,"SMOG.csv"),row.names=FALSE,col.names = "SMOG")
#write.csv(FOG.score.vec,file  =paste0(path,"FOG.csv"),row.names=FALSE,col.names = "FOG")
#write.csv(FleshKincaid.score.vec,file=paste0(path,"FK.csv"),row.names=FALSE,col.names = "Flesch Kincaid")