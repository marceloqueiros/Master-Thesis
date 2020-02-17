#rm(list = ls())

library(XBRL)
library(koRpus)
library(koRpus.lang.en)

## Setting stringsAsFactors = FALSE is highly recommended to avoid data frames to create factors from character vectors.
options(stringsAsFactors = FALSE)
## XBRL instance file to be analyzed, accessed

path = "C:/Users/Marcelo Queirós/Documents/MIEI/Ano 2/Tese/R TESE/files_xml/"
inst = paste(p, "amzn-20171231.xml", sep="")
#inst <- "https://www.sec.gov/Archives/edgar/data/21344/000002134413000050/ko-20130927.xml"

xbrl.vars <- xbrlDoAll(inst, cache.dir=paste(p, "/XBRLcache", sep=""), prefix.out="xml", verbose=TRUE, delete.cached.inst = TRUE)




tagged.text <- treetag(
  "sample_text.txt",
  treetagger="manual",
  lang="en",
  TT.options=list(
    path="C:/Users/Marcelo Queirós/Documents/MIEI/Ano 2/Tese/R TESE/treetagger",
    preset="en"
  ),
  doc_id="sample"
)

#available.koRpus.lang()      # see all available language packages
#install.koRpus.lang("en") 
install.packages("koRpus.lang.en", repos="https://undocumeantit.github.io/repos/l10n/")
flesch("I am", force.lang="en")

#FOG(txt.file"", hyphen = NULL, parameters = list(syll = 3, const = 0.4,suffix = c("es", "ed", "ing")), ...)
FOG("I am", hyphen = NULL, parameters = list(syll = 3, const = 0.4,suffix = c("es", "ed", "ing")))
