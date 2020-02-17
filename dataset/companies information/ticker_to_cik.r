require(XML)
require(RCurl)

getCIK = function(ticker) {
  stopifnot(is.character(ticker))
  uri = "https://www.sec.gov/cgi-bin/browse-edgar"
  response = getForm(uri,CIK=ticker,action="getcompany")
  html = htmlParse(response)
  CIKNode = getNodeSet(html, "//acronym[@title=\"Central Index Key\"][text() = \"CIK\"]")
  CIKNodeText = sapply(CIKNode, function(x) xmlValue(getSibling(getSibling(x))))
  CIK = sub(" .*","",CIKNodeText)
  CIK = sub("^0*","",CIK)
  CIK
}

processFile = function(filepath) {
  con = file(filepath, "r")
  i=1
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    tickers <- c(tickers, line)
    i <-i+1
  }
  close(con)
  tickers
}
tickers = 0
filepath = "C:\\Users\\Marcelo Queirós\\Documents\\MIEI\\Ano 2\\Tese\\dataset\\"
tickers <- processFile(filepath + "tickers.txt")

cik=rep(0.,length(tickers))
for (i in 1:length(tickers)){
  print(tickers[i+1])
  cik[i] <- getCIK(tickers[i+1])
}
write.csv(cik,file=paste0(filepath,"cik.csv"),row.names=FALSE,col.names = FALSE)