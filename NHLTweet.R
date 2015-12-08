#Function for Tweeting my NHL Picks
NHLTweet<-function(){
  Date<-Sys.Date()
  date2=as.character(Date)
  Strsp<-strsplit(date2,split="-")
  myUrl=paste("http://www.sportsbookreview.com/nhl-hockey/odds-scores/",Strsp[[1]][1],Strsp[[1]][2],Strsp[[1]][3],sep="")
  odds<-read_html(myUrl)
  oddsTab<-html_table(odds,fill=TRUE)
  len<-length(oddsTab)
  
  NHLALL<-NHLDataCollect()
  closeAllConnections()

  updateStatus(NHLPick2(Sys.Date()))
}
