#Collect All NHL Data for 2015-16 Season
NHLDataCollect<-function(){
date<-as.Date("2015-10-07")
NHLALL<-data.frame()
while(date<Sys.Date()){
    Date<-date
    date2=as.character(Date)
    Strsp<-strsplit(date2,split="-")
    myUrl=paste("http://www.sportsbookreview.com/nhl-hockey/odds-scores/",Strsp[[1]][1],Strsp[[1]][2],Strsp[[1]][3],sep="")
    odds<-read_html(myUrl)
    oddsTab<-html_table(odds,fill=TRUE)
    len<-length(oddsTab)
    Home<-0
    Away<-0
    Over<-0
    Under<-0
    HScore<-0
    AScore<-0
    OU<-0
    if(length(oddsTab[[1]][3, 2])!=0){
        for(i in 1:len){
          Home[i]<-oddsTab[[i]][3,2]
          Away[i]<-oddsTab[[i]][2,2]
          TOver<-oddsTab[[i]][2,7]
          TUnder<-oddsTab[[i]][3,7]
          OU[i]<-5
          if(strsplit(TOver,split="")[[1]][2]=="½"){
            OU[i]<-5.5
              Over[i]<-paste(strsplit(TOver,split="")[[1]][4],strsplit(TOver,split="")[[1]][5],strsplit(TOver,split="")[[1]][6],strsplit(TOver,split="")[[1]][7],sep="")
          }
          if(strsplit(TOver,split="")[[1]][2]!="½"){
              Over[i]<-paste(strsplit(TOver,split="")[[1]][3],strsplit(TOver,split="")[[1]][4],strsplit(TOver,split="")[[1]][5],strsplit(TOver,split="")[[1]][6],sep="")
          }
          if(strsplit(TUnder,split="")[[1]][2]=="½"){
            Under[i]<-paste(strsplit(TUnder,split="")[[1]][4],strsplit(TUnder,split="")[[1]][5],strsplit(TUnder,split="")[[1]][6],strsplit(TUnder,split="")[[1]][7],sep="")
          }
          if(strsplit(TUnder,split="")[[1]][2]!="½"){
            Under[i]<-paste(strsplit(TUnder,split="")[[1]][3],strsplit(TUnder,split="")[[1]][4],strsplit(TUnder,split="")[[1]][5],strsplit(TUnder,split="")[[1]][6],sep="")
          }
          HScore[i]<-oddsTab[[i]][3,3]
          AScore[i]<-oddsTab[[i]][2,3]
          
        }
        temp<-cbind(Date,Home,HScore,Away,AScore,OU,Over,Under)
        NHLALL<-rbind(NHLALL,temp)
    }
    date<-date+1
    
}
NHLALL$HScore<-as.character(NHLALL$HScore)
NHLALL$AScore<-as.character(NHLALL$AScore)
NHLALL$OU<-as.character(NHLALL$OU)
NHLALL$Over<-as.character(NHLALL$Over)
NHLALL$Under<-as.character(NHLALL$Under)
NHLALL$HScore<-as.numeric(NHLALL$HScore)
NHLALL$AScore<-as.numeric(NHLALL$AScore)
NHLALL$OU<-as.numeric(NHLALL$OU)
NHLALL$Over<-as.numeric(NHLALL$Over)
NHLALL$Under<-as.numeric(NHLALL$Under)
NHLALL$Date<-as.numeric(as.character(NHLALL$Date))

NHLALL$Total<-NHLALL$HScore+NHLALL$AScore
NHLALL$OUResult<-ifelse(NHLALL$Total>NHLALL$OU,"O","U")
NHLALL$OUResult<-ifelse(NHLALL$Total==NHLALL$OU,"P",NHLALL$OUResult)
HBB<-0
ABB<-0
for(i in 1:nrow(NHLALL)){
  HBB[i]<-0
  ABB[i]<-0
  for(k in 1:nrow(NHLALL)){
    if(as.Date.numeric(NHLALL$Date[i]-1,Sys.Date()-as.numeric(Sys.Date()))==as.Date.numeric(NHLALL$Date[k],Sys.Date()-as.numeric(Sys.Date()))){
      if(NHLALL$Home[i]==NHLALL$Home[k] || NHLALL$Home[i]==NHLALL$Away[k]){
        HBB[i]<-1
      }
      if(NHLALL$Away[i]==NHLALL$Home[k] || NHLALL$Away[i]==NHLALL$Away[k]){
        ABB[i]<-1
      }
    }
  }
}
NHLALL$HBB<-HBB
NHLALL$ABB<-ABB
return(NHLALL)
}
