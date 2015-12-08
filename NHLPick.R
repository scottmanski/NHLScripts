#NHLPick Function Modified for Tweeting
NHLPick2<-function(date){
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
  if(length(oddsTab[[1]][3, 2])==0){
    return("No Games Today") 
  }
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
  day<-cbind(Date,Home,Away,OU,Over,Under)
  day<-data.frame(day)
  
  day$OU<-as.character(day$OU)
  day$Over<-as.character(day$Over)
  day$Under<-as.character(day$Under)
  day$OU<-as.numeric(day$OU)
  day$Over<-as.numeric(day$Over)
  day$Under<-as.numeric(day$Under)
  
  
  NHL<-NHLALL[NHLALL$Date<date,]
  for(j in 1:nrow(NHL)){
    if(NHL$OUResult[j]=="U"){
      NHL$OUResult[j]<-0
    }
    if(NHL$OUResult[j]=="O"){
      NHL$OUResult[j]<-1
    }
    if(NHL$OUResult[j]=="P"){
      NHL$OUResult[j]<-0
      if(NHL$OU[j]==5){
        NHL$OUResult[j]<-0
      }
    }
  }
  NHL$OUResult<-as.numeric(NHL$OUResult)
  
  HBB<-0
  ABB<-0
  temp<-NHLALL[as.numeric(NHLALL$Date)==as.numeric(as.character(day$Date[1]))-1,]
  for(i in 1:nrow(day)){
    HBB[i]<-0
    ABB[i]<-0
    if(nrow(temp)>0){
      for(k in 1:nrow(temp)){
        if(as.numeric(as.character(day$Date[i]))-1==as.Date.numeric(temp$Date[k],Sys.Date()-as.numeric(Sys.Date()))){
          if(as.character(day$Home[i])==as.character(temp$Home[k]) || as.character(day$Home[i])==as.character(temp$Away[k])){
            HBB[i]<-1
          }
          if(as.character(day$Away[i])==as.character(temp$Home[k]) || as.character(day$Away[i])==as.character(temp$Away[k])){
            ABB[i]<-1
          }
        }
      }
    }
  }
  day$HBB<-HBB
  day$ABB<-ABB
  
  
  
  
  mod1<-glm(OUResult~Home+Away+OU+I((Over<0)*Over)+I((Under<0)*Under)+HBB*ABB,data=NHL,family=binomial())
  guess<-predict(mod1,newdata=day,type="response")
  
  day$myBet<-ifelse(guess>0.5,"O","U")
  
  if(HScore[1]!="-" && date<Sys.Date()){
    HScore<-as.numeric(HScore)
    AScore<-as.numeric(AScore)
    day$Total<-HScore+AScore
    day$OUResult<-ifelse(day$Total>day$OU,"O","U")
    day$OUResult<-ifelse(day$Total==day$OU,"P",day$OUResult)
    day$Win<-ifelse(day$myBet==day$OUResult,"W","L")
    day$Win<-ifelse(day$OUResult=="P","D",day$Win)
    winnings<-0
    for(k in 1:nrow(day)){
      winnings[k]<-0
      if(day$Win[k]=="L"){
        if(day$myBet[k]=="U"){
          winnings[k]<-day$Under[k]
          if(day$Under[k]>0){
            winnings[k]<--100
          }
        }
        if(day$myBet[k]=="O"){
          winnings[k]<-day$Over[k]
          if(day$Over[k]>0){
            winnings[k]<--100
          }
        }
      }
      if(day$Win[k]=="W"){
        if(day$myBet[k]=="U"){
          winnings[k]<-day$Under[k]
          if(day$Under[k]<0){
            winnings[k]<-100
          }
        }
        if(day$myBet[k]=="O"){
          winnings[k]<-day$Over[k]
          if(day$Over[k]<0){
            winnings[k]<-100
          }
        }
      }
    }
    
    #BetCost<-ifelse(day$myBet=="O",day$Over,day$Under)
    #BetCost<-ifelse(BetCost<0,BetCost,-100)
    #winnings<-ifelse(day$OUResult==day$myBet,-1*BetCost,ifelse(BetCost>0,BetCost,-100))
    #winnings<-ifelse(day$OUResult=="P",0,winnings)
    day$Winnings<-winnings
  }
  Bet<-0
  for(m in 1:nrow(day)){
    Bet[m]<-0
    if(day$myBet[m]=="O" && day$Over[m]<0){
      Bet[m]<-1
    }
    if(day$myBet[m]=="U" && day$Under[m]<0){
      Bet[m]<-1
    }
  }
  day$Bet<-Bet
  closeAllConnections()
  myText<-""
  n<-nrow(day)-1
  for(m in 1:n){
    myText<-paste(myText,day$Home[m],":",day$myBet[m],", ",sep="")
  }
  myText<-paste(myText,day$Home[nrow(day)],":",day$myBet[nrow(day)],sep="")
  return(myText)
}
