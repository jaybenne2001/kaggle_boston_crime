setwd("/Users/Divye/Downloads/DM_Project/")

teamstatsPM<-read.csv('Team_Statistics_per_Match.csv',stringsAsFactors = F)
teamstatsPM$X<-NULL
matchSummary<-read.csv('Match_Summary.csv',stringsAsFactors = F)
matchSummary$X<-NULL

# Joining home and away stats per match

library(dplyr)
home<-teamstatsPM[teamstatsPM$Qualifyer=='home',]
away<-teamstatsPM[teamstatsPM$Qualifyer=='away',]

names(home)<-paste0("home_",names(home))
names(home)

names(away)<-paste0("away_",names(away))
names(away)

matchSummary<-merge(matchSummary,home,by.x='Match_ID',by.y = 'home_Match_ID')
matchSummary<-merge(matchSummary,away,by.x='Match_ID',by.y = 'away_Match_ID')

matchSummary$scheduled<-as.Date(matchSummary$scheduled)

teamstatsPM<-merge(teamstatsPM,matchSummary[,c('Match_ID','scheduled')],by= 'Match_ID',all.x=T)

getPrevStats<-function(comp_id,match_date){
  match_date<-as.Date(match_date)
  
  temp<-teamstatsPM[teamstatsPM$Team_ID==comp_id & teamstatsPM$scheduled < match_date,]
  
  temp<-tail(temp[ordered(temp$scheduled),],5)
  
  keep<-c('Assists','Deaths','Headshots','Kills')
  
  stats<-temp[,keep]
  g<-c(colMeans(stats),no_matches=nrow(stats))
  return(g)
}





library(data.table)

#Problem=======
trial<-mapply(getPrevStats,comp_id=matchSummary$home_Team_ID,MoreArgs(match_date=matchSummary$scheduled) )
cbind(matchSummary, 
      setNames( lapply(matchSummary, FUN = getPrevStats(home_Team_ID, scheduled), 
                       matchSummary)))
df<-data.frame()

df<-getPrevStats(comp_id = 'sr:competitor:220624',match_date ='2016-09-16')

for ( i in 1:nrow(matchSummary)){
  df<-rbind(df,getPrevStats(comp_id = matchSummary$home_Team_ID[i],match_date = matchSummary$scheduled[i]))
}
df<-as.data.frame(df)      
df<-df[2:nrow(df),]
df_home<-df

# For away

df<-data.frame()

df<-getPrevStats(comp_id = 'sr:competitor:220624',match_date ='2016-09-16')

for ( i in 1:nrow(matchSummary)){
  df<-rbind(df,getPrevStats(comp_id = matchSummary$away_Team_ID[i],match_date = matchSummary$scheduled[i]))
}
df<-as.data.frame(df)      
df<-df[2:nrow(df),]
df_away<-df
names(df_away)<-paste0('avg_away',names(df_away))
names(df_home)<-paste0('avg_home',names(df_home))

matchSummary<-cbind(matchSummary,cbind(df_home,df_away))

names(matchSummary)

"C:\\Users\\Vivek\\Documents\\learn stuff\\fall\\rdatamining project\\final_files_GO_files"
dim(matchSummary)
matchScores <- read.csv('Match_Scores.csv',stringsAsFactors = F)
matchScores$winner <- ifelse(matchScores$home_score>matchScores$away_score, "home", "away")
matchScores
matchSummary <- merge(x = matchSummary, y = matchScores[,c('Match_ID', 'winner')], by.x = 'Match_ID', by.y = 'Match_ID')
head(matchSummary)
matchSummary$winner <- as.factor(matchSummary$winner)


matchProbs <- read.csv('Player_Team_Histories.csv',stringsAsFactors = F)
matchProbs <- matchProbs[matchProbs$Bet_Selection=="home_team_winner",]
head(matchProbs)


matchSummary <- merge(x = matchSummary, y = matchProbs[,c('Match_ID', 'Probability')], by.x = 'Match_ID', 
                      by.y = 'Match_ID')

names(matchSummary)



matchSummary
matchSummary$diff_kills <- matchSummary$avg_homeKills - matchSummary$avg_awayKills
matchSummary$diff_deaths <- matchSummary$avg_homeDeaths - matchSummary$avg_awayDeaths
matchSummary$diff_headshots <- matchSummary$avg_homeHeadshots - matchSummary$avg_awayHeadshots
matchSummary$diff_assists <- matchSummary$avg_homeAssists - matchSummary$avg_awayAssists
names(matchSummary)

keep <- c("home_Kills",
          "away_Kills",
  "avg_awayno_matches",
          "avg_awayKills",
          "avg_awayHeadshots",
          "avg_awayDeaths",
          "avg_awayAssists", "avg_homeno_matches",
          "avg_homeKills",
          "avg_homeHeadshots",
          "avg_homeDeaths",
          "avg_homeAssists",
          "diff_kills",
          "diff_deaths",
          "diff_headshots",
          "diff_assists",
          "Probability",
          "winner")

str(matchSummary)

library(caret)

model_data <- matchSummary[,(names(matchSummary) %in% keep)]
dim(model_data)
dim(na.omit(model_data))
dim(model_data)
model_data <- na.omit(model_data)
dim(model_data)


head(model_data)

class(model_data)

#ELO Ratings=============================================
matchSummary <- unique(matchSummary)
matchSummary$ELOH <- as.numeric(0)
matchSummary$ELOA <- as.numeric(0)
matchSummary$lh <- as.numeric(0)
matchSummary$la <- as.numeric(0)
table(matchSummary$winner)
matchSummary$winner <- as.numeric(matchSummary$winner)
matchSummary$home_winner[matchSummary$winner==2] <- 1
matchSummary$home_winner[matchSummary$winner==1] <- 0
matchSummary$winner <- as.factor(matchSummary$winner)
levels(matchSummary$winner) <-  c("away", "home")
table(matchSummary$winner)
table(matchSummary$home_winner)

names(matchSummary)
str(matchSummary$winner)

matchSummary <- as.data.frame(matchSummary[order(matchSummary$scheduled),])
rownames(matchSummary) <- c(1:nrow(matchSummary))


rm(i)
for (i in (1:nrow(matchSummary))){
  Match_ID <- matchSummary$Match_ID[i]
  Home_Team_ID = matchSummary$home_Team_ID[i]
  Away_Team_ID = matchSummary$away_Team_ID[i]
  match_date = as.Date(matchSummary$scheduled)[i]
  k0 = 10
  c = 10
  d = 400
  m = 1
  
  temp<-matchSummary[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled <=match_date & matchSummary$Match_ID !=Match_ID,]
  temp<-tail(temp[ordered(temp$scheduled),],1)
  
  if (length(temp$scheduled)==0){
    l0H <- as.numeric(0)
    l0A <- as.numeric(0)
    eh0 <- as.numeric(0)
    ea0 <- as.numeric(0)
  }else {
    l0H <- temp$lh
    l0A <- temp$la
    eh0 <- matchSummary$ELOH[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==temp$scheduled]
    ea0 <- matchSummary$ELOA[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==temp$scheduled]
  }
  ah <- matchSummary$home_winner[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==match_date & matchSummary$Match_ID==Match_ID]
  aa <- 1-ah
  del <- abs(matchSummary$home_Kills[i] - matchSummary$away_Kills[i])
  l1h = as.numeric(l0H + (k0*(1+del)^m)*(ah - eh0))
  
  temp<-matchSummary[matchSummary$away_Team_ID==Away_Team_ID & matchSummary$scheduled <=match_date & matchSummary$Match_ID!=Match_ID,]
  temp<-tail(temp[ordered(temp$scheduled),],1)
  
  l1a = as.numeric(l0A + (k0*(1+del)^m)*(aa - ea0))
  
  eh0 <- 1/(1+c^((l0A-l0H)/d))
  ea0 <- 1-eh0
  
  matchSummary$ELOH[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==match_date & matchSummary$Match_ID==Match_ID] <- eh0
  matchSummary$ELOA[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==match_date & matchSummary$Match_ID==Match_ID] <- ea0
  
  matchSummary$lh[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==match_date & matchSummary$Match_ID==Match_ID] <- l1h
  matchSummary$la[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==match_date & matchSummary$Match_ID==Match_ID] <- l1a
  
}

a <- matchSummary[matchSummary$home_Team_ID=="sr:competitor:220606" | matchSummary$away_Team_ID=="sr:competitor:220606",]


train_index <- createDataPartition(y = model_data$winner, times=1, p=0.8, list = FALSE) 
train_index

train <- model_data[train_index,]

test <- model_data[-train_index,]

head(train)
head(test)

control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs = TRUE)
seed <- 10
metric <- "ROC"
set.seed(seed)
mtry <- 5
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(winner~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)
summary(rf_default)

rf_default$results




#ELO Ratings=============================================
rm(i)
for (i in 1:nrow(matchSummary)){
  Match_ID <- matchSummary$Match_ID[i]
  Home_Team_ID = matchSummary$home_Team_ID[i]
  Away_Team_ID = matchSummary$away_Team_ID[i]
  match_date = as.Date(matchSummary$scheduled)[i]
  k0 = 10
  c = 10
  d = 400
  m = 1
  
  temp<-matchSummary[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled <=match_date,]
  temp<-tail(temp[ordered(temp$scheduled),],1)
  
  if (length(temp$scheduled)==0){
    l0H <- as.numeric(0)
    l0A <- as.numeric(0)
    del <- as.numeric(0)
    eh0 <- as.numeric(0)
    ea0 <- as.numeric(0)
  }else {
    l0H <- temp$lh
    l0A <- temp$la
    del <- abs(temp$home_Kills - temp$away_Kills)
    eh0 <- matchSummary$ELOH[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==temp$scheduled]
    ea0 <- matchSummary$ELOA[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==temp$scheduled]
  }
  ah <- matchSummary$home_winner[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==match_date & matchSummary$Match_ID==Match_ID]
  aa <- 1-ah
  
  l1h = as.numeric(l0H + (k0*(1+del)^m)*(ah - eh0))
  
  temp<-matchSummary[matchSummary$away_Team_ID==Away_Team_ID & matchSummary$scheduled <=match_date,]
  temp<-tail(temp[ordered(temp$scheduled),],1)
  
  if (length(temp$scheduled)==0){
    del <- 0
  }else {
    del <- abs(temp$home_Kills - temp$away_Kills)
  }
  l1a = as.numeric(l0A + (k0*(1+del)^m)*(aa - ea0))
  
  eh0 <- 1/(1+c^((l0A-l0H)/d))
  ea0 <- 1-eh0
  
  matchSummary$ELOH[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==match_date & matchSummary$Match_ID==Match_ID] <- eh0
  matchSummary$ELOA[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==match_date & matchSummary$Match_ID==Match_ID] <- ea0
  
  matchSummary$lh[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==match_date & matchSummary$Match_ID==Match_ID] <- l1h
  matchSummary$la[matchSummary$home_Team_ID==Home_Team_ID & matchSummary$scheduled==match_date & matchSummary$Match_ID==Match_ID] <- l1a
  
}

a <- matchSummary[matchSummary$home_Team_ID=="sr:competitor:220606" | matchSummary$away_Team_ID=="sr:competitor:220606",]




