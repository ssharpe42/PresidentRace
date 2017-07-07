setwd('/Users/Sam/Desktop/Projects/Presidents Race/')
library(dplyr)
library(tidyr)
library(dbConnect)
library(rpart)
library(zoo)
library(MLmetrics)
library(rpart.plot)
options(stringsAsFactors = F)

#Retrosheet database
dbRetro=dbConnect(MySQL(),user="root",
                  host="localhost",
                  dbname="retrosheet",
                  password="root",
                  unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")

#Load play by play data
pbp = fetch(dbSendQuery(dbRetro, "select * from events_bck where HOME_TEAM_ID='WAS'"), n = -1)
pbp[, grepl('_FL$', names(pbp))] = lapply(pbp[, grepl('_FL$', names(pbp))], as.logical)
pbp = group_by(pbp, GAME_ID) %>% 
    filter(INN_CT <= 3 | (INN_CT ==4 & BAT_HOME_ID==0)) %>%
    filter(row_number()==n()) %>%
    ungroup %>%
    transmute(ScoreDiff = HOME_SCORE_CT-AWAY_SCORE_CT,
              ID = substr(GAME_ID, 4,12)) 

#Game logs 
game_logs = fetch(dbSendQuery(dbRetro, paste0("select * from games_bck where HOME_TEAM_ID='WAS' OR AWAY_TEAM_ID='WAS'")),n = -1) %>%
    mutate(Date = as.Date(paste(substr(GAME_ID, 4, 7),substr(GAME_ID, 8,9), substr(GAME_ID, 10,11), sep = '-')),
           ID = paste0(GAME_DT, GAME_CT),
           START_GAME_TM = ifelse(START_GAME_TM<1100, START_GAME_TM+1200, START_GAME_TM),
           #Is the game in the NL East?
           InDivision = as.integer(AWAY_TEAM_ID %in% c('ATL','FLO','MIA','NYN','PHI')),
           #Did the Nats Win?
           Win  = as.integer((HOME_TEAM_ID=='WAS' & HOME_SCORE_CT > AWAY_SCORE_CT) |
                                 (AWAY_TEAM_ID=='WAS' & HOME_SCORE_CT < AWAY_SCORE_CT))) %>%
    group_by(YEAR_ID) %>%
    arrange(Date) %>%
    #Previous game score differential
    mutate(LastGameScoreDiff = lag(HOME_SCORE_CT) - lag(AWAY_SCORE_CT),
           #Make the score diff of first game of the season 0
           LastGameScoreDiff = ifelse(is.na(LastGameScoreDiff),0,LastGameScoreDiff),
           #Current Winning Percentage
           NatsWinPerc = cummean(Win),
           #Current Winning Percentage as of start of game (first game of season = .5)
           NatsWinPerc = ifelse(row_number()==1,.5,lag(NatsWinPerc))) %>% ungroup %>%
    #Keep only home games since races occur at home
    filter(HOME_TEAM_ID=='WAS')

#President Race data
PresData = read.csv('PresRaceData.csv') %>%
    replace_na(list(Game = 0)) %>%
    #Filter out races without winners or cancelled races
    filter(!WINNER %in% c('','N/A')) %>%
    mutate(DATE = as.Date(DATE),
           ID = paste0(substr(DATE,1,4),substr(DATE,6,7), substr(DATE,9,10), Game),
           #Label relay or parter races vs. races with a single winner
           Type = ifelse(grepl('\\/|Split',WINNER),'Relay/Tie','Single'),
           #Relabel winners with under 6% success (Bill) as "Other"
           WINNER = ifelse(grepl('Werth|\\*|N/A|Screech|Calvin|Herbie|Bill',WINNER),'Other',
                           ifelse(Type=='Relay/Tie', 'Relay/Tie', WINNER)),
           WINNER2 = WINNER,
           Cnt = 1)%>%
    #create dummy variables for each President
    spread(WINNER2, Cnt, fill =0) %>%
    #make another set of dummy variables for last "k" win percentage
    mutate(Abe5 = Abe, George5 = George, Other5 = Other, Teddy5 = Teddy, Tom5 = Tom,
           Abe_Wperc = Abe, George_Wperc = George, Other_Wperc = Other, Teddy_Wperc = Teddy, Tom_Wperc = Tom) %>%
    group_by(Season) %>%
    #Create 3 game, 5 game, and Season winning percentage variables
    mutate_each(funs(rollmean(.,k = 3, fill =1/5/3, align = 'right')), Abe, George, Other, Teddy, Tom) %>%
    mutate_each(funs(rollmean(.,k = 5, fill =1/5/5, align = 'right')), Abe5, George5, Other5, Teddy5, Tom5) %>%
    mutate_each(funs(cummean), Abe_Wperc, George_Wperc , Other_Wperc, Teddy_Wperc , Tom_Wperc)%>%
    #Make each win perc as of the beginning of the current game
    mutate_each(funs(lag(., default = 1/5)), Abe, George, Other, Teddy, Tom) %>%
    mutate_each(funs(lag(., default = 1/5)), Abe5, George5, Other5, Teddy5, Tom5) %>%
    mutate_each(funs(lag(., default = 1/5)), Abe_Wperc, George_Wperc , Other_Wperc, Teddy_Wperc , Tom_Wperc)%>%
    #Let Teddy win campaign might have a big effect on when teddy wins
    mutate(HasTeddyWonYet = row_number()!=1 & lag(WINNER) =='Teddy',
           HasTeddyWonYet = ifelse(cumsum(HasTeddyWonYet)>0,'YES','NO'),
           #Which race is the current race? 
           RaceNum = row_number(),
           #Who was the last race winner?
           LastWinner = ifelse(row_number()==1,'First Game',lag(WINNER)),
           LastWinner2 = ifelse(row_number()<=2,'First 2 Games', lag(WINNER,2)),
           LastWinner3 = ifelse(row_number()<=3,'First 3 Games', lag(WINNER,3))) %>%
    #Filter out partner races
    filter(Type != 'Relay/Tie') 

#Final Data Frame
FullData = left_join(game_logs, PresData, by = 'ID') %>% 
    left_join(., pbp) %>%
    filter(!is.na(WINNER))%>%
    select( START_GAME_TM, ATTEND_PARK_CT, WINNER, InDivision, HasTeddyWonYet , LastGameScoreDiff, NatsWinPerc, ScoreDiff,
            RaceNum, LastWinner, LastWinner2, LastWinner3,Abe5, George5, Other5, Teddy5, Tom5, Abe, George, Other, Teddy, Tom,
            Abe_Wperc, George_Wperc , Other_Wperc, Teddy_Wperc , Tom_Wperc)

FullData[,sapply(FullData, is.character)] = lapply(FullData[, sapply(FullData, is.character)], as.factor)



############  Guess Teddy  ################
FullDataT = left_join(game_logs, PresData, by = 'ID') %>%
    left_join(., pbp) %>%
    filter(!is.na(WINNER))%>%
    mutate(WINNER = ifelse(WINNER=='Teddy','Teddy','NotTeddy'),
           NotTeddy5 = 1- Teddy5, NotTeddy = 1- Teddy, NotTeddy_Wperc = 1-Teddy_Wperc)%>%
    select( START_GAME_TM, ATTEND_PARK_CT, WINNER, InDivision, HasTeddyWonYet , LastGameScoreDiff, NatsWinPerc, ScoreDiff,
            RaceNum, LastWinner, LastWinner2, LastWinner3,Teddy5, NotTeddy5, NotTeddy, Teddy,NotTeddy_Wperc, Teddy_Wperc,
            Abe5, George5, Other5,  Tom5, Abe, George, Other,  Tom,
            Abe_Wperc, George_Wperc , Other_Wperc,  Tom_Wperc)

FullDataT[,sapply(FullDataT, is.character)] = lapply(FullDataT[, sapply(FullDataT, is.character)], as.factor)

saveRDS(FullData, 'FullData.RDS')
saveRDS(FullDataT, 'FullDataT.RDS')
saveRDS(FullData, 'app/data/FullData.RDS')
saveRDS(FullDataT, 'app/data/FullDataT.RDS')



