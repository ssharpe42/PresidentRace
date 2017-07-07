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
           #Relabel retired and non-president winners as "Other"
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
    mutate_each(funs(rollmean(.,k = 3, fill =1/5, align = 'right')), Abe, George, Other, Teddy, Tom) %>%
    mutate_each(funs(rollmean(.,k = 5, fill =1/5, align = 'right')), Abe5, George5, Other5, Teddy5, Tom5) %>%
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

saveRDS(FullData, 'presraceapp/data/FullData.RDS')
saveRDS(FullDataT, 'presraceapp/data/FullDataT.RDS')


################ Graphs  ###############
library(ggplot2)
library(scales)
library(ggthemes)

#Cumulative wins over time
PresData %>% 
    select(-Abe, -George, -Other, -Teddy, -Tom) %>%
    arrange(DATE) %>%
    mutate(cnt =1) %>%
    spread(WINNER, cnt, fill = 0) %>%
    group_by(DATE) %>%
    summarise_each(funs(sum), Abe, George, Other, Teddy, Tom) %>%
    ungroup %>%
    mutate_each(funs(cumsum),-DATE) %>%
    gather(President, Wins, -DATE) %>%
    ggplot()+
    geom_line(aes(x = DATE, y = Wins, color = President), size =1) +
    theme_fivethirtyeight() +
    labs(title = 'Race to the Standings', subtitle='Cumulative Wins Over Time')+
    theme(text = element_text(size = 16))


#Attendance distribution by winner
FullData %>% 
    filter(ATTEND_PARK_CT>0)%>%
    ggplot()+
    geom_density(aes(x = ATTEND_PARK_CT, fill = WINNER), alpha = .5) +
    theme_fivethirtyeight() +
    scale_x_continuous(name = 'Game Attendance', labels = comma) +
    scale_y_continuous(name = 'Density')+
    labs(title = 'Nationals Park Attendance Distribution', subtitle='by Presidents Race Winner')+
    theme(text = element_text(size = 16))


#Attendance and Score Difference
FullData %>% 
    filter(ATTEND_PARK_CT>0)%>%
    group_by(ATTEND_PARK_CT =  cut(ATTEND_PARK_CT,breaks  = floor(quantile(ATTEND_PARK_CT,c(0,.25,.5,.75,1))/100)*100 + c(0,0,0,0,100)
                                   ,dig.lab=5, include.lowest=F),
             ScoreDiff = cut(ScoreDiff, breaks = floor(quantile(ScoreDiff,c(0,.25,.5,.75,1)) + c(-1,0,0,1,1))))%>%
    summarise(Teddy = mean(WINNER=='Teddy'), George = mean(WINNER=='George'),
              Tom = mean(WINNER=='Tom'), Abe = mean(WINNER=='Abe'),
              Other = mean(WINNER=='Other'), N = n()) %>%
    gather(President, WinPerc, -ATTEND_PARK_CT, -ScoreDiff,-N)%>%
    ggplot()+
    geom_point(aes(x = ScoreDiff, y = ATTEND_PARK_CT, colour = WinPerc, size=  N)) +
    facet_wrap(~President)+
    theme_fivethirtyeight() +
    scale_x_discrete(name = "\nNationals' Lead in the 4th Inning") +
    scale_y_discrete(name = 'Game Attendance\n')+
    scale_colour_gradient(name = "President Win Percentage", label = percent, breaks = c(0,.25,.5))+
    scale_size_area(name = '# of Games') +
    labs(title = "Success with a Crowd", subtitle="Win Percentage by Game Attendance and Nationals' Lead")+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16))

#Win Perc in last 3 vs Win Perc
Last3Result = FullData %>%select(Abe, Tom, Teddy, George, Other, WINNER) %>%
    gather(President, WinPerc3, -WINNER)%>%
    mutate(cnt = 1, Row = row_number()) %>%
    spread(WINNER, cnt, fill = 0)%>%
    select(-Row) %>%
    filter(WinPerc3 %in% c(0,1/3,2/3,1))%>%
    group_by(President, WinPerc3)

#Win percentage in next race
Last3WinPerc = summarise_each(Last3Result, funs(mean)) %>%
    ungroup%>%
    gather(President2, WinPerc,-President, -WinPerc3) %>%
    filter(President==President2) 

#Count of races where last three races had x winning percentage
Last3N = summarise_each(Last3Result, funs(n())) %>%
    ungroup%>%
    gather(President2, N,-President, -WinPerc3) %>%
    filter(President==President2) 

#Plot previous 3 race win perc vs next win perc
inner_join(Last3WinPerc, Last3N)%>%
    mutate(SE = sqrt(WinPerc*(1-WinPerc)/N))%>%
    ggplot(aes(x = WinPerc3, y = WinPerc))+
    geom_point()+
    geom_errorbar(aes(ymin=WinPerc-SE, ymax=WinPerc+SE), width = .25) +
    facet_wrap(~President)+
    theme_fivethirtyeight() +
    scale_x_continuous(name = "\nWinning Percentage Previous 3 Races", labels = percent) +
    scale_y_continuous(name = "Wining Percentage in the Next Race\n", labels = percent)+
    labs(title = "Streakiness")+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16))

#### ATTENDANCE ###
#Get full data with dates
FullDataAll= left_join(game_logs, PresData, by = 'ID') %>%
    left_join(., pbp) %>%
    filter(!is.na(WINNER))

#Plot rolling 15 game attendance over time
FullDataAll %>% 
    filter(ATTEND_PARK_CT>0)%>%
    group_by(DATE) %>% 
    filter(row_number()==1)%>%ungroup %>%
    arrange(DATE) %>%
    mutate(ATTEND_PARK_CT= rollmean(ATTEND_PARK_CT, 15, align = 'right', fill =NA))%>%
    ggplot()+
    geom_line(aes(x = DATE, y = ATTEND_PARK_CT, group = Season))+
    theme_fivethirtyeight() +
    scale_x_date(name = "") +
    scale_y_continuous(name = "Attendance (15 Game Rolling Average)\n", labels =comma)+
    labs(title = "Nationals Attendance 2008 - 2016", subtitle = '15 Game Rolling Average')+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16))


