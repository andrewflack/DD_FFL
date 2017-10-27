# clean up raw scores to create a tidy data set

### remove rows that are blank or incomplete 

# occurs when an owner started fewer than 8 players, or started a player who is on bye
# disregard the "slot" column because it reads N/A until the 2014 season
scores.clean <- scores.raw[complete.cases(scores.raw[,1:7]),]

# make sure the correct number of rows were removed
# summary(complete.cases(scores.raw[,1:7]))
#    Mode   FALSE    TRUE    NA's 
# logical      53    4104       0

# there are also a few rows that have "--" in the pts column
# this also occurs when an owner starts fewer than 8 players
scores.clean <- scores.clean[-grep("--",scores.clean$PTS,fixed=TRUE),]

# do an initial rename of columns just to make the rest of script easier to read
# will do a full column name cleanup at the end as well
colnames(scores.clean) <- c("player.team.pos","NFLopp","NFLstatus","FFLpts","year","week","teamID","slot")


### work on splitting up the player.team.pos column

# first, split on comma to get player in one col and team_pos in another column
# firstsplit <- do.call(rbind,strsplit(scores.clean$player.team.pos,",",fixed=TRUE))
# firstsplit <- as.data.frame(firstsplit)
# colnames(firstsplit) <- c("player","teampos")
scores.clean <- scores.clean %>% separate(player.team.pos, c("player", "teampos"), sep = ",", fill = "right", extra = "merge")

# remove probable (P), questionable (Q), out (O), suspended (SSPD) and all other punctionation besides slash, remove * from player col
# firstsplit$teampos <- gsub("\\s+Q$|\\s+P$|\\s+O$|\\s+SSPD|\\s+IR|[^[:alnum:][:space:]]","",firstsplit$teampos)
# firstsplit$player <- gsub("[^[:alnum:][:space:]/'-]","",firstsplit$player)
# firstsplit$teampos <- gsub("\\s{1}DST\\s"," ",firstsplit$teampos) # remove first occurrence of "D/ST"
# firstsplit$player <- gsub("\\s{1}DST\\s"," ",firstsplit$player) # remove first occurrence of "D/ST"
scores.clean$teampos <- gsub("\\s+Q$|\\s+P$|\\s+O$|\\s+SSPD|\\s+IR|[^[:alnum:][:space:]]","",scores.clean$teampos)
scores.clean$player <- gsub("[^[:alnum:][:space:]/'-]","",scores.clean$player)
scores.clean$teampos <- gsub("\\s{1}DST\\s"," ",scores.clean$teampos) # remove first occurrence of "D/ST"
scores.clean$player <- gsub("\\s{1}DST\\s"," ",scores.clean$player) # remove first occurrence of "D/ST"

# remove leading whitespace from teampos column
# firstsplit$teampos <- gsub("^\\s+|\\s+$", "",firstsplit$teampos)
scores.clean$teampos <- gsub("^\\s+|\\s+$", "",scores.clean$teampos)

# second, split teampos column on space

# secondsplit <- do.call(rbind,strsplit(firstsplit$teampos,"[[:space:]]{1}"))
# secondsplit <- as.data.frame(secondsplit)
# colnames(secondsplit) <- c("NFLteam","pos")
scores.clean <- scores.clean %>% separate(teampos, c("NFLteam", "pos"), extra = "merge")


# replace full team names with team abbreviations
scores.clean$NFLteam <- gsub("\\<49ers\\>","SF",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Bears\\>","Chi",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Bengals\\>","Cin",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Bills\\>","Buf",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Broncos\\>","Den",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Browns\\>","Cle",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Cardinals\\>","Ari",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Chargers\\>","SD",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Chiefs\\>","KC",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Colts\\>","Ind",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Dolphins\\>","Mia",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Eagles\\>","Phi",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Lions\\>","Det",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Packers\\>","GB",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Panthers\\>","Car",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Patriots\\>","NE",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Raiders\\>","Oak",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Rams\\>","StL",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Ravens\\>","Bal",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Seahawks\\>","Sea",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Steelers\\>","Pit",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Texans\\>","Hou",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Titans\\>","Ten",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Vikings\\>","Min",scores.clean$NFLteam)
scores.clean$NFLteam <- gsub("\\<Was\\>","Wsh",scores.clean$NFLteam)

# # bind firstsplit$players and secondsplit to scores.clean
# scores.clean <- cbind(scores.clean,firstsplit[,1],secondsplit)
# colnames(scores.clean) <- c("player.team.pos","NFLopp","NFLstatus","FFLpts","year","week","teamID","slot","player","NFLteam","pos") #cbind doesn't preserve player column name for some reason?


### clean up the NFLopp column

# create new column for home/away and fill with "home"
scores.clean$homeaway <- rep("home",length(scores.clean$NFLopp))

# find all the rows that have the @ symbol, and mark those as away games
scores.clean[grep("@", scores.clean$NFLopp, fixed=TRUE, value=FALSE),"homeaway"] <- "away"

# ..then remove the @'s from NFLopp
scores.clean$NFLopp <- gsub("@", "", scores.clean$NFLopp, fixed=TRUE)

# change any occurrence of "Was" to "Wsh"
scores.clean$NFLopp <- gsub("\\<Was\\>","Wsh",scores.clean$NFLopp)


### clean up the NFL status column

# remove the weird » character and leading whitespace
scores.clean$NFLstatus <- gsub("»", "", scores.clean$NFLstatus, fixed=TRUE)
scores.clean$NFLstatus <- gsub("-", " ", scores.clean$NFLstatus, fixed=TRUE)
scores.clean$NFLstatus <- gsub("^\\s+|\\s+$", "",scores.clean$NFLstatus)

# split on space
# NFLstatussplit <- do.call(rbind,strsplit(scores.clean$NFLstatus," ",fixed=TRUE)) 
# NFLstatussplit <- as.data.frame(NFLstatussplit)
# colnames(NFLstatussplit) <- c("winloss","NFLteamscore","NFLoppscore")
# 
# # bind back to the scores.clean DF
# scores.clean <- cbind(scores.clean,NFLstatussplit)
scores.clean <- scores.clean %>% separate(NFLstatus, c("winloss", "NFLteamscore", "NFLoppscore"))

### merge teams and owners with scores.clean
scores.clean <- merge(ddffl.teamsandowners,scores.clean,by=c("year","teamID"))


# ### drop unnecessary columns 
# scores.clean <- subset(scores.clean,select=-c(player.team.pos,NFLstatus))


### reassign data types as necessary
scores.clean[,c("year","teamID","name","owner","NFLopp","week","player","NFLteam","pos","homeaway","winloss")] <- 
  lapply(scores.clean[,c("year","teamID","name","owner","NFLopp","week","player","NFLteam","pos","homeaway","winloss")], as.factor)

scores.clean$FFLpts <- as.numeric(scores.clean$FFLpts)
scores.clean$NFLteamscore <- as.numeric(as.character(scores.clean$NFLteamscore))
scores.clean$NFLoppscore <- as.numeric(as.character(scores.clean$NFLoppscore))

### make sure data types match in teamsandowners and weeklymatchups
ddffl.teamsandowners[,c("year","teamID","name","owner")] <- lapply(ddffl.teamsandowners[,c("year","teamID","name","owner")] , as.factor) 
ddffl.weeklymatchups[,c("year","week","teamID","opp_teamID")] <- lapply(ddffl.weeklymatchups[,c("year","week","teamID","opp_teamID")] , as.factor) 


### FUTURE WORK - also merge FFL opponent using week and teamID

### use dplyr to organize the dataframe a little nicer
# scores.clean <- scores.clean %>%
#   group_by(year,week,teamID) %>%
#   select(year,week,teamID,name,owner,player,pos,slot,FFLpts,NFLteam,NFLopp,homeaway,winloss,NFLteamscore,NFLoppscore)

