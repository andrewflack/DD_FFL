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
firstsplit <- do.call(rbind,strsplit(scores.clean$player.team.pos,",",fixed=TRUE))
firstsplit <- as.data.frame(firstsplit)
colnames(firstsplit) <- c("player","teampos")

# remove probable (P), questionable (Q), out (O), suspended (SSPD) and all other punctionation besides slash, remove * from player col
firstsplit$teampos <- gsub("\\s+Q$|\\s+P$|\\s+O$|\\s+SSPD|\\s+IR|[^[:alnum:][:space:]/]","",firstsplit$teampos)
firstsplit$player <- gsub("[^[:alnum:][:space:]/'-]","",firstsplit$player)
firstsplit$teampos <- gsub("\\s{1}D/ST\\s"," ",firstsplit$teampos) # remove first occurrence of "D/ST"
firstsplit$player <- gsub("\\s{1}D/ST\\s"," ",firstsplit$player) # remove first occurrence of "D/ST"

# remove leading whitespace from teampos column
firstsplit$teampos <- gsub("^\\s+|\\s+$", "",firstsplit$teampos)

# second, split teampos column on space

secondsplit <- do.call(rbind,strsplit(firstsplit$teampos,"[[:space:]]{1}"))
secondsplit <- as.data.frame(secondsplit)
colnames(secondsplit) <- c("NFLteam","pos")

# replace full team names with team abbreviations
secondsplit$NFLteam <- gsub("\\<49ers\\>","SF",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Bears\\>","Chi",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Bengals\\>","Cin",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Bills\\>","Buf",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Broncos\\>","Den",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Browns\\>","Cle",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Cardinals\\>","Ari",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Chargers\\>","SD",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Chiefs\\>","KC",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Colts\\>","Ind",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Dolphins\\>","Mia",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Eagles\\>","Phi",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Lions\\>","Det",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Packers\\>","GB",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Panthers\\>","Car",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Patriots\\>","NE",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Raiders\\>","Oak",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Rams\\>","StL",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Ravens\\>","Bal",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Seahawks\\>","Sea",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Steelers\\>","Pit",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Texans\\>","Hou",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Titans\\>","Ten",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Vikings\\>","Min",secondsplit$NFLteam)
secondsplit$NFLteam <- gsub("\\<Was\\>","Wsh",secondsplit$NFLteam)

# bind firstsplit$players and secondsplit to scores.clean
scores.clean <- cbind(scores.clean,firstsplit[,1],secondsplit)
colnames(scores.clean) <- c("player.team.pos","NFLopp","NFLstatus","FFLpts","year","week","teamID","slot","player","NFLteam","pos") #cbind doesn't preserve player column name for some reason?


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
NFLstatussplit <- do.call(rbind,strsplit(scores.clean$NFLstatus," ",fixed=TRUE)) 
NFLstatussplit <- as.data.frame(NFLstatussplit)
colnames(NFLstatussplit) <- c("winloss","NFLteamscore","NFLoppscore")

# bind back to the scores.clean DF
scores.clean <- cbind(scores.clean,NFLstatussplit)


### merge teams and owners with scores.clean
scores.clean <- merge(ddffl.teamsandowners,scores.clean,by=c("year","teamID"))


### drop unnecessary columns 
scores.clean <- subset(scores.clean,select=-c(player.team.pos,NFLstatus))


### reassign data types as necessary
scores.clean[,c("year","teamID","name","owner","NFLopp","week","player","NFLteam","pos","homeaway","winloss")] <- 
  lapply(scores.clean[,c("year","teamID","name","owner","NFLopp","week","player","NFLteam","pos","homeaway","winloss")], as.factor)

#####THIS SEEMS TO SCREW UP TEAM SCORES
scores.clean$FFLpts <- as.numeric(scores.clean$FFLpts)
scores.clean$NFLteamscore <- as.numeric(as.character(scores.clean$NFLteamscore))
scores.clean$NFLoppscore <- as.numeric(as.character(scores.clean$NFLoppscore))
# scores.clean[,c("FFLpts","NFLteamscore","NFLoppscore")] <- 
#   lapply(scores.clean[,c("FFLpts","NFLteamscore","NFLoppscore")], as.numeric)

### FUTURE WORK - also merge FFL opponent using week and teamID

### use dplyr to organize the dataframe a little nicer
# scores.clean <- scores.clean %>%
#   group_by(year,week,teamID) %>%
#   select(year,week,teamID,name,owner,player,pos,slot,FFLpts,NFLteam,NFLopp,homeaway,winloss,NFLteamscore,NFLoppscore)

