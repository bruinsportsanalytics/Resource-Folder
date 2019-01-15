##############################################################################
# SCRAPING BBALL REF PLAYER IDS + BIRTH LOCATIONS             

# Outputs a csv file of every player on bballref, including name, bballref id, etc.
##############################################################################

## Author: Ashwin Ghadiyaram

## Property of Sports Analytics Club of North Carolina State University

## Allowed to be shared among other Sports Analytics Clubs at other universities


### SET YOUR WORKING DIRECTORY ###
setwd("C:/Users/WangData/Documents/NBA Code/wd/")

rm(list=ls())

##############################################################################

# Read in Player Names + IDs as lines to parse through

html_players <- "https://www.basketball-reference.com/players/"
interval <- letters[c(1:23,25,26)]

alpha <- NULL   
somanylines <- NULL
somanylines <- as.list(somanylines)

for(alpha in interval){
  link_player <- paste(html_players,alpha,sep="")
  NCSU <- readLines(link_player)
  indiceone <- grep("<tbody>",NCSU)[1] + 1
  indicetwo <- grep("</tbody></table>",NCSU)[1] - 2
  NCSU <- NCSU[indiceone:indicetwo]
  somanylines <- c(somanylines,NCSU)
}

beta <- 1
PlayerName <- NULL
PlayerID <- NULL

while(beta < length(somanylines) + 1) {
  aaa <- somanylines[beta]
  bbb <- gsub("<tr ><th scope=\"row\" class=\"left \" data-append-csv=","",aaa)
  ccc <- strsplit(bbb,split=">")
  ddd_ID <- ccc[[1]][1]
  if(ccc[[1]][2]=="<strong"){
    ddd_Name <- ccc[[1]][4]}
  else {
    ddd_Name <- ccc[[1]][3]
  }
  eee_ID <- strsplit(ddd_ID,split="")
  fff_ID <- paste(eee_ID[[1]][2:10],collapse="")
  eee_Name <- gsub("</a","",ddd_Name)
  PlayerName[beta] <- eee_Name
  PlayerID[beta] <- fff_ID
  beta <- beta + 1
}

PlayerID <- gsub('\"', "", PlayerID, fixed = TRUE)
PlayerID <- gsub(" ","",PlayerID)


PlayerID # Vector of player IDs in order
PlayerName # Vector of player names in order

# Make a data frame of these two vectors "Player_df"

Player_df <- cbind(PlayerName,PlayerID)

########################################

# Now to append birth location

html_players <- "https://www.basketball-reference.com/players/"
manatee <- 1
cityvector <- NULL
countryorstatevector <- NULL

while (manatee < nrow(Player_df)+1) {
  nbaplayerID <- Player_df[manatee,2]
  firstletter <- strsplit(nbaplayerID,split="")[[1]][1]
  newlink <- paste(html_players,firstletter,"/",nbaplayerID,".html",sep="")
  stringparsingsucks <- readLines(newlink)
  indice.birthplace <- grep("birthPlace",stringparsingsucks) + 1
  stringparsingsucks <- stringparsingsucks[indice.birthplace]
  stringparsingsucks <- gsub("in&nbsp;","",stringparsingsucks)
  stringparsingsucks <- gsub("</a></span>","",stringparsingsucks)
  stringparsingsucks <- gsub(" ","",stringparsingsucks)
  stringparsingsucks <- gsub(",&nbsp;<ahref='/friv/birthplaces.cgi?country=..&state=..'>",";",stringparsingsucks)
  stringparsingsucks <- strsplit(stringparsingsucks,split=";")
  cityvector[manatee] <- stringparsingsucks[[1]][1]
  countryorstatevector[manatee] <- stringparsingsucks[[1]][2]
  manatee <- manatee + 1
}

cityvector <- gsub(",&nbsp","",cityvector, fixed=TRUE)
countryorstatevector <- gsub("<ahref='/friv/birthplaces.cgi?","",countryorstatevector)
countryorstatevector <- strsplit(countryorstatevector,split=">")

row <- NULL
newcountryorstatevector <- NULL
for (row in 1:length(countryorstatevector)){
  newcountryorstatevector[row] <- countryorstatevector[[row]][1]
}
newcountryorstatevector <- gsub("state=","",newcountryorstatevector)
newcountryorstatevector <- gsub("country=","",newcountryorstatevector)
newcountryorstatevector <- gsub("?","",newcountryorstatevector,fixed=TRUE)
newcountryorstatevector <- gsub("'","",newcountryorstatevector,fixed=TRUE)
newcountryorstatevector <- strsplit(newcountryorstatevector,split="&")

NAindices <- which(is.na(newcountryorstatevector))

row <- NULL
for (row in NAindices) {
  newcountryorstatevector[[row]] <- c("UNKNOWN","UNKNOWN")
}

row <- NULL
countryvector <- NULL
statevector <- NULL
for (row in 1:length(newcountryorstatevector)) {
  countryvector[row] <- newcountryorstatevector[[row]][1]
  statevector[row] <- newcountryorstatevector[[row]][2]
}

NAindices2 <- which(is.na(statevector))
row <- NULL
for (row in NAindices2) {
  statevector[row] <- "UNKNOWN"
}

Player_df <- cbind(Player_df,cityvector,countryvector,statevector)
Player_df <- as.data.frame(Player_df)
colnames(Player_df) <- c("Player_Name","Player_ID","Home_City","Country","State")


Player_df$Player_Name <- as.character(Player_df$Player_Name) #
Player_df$Player_ID <- as.character(Player_df$Player_ID) #
Player_df$Home_City <- as.character(Player_df$Home_City) #
Player_df$Country <- as.character(Player_df$Country) #
Player_df$State <- as.character(Player_df$State) #

# Additional Debugging 
errorindices <- grep("ahref",Player_df$Home_City)
length(errorindices) 
row <- NULL 

for (row in errorindices){
  smh <- Player_df$Home_City[row]
  smh <- gsub("<ahref='/friv/birthplaces.cgi?","",smh,fixed = TRUE)
  smh <- strsplit(smh,split=">")
  smh <- smh[[1]][1]
  smh <- gsub("country=","",smh)
  smh <- gsub("state=","",smh)
  smh <- gsub("'","",smh)
  smh <- strsplit(smh,split="&")
  Player_df$Home_City[row] <- "UNKNOWN"
  Player_df$Country[row] <- smh[[1]][1]
  Player_df$State[row] <- smh[[1]][2]
}

# fix NA + <NA> in columns too
NAindicesfinal <- which(is.na(Player_df$Home_City))
row <- NULL
for (row in NAindicesfinal) {
  Player_df$Home_City[row] <- "UNKNOWN"
}

NAindicesfinal <- which(is.na(Player_df$Country))
row <- NULL
for (row in NAindicesfinal) {
  Player_df$Country[row] <- "UNKNOWN"
}

NAindicesfinal <- which(is.na(Player_df$State))
row <- NULL
for (row in NAindicesfinal) {
  Player_df$State[row] <- "UNKNOWN"
}

# Fix up to have separate columns for first, middle, and last name.

row <- 1
First_Name <- NULL
Middle_Name <- NULL
Last_Name <- NULL
while (row < nrow(Player_df)+1){
  string <- Player_df$Player_Name[row]
  string <- strsplit(string,split=" ")
  if (length(string[[1]])==2){
    First_Name[row] <- string[[1]][1]
    Middle_Name[row] <- "NONE"
    Last_Name[row] <- string[[1]][2]
  }else{
    First_Name[row] <- string[[1]][1]
    Middle_Name[row] <- string[[1]][2]
    Last_Name[row] <- string[[1]][3]
  }
  row <- row + 1
}


Player_df <- Player_df[,2:5]
Player_df <- cbind(First_Name,Middle_Name,Last_Name,Player_df)

#Publish final product (Player_df)

write.csv(Player_df,file="BBallRef Player Info.csv",col.names=TRUE,row.names=FALSE,sep=",")


# Player_df contains the name, bballref ID, and birth location for every player ever.

#######################################################################
#######################################################################
#######################################################################
#######################################################################