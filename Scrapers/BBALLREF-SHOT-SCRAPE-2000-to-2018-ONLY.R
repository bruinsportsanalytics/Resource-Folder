#####################################################
## Shot Chart Scraping from BBALLREFERENCE website ##
## ##################################################
## ONLY FOR 2000-2001 season to 2017-2018 season!  ##
#####################################################

## Author: Ashwin Ghadiyaram

## Property of Sports Analytics Club of North Carolina State University

## Allowed to be shared among other Sports Analytics Clubs at other universities

### check working directory! ###
setwd("C:/Users/WangData/Documents/NBA Code/wd/")

### MAKE SURE THAT THE BBALLREF PLAYER INFO CSV IS IN THE WORKING DIRECTORY! ###
# If you don't have this, use the scraper R file to get that csv!

####################################################################################

rm(list=ls())

####################
# Install Packages #
####################

if (length(grep("stringr",installed.packages())) < 1) {
  install.packages("stringr")
}
if (length(grep("readr",installed.packages())) < 1) {
  install.packages("readr")
}


require(stringr)
require(readr)


# Make sure the .csv file of bballreference scraped player info is in your 
# working directory. If it is not, this step will not work.


# Read in the .csv file of bballreference scraped player info
# check if name of .csv file is correct in the read.csv line below

Player_df <- read.csv("BBallRef Player Info.csv",sep=",")
Player_df$First_Name <- as.character(Player_df$First_Name)
Player_df$Middle_Name <- as.character(Player_df$Middle_Name)
Player_df$Last_Name <- as.character(Player_df$Last_Name)
Player_df$Player_ID <- as.character(Player_df$Player_ID) 
Player_df$Home_City <- as.character(Player_df$Home_City) 
Player_df$Country <- as.character(Player_df$Country) 
Player_df$State <- as.character(Player_df$State) 



# Player_df is the data frame containing bball ref info

# Building the huge data frame of shot data:

Shot_df <- NULL

########################################################
############## start player loop here ##################
########################################################

SAC <- NULL 

## If you want to pull the player info in chunks, adjust the 
## statement in the "for loop" accordingly right here!
# Basically, the loop starts from the 1st player in the bballref
# player csv and goes all the way until the last entry by default

# Adjust it if you want!
for (SAC in 1:nrow(Player_df)){
  
df <- NULL
if (Player_df$Last_Name[SAC] == "Salle"){
  errorcheck = "b"} else if(Player_df$Last_Name[SAC] == "Colo"){
    errorcheck = "d"} else if(Player_df$Last_Name[SAC] == "Negro"){
      errorcheck = "d"} else if(Player_df$Last_Name[SAC]=="Grey"){
        errorcheck = "l"
      }else if(Player_df$Last_Name[SAC]=="III"){
        errorcheck = "l"}else if(Player_df$Middle_Name[SAC]=="Mbah"){
          errorcheck = "m"} else if (Player_df$Last_Name[SAC]=="Arsdale"){
            errorcheck = "v"
          } else if (Player_df$Last_Name[SAC]=="Breda"){
            errorcheck = "v"
          }else if (Player_df$Middle_Name[SAC]=="Van"){
            errorcheck = "v"
          } else if (Player_df$Last_Name[SAC]=="Nieda"){
            errorcheck = "v"
          } else if (Player_df$First_Name[SAC]=="Metta"){
            errorcheck = "a"
          } else{
    errorcheck <- strsplit(Player_df$Last_Name[SAC],split="")[[1]][1]}
errorcheck <- tolower(errorcheck)
html_player <- paste("https://www.basketball-reference.com/players/",errorcheck,sep="")
newhtml <- readLines(html_player)
location <- grep(Player_df$Player_ID[SAC],newhtml)

if(Player_df$Middle_Name[SAC]=="Lemon,"){
  newline <- 2018
  newline2 <- 2018
} else if (Player_df$Last_Name[SAC]=="Qi"){
  newline <- 2018
  newline2 <- 2018} else if (Player_df$Last_Name[SAC]=="Sy"){
    newline <- 2011
    newline2 <- 2011
  } else if (Player_df$First_Name[SAC]=="Metta"){
    newline <- 2001
    newline2 <- 2017
  } else if (Player_df$Last_Name[SAC]=="Grey"){
    newline <- 1969
    newline2 <- 1970
  }else{
newline <- newhtml[location]
newline <- strsplit(newline,split="\"year_min\" >")
newline <- newline[[1]][2]
newline <- strsplit(newline,split="<")
newline <- as.numeric(newline[[1]][1])

newline2 <- newhtml[location]
newline2 <- strsplit(newline2,split="\"year_max\" >")
newline2 <- newline2[[1]][2]
newline2 <- strsplit(newline2,split="<")
newline2 <- as.numeric(newline2[[1]][1])}


###########################
## Put if statement here ##
###########################
if (newline2 >= 2001){

###########################
## Put year loop here #####
###########################
Year <- NULL
finish <- newline2
if (newline >= 2001){
  start <- newline
}else{start <- 2001}


# INPUT PLAYER ID FROM BBALLREFERENCE
Player_ID <-   Player_df$Player_ID[SAC]

# INPUT FIRST NAME
FirstName <- Player_df$First_Name[SAC]

# INPUT MIDDLE NAME
MiddleName <- Player_df$Middle_Name[SAC]

# INPUT LAST NAME
LastName <- Player_df$Last_Name[SAC]

Name <- NULL
if(MiddleName == "NONE"){
  Name <- paste(FirstName,LastName,sep=" ")
}else{paste(FirstName,MiddleName,LastName,sep=" ")}

if(MiddleName=="Lemon,"){INTERVAL = 2018}else{INTERVAL=start:finish}

for(Year in INTERVAL){
  
df <- NULL
Year <- as.character(Year)

## Scraping the Page Source ##
html_1 <- "https://www.basketball-reference.com/players/"
html_2 <- strsplit(Player_ID,split="")[[1]][1]
Link <- paste(html_1,html_2,"/",Player_ID,"/shooting/",Year, sep="")

## Truncating the Scraped Data ##

Player <- readLines(Link)
if(is.na(grep("<img src=",Player)[2] + 3)==FALSE){
indice_1 <- grep("<img src=",Player)[2] + 3
indice_2 <- grep("global.nonempty",Player) - 17
Player <- Player[indice_1:indice_2]
writeLines(Player,"Player.csv")
Player_lines <- read.table(file="Player.csv",sep=",",check.names=F, header = F, colClasses = "character")

## Cleaning (not fun) ##
basketball <- 1
while (basketball < 1 + nrow(Player_lines)) {
  a <- gsub(",",";",Player_lines[basketball,1])
  b <- gsub("<br>",";",a)
  c <- gsub("<div style=","",b)
  d <- gsub(" vs ",";HOME;",c)
  e <- gsub("class=tooltip miss>&",";",d)
  f <- gsub("Jan ","01",e)
  g <- gsub("Feb ","02",f)
  h <- gsub("Mar ","03",g)
  i <- gsub("Apr ","04",h)
  j <- gsub("May ","05",i)
  k <- gsub("Jun ","06",j)
  l <- gsub("Jul ","07",k)
  m <- gsub("Aug ","08",l)
  n <- gsub("Sep ","09",m)
  o <- gsub("Oct ","10",n)
  p <- gsub("Nov ","11",o)
  q <- gsub("Dec ","12",p)
  r <- gsub(" tip=","",q)
  s <- gsub("; 20","",r)
  t <- gsub("; ",";",s)
  u <- gsub(" ;",";",t)
  v <- gsub("top:","",u)
  w <- gsub("left:","",v)
  x <- gsub("px","",w)
  y <- gsub("st Qtr","",x)
  z <- gsub("nd Qtr","",y)
  aa <- gsub("rd Qtr","",z)
  bb <- gsub("th Qtr","",aa)
  bbbB <- gsub("st OT","OT",bb)
  cccC <- gsub("nd OT","OT",bbbB)
  dddD <- gsub("rd OT","OT",cccC)
  eeeE <- gsub("th OT","OT",dddD)
  cc <- gsub(" remaining","",eeeE)
  dd <- gsub("... trails ","",cc)
  ee <- gsub("... leads ","",dd)
  ff <- gsub("... now trails ","",ee)
  gg <- gsub("... now leads ","",ff)
  #hh <- gsub("-",";",gg)
  obama = strsplit(gg,split="")
  obama2 = grep("-",obama[[1]])
  obama2 = sort(obama2,decreasing=TRUE)
  obama3 = obama2[1]
  obama4 = obama2[2]
  obama[[1]][obama3] = ";"
  obama[[1]][obama4] = ";"
  hh = paste(obama[[1]],collapse="")
  ii <- gsub("Missed ","MISS;",hh)
  jj <- gsub("Made ","MAKE;",ii)
  kk <- gsub("pointer from ","",jj)
  ll <- gsub(" ft","",kk)
  mm <- gsub("class=tooltip make>&",";",ll)
  nn <- gsub(" at ",";AWAY;",mm)
  oo <- gsub("... tied ","",nn)
  pp <- gsub(";..[ABCDEFGHIJKLMNOPQRSTUVWXYZ] ",";",oo)
  qq <- gsub(" ;",";",pp)
  rr <- gsub(";;","",qq)
  ss <- gsub("MAKE","1",rr)
  tt <- gsub("MISS","0",ss)
  
  
  Player_lines[basketball,1] <- tt
  basketball <- basketball+1
}

df <- NULL
counting <- NULL


# Converts cleaned data into a data frame
df <- data.frame(str_split_fixed(Player_lines[,1],";",13),stringsAsFactors = FALSE)

# Additional cleaning of columns and date information
switch <- df
df[,5] <- switch[,6]
df[,6] <- switch[,5]


date <- df[,3]
date2 <- matrix(date,nrow=length(date))
date3 <- strsplit(as.character(date2),split="")

datefix <- 1
while (datefix < 1+nrow(date2)) {
  if (length(date3[[datefix]])==5) {
    date3[[datefix]][3] <- paste("0",date3[[datefix]][3],sep="")
    date3[[datefix]] <- paste(date3[[datefix]][1],date3[[datefix]][2],date3[[datefix]][3],date3[[datefix]][4],date3[[datefix]][5],sep="")
  }
  datefix <- datefix + 1
}

datefix2 <- 1
while (datefix2 <1+nrow(date2)) {
  if(length(date3[[datefix2]])==6){
    date3[[datefix2]] <- paste(date3[[datefix2]][1],date3[[datefix2]][2],date3[[datefix2]][3],date3[[datefix2]][4],date3[[datefix2]][5],date3[[datefix2]][6],sep="")
  }
  datefix2 <- datefix2 + 1
}

date3 <- unlist(date3)
df[,3] <- date3


df[,13] <- gsub(";","",df[,13])


# Append a name and year column
col_year <- matrix(rep(Year,nrow(df)),nrow=nrow(df))
if(LastName=="Colo"){
  Name <- "Nando De Colo"}else if(LastName=="Negro"){
    Name <- "Vinny Del Negro"}else if(MiddleName=="Lemon,"){
      Name <- "Walt Lemon,Jr."} else if(LastName=="III"){
        Name <- "John Lucas III"
      } else if (MiddleName=="Mbah"){
        Name <- "Luc Mbah a Moute"
      } else if (LastName=="McAdoo"){
        Name <- "James Michael McAdoo"
      } else if (LastName=="Navarro"){
        Name <- "Juan Carlos Navarro"
      } else if (LastName=="Ramos"){
        Name <- "Peter John Ramos"
      } else if (LastName=="Exel"){
        Name <- "Nick Van Exel"
      } else if (FirstName=="Keith" & MiddleName=="Van"){
        Name <- "Keith Van Horn"
      } else if (FirstName=="Norm" & MiddleName=="Van"){
        Name <- "Norm Van Lier"
      } else if (FirstName=="Dennis" & MiddleName=="Van"){
        Name <- "Dennis Van Zant"
      } else if (FirstName=="Metta"){
        Name <- "Metta World Peace"
      }
col_name <- matrix(rep(Name,nrow(df)),nrow=nrow(df))
col_ID <- matrix(rep(Player_ID,nrow(df)),nrow=nrow(df))
df <- cbind(col_ID,col_name,col_year,df)

# Generating column names for the data frame
col000 <- "ID"
col00 <- "Player"
col0 <- "Season"
col1 <- "Top(px)"
col2 <- "Left(px)"
col3 <- "Date"
col4 <- "Team"
col5 <- "Opponent"
col5.5 <- "Location"
col6 <- "Quarter"
col7 <- "Game_Clock"
col8 <- "Outcome"
col9 <- "Shot_Value"
col10 <- "Shot_Distance(ft)"
col11 <- "Team_Score"
col12 <- "Opponent_Score"

columns <- c(col000,col00,col0,col1,col2,col3,col4,col5,col5.5,col6,col7,col8,col9,col10,col11,col12)

colnames(df) <- columns

name.width <- max(sapply(names(df), nchar))
format(df, width = name.width, justify = "centre")


if(is.null(Shot_df==TRUE)){Shot_df <- df} else{Shot_df <- rbind(Shot_df,df)}}}}}


# Final output is a data frame titled "Shot_df"

write.csv(Shot_df,file="Shots.csv")


