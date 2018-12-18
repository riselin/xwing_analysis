# Regionals analysis
# 10.12.18
# once json is available: use https://konklone.io/json/ to change json data to csv
# raw data at https://listfortress.com/
# limit to extended format and large (>20) events with full swiss, preferably nationals



rm(list=ls())
#setwd("C:/Users/riselin/Documents/polybox/privat/xwing/Turniere_2.0/")
#setwd("~/polybox/privat/xwing/Turniere_2.0/Nationals2018_2019")

library(stringr)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(reshape2)
library(gridExtra)
library(cowplot)
library(scales)
library(igraph)
library(plyr)
library(sfsmisc)
library(outliers)


#--------- Functions and Constants------
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
factioncolors <- c("darkgreen", "red2", "orange")
moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
} #necessary for dat_assembly
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.d.cAnada.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
split_score <- function(fdata, frounds=6){
  int_data <- fdata
  d.win <- int_data$score
  rounds <- frounds
  d.loss <- rounds-d.win
  int_data$losses <- d.loss
  int_data
}
dat_assembly <- function(fdata){
  fdata <- fdata[moveme(names(fdata), "playerName first; faction after playerName; score after faction; losses after score;  swissRank after losses; 
                        cutRank after swissRank; date after cutRank; listID after date; points after listID")]
  int_data <- fdata[,c(1:9, 13:(ncol(fdata)))]
  int_data <- rename(int_data, c("playerName" = "name", "score" = "wins", "swissRank" = "swiss", "cutRank" = "cut"))
  int_data
} #requires moveme
getArcRow <- function(squad_data, factionF=NULL){
  sums <- c()
  i <- 1
  non_primary <- 0
  overall <- 0
  j <- 1
  for (i in 1:nrow(squad_data)){
    j <- 1
    max_j <- length(squad_data[squad_data[,"matchID"]==squad_data[i,"matchID"],"matchID"]) # store the amount of rows with the same matchID
    
    while(squad_data[i,"matchID"]==squad_data[i+j,"matchID"]){ #while the next number is the same as the current iteration: keep counting
      j <- j+1
      if(i+j>nrow(squad_data)){
        break
      }
      if (j == max_j){
        non_primary <- 0
      }
    }
    if (squad_data[i,"arc"]=="pwt" | squad_data[i,"arc"]=="swt" | squad_data[i,"arc"]=="mobile" | squad_data[i,"arc"]=="double" ){ #if this iteration had a non-normal arc: remember that!
      non_primary <- non_primary + 1
      overall <- overall+1
    }
    #print(paste("i: ", i,"non_primary: ", non_primary, "matchID: ", squad_data[i,"matchID"], "j: ", j, "max j: ", max_j))
    if (!is.null(factionF)){ #go here if a faction was determined
      if (j == 1 && squad_data[i,"faction"]==factionF && non_primary > 0){ 
        # if j reached the maximal value, and if the faction is correct, and if the tier is in cut, and if you remember a non-normal arc
        # so only once per squad!
        sums <- c(sums,i)
      }
    }
    else if(is.null(factionF)){
      if (j == 1  & non_primary > 0){
        sums <- c(sums,i)
      }
    } #end else
    if(i+j>nrow(squad_data)){ #can't go higher than total number of rows
      break
    }
  }
  sums
}
getSquadsize <- function(fdata, factionF=NULL){
  sum <- c()
  i <- 1
  for (i in 1:nrow(fdata)){
    j <- 1
    size_squad <- 0
    max_j <- length(fdata[fdata[,"matchID"] == fdata[i,"matchID"],"matchID"])
    if (i+j > nrow(fdata)){
      break
    }
    while (fdata[i,"matchID"]==fdata[i+j,"matchID"]){
      j <- j+1
      if (i+j > nrow(fdata)){
        break
      }
    }#end while
    #print(paste("i: ", i, "matchID: ", fdata[i,"matchID"], "j: ", j, "max j: ", max_j)) #for testing
    if(!is.null(factionF)){
      if (j == 1 && fdata[i, "faction"]==factionF){
        size_squad <- max_j
        sum <- c(sum, size_squad)
      }
    }#end if isnull
    else{
      if (j == 1){
        size_squad <- max_j
        sum <- c(sum,size_squad)
      }
    }
  }
  sum
}
getDetails <- function(fdata, factionF = NULL){
  # write a function that assembles the info wrt wins/losses, squadsize, hp, ps and arcs into a single row
  sums <- data.frame() #final output
  i <- 1
  generics <- 0
  dat <- c()
  for (i in 1:nrow(fdata)){
    wins <- 0
    losses <- 0
    j <- 1
    max_j <- length(fdata[fdata[,"matchID"] == fdata[i,"matchID"],"matchID"])
    while (i==nrow(fdata) || fdata[i,"matchID"]==fdata[i+j,"matchID"]){ #set j. the first iteration will go to j == max_j, then lower and lower
      j <- j+1
      if (i+j >= nrow(fdata)){
        break
      }
      if (j == max_j){
        non_primary <- 0
      }
      
    }#end while
    if (i == nrow(fdata)){
      j <- 1
    }
    if (fdata[i, "pilottype"]=="generic"){
      generics <- generics + 1
    }
    if(!is.null(factionF)){
      if (j == 1 && fdata[i, "faction"]==factionF){
        size_squad <- max_j
        wins <- fdata[i,"wins"]
        losses <- fdata[i,"losses"]
        hp <- sum(fdata[fdata[,"matchID"]==fdata[i,"matchID"], "hp"])
        ps <- mean(fdata[fdata[,"matchID"]==fdata[i,"matchID"], "ps"])
        dat <- cbind(matchID=fdata[i,"matchID"], squadsize=size_squad, generics = generics, wins=wins, losses=losses, HP=hp, PS=ps)
        sums <- rbind(sums, dat)
      }#end j==1 if
    }#end if isnull
    else{
      if (j == 1){
        #print(paste("in else", i, j, "sums",sums[nrow(sums),5]))
        size_squad <- max_j
        wins <- fdata[i,"wins"]
        losses <- fdata[i,"losses"]
        hp <- sum(fdata[fdata[,"matchID"]==fdata[i,"matchID"], "hp"])
        ps <- mean(fdata[fdata[,"matchID"]==fdata[i,"matchID"], "ps"])
        dat <- cbind(matchID=fdata[i,"matchID"],squadsize=size_squad, generics = generics, wins=wins, losses=losses, HP=hp, PS=ps)
        sums <- rbind(sums, dat)
      } #end j==1 if
    }#end else
    #print(paste("after while", i, j))
  }
  sums
}
faction_details <- function(faction_data, ffaction=NULL){
  details <- getDetails(faction_data, ffaction)
  if(is.null(ffaction)){ffaction <- "all"}
  total_lists <- nrow(details)
  total_ships <- sum(details[,"squadsize"])
  faction_avg <- round(total_ships/nrow(details), digits = 2)
  faction_median <- median(details[,"squadsize"])
  faction_mode <- getmode(details[,"squadsize"])
  faction_win <- sum(details[,"wins"])
  faction_loss <- sum(details[,"losses"])
  total_games <- faction_win+faction_loss
  win_percentage <- round(100*faction_win/total_games, digits = 2)
  avg_hp <- mean(details[,"HP"])
  avg_ps <- mean(details[,"PS"])
  data.frame(ffaction, total_lists, total_ships, total_games, faction_avg, faction_median, faction_mode, faction_win, faction_loss, win_percentage, avg_hp, avg_ps) # , faction_win, faction_loss, win_percentage
}
getHarpoonDetails <- function(faction_data, ffaction=NULL, harpooncount){
  faction_win <- sum(faction_data[faction_data[,"faction"]==ffaction & faction_data[,"harpoon_count"]>harpooncount,"wins"])
  faction_loss <- sum(faction_data[faction_data[,"faction"]==ffaction & faction_data[,"harpoon_count"]>harpooncount,"losses"])
  total_games <- faction_win+faction_loss
  win_percentage <- 100*faction_win/total_games
  data.frame(ffaction, total_games, faction_win,faction_loss,win_percentage)
}
plot_multigraph_details <- function(subdata, poons, labeltext){
  all_games <- rbind(getHarpoonDetails(subdata, ffaction="galacticempire", harpooncount = poons),
                     getHarpoonDetails(subdata, ffaction="rebelalliance", harpooncount = poons),
                     getHarpoonDetails(subdata, ffaction="scumandvillainy", harpooncount = poons))
  plot <- ggplot(all_games, aes(x=ffaction, y=win_percentage, fill=ffaction)) +
    geom_bar(stat="identity", position = "dodge", col="black") +
    geom_text(aes(label=total_games, vjust=1), position=position_dodge(width=0.9)) +
    labs(x="Faction", y="win percentage [%]", title=labeltext) +
    scale_fill_manual(values = factioncolors)+
    coord_cartesian(ylim=c(0,100)) +
    theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(), axis.text.x = element_text(angle = 45, hjust = 0.6, vjust = 0.7))
  
  plot #change to plot
}
getPerSquad <- function(fdata, category, factionF=NULL, condition){
  tempdata <- data.frame()
  tempdata <- data.frame(matchID=rep(c(""), times=nrow(fdata)))
  tempdata[,"matchID"]<- fdata[,"matchID"]
  tempdata[,category] <- fdata[,category]
  #now with the correct dataframe
  total <- c()
  i <- 1
  sums <- c()
  output <- c()
  for (i in 1:nrow(tempdata)){
    j <- 1
    max_j <- length(tempdata[tempdata[,"matchID"]==tempdata[i,"matchID"],"matchID"]) # store the amount of rows with the same matchID
    
    while(tempdata[i,"matchID"]==tempdata[i+j,"matchID"]){ #while the next number is the same as the current iteraion: keep couning
      j <- j+1
      if(i+j>nrow(tempdata)){
        break
      }
      if (j == max_j){
        total <- 0
      }
    }
    if (condition[i]){ #if this iteraion fulfilled the condiion
      total <- total + 1
    }
    if (!is.null(factionF)){ #go here if a faction was determined
      if (j == 1 && tempdata[i,"faction"]==factionF && total > 0){ 
        # if j reached the maximal value, and if the faction is correct, and if the tier is in cut, and if you remember a non-normal arc
        # so only once per squad!
        sums <- c(sums,i)
        rows <- c(i, tempdata[i,"matchID"])
        output <- rbind(output, rows)
      }
    }
    else if(is.null(factionF)){
      if (j == 1  & total > 0){
        sums <- c(sums,i)
        rows <- c(i, tempdata[i,"matchID"])
        output <- rbind(output, rows)
      }
    } #end else
    if(i+j>nrow(tempdata)){
      break
    }
  }#end for
  colnames(output) <- c("row", "matchID")
  rownames(output) <- NULL
  output
}
tournamentAssembly <-  function(ffdata, ffrounds=6){ #requires dat_assembly, split_score
  fdata <- split_score(ffdata, frounds=ffrounds) #function!
  fdata <- dat_assembly(fdata) #function!
  fdata
}

#--------- DATABASE to identify generics, health, PS, arcs-----
d.database <- read.csv("./database2.csv", header = T, sep = ";")
d.database[,1] <- as.character(d.database[,1]) #faction
d.database[,2] <- as.character(d.database[,2]) #ship
d.database[,3] <- as.character(d.database[,3]) #pilot
d.database[,4] <- as.integer(d.database[,4]) #ps
d.database[,5] <- as.integer(d.database[,5]) #health
d.database[,6] <- as.character(d.database[,6]) #arc
d.database[,7] <- as.character(d.database[,7]) #pilottype
d.database[,8] <- as.integer(d.database[,8]) #attack
d.database[,9] <- as.integer(d.database[,9]) #agility


#--------- start data prep -----
lists_possible <- c()
lists_entered <- c()
#ignoring nordics, mynock, crossroads, spanish, but including czech, slovak, dutch
# #NordicsNats, 28.10.18
# d.nordics <- read.csv("./Nationals2018_2019/parsed/parsed-20181028_NordicsNats.csv", header = T, sep = ",")
# lists_possible <- c(lists_possible, nrow(d.nordics))
# d.nordics <- tournamentAssembly(d.nordics)
# lists_entered <- c(lists_entered, max(d.nordics$matchID))

#SlovakNats, 03.11.18
d.slovak <- read.csv("./Tournaments2018/parsed/parsed-20181103_SlovakNats.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.slovak))
d.slovak <- tournamentAssembly(d.slovak, ffrounds = 5)
lists_entered <- c(lists_entered, max(d.slovak$listID))

# #MynockOpen, 03.11.18
# d.mynock <- read.csv("./Tournaments2018/parsed/parsed-20181103_MynockOpen.csv", header = T, sep = ",")
# lists_possible <- c(lists_possible, nrow(d.mynock))
# d.mynock <- tournamentAssembly(d.mynock)
# lists_entered <- c(lists_entered, max(d.mynock$listID))

#NerfherderUK, 10.11.18
d.nerfherder <- read.csv("./Tournaments2018/parsed/parsed-20181110_NerfherderUK.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.nerfherder))
d.nerfherder <- tournamentAssembly(d.nerfherder)
lists_entered <- c(lists_entered, max(d.nerfherder$listID))

#CzechNats, 17.11.18
d.czech <- read.csv("./Tournaments2018/parsed/parsed-20181117_CzechNats.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.czech))
d.czech <- tournamentAssembly(d.czech, ffrounds = 5)
lists_entered <- c(lists_entered, max(d.czech$listID))

#DutchNats, 17.11.18
d.dutch <- read.csv("./Tournaments2018/parsed/parsed-20181117_DutchNats.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.dutch))
d.dutch <- tournamentAssembly(d.dutch)
lists_entered <- c(lists_entered, max(d.dutch$listID))

#WWsunday, 18.11.18
d.wwsunday <- read.csv("./Tournaments2018/parsed/parsed-20181118_WWsunday.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.wwsunday))
d.wwsunday <- tournamentAssembly(d.wwsunday, ffrounds = 5)
lists_entered <- c(lists_entered, max(d.wwsunday$listID))

#Walker Classic, 19.11.18
d.walker <- read.csv("./Tournaments2018/parsed/parsed-20181119_WalkerClassic.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.walker))
d.walker <- tournamentAssembly(d.walker, ffrounds = 5)
lists_entered <- c(lists_entered, max(d.walker$listID))

#AustralianNats, 24.11.18
d.australian <- read.csv("./Tournaments2018/parsed/parsed-20181124_AustralianNats.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.australian))
d.australian <- tournamentAssembly(d.australian)
lists_entered <- c(lists_entered, max(d.australian$listID))

#Renegade, 29.11.18
d.renegade <- read.csv("./Tournaments2018/parsed/parsed-20181129_RenegadeTourney.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.renegade))
d.renegade <- tournamentAssembly(d.renegade, ffrounds = 5)
lists_entered <- c(lists_entered, max(d.renegade$listID))

#PaxUnplugged, 03.12.18
d.pax <- read.csv("./Tournaments2018/parsed/parsed-20181203_PaxUnplugged.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.pax))
d.pax <- tournamentAssembly(d.pax)
lists_entered <- c(lists_entered, max(d.pax$listID))

# #Missouri Champs, 09.12.18 Team championship
# d.misschamp <- read.csv("./Tournaments2018/parsed/parsed-20181209_MissouriChamps.csv", header = T, sep = ",")
# lists_possible <- c(lists_possible, nrow(d.misschamp))
# d.misschamp <- tournamentAssembly(d.misschamp, ffrounds = 4)
# lists_entered <- c(lists_entered, max(d.misschamp$listID))

#--------- d.wide composition -----

#full only
d.widefull <- rbind(d.slovak, d.nerfherder, d.czech, d.dutch, d.wwsunday, d.walker, d.australian, d.renegade, d.pax) #d.mynock, d.misschamp
d.wide <- d.widefull


#--------- change same pilot names and ship names to rebelalliance"..." and scumandvillainy"..."-----

d.wide[,"ship"] <- as.character(d.wide[,"ship"])
#d.wide <- data_BU
d.wide[d.wide[,"faction"]=="rebelalliance"&d.wide[,"ship"]=="z95af4headhunter","ship"] <- "z95af4headhunterREBEL"
d.wide[d.wide[,"faction"]=="scumandvillainy"&d.wide[,"ship"]=="z95af4headhunter","ship"] <- "z95af4headhunterSCUM"
d.wide[d.wide[,"faction"]=="galacticrepublic"&d.wide[,"ship"]=="z95af4headhunter","ship"] <- "z95af4headhunterREPUBLIC"

d.wide[d.wide[,"faction"]=="rebelalliance"&d.wide[,"ship"]=="hwk290lightfreighter","ship"] <- "hwk290lightfreighterREBEL"
d.wide[d.wide[,"faction"]=="scumandvillainy"&d.wide[,"ship"]=="hwk290lightfreighter","ship"] <- "hwk290lightfreighterSCUM"

d.wide[d.wide[,"faction"]=="rebelalliance"&d.wide[,"ship"]=="btla4ywing","ship"] <- "btla4ywingREBEL"
d.wide[d.wide[,"faction"]=="scumandvillainy"&d.wide[,"ship"]=="btla4ywing","ship"] <- "btla4ywingSCUM"

d.wide[d.wide[,"faction"]=="rebelalliance"&d.wide[,"ship"]=="tiefighter","ship"] <- "tiefighterREBEL"
d.wide[d.wide[,"faction"]=="scumandvillainy"&d.wide[,"ship"]=="tiefighter","ship"] <- "tiefighterSCUM"

# d.wide[d.wide[,"faction"]=="rebelalliance"&d.wide[,"ship"]=="arc170starfighter","ship"] <- "arc170starfighterREBEL"
# d.wide[d.wide[,"faction"]=="galacticrepublic"&d.wide[,"ship"]=="arc170starfighter","ship"] <- "arc170starfighterREPUBLIC"
# 
# 
# 
# d.wide[d.wide[,"id"]=="sabinewren"&d.wide[,"ship"]=="attackshuttle","id"] <- "SabineShuttle"
# d.wide[d.wide[,"id"]=="sabinewren"&d.wide[,"ship"]=="Rebel TIE","id"] <- "SabineTIE"
# d.wide[d.wide[,"id"]=="sabinewren"&d.wide[,"ship"]=="lancerclasspursuitcraft","id"] <- "SabineLancer"
# 
# d.wide[d.wide[,"id"]=="ezrabridger"&d.wide[,"ship"]=="attackshuttle","id"] <- "EzraShuttle"
# d.wide[d.wide[,"id"]=="ezrabridger"&d.wide[,"ship"]=="sheathipedeclassshuttle","id"] <- "EzraBug"
# 
# d.wide[d.wide[,"id"]=="kyloren"&d.wide[,"ship"]=="upsilonclassshuttle","id"] <- "KyloShuttle"
# d.wide[d.wide[,"id"]=="kyloren"&d.wide[,"ship"]=="tiesilencer","id"] <- "KyloSilencer"

#--------- Assign PS, HP, arcs -----
# check HWK for moldycrow title, adjust attack and arc accordingly
# same for "punishing one" on jumpmaster
# adjust attack based on secondary weapons (cannons)
# adjust hull/shield based on upgrades: Titles, Modifications
# Idea: assign those stats from d.database to look at their averages per tier and faction
data_BU <- d.wide
widenames <- names(d.wide)
for (i in 1:ncol(d.wide)){
  d.wide[is.na(d.wide[,widenames[i]]), widenames[i]] <- ""
}
#!check the numbers, confirm
for (i in 1:nrow(d.wide)){
  for (j in 1:nrow(d.database)){
    if (d.wide[i,"id"]==d.database[j,3]){ #10 = ship; 2 = ship, 9 and 3 are pilot
      d.wide[i,"ps"] <- d.database[j,4]
      d.wide[i,"hp"] <- d.database[j,5]
      d.wide[i, "pilottype"] <- d.database[j,7]
      d.wide[i, "attack"] <- d.database[j,8]
      d.wide[i, "agility"] <- d.database[j,9]
      next #break the for loop (or if?)
    }

  }
}

for (i in 1:nrow(d.wide)){
  if (!is.na(d.wide[i,"modification1"]) && d.wide[i,"modification1"]=="hullupgrade"){
    d.wide[i,"hp"] <- d.wide[i,"hp"]+1
  }
  if (!is.na(d.wide[i,"modification1"]) && d.wide[i,"modification1"]=="shieldupgrade"){
    d.wide[i,"hp"] <- d.wide[i,"hp"]+1
  }
  if (!is.na(d.wide[i,"modification2"]) && d.wide[i,"modification2"]=="hullupgrade"){
    d.wide[i,"hp"] <- d.wide[i,"hp"]+1
  }
  if (!is.na(d.wide[i,"modification2"]) && d.wide[i,"modification2"]=="shieldupgrade"){
    d.wide[i,"hp"] <- d.wide[i,"hp"]+1
  }
  if (d.wide[i,"title1"]=="virago"){
    d.wide[i,"hp"] <- d.wide[i,"hp"]+1
  }
  if (d.wide[i,"title1"]=="punishingone"){
    d.wide[i,"attack"] <- 3
  }
  if (d.wide[i,"title1"]=="moldycrow"){
    d.wide[i,"attack"] <- 3
  }
}


d.wide[,"matchID"] <- ""
d.wide[,"uniqueID"] <- paste(d.wide[,"date"], d.wide[,"matchID"], d.wide[,"swiss"], d.wide[,"name"])
i <- 1
for (i in 1:length(unique(d.wide[,"uniqueID"]))){
  d.wide[d.wide[,"uniqueID"]==unique(d.wide[,"uniqueID"])[i],"matchID"] <- i
}

#--------- DATA PREP COMPLETE -----
rm(ls=d.slovak, d.mynock, d.nerfherder, d.czech, d.dutch, d.wwsunday, d.walker, d.australian, d.renegade, d.pax, d.misschamp, 
   d.widefull, i, j, widenames)
d.complete <- d.wide[,1:44]
colnames(d.complete) <- c("player","faction","wins","losses","swiss", "cut", "date","listID", "points","pilot","ship", #11
                          "talent1", "talent2", "force1", "sensor1", "tech1", "tech2", "cannon1", "turret1", "torpedo1", "torpedo2", "missile1", "missile2", #12
                          "crew1", "crew2", "crew3", "gunner1", "gunner2", "astromech1", "illicit1", "illicit2", "device1", "device2", "title1", "configuration1", #12
                          "modification1", "modificaion2", "modification3", 
                          "ps", "hp", "pilottype", "attack", "agility", "matchID") #44
d.complete[,"wins"] <- as.integer(d.complete[,"wins"])
d.complete[,"losses"] <- as.integer(d.complete[,"losses"])
d.complete[,"swiss"] <- as.integer(d.complete[,"swiss"])
d.complete[,"pilot"] <- as.character(d.complete[,"pilot"])
d.complete[,"listID"] <- as.integer(d.complete[,"listID"])
d.complete[,"matchID"] <- as.integer(d.complete[,"matchID"])
row.names(d.complete) <- NULL
#str(d.complete)

#--------- basic numbers ------
squad_number <- length(unique(d.complete$matchID))#725 lists
squad_number_cut <- length(unique(d.complete[d.complete[,"cut"]!="","matchID"])) #109 in the cut
#ratio cut to total:
squad_number_cut/squad_number #15.03

#--------- factiondetails ------
#function to display per faction: #ships, #players per faction, #games per faction (win+loss), #wins, #losses, #%win, %nonnormalarc
# also replaces getSquadsize
factiondetails <- rbind(faction_details(d.complete, ffaction = "galacticempire"),
                        faction_details(d.complete, ffaction = "rebelalliance"),
                        faction_details(d.complete, ffaction = "scumandvillainy"))

faction_plot <- ggplot(factiondetails, aes(x=ffaction, y=total_lists, fill=ffaction)) +
  geom_bar(stat="identity", position = "dodge", col="black") +
  geom_text(aes(label=total_games, vjust=1), position=position_dodge(width=0.9)) +
  labs(x="Faction", y="total lists", title="lists analyzed") +
  scale_fill_manual(values = factioncolors)+
  coord_cartesian(ylim=c(0,300)) +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(), axis.text.x = element_text(angle = 45, hjust = 0.6, vjust = 0.7))

faction_plot

ps_distr <- getDetails(d.complete)[,"PS"]
d_ps <- density(ps_distr)
plot(d_ps, main="PS", xlab = "average PS per squad")

hp_distr <- getDetails(d.complete)[,"HP"]
d_hp <- density(hp_distr)
plot(d_hp, main="HP", xlab = "summed healthpoints per squad")

#--------- cut only ------
d.cut <- d.complete[d.complete[, "cut"]!="",]
factiondetails_cut <- rbind(faction_details(d.cut, ffaction = "galacticempire"), 
                            faction_details(d.cut, ffaction = "rebelalliance"),
                            faction_details(d.cut, ffaction = "scumandvillainy"))

ps_cut_distr <- getDetails(d.cut)[,"PS"]
d_ps_cut <- density(ps_cut_distr)
plot(d_ps_cut, main="PS in cuts", xlab = "average PS per squad")

hp_cut_distr <- getDetails(d.cut)[,"HP"]
d_hp_cut <- density(hp_cut_distr)
plot(d_hp_cut, main="HP in cuts", xlab = "summed healthpoints per squad")

#--------- GOOD: squads with 1 or more ships of type X -------

unique(d.complete[,"ship"])
plotShipsPerSquad <- function(fdata, cutoff=25, factiondetailsdata, plotlabel=perc_faction, plottitle=NULL){
  topXships <- head(as.data.frame(sort(table(fdata$ship), decreasing = T)),cutoff)
  topXships[,1] <- as.character(topXships[,1]) #use as labels
  for(i in 1:nrow(topXships)){
    cond <- fdata[,"ship"]==topXships[i,1]
    squads <- nrow(getPerSquad(fdata, "ship", condition = cond))
    topXships[i,"squads"] <- squads
    topXships[i,"perc"] <- round((100*squads)/sum(factiondetailsdata[,2]),1)
  }
  for (i in 1:nrow(topXships)){ #assign faction, calculate percentage of faction
    for (j in 1:nrow(d.database)){
      if (topXships[i,"Var1"] == d.database[j,"ship"]){
        topXships[i,"faction"] <- d.database[j,"faction"]
        topXships[i, "perc_faction"] <- round((100*topXships[i,"squads"])/(factiondetailsdata[factiondetailsdata[,"ffaction"]==topXships[i,"faction"],2]),1) 
        break
      }
    }
  }
  topXships <- topXships[order(topXships$perc, decreasing = T),]
  topXships <- topXships[order(topXships$faction),]
  name_order <- as.character(topXships$Var1)
  topXships[,1] <- ordered(name_order, levels = name_order)
  rownames(topXships) <- NULL
  topXships <- topXships[,c(1,5,2,3,4,6)]
  p <- ggplot(topXships, aes(x=Var1, y=squads, fill = faction))+
    geom_bar(stat="identity", position = "dodge", col="black") +
    geom_text(aes(label=topXships[,plotlabel], vjust=1.2), position=position_dodge(width=0.9)) +
    labs(x="Ship", y="squads with at least one [#]", title=plottitle) +
    scale_fill_manual(values = factioncolors)+
    theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))#, hjust = 0.7, vjust = 0.7
  p
}
plotShipsPerSquad(d.complete, factiondetailsdata = factiondetails, plotlabel = "perc_faction", plottitle = "swiss, November - December 2018, % of faction")
plotShipsPerSquad(d.cut, factiondetailsdata = factiondetails_cut, plotlabel = "perc_faction", plottitle = "cut, November - December 2018, % of faction")


#control if in doubt:
#length(unique(d.complete[d.complete[,"ship"]=="t65xwing","matchID"]))

#--------- GOOD: Analysis of PS -----

table(d.complete$ps)
table(as.character(d.complete[d.complete[,"faction"]=="rebelalliance","ps"])) #the 705 rebel ships

ggplot(d.complete, aes(ps, fill=faction))+
  geom_histogram(binwidth = 0.5, col = "black")+
  scale_fill_manual(values = factioncolors) +
  coord_cartesian(xlim = c(1,6), ylim = c(0,800))+
  labs(title="Histogram for Pilot Skill", x="PS", y="count")
table(as.character(d.complete[d.complete[,"ps"]==6 & d.complete[,"faction"]=="rebelalliance","pilot"]))
table(as.character(d.complete[d.complete[,"ps"]==6 & d.complete[,"faction"]=="galacticempire","pilot"]))
table(as.character(d.complete[d.complete[,"ps"]==6 & d.complete[,"faction"]=="scumandvillainy","pilot"]))
sum(table(as.character(d.complete[d.complete[,"faction"]=="galacticempire","ps"])))

ggplot(d.cut, aes(ps, fill=faction))+
  geom_histogram(binwidth = 0.5, col = "black")+
  scale_fill_manual(values = factioncolors) +
  coord_cartesian(xlim = c(1,6), ylim = c(0,120))+
  labs(title="Histogram for Pilot Skill", x="PS", y="count")

table(d.complete[d.complete[,"ps"]>4, "points"])
unique(d.complete[d.complete[,"ps"]>4, "matchID"])
length(unique(d.complete[d.complete[,"ps"]>4, "matchID"]))
table(d.complete[d.complete[,"ps"]>4, "matchID"])
sum(table(d.complete[d.complete[,"ps"]>4, "matchID"])==1)
table(d.complete[d.complete[,"ps"]>4, "pilot"])
sort(table(d.complete[d.complete[,"ps"]>4, "pilot"]), decreasing = T)
d.highps <- d.complete[d.complete[,"ps"]>4, ]
d.highpspilots <- data.frame(sort(table(d.complete[d.complete[,"ps"]>4, "pilot"]), decreasing = T))
names(d.highpspilots) <- c("pilot", "amount")
ggplot(d.highpspilots, aes(pilot, amount)) + 
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  annotate(geom = "text", x = 20, y = 50, label = "Total lists: 525\nTotal ships: 1856\nTotal lists with ships I5 or I6: 434 (83%)\nTotal ships I5 or I6: 754 (40%)")

d.highpspilots2 <- data.frame(sort(table(d.complete[d.complete[,"ps"]>4 & d.complete[,"points"]!="", "pilot"]), decreasing = T))

names(d.highpspilots2) <- c("pilot", "amount")
ggplot(d.highpspilots2, aes(pilot, amount)) + 
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  annotate(geom = "text", x = 20, y = 50, label = "Total lists: 485\nTotal ships: 1709\nTotal lists with ships I5 or I6: 401 (83%)\nTotal ships I5 or I6: 698 (41%)")

length(d.complete[d.complete[,"points"]!="", "matchID"])
d.highps2 <- d.complete[d.complete[,"ps"]>4 & d.complete[,"points"]!="", ]
length(unique(d.highps2[d.highps2[,"ps"]>4, "matchID"])) #401
sum(table(d.highps2[d.highps2[,"ps"]>4, "matchID"])) #698
sum(table(d.highps2[d.highps2[,"ps"]>4, "points"])[1:13]) #252 #->now need to know for unique matchIDs
sum(table(d.highps2[!duplicated(d.highps2$matchID),"points"])[1:16])
sum(table(d.highps2[!duplicated(d.highps2$matchID),"points"])[1:7])/401 #below 190
sum(table(d.highps2[!duplicated(d.highps2$matchID),"points"])[1:13])/401

d.lowbids <- d.complete[d.complete[,"ps"]>4 & d.complete[,"points"]>198, ]
d.lowbids2 <- data.frame(sort(table(d.complete[d.complete[,"ps"]>4 & d.complete[,"points"]>198, "pilot"]), decreasing = T))
length(unique(d.lowbids[d.lowbids[,"ps"]>4, "matchID"])) #193
sum(table(d.lowbids[d.lowbids[,"ps"]>4, "matchID"])) #307
names(d.lowbids2) <- c("pilot", "amount")
ggplot(d.lowbids2, aes(pilot, amount)) + 
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  annotate(geom = "text", x = 18, y = 30, label = "Total lists in dataset: 485\nTotal ships in dataset: 1709\nTotal lists with ships I5 or I6, filtered for low bids: 193 (40% of entire dataset)\nTotal ships I5 or I6 and low bids: 397 (23% of entire dataset)")+
  annotate(geom = "text", x = 20, y = 50, label = "Filtered for lists with 199 or 200 points!")

d.highpscombined <- merge(d.highpspilots2, d.lowbids2, by = "pilot")
names(d.highpscombined) <- c("pilot", "high_bid", "low_bid")
d.highpscombined <- d.highpscombined[order(d.highpscombined$high_bid, decreasing = T),]

ggplot(d.highpscombined, aes(x=pilot)) + 
  geom_bar(aes(y = high_bid),stat="identity", position ="identity", fill = "lightblue")+
  geom_bar(aes(y = low_bid),stat="identity", position ="identity", fill = "pink")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #annotate(geom = "text", x = 18, y = 30, label = "Total lists in dataset: 485\nTotal ships in dataset: 1709\nTotal lists with ships I5 or I6, filtered for low bids: 193 (40% of entire dataset)\nTotal ships I5 or I6 and low bids: 397 (23% of entire dataset)")+
  annotate(geom = "text", x = 20, y = 50, label = "Pink: Filtered for lists with 196 to 200 points!")

#--------- Analysis of HP  ------
table(d.complete$hp)
ggplot(d.complete, aes(hp, fill=faction))+
  geom_histogram(binwidth = 0.5, col = "black")+
  scale_fill_manual(values = factioncolors) +
  coord_cartesian(xlim = c(3,17), ylim = c(0,700))+
  labs(title="Histogram for Hull+Shield", x="HP", y="count")

sum(table(d.complete[d.complete[,"hp"]>8,"ship"]))
length(unique(d.complete[d.complete[,"hp"]>8,"matchID"]))
d.complete[d.complete[,"hp"]>16,"pilot"]

#--------- unique vs generic-------
#Goal: determine the amount of squads with pure generics or pure unique ships.
table(d.complete$pilottype) #733 generics, 2249 uniques (75.4%)

getPilottype <- function(squad_data, factionF=NULL){
  sums <- c()
  i <- 1
  unique_type <- 0
  overall <- 0
  j <- 1
  for (i in 1:nrow(squad_data)){
    j <- 1
    max_j <- length(squad_data[squad_data[,"matchID"]==squad_data[i,"matchID"],"matchID"]) # store the amount of rows with the same matchID
    while(squad_data[i,"matchID"]==squad_data[i+j,"matchID"]){ #while the next number is the same as the current iteration: keep counting
      j <- j+1
      if(i+j>nrow(squad_data)){
        break
      }
      if (j == max_j){
        unique_type <- 0
      }
    } #end while
    if (squad_data[i,"pilottype"]!="generic" ){ #if this iteration had a non-normal arc: remember that!
      unique_type <- unique_type + 1
      overall <- overall+1
    }
    if (!is.null(factionF)){ #go here if a faction was determined
      if (j == 1 & squad_data[i,"faction"]==factionF & unique_type > 0){ 
        sums <- c(sums,i)
      }
    }
    else if(is.null(factionF)){
      if (j == 1 & unique_type > 0){
        sums <- c(sums,i)
      }
    } #end else
    if(i+j>nrow(squad_data)){ #can't go higher than total number of rows
      break
    }
  }
  sums
}
unique_pilots <- getPilottype(d.complete) #1236
ID_uniqeupilots <- d.complete[unique_pilots,"matchID"]
length(ID_uniqeupilots)/squad_number #97.24%


d.generics <- d.complete[!(d.complete[,"matchID"]%in%ID_uniqeupilots),]
length(unique(d.generics$matchID)) #20 squads used generics only!
table(d.generics$ship)

unique_pilots_imperial <- getPilottype(d.complete, factionF = "galacticempire") # 252/264
unique_pilots_rebel <- getPilottype(d.complete, factionF = "rebelalliance") # 197/203
unique_pilots_scum <- getPilottype(d.complete, factionF = "scumandvillainy") # 256/258

rm(ls=unique_pilots, unique_pilots_imperial, unique_pilots_rebel, unique_pilots_scum, ID_uniqeupilots)


#--------- CRAP: average squad size-------

ggplot(factiondetails, aes(factor(ffaction), faction_avg, fill=ffaction)) + 
  geom_bar(stat="identity", position = "dodge", col="black") + 
  geom_text(aes(label=faction_avg, vjust=-0.5), position=position_dodge(width=0.9)) +
  labs(x="tiers", y="average squadron size") +
  scale_fill_manual(values = factioncolors)+
  coord_cartesian(ylim=c(2,5)) + 
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(), axis.text.x = element_text(angle = 45, hjust = 0.6, vjust = 0.7))

ggplot(factiondetails, aes(factor(ffaction), total_lists, fill=ffaction)) + 
  scale_fill_manual(values = factioncolors)+
  geom_bar(stat="identity", position = "dodge", col="black") + 
  geom_text(aes(label=total_lists, vjust=-0.5), position=position_dodge(width=0.9)) +
  labs(x="tiers", y="number of squads") +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(), axis.text.x = element_text(angle = 45, hjust = 0.6, vjust = 0.7))

ggplot(factiondetails, aes(factor(ffaction), total_ships, fill=ffaction)) + 
  geom_bar(stat="identity", position = "dodge", col="black") + 
  geom_text(aes(label=total_ships, vjust=-0.5), position=position_dodge(width=0.9)) +
  scale_fill_manual(values = factioncolors) +
  labs(x="tiers", y="amount of used ships") +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(), axis.text.x = element_text(angle = 45, hjust = 0.6, vjust = 0.7))

ggplot(factiondetails, aes(factor(ffaction), win_percentage, fill=ffaction)) + 
  geom_bar(stat="identity", position = "dodge", col="black") + 
  geom_text(aes(label=win_percentage, vjust=-0.5), position=position_dodge(width=0.9)) +
  scale_fill_manual(values = factioncolors) +
  labs(x="tiers", y="win percentage [%]") +
  coord_cartesian(ylim=c(0,100)) + 
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(), axis.text.x = element_text(angle = 45, hjust = 0.6, vjust = 0.7))


#--------- individual ship/pilot analysis ------
d.wedge <- d.complete[d.complete[,"pilot"]=="wedgeantilles",] 
mean(d.wedge$wins) #2.78
getmode(d.wedge$wins) #3
median(d.wedge$wins) #3
table(d.wedge$wins)
table(d.wedge$torpedo1) #46 no torpedoes, 60 proton torps, 2 adv prot, 1 ion
mean(d.wedge[d.wedge[,"torpedo1"]=="protontorpedoes", "wins"]) #2.83
mean(d.wedge[d.wedge[,"torpedo1"]=="", "wins"])
hist(d.wedge$swiss)

d.jan <- d.complete[d.complete[,"pilot"]=="janors",] 
mean(d.jan$wins) #2.57
getmode(d.jan$wins) #2
median(d.jan$wins) #2.5
table(d.jan$wins)
table(d.jan$title1) #24 with, 4 without
mean(d.jan[d.jan[,"title1"]=="moldycrow", "wins"]) #2.75
mean(d.jan[d.jan[,"title1"]=="", "wins"]) #1.5
hist(d.jan$swiss)

janids <- d.complete[d.complete[,"pilot"]=="janors", "matchID"]
wedgeids <- d.complete[d.complete[,"pilot"]=="wedgeantilles", "matchID"]
sort(table(as.character(d.complete[d.complete[,"matchID"]%in% janwedge,"pilot"])))
janwedge <- janids[janids %in% wedgeids]
d.janwedge <- d.complete[d.complete[,"matchID"]%in% janwedge & d.complete[,"pilot"]=="wedgeantilles",]
mean(d.janwedge$wins) #2.87
getmode(d.janwedge$wins) #4
median(d.janwedge$wins) #3
table(d.janwedge$wins)
table(d.janwedge$torpedo1) #46 no torpedoes, 60 proton torps, 2 adv prot, 1 ion
mean(d.janwedge[d.janwedge[,"torpedo1"]=="protontorpedoes", "wins"]) #2.83
mean(d.janwedge[d.janwedge[,"torpedo1"]=="", "wins"])
hist(d.janwedge$swiss)

d.soontir <- d.complete[d.complete[,"pilot"]=="soontirfel",]
mean(d.soontir$wins) #3.28
getmode(d.soontir$wins) #4
median(d.soontir$wins) #3
table(d.soontir$wins)
table(d.soontir$talent1) #23/53 no talent, 14 pred, 6 juke
mean(d.soontir[d.soontir[,"talent1"]=="juke", "wins"]) #2.67
mean(d.soontir[d.soontir[,"talent1"]=="predator", "wins"]) #3.64
mean(d.soontir[d.soontir[,"talent1"]=="", "wins"]) #3

#--------- CRAP: Windistribution for pilots/ships -------
getWindistr <- function(fdata, pilot = NULL, ship = NULL){
  if (!is.null(pilot)){
    dataf <- fdata[fdata[,"pilot"]==pilot,]
    name <- pilot
  }
  if (!is.null(ship)){
    dataf <- fdata[fdata[,"ship"]==ship,]
    name <- ship
  }
  perc <- dataf$wins/(dataf$wins+dataf$losses)
  p <- ggplot(dataf, aes(x=perc)) +
    geom_histogram(bins = 0.15, binwidth = 0.01)+
    scale_x_continuous(breaks = seq(0,1,0.25), limits = c(-0.2,1.2))+
    scale_y_continuous(breaks = seq(0,35,5), limits = c(0,35))+
    labs(x=paste("amount of wins for ", name, sep=""), y="frequency")
  p
}
getWindistr(d.complete, pilot = "wedgeantilles")

#--------- Winrates per pilot -----
getPilotrates <- function(datasource, fpilot){
  wins <- c()
  losses <- c()
  i <- 1
  j <- 1
  pilot_count <- 0
  tempID <- 0
  for (i in 1:nrow(datasource)){
    j <- 1
    fdetail <- 0
    max_j <- length(datasource[datasource[,"matchID"]==datasource[i,"matchID"],"matchID"]) # store the amount of rows with the same matchID
    while(datasource[i,"matchID"]==datasource[i+j,"matchID"]){ #while the next number is the same as the current iteration: keep counting
      j <- j+1
      if(i+j>nrow(datasource)){
        break
      }
      if (j == max_j){
        pilot_count <- 0
      }
    } #end while
    
    if (datasource[i,"pilot"]==fpilot & tempID != datasource[i, "matchID"]){ #if this iteration had the correct pilot: remember that!
      tempID <- datasource[i,"matchID"]
      pilot_count <- pilot_count + 1
      fdetail <- 1
      wins <- c(wins,datasource[i,"wins"])
      losses <- c(losses,datasource[i,"losses"])
    }
    if(i+j>nrow(datasource)){ #can't go higher than total number of rows
      break
    }
  }
  out <- cbind(sum(wins), sum(losses))
  out
}


# testrate <- getPilotrates(d.complete, "wedgeantilles")
# 
# length(unique(d.complete[d.complete[,"pilot"]=="wedgeantilles","matchID"]))

get_pilotdetails <- function(fdata, fthreshold=10){
  fpilot <- unique(as.character(fdata[,"pilot"])) # make list with all pilot names
  output <- data.frame()
  for (k in 1:length(fpilot)){ # iterate through list with pilot names
    if (length(unique(fdata[fdata[,"pilot"]==fpilot[k],"matchID"]))<fthreshold){ #col 5 is matchID - count how many squads used pilot k
      next
    }
    pilot <- fpilot[k] #add pilot name
    rates <- getPilotrates(datasource = fdata, fpilot = pilot) #add wins and losses
    result <- cbind(pilot, rates) #bind together in same row
    output <- rbind(output, result) #bind together by row
  } #end for
  output
}

winpilots <- get_pilotdetails(d.complete, fthreshold = 30)
winpilots[,"percentage"] <- round(100*as.numeric(as.character(winpilots[,2]))/(as.numeric(as.character(winpilots[,2]))+as.numeric(as.character(winpilots[,3]))), digits=2)
winpilots[,"total_games"] <- as.numeric(as.character(winpilots[,2]))+as.numeric(as.character(winpilots[,3]))
#winpilots[,"faction"] <- d.complete[d.complete[,"pilot"]==winpilots[,"pilot"],"faction"]
ggplot(winpilots, aes(x=total_games, y=percentage)) +
  geom_point() +
  coord_flip() +
  geom_text(aes(label=pilot,hjust=0.5, vjust=-0.5), size = 4.5)
  #geom_smooth(method = "lm", formula = y~x)

lm_winperc <- lm(total_games~percentage, winpilots)
summary.lm(lm_winperc)
sum(d.complete$wins)/(sum(d.complete$wins)+sum(d.complete$losses))
