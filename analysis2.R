# dynamic xwing analysis

rm(list = ls())
setwd("//pasteur/SysBC-Home/riselin/Documents/polybox/privat/xwing/Turniere_2.0")

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

#Constants -----
factioncolors <- c("darkgreen", "red2", "goldenrod1", "sienna2", "springgreen3")

#Functions -----
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

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
split_score <- function(fdata, frounds=6){
  int_data <- fdata
  d.win <- int_data$score
  rounds <- frounds
  d.loss <- rounds-d.win
  int_data$losses <- d.loss
  int_data
} #necessary for tournament assembly; uses 6 rounds as default
dat_assembly <- function(fdata){
  fdata <- fdata[moveme(names(fdata), "playerName first; faction after playerName; score after faction; losses after score;  swissRank after losses; 
                        cutRank after swissRank; date after cutRank; listID after date; points after listID")]
  int_data <- fdata[,c(1:9, 13:(ncol(fdata)))]
  int_data <- rename(int_data, c("playerName" = "name", "score" = "wins", "swissRank" = "swiss", "cutRank" = "cut"))
  int_data
} #requires moveme
tournamentAssembly <-  function(ffdata, ffrounds=6){ #requires dat_assembly, split_score
  fdata <- split_score(ffdata, frounds=ffrounds) #function!
  fdata <- dat_assembly(fdata) #function!
  fdata
}

getDetails <- function(fdata, factionF = NULL){
  # input: all cols of data set
  # output: matchID, squadsize, generics, wins, losses, hp, ps
  sums <- data.frame() #final output
  i <- 1
  generics <- 0
  dat <- c()
  for (i in 1:nrow(fdata)){
    wins <- 0
    losses <- 0
    j <- 1
    max_j <- length(fdata[fdata[,"matchID"] == fdata[i,"matchID"],"matchID"]) #how many entries are there for a given matchID
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
    if(!is.null(factionF)){ #if limited to a faction
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
} #necessary for faction_details
faction_details <- function(faction_data, ffaction=NULL){
  details <- getDetails(faction_data, ffaction)
  if(is.null(ffaction)){ffaction <- "all"}
  total_lists <- nrow(details)
  total_lists_percentage <- round(total_lists/length(unique(faction_data[,"matchID"])),digits = 2)
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
  data.frame(ffaction, total_lists, total_lists_percentage, total_ships, total_games, faction_avg, faction_median, faction_mode, faction_win, faction_loss, win_percentage, avg_hp, avg_ps) # , faction_win, faction_loss, win_percentage
} #requires getDetails

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
}#used for unique vs generics

getPerSquad <- function(fdata, category, factionF=NULL, condition){
  #input: data all colums, certain category (=col), condition (?)
  #output: row, matchID
  tempdata <- data.frame()
  tempdata <- data.frame(matchID = rep(c(""), times = nrow(fdata)))
  tempdata[, "matchID"]<- fdata[, "matchID"]
  tempdata[, category] <- fdata[, category]
  total <- c()
  i <- 1
  output <- c()
  for (i in 1:nrow(tempdata)){
    j <- 1
    max_j <- length(tempdata[tempdata[,"matchID"]==tempdata[i,"matchID"],"matchID"]) # store the amount of rows with the same matchID
    while(tempdata[i,"matchID"]==tempdata[i+j,"matchID"]){ #while the next number is the same as the current iteration: keep counting
      j <- j+1
      if(i+j>nrow(tempdata)){
        break
      }
      if (j == max_j){
        total <- 0 #at each new start
      }
    }#end while
    if (condition[i]){ #if this iteration fulfilled the condition
      total <- total + 1
    }#end condition-if
    if (!is.null(factionF)){ #go here if a faction was determined
      if (j == 1 && tempdata[i,"faction"]==factionF && total > 0){ 
        # if j reached the maximal value, and if the faction is correct, and if the tier is in cut, and if you remember a non-normal arc
        # so only once per squad!
        rows <- c(i, tempdata[i,"matchID"], total)
        output <- rbind(output, rows)
      }
    }#end faction-if
    else if(is.null(factionF)){
      if (j == 1  & total > 0){
        rows <- c(i, tempdata[i,"matchID"], total)
        output <- rbind(output, rows)
      }
    } #end else
    if(i+j>nrow(tempdata)){
      break
    }
  }#end for
  colnames(output) <- c("row", "matchID", "total")
  rownames(output) <- NULL
  output
} #used on plotColPerSquad
plotColPerSquad <- function(fdata, selectColumn=ship, cutoff=21, factiondetailsdata, plotlabel=perc_faction, plottitle=NULL){
  #input: dataframe, cutoff = top "X" ships of the meta, factiondetails = either cut or swiss factiondetails, 
  # plotlabel = which numbers shown on bar, plottitle = title of graph
  #output: barplot, sorted by faction showing how many squads had at least one selectColumn of type X plus percentage
  topXcond <- head(as.data.frame(sort(table(fdata[,selectColumn]), decreasing = T)),cutoff) #get "cutoff" most frequent ships
  topXcond[,1] <- as.character(topXcond[,1]) #use as labels for x-axis
  for(i in 1:nrow(topXcond)){
    cond <- fdata[,selectColumn]==topXcond[i,1] #true or false
    squads <- nrow(getPerSquad(fdata, selectColumn, condition = cond)) #if true: this selectColumn will be counted
    topXcond[i,"squads"] <- squads
    topXcond[i,"perc"] <- round((100*squads)/sum(factiondetailsdata[,2]),1)
  }
  for (i in 1:nrow(topXcond)){ #assign faction, calculate percentage of faction
    for (j in 1:nrow(d.database)){
      if (topXcond[i,"Var1"] == d.database[j,selectColumn]){
        topXcond[i,"faction"] <- d.database[j,"faction"]
        topXcond[i, "perc_faction"] <- round((100*topXcond[i,"squads"])/(factiondetailsdata[factiondetailsdata[,"ffaction"]==topXcond[i,"faction"],2]),1) 
        break
      }
    }
  }
  topXcond <- topXcond[order(topXcond$perc, decreasing = T),]
  topXcond <- topXcond[order(topXcond$faction),]
  name_order <- as.character(topXcond$Var1)
  topXcond[,1] <- ordered(name_order, levels = name_order)
  rownames(topXcond) <- NULL
  topXcond <- topXcond[,c(1,5,2,3,4,6)]
  p <- ggplot(topXcond, aes(x=Var1, y=squads, fill = faction))+
    geom_bar(stat="identity", position = "dodge", col="black") +
    geom_text(aes(label=topXcond[,plotlabel], vjust=1.2), position=position_dodge(width=0.9)) +
    labs(x=selectColumn, y="squads with at least one [#]", title=plottitle) +
    scale_fill_manual(values = c("springgreen3", "darkgreen", "red2", "sienna2", "goldenrod1"))+
    theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))#, hjust = 0.7, vjust = 0.7
  p
}# squads with 1 or more ships of type x per squad

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
  out <- cbind(wins=sum(wins), losses=sum(losses))
  out
}
getPilotdetails <- function(fdata, fthreshold=10){
  fpilot <- unique(as.character(fdata[,"pilot"])) # make list with all pilot names
  output <- data.frame()
  for (k in 1:length(fpilot)){ # iterate through list with pilot names
    if (length(unique(fdata[fdata[,"pilot"]==fpilot[k],"matchID"]))<fthreshold){ #col 5 is matchID - count how many squads used pilot k
      next
    }
    pilot <- fpilot[k] #add pilot name
    faction <- d.database[d.database[,"pilot"]==fpilot[k], "faction"]
    rates <- getPilotrates(datasource = fdata, fpilot = pilot) #add wins and losses
    result <- cbind(pilot, faction, rates) #bind together in same row
    output <- rbind(output, result) #bind together by row
  } #end for
  output
}

getArchetype <- function(fdata, category, factionF=NULL, condition){
  #input: data all colums, certain category (=col), condition (?)
  #output: row, matchID
  tempdata <- data.frame()
  tempdata <- data.frame(matchID = rep(c(""), times = nrow(fdata)))
  tempdata[, "matchID"]<- fdata[, "matchID"]
  tempdata[, category] <- fdata[, category]
  total <- c()
  tempship <- c()
  i <- 1
  output <- c()
  for (i in 1:nrow(tempdata)){
    j <- 1
    max_j <- length(tempdata[tempdata[,"matchID"]==tempdata[i,"matchID"],"matchID"]) # store the amount of rows with the same matchID
    while(tempdata[i,"matchID"]==tempdata[i+j,"matchID"]){ #while the next number is the same as the current iteration: keep counting
      j <- j+1
      if(i+j>nrow(tempdata)){
        break
      }
      if (j == max_j){
        total <- 0 #at each new start
      }
    }#end while
    if (condition[i]){ #if this iteration fulfilled the condition
      total <- total + 1
    }#end condition-if
    
    if (!is.null(factionF)){ #go here if a faction was determined
      if (j == 1 && tempdata[i,"faction"]==factionF && total > 0){ 
        # if j reached the maximal value, and if the faction is correct, and if the tier is in cut, and if you remember a non-normal arc
        # so only once per squad!
        tempship <- paste(tempdata[tempdata[,"matchID"]==tempdata[i,"matchID"],"ship"], collapse = " ")
        rows <- c(i, tempdata[i,"matchID"], total, tempship)
        output <- rbind(output, rows)
        tempship <- c()
      }
    }#end faction-if
    else if(is.null(factionF)){
      if (j == 1  & total > 0){
        tempship <- paste(tempdata[tempdata[,"matchID"]==tempdata[i,"matchID"],"ship"], collapse = " ")
        rows <- c(i, tempdata[i,"matchID"], total, tempship)
        output <- rbind(output, rows)
        tempship <- c()
      }
    } #end else
    if(i+j>nrow(tempdata)){
      break
    }
  }#end for
  colnames(output) <- c("row", "matchID", "total", "archetype")
  rownames(output) <- NULL
  output
} #used on plotColPerSquad

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
d.database[,10] <- as.integer(d.database[,10]) #cost

#--------- start data prep -----
lists_possible <- c()
lists_entered <- c()

# #Toronto Hyperspace Qualifier, 24.2.19
# d.toronto <- read.csv("./wave2_part2/parsed/parsed-20190224TorontoHyperspaceQualifierCA.csv", header = T, sep = ",")
# lists_possible <- c(lists_possible, nrow(d.toronto))
# d.toronto <- tournamentAssembly(d.toronto, ffrounds = 6)
# lists_entered <- c(lists_entered, max(d.toronto$listID))

#Blacksun Trials AU, 2.3.19
d.blacksun <- read.csv("./wave2_part2/parsed/parsed-20190302blacksuntrialsAU.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.blacksun))
d.blacksun <- tournamentAssembly(d.blacksun, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.blacksun$listID))

#Redmond HS Trials US, 2.3.19
d.redmond <- read.csv("./wave2_part2/parsed/parsed-20190302RedmondHyperspaceTrialUS.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.redmond))
d.redmond <- tournamentAssembly(d.redmond, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.redmond$listID))

#Malmo HS Trials SE, 2.3.19
d.malmo <- read.csv("./wave2_part2/parsed/parsed-20190302MalmoHyperspaceTrialSE.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.malmo))
d.malmo <- tournamentAssembly(d.malmo, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.malmo$listID))

#Fantasy HS Trials DE, 9.3.19
d.fantasy <- read.csv("./wave2_part2/parsed/parsed-20190309DeutschlandTrial1DE.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.fantasy))
d.fantasy <- tournamentAssembly(d.fantasy, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.fantasy$listID))

#Bromley HS Trials UK, 9.3.19
d.bromley <- read.csv("./wave2_part2/parsed/parsed-20190309BromleyHyperspaceTrialUK.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.bromley))
d.bromley <- tournamentAssembly(d.bromley, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.bromley$listID))

#Ohio HS Trials US, 10.3.19
d.ohio <- read.csv("./wave2_part2/parsed/parsed-20190310OhioHyperspaceTrialUS.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.ohio))
d.ohio <- tournamentAssembly(d.ohio, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.ohio$listID))

#Bathurst HS Trials AU, 16.3.19
d.bathurst <- read.csv("./wave2_part2/parsed/parsed-20190316BathurstHyperspaceTrialAU.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.bathurst))
d.bathurst <- tournamentAssembly(d.bathurst, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.bathurst$listID))

#Element HS Trials Stockport UK, 16.3.19
d.elementMar <- read.csv("./wave2_part2/parsed/parsed-20190316ElementHyperspaceTrialUK.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.elementMar))
d.elementMar <- tournamentAssembly(d.elementMar, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.elementMar$listID))

#Kentucky HS Trials US, 16.3.19
d.kentucky <- read.csv("./wave2_part2/parsed/parsed-20190316KentuckyHyperspaceTrialUS.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.kentucky))
d.kentucky <- tournamentAssembly(d.kentucky, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.kentucky$listID))

#Minnesota HS Trials US, 16.3.19
d.minnesota <- read.csv("./wave2_part2/parsed/parsed-20190316MinnesotaHyperspaceTrialUS.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.minnesota))
d.minnesota <- tournamentAssembly(d.minnesota, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.minnesota$listID))

#Austin HS Trials US, 16.3.19
d.austin <- read.csv("./wave2_part2/parsed/parsed-20190316AustinHyperspaceTrialUS.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.austin))
d.austin <- tournamentAssembly(d.austin, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.austin$listID))

#Quebec HS Trials CA, 16.3.19
d.quebec <- read.csv("./wave2_part2/parsed/parsed-20190316QuebecHyperspaceTrialCA.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.quebec))
d.quebec <- tournamentAssembly(d.quebec, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.quebec$listID))

#Arizona HS Trials US, 17.3.19
d.arizona <- read.csv("./wave2_part2/parsed/parsed-20190317ArizonaHyperspaceTrialUS.csv", header = T, sep = ",")
lists_possible <- c(lists_possible, nrow(d.arizona))
d.arizona <- tournamentAssembly(d.arizona, ffrounds = 6)
lists_entered <- c(lists_entered, max(d.arizona$listID))

#--------- d.wide composition -----
d.wide <- rbind(d.blacksun, d.redmond, d.fantasy, d.ohio, d.bromley, d.malmo,
                            d.bathurst, d.elementMar, d.kentucky, d.minnesota, d.arizona, d.quebec, d.austin)

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

d.wide[d.wide[,"faction"]=="rebelalliance"&d.wide[,"ship"]=="tiefighter","ship"] <- "tielnfighterREBEL"

d.wide[d.wide[,"ship"]=="upsilonclassshuttle","ship"] <- "upsilonclasscommandshuttle"


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

#change attack to 3 for sf if with sf gunner

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
rm(ls = d.blacksun, d.redmond, d.fantasy, d.ohio, d.bromley, d.malmo,
   d.bathurst, d.elementMar, d.kentucky, d.minnesota, d.arizona, d.quebec, d.austin)
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



squad_number <- length(unique(d.complete$matchID))#624 lists
squad_number_cut <- length(unique(d.complete[d.complete[,"cut"]!="","matchID"])) #110 in the cut
#ratio cut to total:
squad_number_cut/squad_number #17.6%

#--------- factiondetails swiss ------
factiondetails_swiss <- rbind(faction_details(d.complete, ffaction = "galacticempire"),
                        faction_details(d.complete, ffaction = "rebelalliance"),
                        faction_details(d.complete, ffaction = "scumandvillainy"),
                        faction_details(d.complete, ffaction = "resistance"),
                        faction_details(d.complete, ffaction = "firstorder"))

faction_plot_swiss <- ggplot(factiondetails_swiss, aes(x=ffaction, y=total_lists, fill=ffaction)) +
  geom_bar(stat="identity", position = "dodge", col="black") +
  geom_text(aes(label=total_ships, vjust=1), position=position_dodge(width=0.9)) +
  labs(x="Faction", y="total lists", title="lists analyzed") +
  scale_fill_manual(values = factioncolors)+
  coord_cartesian(ylim=c(0,200)) +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(), axis.text.x = element_text(angle = 45, hjust = 0.6, vjust = 0.7))

#faction_plot_swiss

#--------- factiondetails cut ------
d.cut <- d.complete[d.complete[, "cut"]!="",]
factiondetails_cut <- rbind(faction_details(d.cut, ffaction = "galacticempire"), 
                            faction_details(d.cut, ffaction = "rebelalliance"),
                            faction_details(d.cut, ffaction = "scumandvillainy"),
                            faction_details(d.cut, ffaction = "resistance"),
                            faction_details(d.cut, ffaction = "firstorder"))

faction_plot_cut <- ggplot(factiondetails_cut, aes(x=ffaction, y=total_lists, fill=ffaction)) +
  geom_bar(stat="identity", position = "dodge", col="black") +
  geom_text(aes(label=total_ships, vjust=1), position=position_dodge(width=0.9)) +
  labs(x="Faction", y="total lists", title="lists analyzed, cut") +
  scale_fill_manual(values = factioncolors)+
  coord_cartesian(ylim=c(0,50)) +
  theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(), axis.text.x = element_text(angle = 45, hjust = 0.6, vjust = 0.7))

#faction_plot_cut
#--------- find squads with 1 or more X -------
unique(d.complete[,"ship"])

perSquad_swiss_ships <- plotColPerSquad(d.complete, selectColumn = "ship", cutoff = 21, factiondetailsdata = factiondetails_swiss, plotlabel = "perc_faction", plottitle = "swiss, Ships, Feb-MidMarch 2019, % of faction")
perSquad_cut_ships <- plotColPerSquad(d.cut, selectColumn = "ship", cutoff = 21, factiondetailsdata = factiondetails_cut, plotlabel = "perc_faction", plottitle = "cut, Ships, Feb-MidMarch 2019, % of faction")

perSquad_swiss_pilots <- plotColPerSquad(d.complete, selectColumn = "pilot", cutoff = 30, factiondetailsdata = factiondetails_swiss, plotlabel = "perc_faction", plottitle = "swiss, Pilots, Feb-MidMarch 2019, % of faction")
perSquad_cut_pilots <- plotColPerSquad(d.cut, selectColumn = "pilot", cutoff = 30, factiondetailsdata = factiondetails_cut, plotlabel = "perc_faction", plottitle = "cut, Pilots, Feb-MidMarch 2019, % of faction")

#--------- unique vs generic-------
#Goal: determine the amount of squads with pure generics or pure unique ships.
table(d.complete$pilottype)[1]/nrow(d.complete) #27% generics
table(d.complete$pilottype)[2]/nrow(d.complete) #73% uniques
d.genUnique <- as.data.frame(table(d.complete$pilottype))
colnames(d.genUnique) <- c("type", "count")
pie(d.genUnique$count, labels = d.genUnique$type, main = "Generic vs Unique, Swiss")

unique_pilots <- getPilottype(d.complete) #18.3.19: 808/879 squads with uniques; only trials: 578/624
ID_uniqeupilots <- d.complete[unique_pilots,"matchID"]
length(ID_uniqeupilots)/squad_number #92% of squads had 1 or more uniques


d.generics <- d.complete[!(d.complete[,"matchID"]%in%ID_uniqeupilots),]
length(unique(d.generics$matchID)) #48 squads used generics only!
sort(table(d.generics$ship), decreasing = T) #18.3.19: 113 y, 27t65, 22 sf, 17 rz2, 9 firesprays, 13 strikers



unique_pilots_imperial <- getPilottype(d.complete, factionF = "galacticempire") #18.3.19: 140/143 97.9%; 96/99 squads
unique_pilots_rebel <- getPilottype(d.complete, factionF = "rebelalliance") #18.3.19: 247/290 85.2%; 165/195 
unique_pilots_scum <- getPilottype(d.complete, factionF = "scumandvillainy") #18.3.19: 111/119 93.3%; 66/70
unique_pilots_resistance <- getPilottype(d.complete, factionF = "resistance") #18.3.19: 207/215 96.3%; 152/157
unique_pilots_fo <- getPilottype(d.complete, factionF = "firstorder") #18.3.19: 103/112 92%; 72/78

rm(ls=unique_pilots, unique_pilots_imperial, unique_pilots_rebel, unique_pilots_scum, ID_uniqeupilots, unique_pilots_resistance,unique_pilots_fo)

#--------- Archetype Analysis------
#!!! STILL FAULTY, the last entry is not included with getArchetype!
#Idea: group lists by their ship type

getArchetypeList <- function(fdata){
  d.temp <- fdata[,c(1:11,44)]
  d.temp <- d.temp[order(d.temp$pilot),]
  d.temp <- d.temp[order(d.temp$ship),]
  d.temp <- d.temp[order(d.temp$matchID),]
  d.archetype <- d.temp[,c("ship", "matchID")]
  tempcond <- d.archetype[,"ship"]!=""
  d.archetype <- getArchetype(d.archetype, "ship", factionF = NULL, condition = tempcond)
  overview_archetype <- as.data.frame(table(d.archetype[,"archetype"]))
  conc_archetype <- data.frame()
  for (i in 1:nrow(overview_archetype)){
    pastename <- c()
    for (j in 1:length(table(str_split(names(table(d.archetype[,"archetype"])[i]), pattern=" ")))){
      tempname <- paste(table(str_split(names(table(d.archetype[,"archetype"])[i]), pattern=" "))[[j]],
                        names(table(str_split(names(table(d.archetype[,"archetype"])[i]), pattern=" ")))[[j]], sep="x")
      pastename <- paste(pastename, tempname, sep=" ")
    }
    conc_archetype[i,"archetype"] <- c(pastename)
  }
  overview_archetype[,1] <- conc_archetype
  overview_archetype <- overview_archetype[order(overview_archetype$Freq, decreasing = T),]
  colnames(overview_archetype) <- c("archetype", "freq")
  rownames(overview_archetype) <- NULL
  overview_archetype
}

archetypeSwiss <- getArchetypeList(d.complete)
archetypeCut <- getArchetypeList(d.cut)

archetypeSwiss
archetypeCut

?merge
mergeArchetype <- merge(archetypeSwiss, archetypeCut, by = "archetype")
mergeArchetype[,4] <- round(mergeArchetype[,3]/mergeArchetype[,2], digits = 2)
mergeArchetype <- mergeArchetype[order(mergeArchetype$freq.y, decreasing = T),]
colnames(mergeArchetype) <- c("archetype", "freq swiss", "freq cut", "conversion")
rownames(mergeArchetype) <- NULL
#--------- Win/Loss analysis-----

winpilots_swiss <- getPilotdetails(d.complete, fthreshold = 10)
winpilots_swiss[,"percentage"] <- round(100*as.numeric(as.character(winpilots_swiss[,"wins"]))/(as.numeric(as.character(winpilots_swiss[,"wins"]))+as.numeric(as.character(winpilots_swiss[,"losses"]))), digits=2)
winpilots_swiss[,"total_games"] <- as.numeric(as.character(winpilots_swiss[,"wins"]))+as.numeric(as.character(winpilots_swiss[,"losses"]))
winrates_swiss <- ggplot(winpilots_swiss, aes(x=total_games, y=percentage, fill=faction)) +
  geom_point() +
  coord_flip() +
  geom_text(aes(label=pilot,hjust=0.5, vjust=-0.5, color = faction), size = 3.5)+
  scale_color_manual(values = c("sienna2", "goldenrod1","red2","darkgreen","springgreen3"))

winpilots_cut <- getPilotdetails(d.cut, fthreshold = 5)
winpilots_cut[,"percentage"] <- round(100*as.numeric(as.character(winpilots_cut[,"wins"]))/(as.numeric(as.character(winpilots_cut[,"wins"]))+as.numeric(as.character(winpilots_cut[,"losses"]))), digits=2)
winpilots_cut[,"total_games"] <- as.numeric(as.character(winpilots_cut[,"wins"]))+as.numeric(as.character(winpilots_cut[,"losses"]))
winrates_cut <- ggplot(winpilots_cut, aes(x=total_games, y=percentage)) +
  geom_point() +
  coord_flip() +
  geom_text(aes(label=pilot,hjust=0.5, vjust=-0.5, color = faction), size = 3.5)+
  scale_color_manual(values = c("sienna2","red2","darkgreen","springgreen3"))


#--------- Analysis of PS -----
table(d.complete$ps)
table(as.character(d.complete[d.complete[,"faction"]=="rebelalliance","ps"]))#the 727 rebel ships
table(as.character(d.complete[d.complete[,"faction"]=="galacticempire","ps"]))#the 478 imperial ships
table(as.character(d.complete[d.complete[,"faction"]=="resistance","ps"]))#the 577 resistance ships. 62% (361) are high ps. And 114/288 i5 are Lulo (40%)

hist_ps_swiss <- ggplot(d.complete, aes(ps, fill=faction))+
  geom_histogram(binwidth = 0.5, col = "black")+
  scale_fill_manual(values = c("springgreen3", "darkgreen", "red2", "sienna2", "goldenrod1")) +
  coord_cartesian(xlim = c(1,7), ylim = c(0,650))+
  labs(title="Histogram for Pilot Skill", x="PS", y="count")
hist_ps_cut <- ggplot(d.cut, aes(ps, fill=faction))+
  geom_histogram(binwidth = 0.5, col = "black")+
  scale_fill_manual(values = c("springgreen3", "darkgreen", "red2", "sienna2", "goldenrod1")) +
  coord_cartesian(xlim = c(1,7), ylim = c(0,150))+
  labs(title="Histogram for Pilot Skill, cut", x="PS", y="count")

table(as.character(d.complete[d.complete[,"ps"]>4 & d.complete[,"faction"]=="rebelalliance","pilot"]))
table(as.character(d.complete[d.complete[,"ps"]>4 & d.complete[,"faction"]=="galacticempire","pilot"]))
table(as.character(d.complete[d.complete[,"ps"]==6 & d.complete[,"faction"]=="scumandvillainy","pilot"]))
table(as.character(d.complete[d.complete[,"ps"]>4 & d.complete[,"faction"]=="resistance","pilot"]))

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
  annotate(geom = "text", x = 20, y = 50, label = "Total lists: 599\nTotal ships: 2297\nTotal lists with ships I5 or I6: 511 (85%)\nTotal ships I5 or I6: 994 (43%)")

d.highpspilots2 <- data.frame(sort(table(d.complete[d.complete[,"ps"]>4 & d.complete[,"points"]!="", "pilot"]), decreasing = T))

names(d.highpspilots2) <- c("pilot", "amount")
ggplot(d.highpspilots2, aes(pilot, amount)) + 
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  annotate(geom = "text", x = 20, y = 50, label = "Total lists: 586\nTotal ships: 2257\nTotal lists with ships I5 or I6: 498 (85%)\nTotal ships I5 or I6: 967 (43%)")

length(unique(d.complete[d.complete[,"points"]!="", "matchID"]))
length(d.complete[d.complete[,"points"]!="", "matchID"])
length(unique(d.complete[d.complete[,"ps"]>4&d.complete[,"points"]!="", "matchID"]))
d.highps2 <- d.complete[d.complete[,"ps"]>4 & d.complete[,"points"]!="", ]
length(unique(d.highps2[d.highps2[,"ps"]>4, "matchID"])) #498 lists with full points
sum(table(d.highps2[d.highps2[,"ps"]>4, "matchID"])) #967 ships above i4
sum(table(d.highps2[d.highps2[,"ps"]>4, "points"])[1:13]) #252 #->now need to know for unique matchIDs
sum(table(d.highps2[!duplicated(d.highps2$matchID),"points"])[1:18]) #all 498
table(d.highps2[!duplicated(d.highps2$matchID),"points"]) #312 or 63% of lists have 0-2pt bid
sum(table(d.highps2[!duplicated(d.highps2$matchID),"points"])[1:7])/498 #below 190; 4.8%
sum(table(d.highps2[!duplicated(d.highps2$matchID),"points"])[1:13])/498 #low to 195; 26%

d.lowbids <- d.complete[d.complete[,"ps"]>4 & d.complete[,"points"]>198, ]
d.lowbids2 <- data.frame(sort(table(d.complete[d.complete[,"ps"]>4 & d.complete[,"points"]>197, "pilot"]), decreasing = T))
length(unique(d.lowbids[d.lowbids[,"ps"]>4, "matchID"])) #263
sum(table(d.lowbids[d.lowbids[,"ps"]>4, "matchID"])) #445
names(d.lowbids2) <- c("pilot", "amount")
ggplot(d.lowbids2, aes(pilot, amount)) + 
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #annotate(geom = "text", x = 18, y = 30, label = "Total lists in dataset: 586\nTotal ships in dataset: 2257\nTotal lists with ships I5 or I6, filtered for low bids: 276 (45% of entire dataset)\nTotal ships I5 or I6 and low bids: 454 (46% of entire dataset)")+
  annotate(geom = "text", x = 20, y = 50, label = "Filtered for lists with 199 or 200 points!")

d.highpscombined <- merge(d.highpspilots2, d.lowbids2, by = "pilot")
names(d.highpscombined) <- c("pilot", "high_bid", "low_bid")
d.highpscombined <- d.highpscombined[order(d.highpscombined$high_bid, decreasing = T),]

highpsbids <- ggplot(d.highpscombined, aes(x=pilot)) + 
  geom_bar(aes(y = high_bid),stat="identity", position ="identity", fill = "lightblue")+
  geom_bar(aes(y = low_bid),stat="identity", position ="identity", fill = "pink")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #annotate(geom = "text", x = 18, y = 30, label = "Total lists in dataset: 485\nTotal ships in dataset: 1709\nTotal lists with ships I5 or I6, filtered for low bids: 193 (40% of entire dataset)\nTotal ships I5 or I6 and low bids: 397 (23% of entire dataset)")+
  annotate(geom = "text", x = 20, y = 50, label = "Pink: Filtered for lists with 198 to 200 points!")

#--------- Analysis of HP  ------
table(d.complete$hp)
hist_hp_swiss <- ggplot(d.complete, aes(hp, fill=faction))+
  geom_histogram(binwidth = 0.5, col = "black")+
  scale_fill_manual(values = c("springgreen3", "darkgreen", "red2", "sienna2", "goldenrod1")) +
  coord_cartesian(xlim = c(3,17), ylim = c(0,600))+
  labs(title="Histogram for Hull+Shield", x="HP", y="count")

hist_hp_cut<- ggplot(d.cut, aes(hp, fill=faction))+
  geom_histogram(binwidth = 0.5, col = "black")+
  scale_fill_manual(values = c("springgreen3", "darkgreen", "red2", "sienna2", "goldenrod1")) +
  coord_cartesian(xlim = c(3,17), ylim = c(0,120))+
  labs(title="Histogram for Hull+Shield", x="HP", y="count")

#HP per squad: matchID, faction, HP
sumPerSquad <- function(fdata, category, factionF=NULL){
  #input: data all colums, certain category (=col), condition (?)
  #output: row, matchID
  tempdata <- data.frame()
  tempdata <- data.frame(matchID = rep(c(""), times = nrow(fdata)))
  tempdata[, "matchID"]<- fdata[, "matchID"]
  tempdata[, "faction"] <- fdata[, "faction"]
  tempdata[, category] <- fdata[, category]
  i <- 1
  output <- data.frame()
  for (i in 1:nrow(tempdata)){
    j <- 1
    max_j <- length(tempdata[tempdata[,"matchID"]==tempdata[i,"matchID"],"matchID"]) # store the amount of rows with the same matchID
    while(tempdata[i,"matchID"]==tempdata[i+j,"matchID"]){ #while the next number is the same as the current iteration: keep counting
      j <- j+1
      if(i+j>nrow(tempdata)){
        break
      }
      if (j == max_j){
        total <- 0 #at each new start
      }
    }#end while
    if (!is.null(factionF)){ #go here if a faction was determined
      if (j == 1 && tempdata[i,"faction"]==factionF){ 
        # if j reached the maximal value, and if the faction is correct, and if the tier is in cut, and if you remember a non-normal arc
        # so only once per squad!
        sumhp <- sum(tempdata[tempdata[,"matchID"]==tempdata[i,"matchID"],"hp"])
        rows <- c(i, tempdata[i,"matchID"], sumhp, tempdata[i,"faction"])
        output <- rbind(output, rows)
      }
    }#end faction-if
    else if(is.null(factionF)){
      if (j == 1){
        sumhp <- sum(tempdata[tempdata[,"matchID"]==tempdata[i,"matchID"],"hp"])
        rows <- c(i, tempdata[i,"matchID"], sumhp, tempdata[i,"faction"])
        output <- rbind(output, rows)
      }
    } #end else
    if(i+j>nrow(tempdata)){
      break
    }
  }#end for
  colnames(output) <- c("row", "matchID", "hp", "faction")
  rownames(output) <- NULL
  output
} #used on plotColPerSquad

# rebelsHP <- sumPerSquad(d.complete, "hp", factionF = "rebelalliance")
# resistanceHP <- sumPerSquad(d.complete, "hp", factionF = "resistance")
# imperialHP <- sumPerSquad(d.complete, "hp", factionF = "galacticempire")
# scumHP <- sumPerSquad(d.complete, "hp", factionF = "scumandvillainy")
# foHP <- sumPerSquad(d.complete, "hp", factionF = "firstorder")
allHP_swiss <- sumPerSquad(d.complete, "hp")
allHP_swiss$faction <- as.factor(allHP_swiss$faction)
allHP_swiss$faction
levels(allHP_swiss$faction) <- c("firstorder", "galacticempire", "rebelalliance", "resistance", "scumandvillainy")
min(allHP_swiss$hp)
max(allHP_swiss$hp)
hist_squadhp_swiss <- ggplot(allHP_swiss, aes(hp, fill=faction))+
  geom_histogram(binwidth = 0.5, col = "black")+
  scale_fill_manual(values = c("springgreen3", "darkgreen", "red2", "sienna2", "goldenrod1")) +
  coord_cartesian(xlim = c(min(allHP_swiss$hp),max(allHP_swiss$hp)), ylim = c(0,100))+
  labs(title="Histogram for HP", x="HP", y="count")

allHP_cut <- sumPerSquad(d.cut, "hp")
allHP_cut$faction <- as.factor(allHP_cut$faction)
allHP_cut$faction
levels(allHP_cut$faction) <- c("firstorder", "galacticempire", "rebelalliance", "resistance", "scumandvillainy")
min(allHP_cut$hp)
max(allHP_cut$hp)
hist_squadhp_cut <- ggplot(allHP_cut, aes(hp, fill=faction))+
  geom_histogram(binwidth = 0.5, col = "black")+
  scale_fill_manual(values = c("springgreen3", "darkgreen", "red2", "sienna2", "goldenrod1")) +
  coord_cartesian(xlim = c(min(allHP_cut$hp),max(allHP_cut$hp)), ylim = c(0,25))+
  labs(title="Histogram for HP", x="HP", y="count")



#--------- A-wing Analysis -----
d.rz2 <- d.complete[d.complete[,"ship"]=="rz2awing",]
length(unique(d.rz2$player)) #116 players
length(unique(d.rz2$matchID)) #125 lists
length(unique(d.rz2$matchID)) /squad_number #21% of all squads
sum(table(d.rz2$matchID) == 5) #14 of 125 had 5 Awings (11%)
sum(table(d.rz2$matchID) == 1)
length(unique(d.rz2$player))

fiveawings <- table(d.rz2$matchID) == 5
fiveawings_ids <- as.integer(names(fiveawings[fiveawings==1]))
length(unique(d.rz2[d.rz2[,"matchID"]%in%fiveawings_ids, "player"])) #13 players
rm(ls = fiveawings, fiveawings_ids, d.rz2)
#do with getPerSquad
cond <- d.complete[,"ship"]=="rz2awing"
getPerSquad(d.complete, "ship", condition = cond)
rm(ls = cond)
#--------- Y-wing Analysis -----
d.ywing <- d.complete[d.complete[,"ship"]=="btla4ywingREBEL",] #279 ships
length(unique(d.ywing$player)) #129 players
length(unique(d.ywing$matchID)) #132 lists
length(unique(d.ywing$matchID)) /squad_number #22% of all squads
sum(table(d.ywing$matchID) == 5) #16 of 132 lists (12%)
sum(table(d.ywing$matchID) == 4) #9
sum(table(d.ywing$matchID) == 1)
rm(ls = d.ywing)


#Output -----

faction_plot_swiss
faction_plot_cut
perSquad_swiss_ships
perSquad_cut_ships
perSquad_swiss_pilots
perSquad_cut_pilots
hist_ps_swiss
hist_ps_cut
highpsbids
hist_hp_swiss
hist_hp_cut
hist_squadhp_swiss
hist_squadhp_cut
winrates_swiss
winrates_cut
pie(d.genUnique$count, labels = d.genUnique$type, main = "Generic vs Unique, Swiss")
write.table(factiondetails_swiss, file = './factiondetails_swiss.csv',row.names=FALSE, sep=";")
write.table(factiondetails_cut, file = './factiondetails_cut.csv',row.names=FALSE, sep=";")
write.table(archetypeSwiss, file = './archetype_swiss.csv',row.names=FALSE, sep=";")
write.table(archetypeCut, file = './archetype_cut.csv',row.names=FALSE, sep=";")
write.table(mergeArchetype, file = './mergeArchetype.csv',row.names=FALSE, sep=";")
