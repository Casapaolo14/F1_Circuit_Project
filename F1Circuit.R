library(readr)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)



# DATA imported from CSV files
circuitsF1 <- read.csv("C:/Users/paolo/Desktop/F1_Circuit_Project/data/circuits.csv", header = TRUE, stringsAsFactors=FALSE)
racesF1 <- read.csv("C:/Users/paolo/Desktop/F1_Circuit_Project/data/races.csv", header = TRUE, stringsAsFactors=FALSE)
resultsF1 <- read.csv("C:/Users/paolo/Desktop/F1_Circuit_Project/data/results.csv", header = TRUE, stringsAsFactors=FALSE)
driversF1 <- read.csv("C:/Users/paolo/Desktop/F1_Circuit_Project/data/drivers.csv", header = TRUE, stringsAsFactors=FALSE)
qualifyF1 <- read.csv("C:/Users/paolo/Desktop/F1_Circuit_Project/data/qualifying.csv", header = TRUE, stringsAsFactors=FALSE)
pitstopsF1 <- read.csv("C:/Users/paolo/Desktop/F1_Circuit_Project/data/pit_stops.csv", header = TRUE, stringsAsFactors=FALSE)

############################################################## USEFUL ############################################################## 
#Driver ID and Name [DRIVERID, SURNAME]
driverIdName = driversF1 %>% select(driverId, surname)


# CIRCUITS AND RACES JOINED
#JOIN Races x Circuits
circIdLoc = circuitsF1 %>% select(circuitId, location)
racesIdCId = racesF1 %>% select(raceId, circuitId)
#TABLE WITH EVERY CIRCUIT JOINED WITH EVERY RACE [CIRCUITID, LOCATION, RACEID]
circXrac <- circIdLoc %>% inner_join(racesIdCId, by = "circuitId")


############################################################## USEFUL ############################################################## 


############################ CIRCUIT ALLOWED TO USE, CONNECT TO THIS WITH CIRCUITID

#Number of races in a circuit [LOCATION, CIRCUITID]
raceCount <- count(circXrac, location, circuitId, sort = TRUE) 
#raceCount #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC
raceCountGraph <- raceCount %>% select(location, n)
raceCountPlot <- ggplot(raceCountGraph, aes(x=location, y= n)) + geom_bar(width=0.8, stat='identity') + xlab("Circuits") + ylab("Races") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("C:/Users/paolo/Desktop/F1_Circuit_Project/output/raceCountPlot.png", plot = raceCountPlot, units = "px", width = 2815, height = 1734)


#RACE ALLOWED TO USE [LOCATION, CIRCUITID, N (of races)]
allowedCircuit <- raceCount %>% filter(n >= 10)
#allowedCircuit #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC
allowedCircuitPlot <- ggplot(allowedCircuit, aes(x=reorder(location, -n, sum), y= n)) + geom_bar(width=0.8, stat='identity') + xlab("Circuits") + ylab("Races") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("C:/Users/paolo/Desktop/F1_Circuit_Project/output/allowedCircuitPlot.png", plot = allowedCircuitPlot, units = "px", width = 2815, height = 1734)


#CIRCUIT ALLOWED FOR EVERY RACE
allowedRaces <- allowedCircuit %>% inner_join(circXrac, by = "circuitId") %>% select(circuitId, raceId)


############################ WINNERS PER RACE, CONNECTED TO RACEID WITH DRIVERID
#ALL WINNERS PER RACE [RACEID, DRIVERID]
raceWinners <- resultsF1 %>% filter(position == 1) %>% select(raceId, driverId)

#Winners of All Races [RACEID, DRIVERID, SURNAME]
winnersAllCirc <- raceWinners %>% inner_join(driverIdName, by = "driverId")


#TABLE WITH EVERY WINNER OF EVERY RACE ALLOWED WITH CIRCUITID [RACEID, DRIVERID, SURNAME, CIRCUITID] - 916
winnersRaceID <- winnersAllCirc %>% inner_join(allowedRaces, by = "raceId")
#winnersRaceID = winnersRaceID %>% select(raceId, driverId, circuitId)


############################ POLE QUALIFIERS PER RACE, CONNECTED TO RACEID WITH DRIVERID (NEW QUALIFY SISTEM FROM 2006 TO PRESENT)
#ALL POLE QUALIFIERS PER RACE [RACEID, DRIVERID]
raceQualifiers <- resultsF1 %>% filter(grid == 1) %>% select(raceId, driverId) #raceQualifiers <- resultsF1 %>% filter(grid == 1) %>% select(raceId, driverId)


#Qualifiers of Allowed Races [RACEID, DRIVERID, SURNAME]
qualifyAllCirc <- raceQualifiers %>% inner_join(driverIdName, by = "driverId")

#TABLE WITH EVERY POLE OF EVERY RACE ALLOWED WITH CIRCUITID [RACEID, DRIVERID, CIRCUITID]
qualifyRaceID <- qualifyAllCirc %>% inner_join(allowedRaces, by = "raceId")
qualifyRaceIDCircID <- qualifyRaceID %>% select(raceId, driverId, circuitId, surname)
qualifyRaceID = qualifyRaceID %>% select(raceId, driverId, surname)


#qualifyRaceID = qualifyRaceID %>% select(raceId, driverId, circuitId)


############################ FUNCTION CONFRONTING POLE QUALIFIERS AND RACE WINNERS - CHECKING ONLY FIRST POSITION CHANGES
#WINNERS AND POLES [RACEID, DRIVERID.X, SURNAME.X, CIRCUITID, DRIVERID.Y, SURNAME.Y] WHERE X = WINNER AND Y = POLE
winnersAndPoles <- winnersRaceID %>% inner_join(qualifyRaceID, by = "raceId") # 2006 and next years only

#FUNCTION THAT CHECK IF THE WINNER HAS WON FROM POLE OR NOT
#put a select from winners and poles?
winnerFromPole <- winnersAndPoles
winnerFromPole$hasWonFromPole <- ifelse(winnersAndPoles$driverId.x == winnersAndPoles$driverId.y, 'yes', 'no')

#WINNERFROMPOLECOUNT [CIRCUITID, HASWONFROMPOLE, N]
winnerFromPoleCount <- count(winnerFromPole, circuitId, hasWonFromPole, sort = FALSE)
#winnerFromPoleCount #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC
winnerFromPoleCountWLoc <- winnerFromPoleCount %>% inner_join(circIdLoc, by = "circuitId") %>% select(location, hasWonFromPole, n)
winnerFromPolePlot <- ggplot(winnerFromPoleCountWLoc, aes(x=location, y=n, fill = hasWonFromPole)) +
  geom_bar(width=0.8, stat='identity', position = "dodge") +
  scale_fill_discrete(name = "Has won from pole position") +
  xlab("Circuits") + 
  ylab("Number of drivers") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("C:/Users/paolo/Desktop/F1_Circuit_Project/output/winnerFromPolePlot.png", plot = winnerFromPolePlot, units = "px", width = 2815, height = 1734)


############################ DIFFERENT WINNERS FOR CIRCUIT
#916 winners allowed
winnersDriCirc <- winnersRaceID %>% select(driverId, circuitId, surname)
winnersWCircuitGrouped <- count(winnersDriCirc, circuitId, driverId, sort = TRUE) # USE THIS ONE
winnersWCircuitGroupedSurname <- winnersWCircuitGrouped %>% inner_join(driverIdName, by = "driverId") #for check
#winnersWCircuitGroupedSurname #for check
winnersWCircuitGroupSurCId <- winnersWCircuitGroupedSurname %>% inner_join(circIdLoc, by = "circuitId") #for check
#winnersWCircuitGroupSurCId #for check

#TOTAL NUMBER OF DIFFERENT WINNERS PER CIRCUIT [LOCATION, CIRCUITID, N] (N IS TOTAL)
differentWinnersPerCircuit <- count(winnersWCircuitGroupSurCId, location, circuitId) #count(winnersWCircuitGroupSurCId, circuitId, location)
#differentWinnersPerCircuit #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC
differentWinnersPerCircuitWLoc <- differentWinnersPerCircuit %>% select(location, n)
differentWinnersPlot <- ggplot(differentWinnersPerCircuitWLoc, aes(x=reorder(location, -n, sum), y=n) ) +
  geom_bar(width=0.8, stat='identity', position = "dodge", fill = "darkturquoise") +
  xlab("Circuits") + 
  ylab("Number of Different Winners") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("C:/Users/paolo/Desktop/F1_Circuit_Project/output/differentWinnersPlot.png", plot = differentWinnersPlot, units = "px", width = 2815, height = 1734)


############################ DIFFERENT POLE QUALIFIERS PER CIRCUIT (FROM 2006 TO PRESENT)
qualifyDriCirc <- qualifyRaceIDCircID %>% select(driverId, circuitId, surname)
qualifyWCircuitGrouped <- count(qualifyDriCirc, circuitId, driverId, sort = TRUE) # USE THIS ONE
qualifyWCircuitGroupedSurname <- qualifyWCircuitGrouped %>% inner_join(driverIdName, by = "driverId") #for check
#qualifyWCircuitGroupedSurname #for check
qualifyWCircuitGroupSurCId <- qualifyWCircuitGroupedSurname %>% inner_join(circIdLoc, by = "circuitId") #for check
#qualifyWCircuitGroupSurCId #for check

#TOTAL NUMBER OF DIFFERENT POLEMEN PER CIRCUIT [LOCATION, CIRCUITID, N] (N IS TOTAL)
differentQualifyPerCircuit <- count(qualifyWCircuitGroupSurCId, location, circuitId)
#differentQualifyPerCircuit  #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC
differentQualifyPerCircuitWLoc <- differentQualifyPerCircuit %>% select(location, n)
differentQualifyPlot <- ggplot(differentQualifyPerCircuitWLoc, aes(x=reorder(location, -n, sum), y=n)) +
  geom_bar(width=0.8, stat='identity', position = "dodge", fill = "brown2") +
  xlab("Circuits") + 
  ylab("Times") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("C:/Users/paolo/Desktop/F1_Circuit_Project/output/differentQualifyPlot.png", plot = differentQualifyPlot, units = "px", width = 2815, height = 1734)


############################ MOST WIN OF A SINGLE DRIVER PER CIRCUIT
mostWinsPerCircuit <- winnersWCircuitGroupSurCId %>% group_by(circuitId) %>% summarise(n = max(n))
mostWinsPerCircuitWCId <- mostWinsPerCircuit %>% inner_join(circIdLoc, by = "circuitId") 

#MOST WINS PER CIRCUIT BY ONE PILOT [CIRCUITID, N, LOCATION (FOR BETTER READ)]
#mostWinsPerCircuitWCId  #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC
mostWinsPerCircuitPlot <- ggplot(mostWinsPerCircuitWCId, aes(x=reorder(location, -n), y=n)) +
  geom_bar(width=0.8, stat='identity', position = "dodge", fill = "darkturquoise") +
  xlab("Circuits") + 
  ylab("Most wins of a single driver") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("C:/Users/paolo/Desktop/F1_Circuit_Project/output/mostWinsPerCircuitPlot.png", plot = mostWinsPerCircuitPlot, units = "px", width = 2815, height = 1734)


############################ MOST POLE OF A SINGLE DRIVER PER CIRCUIT
mostPolesPerCircuit <- qualifyWCircuitGroupSurCId %>% group_by(circuitId) %>% summarise(n = max(n))
mostPolesPerCircuitWCId <- mostPolesPerCircuit %>% inner_join(circIdLoc, by = "circuitId") 

#MOST POLES PER CIRCUIT BY ONE PILOT [CIRCUITID, N, LOCATION (FOR BETTER READ)]
#mostPolesPerCircuitWCId #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC
mostPolesPerCircuitPlot <- ggplot(mostPolesPerCircuitWCId, aes(x=reorder(location, -n), y=n)) +
  geom_bar(width=0.8, stat='identity', position = "dodge", fill = "brown2") +
  xlab("Circuits") + 
  ylab("Most pole position of a single driver") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("C:/Users/paolo/Desktop/F1_Circuit_Project/output/mostPolesPerCircuitPlot.png", plot = mostPolesPerCircuitPlot, units = "px", width = 2815, height = 1734)



############################ PERCENTAGE OF WINS FROM POLE PER CIRCUIT
racesWPoleonCircuit <- ddply(winnerFromPoleCount,"circuitId",numcolwise(sum)) 
onlyWinsPerCircuit <- filter(winnerFromPoleCount, hasWonFromPole == "yes") 
totalAndWins <- onlyWinsPerCircuit %>% inner_join(racesWPoleonCircuit, by = "circuitId") %>% select(circuitId, n.x, n.y)

mediaOfWinsFromPole <- totalAndWins
mediaOfWinsFromPole$percentuale = mediaOfWinsFromPole$n.x / mediaOfWinsFromPole$n.y * 100 

#MEDIA OF WINS FROM POLE [CIRCUITID, N.X (WINS), N.Y (TOTAL), PERCENTUALE]
#mediaOfWinsFromPole #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC

mediaOfWinsFromPoleWLoc <- mediaOfWinsFromPole %>% inner_join(circIdLoc, by = "circuitId") %>% select(location, percentuale)
mediaOfWinsFromPolePlot <- ggplot(mediaOfWinsFromPoleWLoc, aes(x=reorder(location, -percentuale), y=percentuale)) +
  geom_bar(width=0.8, stat='identity', position = "dodge") +
  xlab("Circuits") + 
  ylab("Percentage of wins from pole position") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("C:/Users/paolo/Desktop/F1_Circuit_Project/output/mediaOfWinsFromPolePlot.png", plot = mediaOfWinsFromPolePlot, units = "px", width = 2815, height = 1734)


############################ POSITION CHANGES PER CIRCUIT - POSITIONCHANGES/TOTALRACESINACIRCUIT
positionChanged <- resultsF1 %>% select(raceId, grid, position)
positionChanged$positionISChanged <- ifelse(positionChanged$grid == positionChanged$position, 'no', 'yes')
positionChanged <- positionChanged[!positionChanged$position == "\\N", ]

positionChangedCount <- count(positionChanged, raceId, positionISChanged)
positionChangedCircuitId <- positionChangedCount %>% inner_join(circXrac, by = "raceId")
positionHasChanged <- filter(positionChangedCircuitId, positionISChanged == "yes") %>% select(circuitId, n, location) 
positionChangesPerCircuit <- ddply(positionHasChanged,"circuitId",numcolwise(sum))
positionChangesPerCircuitAllowed <- positionChangesPerCircuit %>% inner_join(allowedCircuit, by = "circuitId")
meanOfPositionChanges <- positionChangesPerCircuitAllowed %>% select(circuitId, location)
meanOfPositionChanges$CambiPosizioneMedi = positionChangesPerCircuitAllowed$n.x / positionChangesPerCircuitAllowed$n.y

#MEDIA OF POSITION CHANGES [CIRCUITID, LOCATION, CAMBIPOSIZIONEMEDI]
#meanOfPositionChanges #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC
meanOfPositionChangesPlot <- ggplot(meanOfPositionChanges, aes(x=reorder(location, -CambiPosizioneMedi), y=CambiPosizioneMedi)) +
  geom_bar(width=0.8, stat='identity', position = "dodge") +
  xlab("Circuits") + 
  ylab("Average of position changes") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("C:/Users/paolo/Desktop/F1_Circuit_Project/output/meanOfPositionChangesPlot.png", plot = meanOfPositionChangesPlot, units = "px", width = 2815, height = 1734)



############################ ACCIDENTS PER CIRCUIT - MOST DANGEROUS CIRCUIT
#3 - ACCIDENT
#4 - COLLISION
#31 - RETIRED
#130 - COLLISION DAMAGE
#137 - DAMAGE

statusOfAllRaces <- resultsF1 %>% select(raceId, statusId)
dnfOfAllRaces <- statusOfAllRaces %>% filter(statusId == 3 | statusId == 4 | statusId == 31 | statusId == 130 | statusId == 137)
dnfOfAllRacesCount <- count(dnfOfAllRaces, raceId, statusId)
dnfOfAllowedRacesCount <- dnfOfAllRacesCount %>% inner_join(allowedRaces, by = "raceId") %>% select(circuitId, statusId, n)
dnfAllowedCircuits <- dnfOfAllowedRacesCount %>% select(circuitId, n)

#TOTAL OF DNF PER CIRCUIT [CIRCUITID, N (OF DNF)]
dnfAllowedCircuits <- ddply(dnfAllowedCircuits, "circuitId", numcolwise(sum))
#dnfAllowedCircuits #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC
dnfAllowedCircuitsWLoc <- dnfAllowedCircuits %>% inner_join(circIdLoc, by = "circuitId") %>% select(location, n)
dnfAllowedCircuitsPlot <- ggplot(dnfAllowedCircuitsWLoc, aes(x=reorder(location, -n), y=n)) +
  geom_bar(width=0.8, stat='identity', position = "dodge") +
  xlab("Circuits") + 
  ylab("Number of DNF") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("C:/Users/paolo/Desktop/F1_Circuit_Project/output/dnfAllowedCircuitsPlot.png", plot = dnfAllowedCircuitsPlot, units = "px", width = 2815, height = 1734)


############################  MEAN OF DNF PER CIRCUIT 
totAndDnfPerCircuit <- dnfAllowedCircuits %>% inner_join(allowedCircuit, by = "circuitId")
meanOfDnfPerCircuit <- totAndDnfPerCircuit
meanOfDnfPerCircuit$meanOfDNF = meanOfDnfPerCircuit$n.x / meanOfDnfPerCircuit$n.y
meanOfDnfPerCircuit$meanOfDNF = trunc(meanOfDnfPerCircuit$meanOfDNF * 10^2)/10^2

#MEAN OF DNF PER CIRCUIT [CIRCUITID, MEANOFDNF]
onlyMeanOfDNF <- meanOfDnfPerCircuit %>% select(circuitId, meanOfDNF)
#onlyMeanOfDNF  #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC
onlyMeanOfDNFWLoc <- onlyMeanOfDNF %>% inner_join(circIdLoc, by = "circuitId") %>% select(location, meanOfDNF)
onlyMeanOfDNFPlot <- ggplot(onlyMeanOfDNFWLoc, aes(x=reorder(location, -meanOfDNF), y=meanOfDNF)) +
  geom_bar(width=0.8, stat='identity', position = "dodge") +
  xlab("Circuits") + 
  ylab("Average of DNF") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("C:/Users/paolo/Desktop/F1_Circuit_Project/output/onlyMeanOfDNFPlot.png", plot = onlyMeanOfDNFPlot, units = "px", width = 2815, height = 1734)

