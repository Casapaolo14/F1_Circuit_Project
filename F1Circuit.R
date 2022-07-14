library(readr)
library(dplyr)
library(tidyverse)


# DATA imported from CSV files
circuitsF1 <- read.csv("C:/Users/paolo/Desktop/F1_Circuit_Project/data/circuits.csv", header = TRUE, stringsAsFactors=FALSE)
racesF1 <- read.csv("C:/Users/paolo/Desktop/F1_Circuit_Project/data/races.csv", header = TRUE, stringsAsFactors=FALSE)
resultsF1 <- read.csv("C:/Users/paolo/Desktop/F1_Circuit_Project/data/results.csv", header = TRUE, stringsAsFactors=FALSE)
driversF1 <- read.csv("C:/Users/paolo/Desktop/F1_Circuit_Project/data/drivers.csv", header = TRUE, stringsAsFactors=FALSE)
qualifyF1 <- read.csv("C:/Users/paolo/Desktop/F1_Circuit_Project/data/qualifying.csv", header = TRUE, stringsAsFactors=FALSE)
pitstopsF1 <- read.csv("C:/Users/paolo/Desktop/F1_Circuit_Project/data/pitstops.csv", header = TRUE, stringsAsFactors=FALSE)

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
#raceCount #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC

#RACE ALLOWED TO USE [LOCATION, CIRCUITID, N (of races)]
allowedCircuit <- raceCount %>% filter(n >= 10)
#allowedCircuit #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC

#CIRCUIT ALLOWED FOR EVERY RACE
allowedRaces <- allowedCircuit %>% inner_join(circXrac, by = "circuitId") %>% select(circuitId, raceId)


############################ WINNERS PER RACE, CONNECTED TO RACEID WITH DRIVERID
#ALL WINNERS PER RACE [RACEID, DRIVERID]
raceWinners <- resultsF1 %>% filter(position == 1) %>% select(raceId, driverId)

#Winners of All Races [RACEID, DRIVERID, SURNAME]
winnersAllCirc <- raceWinners %>% inner_join(driverIdName, by = "driverId")


#TABLE WITH EVERY WINNER OF EVERY RACE ALLOWED WITH CIRCUITID [RACEID, DRIVERID, CIRCUITID]
winnersRaceID <- winnersAllCirc %>% inner_join(allowedRaces, by = "raceId")
#winnersRaceID = winnersRaceID %>% select(raceId, driverId, circuitId)


############################ POLE QUALIFIERS PER RACE, CONNECTED TO RACEID WITH DRIVERID (NEW QUALIFY SISTEM FROM 2006 TO PRESENT)
#ALL POLE QUALIFIERS PER RACE [RACEID, DRIVERID]
raceQualifiers <- qualifyF1 %>% filter(position == 1) %>% select(raceId, driverId)

#Qualifiers of Allowed Races [RACEID, DRIVERID, SURNAME]
qualifyAllCirc <- raceQualifiers %>% inner_join(driverIdName, by = "driverId")

#TABLE WITH EVERY POLE OF EVERY RACE ALLOWED WITH CIRCUITID [RACEID, DRIVERID, CIRCUITID]
qualifyRaceID <- qualifyAllCirc %>% inner_join(allowedRaces, by = "raceId")
qualifyRaceID = qualifyRaceID %>% select(raceId, driverId, surname)


#qualifyRaceID = qualifyRaceID %>% select(raceId, driverId, circuitId)


############################ FUNCTION CONFRONTING POLE QUALIFIERS AND RACE WINNERS - CHECKING ONLY FIRST POSITION CHANGES
#WINNERS AND POLES [RACEID, DRIVERID.X, SURNAME.X, CIRCUITID, DRIVERID.Y, SURNAME.Y] WHERE X = WINNER AND Y = POLE
winnersAndPoles <- winnersRaceID %>% inner_join(qualifyRaceID, by = "raceId")

#FUNCTION THAT CHECK IF THE WINNER HAS WON FROM POLE OR NOT
#put a select from winners and poles?
winnerFromPole <- winnersAndPoles
winnerFromPole$hasWonFromPole <- ifelse(winnersAndPoles$driverId.x == winnersAndPoles$driverId.y, 'yes', 'no')

#WINNERFROMPOLECOUNT [CIRCUITID, HASWONFROMPOLE, N]
winnerFromPoleCount <- count(winnerFromPole, circuitId, hasWonFromPole, sort = FALSE)
#winnerFromPoleCount #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC #TO BE USED FOR GRAPHIC

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


############################ DIFFERENT POLE QUALIFIERS PER CIRCUIT




############################ NUMBER OF ALL POSITION CHANGES PER CIRCUIT - USE RESULTF1


############################ PIT STOP NUMBER - MORE VARIABLE RACE

#MEAN PIT STOP TIME?



############################ MOST WIN OF A SINGLE DRIVER PER CIRCUIT


############################ MOST POLE OF A SINGLE DRIVER PER CIRCUIT















