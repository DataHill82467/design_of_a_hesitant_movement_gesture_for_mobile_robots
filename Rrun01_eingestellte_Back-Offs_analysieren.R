library(ggplot2)


###### EINZELN #######
rm(list=ls())
#!Funktionniert für VP 3 und aufwärts!
########################
# Proband einstellen
VP <- 18    ##############
#########################
# Trial einstellen
TR <- 2    ##############
#########################
# Blickwinkel einstellen (H/V)
View <- 'H'  ##############
#########################



# Jetzt wird eingelesen
#Liste mit Probanden erzeugen
(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Change WD to directory of sourcefile
setwd("06_Rohdaten")
ListVP <- list.files()


# BackOffs der ausgewählten Probanden aus jedem Trial in Liste schreiben
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Change WD to directory of sourcefile
  setwd(paste("06_Rohdaten/",ListVP[VP], sep=''))
# Eingewöhnung (Level1)  
  ListStandardBO <- list.files(pattern = "Level1")
  standardBO <- read.delim(ListStandardBO[1])
# Position player
  ListPositionVP <- list.files(pattern = "_player")
  PositionVP <- read.delim(ListPositionVP[1])
# Position roboter
  ListPositionR <- list.files(pattern = "_robot")
  PositionR <- read.delim(ListPositionR[1])
# Die BackOffs aus den Einstelltrials
  if(View == 'V')
  {ListAllBOView<- list.files(pattern = "V_backoff")}
  if(View == 'H')
  {ListAllBOView<- list.files(pattern = "H_backoff")}
# spezifischen BackOff Einstellverlauf für VP, TR und Blickwinkel einlesen
  BO_Speed <- read.delim(ListAllBOView[TR])
# Vorzeichen der Back-off Velocity umkehren
  BO_Speed$Velocity <- BO_Speed$Velocity*(-1)
    
    
#finde den letzten BO einstell cycle 
  for(i in nrow(BO_Speed):1) #analysiert datensatz von hinten nach vorne
  {
    if (BO_Speed$Time[i] == 0.0)
    {
      print(i)
      StartLastBO<-i
      break # n <- i #speichert zeilen index das beginns des letztesn cycle in n
    }
  }

  
######
## PLOTS
#####
# plot Gewöhnungsbackoff
#plot(standardBO$Time, standardBO$Velocity, xlab="time [s]", ylab="velocity [m/s]", sub=sprintf("Standard Back-off (Gewöhnung) by participant %s", VP))
  
# Position VP [!nicht verwenden da Datensatz nicht über die gesamte Versuchsdauert geht!]
#plot(PositionVP$X, PositionVP$Z, xlab="X []", ylab="Y []", sub=sprintf("Position of participant %s during the experiment of total time of %s Minutes", VP, PositionVP$Time[nrow(PositionVP)]/60))
  
# Position Roboter [!nicht verwenden da Datensatz nicht über die gesamte Versuchsdauert geht!]
#plot(PositionR$X, PositionR$Z, xlab="X []", ylab="Y []", sub=sprintf("Position of robot in the experiment of participant %s", VP))
  
  
# plot Einstellbackoff je nach trial, und Blickwinkel
plot(BO_Speed$Time, BO_Speed$Velocity, xlab="time [s]", ylab="velocity [m/s]", col= 'grey', sub=sprintf("Back-off adjustment by participant %s in the %s condition of trial %s . The final Back-Off is black", VP, View, TR))
points(BO_Speed$Time[StartLastBO:nrow(BO_Speed)], BO_Speed$Velocity[StartLastBO:nrow(BO_Speed)], col= 'black')  # plot des letzten eingestellten BO
  
  
  
  
#####Back-off time des letzten eingestellten BO berechnen#####
lastVmax <- 0
  #Ersten Zeitpunkt von BO Vmax finden
  for(i in StartLastBO:nrow(BO_Speed))
  {
    if (i == nrow(BO_Speed))  # Wenn bis zum Ende der Aufzeichnung kein first Vmax gefunden wurde wird first und last Vmax auf ende der Aufzeichnung gesetzt
    {
      print("last point is mid BO")
      firstVmax <- i
      lastVmax <- i
      break
    }
    else if (BO_Speed$Velocity[i+1] == BO_Speed$Velocity[i] | BO_Speed$Velocity[i+1] < BO_Speed$Velocity[i]) # für BO mit konstanter BO phase und BO ohne konstante BO phase first Vmax gefunden
    {
      print("firstVmax found")
      firstVmax <- i
      break
    }
  }
  

  #letzten Zeitpunkt von BO Vmax finden nur wenn dieser noch nicht gefunden wurde
if(lastVmax == 0)
{
  for(i in firstVmax:nrow(BO_Speed))
  {
    if (i == nrow(BO_Speed))  # Wenn bis zum Ende der Aufzeichnung kein last Vmax gefunden wurde wird last Vmax auf ende der Aufzeichnung gesetzt
    {
      print("last point is lastVmax")
      lastVmax <- i
      break
    }
    else if (BO_Speed$Velocity[i+1] < BO_Speed$Velocity[i]) 
    {
      print("lastVmax found")
      lastVmax <- i
      break    
    }
  }
}


  #Mitte des gesamten BO und total BO time berechnen
  midBO <- (firstVmax+lastVmax)/2 # mitte des BO im Dataframe
  mid_BO_time <- (midBO - StartLastBO)*0.1 # Zeit bis zur mitte des letzten BO
  total_BO_time <- mid_BO_time*2 # Zeit bis zur mitte * 2
  
  
  # Print Mitte und Ende des letzten BO
  points(mid_BO_time, 0, col= 'green', cex = 1.5) # plot Back-Off Mitte des letzten BO
  points(total_BO_time, 0, col= 'red', cex = 1.5) # plot Back-Off Ende des letzten BO

  
  
  
  
  
  
  
  
  ######################################
  ######################################
  ############### ALLE #################
  ######################################
  ######################################
  
  rm(list=ls())
  
  
  # Dataframe für Participant, Trial, View, Botime
  data <- data.frame(matrix(0, ncol = 4, nrow = 500))
  colnames(data) <- c("Participant", "Trial", "View", "Botime")
  
  
n <- 0 
for (VP in 1:50)
      {
        for (TR in 1:5)
        {
          for (View in c("V","H"))
          {
            # Jetzt wird eingelesen
            #Liste mit Probanden erzeugen
            (list=ls())
            setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Change WD to directory of sourcefile
            setwd(paste("06_Rohdaten/",alld[i], sep=''))
            ListVP <- list.files()
            
            
            # BackOffs der ausgewählten Probanden aus jedem Trial in Liste schreiben
            setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Change WD to directory of sourcefile
            setwd(paste("06_Rohdaten/",alld[i], sep=''))
            # Die BackOffs aus den Einstelltrials
            if(View == 'V')
            {ListAllBOView<- list.files(pattern = "V_backoff")}
            if(View == 'H')
            {ListAllBOView<- list.files(pattern = "H_backoff")}
            # spezifischen BackOff Einstellverlauf für VP, TR und Blickwinkel einlesen
        if (file.exists(ListAllBOView[TR]))   # Nur versuchen einzulesen falls Datai des spez. Trial vorhanden ist
          {
            BO_Speed <- read.delim(ListAllBOView[TR])
            # Vorzeichen der Back-off Velocity umkehren
            BO_Speed$Velocity <- BO_Speed$Velocity*(-1)
            
            
            #finde den letzten BO einstell cycle 
            for(i in nrow(BO_Speed):1) #analysiert datensatz von hinten nach vorne
            {
              if (BO_Speed$Time[i] == 0.0)
              {
                StartLastBO<-i
                break # n <- i #speichert zeilen index das beginns des letztesn cycle in n
              }
            }
            
            
            
            
            #####Back-off time des letzten eingestellten BO berechnen#####
            lastVmax <- 0
            #Ersten Zeitpunkt von BO Vmax finden
            for(i in StartLastBO:nrow(BO_Speed))
            {
              if (i == nrow(BO_Speed))  # Wenn bis zum Ende der Aufzeichnung kein first Vmax gefunden wurde wird first und last Vmax auf ende der Aufzeichnung gesetzt
              {
                #print("last point is mid BO")
                firstVmax <- i
                lastVmax <- i
                break
              }
              else if (BO_Speed$Velocity[i+1] == BO_Speed$Velocity[i] | BO_Speed$Velocity[i+1] < BO_Speed$Velocity[i]) # für BO mit konstanter BO phase und BO ohne konstante BO phase first Vmax gefunden
              {
                #print("firstVmax found")
                firstVmax <- i
                break
              }
            }
            
            
            #letzten Zeitpunkt von BO Vmax finden nur wenn dieser noch nicht gefunden wurde
            if(lastVmax == 0)
            {
              for(i in firstVmax:nrow(BO_Speed))
              {
                if (i == nrow(BO_Speed))  # Wenn bis zum Ende der Aufzeichnung kein last Vmax gefunden wurde wird last Vmax auf ende der Aufzeichnung gesetzt
                {
                  #print("last point is lastVmax")
                  lastVmax <- i
                  break
                }
                else if (BO_Speed$Velocity[i+1] < BO_Speed$Velocity[i]) 
                {
                  #print("lastVmax found")
                  lastVmax <- i
                  break    
                }
              }
            }
            
            #Mitte des gesamten BO und total BO time berechnen
            midBO <- (firstVmax+lastVmax)/2 # mitte des BO im Dataframe
            mid_BO_time <- (midBO - StartLastBO)*0.1 # Zeit bis zur mitte des letzten BO
            total_BO_time <- mid_BO_time*2 # Zeit bis zur mitte * 2
            
            # Daten in ein dataframe schreiben
            n <- n+1
            data$Participant[n] <- VP
            data$Trial[n] <- TR
            data$View[n] <- View
            data$Botime[n] <- total_BO_time
            print(sprintf("Participant %s, in Trial %s, with View %s likes a back-off time of %s seconds. This is now row %s", VP, TR, View, total_BO_time, n))
            
        }
            else      # Wenn Datei für spez. Trial nicht vorhanden
        {
            total_BO_time <- NaN
            n <- n+1
            data$Participant[n] <- VP
            data$Trial[n] <- TR
            data$View[n] <- View
            data$Botime[n] <- total_BO_time
            print(sprintf("Participant %s, in Trial %s, with View %s likes a back-off time of %s seconds. This is now row %s", VP, TR, View, total_BO_time, n))

        }
  
  
  
          }
        }
      }

# nach visuellem Abgleich BOtime mit erhobenen BO Daten werden offensichtlich fehlerhaft abgespeicherte finale BOs aussortiert (VP: 16T1H)
data$Botime[152] <- NaN # BOtime von 0s ist unmöglich

# BOtime speichern
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Change WD to directory of sourcefile
write.csv(data, file = "BotimeFromDistanceAndSpeed.csv")

  
####### test BOtime count

ggplot(data = data, aes(x = Botime)) + 
  geom_histogram(binwidth= 0.2)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
      axis.text = element_text(size=12), axis.title = element_text(size=13), legend.position="top", legend.title = element_blank(), 
      legend.spacing.x = unit(0.2, 'cm'), legend.text = element_text(size=12), axis.line.x = element_line(color="black", size = 0.5))
  