library(ggplot2)
library(readxl)
library(readr)
library(dplyr)
rm(list=ls())

# EXPERIMENTAL DESIGN (MA Bao):
# 50 participants adjusted the back off length (Bolength) and back off speed (Bospeed) of 
# 5 robots with different sizes [between] (Robotsize) from two perspectives [within] (View)
# the presented robot sizes are set of 50 equally scaled up robots divided into 
# 5 subgroups [within] (XS-L) (Robotcategory)
# new robots are picked for every participant from each subgroup until all robot sizes have been picked
# the order of appearance of subgroups is randomized fro every participant
# 


###############################################################
#################      Daten Einlesen       ###################
###############################################################

(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Change WD to directory of sourcefile
setwd("06_Rohdaten")
alld<- list.files()
allp<- 1
df<- data.frame() #Hilfs-Dataframe
data<- data.frame() #Main Dataframe

# Probanden VR Daten einlesen und in df zusammenfügen
for (i in 1:50){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Change WD to directory of sourcefile
  setwd(paste("06_Rohdaten/",alld[i], sep=''))
  filename<- list.files(pattern = "\\.txt$")
  dat <- read.table(filename[1], skip = 5)
  dat <- cbind(dat,allp,deparse.level = 0)
  dat <- cbind(dat, c(1,1,2,2,3,3,4,4,5,5),deparse.level = 0)
  colnames(dat) <- c("Robotclasstemp", "Bolength", "Bospeed", "View", "Participant", "Trial")
  df <- rbind(df,dat,deparse.level = 0)
  allp<-allp+1
}


#Fügt eine Spalte mit Roboter Größen und eine mit Klassen hinzu
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Change WD to directory of sourcefile
#Daten mit geometrsichen Robotergrößen einlesen
dat2 <- read.csv("Robots_LWH_.csv", header=TRUE)
#Liste für Zuordnung der geometrischen mit den klassifizierten Robotergrößen
list <- c("XS/1","XS/3","XS/5","XS/7","XS/9","XS/11","XS/13","XS/15","XS/17","XS/19","S/1","S/3","S/5","S/7","S/9","S/11","S/13","S/15","S/17","S/19",
          "M/1","M/3","M/5","M/7","M/9","M/11","M/13","M/15","M/17","M/19","L/1","L/3","L/5","L/7","L/9","L/11","L/13","L/15","L/17","L/19",
          "XL/1","XL/3","XL/5","XL/7","XL/9","XL/11","XL/13","XL/15","XL/17","XL/19")
#Liste für neue Klassifizierung
list2 <- c("XS","XS","XS","XS","XS","XS","XS","XS","XS","XS","S","S","S","S","S","S","S","S","S","S",
          "M","M","M","M","M","M","M","M","M","M","L","L","L","L","L","L","L","L","L","L",
          "XL","XL","XL","XL","XL","XL","XL","XL","XL","XL")
#Zuordnung der Robotergrößen zu Roboterklassen und zusammenführen in einem Dataframe
for (i in 1:50){
  df_s <- subset(df, df$Robotclasstemp == list[i])
  df_s <- cbind(df_s, subset(dat2, dat2$Robotclass == list[i]))
  df_s <- cbind(df_s, list2[i])
  data <- rbind(data, df_s)
}
data$Robotclasstemp <- NULL
data$Robotclass <- NULL
colnames(data) <- c("Bolength", "Bospeed", "View", "Participant", "Trial", "Robotsize", "Robotlength", "Robotwidth", "Robotheight", "Robotcategory")

# Runden
data$Bolength <- round(data$Bolength, digits=3)

# Bo Daten in Relation zur Robotergröße/Roboterlänge
data$RelBolengthRsize <- round(data$Bolength/data$Robotlength, digits=3) #BO Länge / Roboter Länge
data$RelBospeedRsize <- round(data$Bospeed/data$Robotlength, digits=3)

# Robotervolumen
data$Robotvolume <- round(data$Robotlength*data$Robotwidth*data$Robotheight, digits=3)


#Demographische Daten anbinden
dat3 <- read.csv("Demographic.csv", sep=";")
colnames(dat3) <- c("Participanttemp", "Height", "Age")
df <- data #df als temporären data.frame verwenden
df_t <- data.frame() #temporär leerer df
df_s <- data.frame() #temporär leerer ds
for (i in 1:50){
  df_s <- subset(df, df$Participant == i)
  df_s <- cbind(df_s, subset(dat3, dat3$Participanttemp == i))
  df_t <- rbind(df_t, df_s)
}
df_t$Participanttemp <- NULL
data <- df_t


#Fragebogen Daten anbinden
dat4 <- read_excel("FragebogenTeil2.xlsx")
colnames(dat4) <- c("Participanttemp", "VRexperience", "BOlegibility", "BOandVisual" , "BOandAudio", "drop1", "drop2", "drop3", "VisualWish", "AudioWish")
dat4$drop1 <- NULL
dat4$drop2 <- NULL
dat4$drop3 <- NULL

df <- data #df als temporären data.frame verwenden
df_t <- data.frame() #temporär leerer df
df_s <- data.frame() #temporär leerer ds
for (i in 1:50){
  df_s <- subset(df, df$Participant == i)
  df_s <- cbind(df_s, subset(dat4, dat4$Participanttemp == i))
  df_t <- rbind(df_t, df_s)
}
df_t$Participanttemp <- NULL
data <- df_t


#Größenwahrnehmung ... -1:kleiner, 0:gleich groß, 1:größer  ... als vorheriger Roboter
dat5 <- read_excel("Wahrnehmung_Groessen.xlsx")
colnames(dat5) <- c("Trialtemp", "Participanttemp", "Sizeperception", "Sizetruth" , "PerceptCorrect")
dat5$Trialtemp <- gsub("Zweiter Roboter zum Ersten", "2", dat5$Trialtemp)
dat5$Trialtemp <- gsub("Dritter Roboter zum Zweiten", "3", dat5$Trialtemp)
dat5$Trialtemp <- gsub("Vierter Roboter zum Dritten", "4", dat5$Trialtemp)
dat5$Trialtemp <- gsub("Fünfter Roboter zum Vierten", "5", dat5$Trialtemp)
for (i in  1:50){
newrow = c(1 , i , NA , NA , "TRUE")
dat5 <- rbind(dat5, newrow)
}
dat5.Trialtemp <- as.factor(dat5$Trialtemp)
dat5$Participanttemp <- as.factor(dat5$Participanttemp)     

df <- data #df als temporären data.frame verwenden
df_t <- data.frame() #temporär leerer df
df_s <- data.frame() #temporär leerer df
df_x <- data.frame() #temporär leerer df
for (i in 1:50){
  df_s <- subset(df, df$Participant == i)
    for (j in 1:5){
    df_x <- subset(df_s, df_s$Trial == j)
    df_x <- cbind(df_x, subset(dat5, dat5$Participanttemp == i & dat5$Trialtemp == j))
    df_t <- rbind(df_t, df_x)
    }
}

df_t$Participanttemp <- NULL
df_t$Trialtemp <- NULL
data <- df_t



# Back-off Zeit einlesen und 
dat6 <- read.csv("BotimeFromDistanceAndSpeed.csv", sep=",")
dat6$X <- NULL
data <- merge(data, dat6, by=c("Participant", "Trial", "View"))



# Assure that categorical factors are treated as such 
# (e.g. Subject #20 is not "twice as much subject" as subject #10)
data$Participant <- as.factor(data$Participant) 



###############################################################
#################         Sample         ###################
###############################################################

# Sample
summary(data$Age)
mean(data$Age)
sd(data$Age)
summary(data$Height)
mean(data$Height)
sd(data$Height)
mean(data$VRexperience)
sd(data$VRexperience)
mean(data$BOlegibility)
sd(data$BOlegibility)
summary(data$PerceptCorrect)

#################      Back-Off Distanz     ###################
###############################################################

#### First example: linear regression to model the effect of (Robotcategory) and (View) on (Bolength)  ####

# graphical inspection
ggplot(data = data, aes(y = Bolength, x = interaction(Robotcategory, View))) + geom_boxplot() + theme_bw()

# graphical check whether random slopes (cf. below) are justified
# Man sieht, dass manche Probanden generell eher hoch und manche eher tief bewerten (--> random intercept je Proband ergibt Sinn);
# Man kann auch unterschiedlichen Umgang mancher Probanden mit steigender Robotergröße, evtl auch je Blickwinkel vermuten; 
# Manche steigen, manche fallen (--> random slope für die Faktoren Robotergröße und Blickwinkel ergeben vmtl. auch Sinn)
V <- subset(data, View=="V")
ggplot(data=V, aes(y=Bolength, x=interaction(Robotlength), group = Participant, fill = Participant)) + geom_line(aes(color=Participant)) + theme_bw() + labs(y="Back-off length [m] for viewpoint straight")
H <- subset(data, View=="H")
ggplot(data=H, aes(y=Bolength, x=interaction(Robotlength), group = Participant, fill = Participant)) + geom_line(aes(color=Participant)) + theme_bw() + labs(y="Back-off length [m] for viewpoint side")


# package nlme
library(nlme)

# Das ist das erste Modell, das wir implementieren, es hat random intercept für Probanden generell, aber keinen random slope 
# für Probanden je Faktor
# Es hat main und interaction effects for Robotlength and View (only main effect would be: Robotlength + View)
# Alle Faktoren tragen hier signifikant bei (Sonja:"p = 0.0000 berichten als p < 0.001")
# Wie bekomme ich Effektstärke? Sonja:"standardisierte koeff. oder Slopes der Faktoren im Modell")
m2 <- lme(Bolength~Robotlength*View, random = ~1|Participant, data = data, method ="ML")
summary(m2)

# plot random effects
plot(ranef(m2))

# plot residuals
# Residuen (Die Abstände der Messwerte vom Vorhersagewert) sollten Normalverteilt sein für lme, aber: 
# Sonja: "Bei derartig hoher Observation Zahl kann man Normalität der Daten auch grafisch betrachten"
plot(residuals(m2))
plot(m2)
#Shapiro Wilk Test for normal distribution of residuals: significant p value means data has no normal distribution
shapiro.test(residuals(m2))
hist(residuals(m2))
# check normal distribution of residuals with q-q plot
qqnorm(residuals(m2))
qqline(residuals(m2))

# Jetzt verändern wir das Modell und Vergleichen es mit dem ursprünglichen Modell um zu sehen welches am besten ist.
# assess significance of interaction effect by model comparison. Dazu wird ein Modell ohne Interaktionseffekt der main effects verglichen
# Der Vergleich ergibt einen signifikanten Unterschied der Modelle, weshalb die Berücksichtigung des Interaktionseffekts die Modellgüte verbessert
m1 <- lme(Bolength~Robotlength+View, random = ~1|Participant, data = data, method = "ML")
summary(m1)
anova(m2, m1)

# Check ob Probandengröße als random intercept das Modell verbessert
# m3 schlechtes Modell, keine Berücksichtigung der Körpergröße empfohlen hohes AIC und BIC
m3 <- lme(Bolength~Robotlength*View, random =~1|Height, data = data, method = "ML")
summary(mH)
anova(m3, m1)
anova(m3, m2)

# check whether random slopes for View improve model fit ("Do people react differently to Viewpoint?")
# Vergleich ergibt keinen signifikanten Unterschied, was besagt, dass kein signifikanter Einfluss eines random Slope für untersch. View je Proband
# auf die Modellgüte vorliegt. Random Slope für View für Participants kann somit vernachlässigt werden
m4 <- lme(Bolength~Robotlength*View, random = ~View|Participant, data = data, method = "ML")
summary(m4)
anova(m2, m4)

# check whether random slopes for Robotlength improve model fit ("Do people react differently to Robotlength?")
# Vergleich ergibt einen signifikanten Unerschied, was besagt, dass die Berücksichtigung des random slope für steigende Roboterlänge je Proband
# einen signifikanten Einfluss auf das Modell hat und berücksichtig werden sollte --> BESTES MODELL
m5 <- lme(Bolength~Robotlength*View, random = ~Robotlength|Participant, data = data, method = "ML")
summary(m5)
anova(m2, m5)

anova(m5, m2)
anova(m5, m1)
anova(m5, m3)
anova(m5, m4)




# nested random effects: include Bolegibility -> Sonja:"Eher nicht relevant. Das müsste ich nur beachten wenn..." 
mL <- lme(Bolength~Robotlength*View, random = ~1|BOlegibility/Participant, data =data, method = "ML")
anova(m2, mL)

# Dieses Modell rechnet mit random slopes für View und Roboterlänge. Es konvergiert nicht, da evtl zu komplex, wird also nicht berücksichtigt.
#mB <- lme(Bolength~Robotlength*View, random = ~(Robotlength+View)|Participant, data = data, method = "ML")
#summary(mB)
#anova(m, mB)

# Die Analyse ergibt: Modell m5 ist das beste.


# Die Coefficients ansehen. also die Intercepts und Slopes für jeden Probanden
coef(m5)



# Plot to support the mixed model
Rlength <- seq(from = 0.175, to = 1.204, length.out = 500)
# intercept and slope from linear mixed model
# Eine Gerade für View = H -> View = 0
View <- 0
BoDistRo <- 0.51+0.20*Rlength-0.10*View+0.14*Rlength*View 
# Eine Gerade für View = V -> View = 1
View <- 1
BoDistRoVi <- 0.51+0.20*Rlength-0.10*View+0.14*Rlength*View


# detailed
ggplot(data = data, aes(y = Bolength, x = Robotlength)) + 
  geom_point(alpha = 0.4, shape = 16, size=2) +
  geom_line(aes(x = Rlength, y = BoDistRo), color = "#0065BD", size=1.5, alpha = 0.9) +
  geom_line(aes(x = Rlength, y = BoDistRoVi), color = "#2a9f55", size=1.5, alpha = 0.9) +
  #geom_smooth(mapping = NULL, data = NULL, stat = "smooth", position = "identity", method = "auto", formula = y ~ x, se = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
  #geom_boxplot(data=data, aes(y = Bolength, group = Robotcategory), notch=TRUE, color="black", fill='#0065BD' , alpha = 0.9, outlier.colour="black") + 
     #stat_summary(fun.y="mean", geom="point", shape=21, size=4, fill="#f2a10c", alpha = 1, aes(y=Bolength, group=Robotcategory)) + 
     #stat_summary(fun.y = mean, geom = "line", color = "#455B8F", aes(group="Robotcategory")) +
  labs(y="Back-off distance [m]") +
  labs(x="Robot length [m]") +
  ylim(0, 4.5) +
  #theme_bw()
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
      axis.text = element_text(size=12), axis.title = element_text(size=13), legend.position="top", legend.title = element_blank(), 
      legend.spacing.x = unit(0.2, 'cm'), legend.text = element_text(size=12), axis.line.x = element_line(color="black", size = 0.5))

# plain
ggplot(data = data, aes(y = Bolength, x = Robotlength)) + 
  # geom_point(alpha = 0.4, shape = 16, size=2) +
  geom_line(aes(x = Rlength, y = BoDistRo), color = "#000000", size=1.5, alpha = 0.9) +
  geom_line(aes(x = Rlength, y = BoDistRoVi), color = "#696969", size=1.5, alpha = 0.9) +
  #geom_smooth(mapping = NULL, data = NULL, stat = "smooth", position = "identity", method = "auto", formula = y ~ x, se = TRUE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
  #geom_boxplot(data=data, aes(y = Bolength, group = Robotcategory), notch=TRUE, color="black", fill='#0065BD' , alpha = 0.9, outlier.colour="black") + 
  #stat_summary(fun.y="mean", geom="point", shape=21, size=4, fill="#f2a10c", alpha = 1, aes(y=Bolength, group=Robotcategory)) + 
  #stat_summary(fun.y = mean, geom = "line", color = "#455B8F", aes(group="Robotcategory")) +
  labs(y="Back-off distance [m]") +
  labs(x="Robot length [m]") +
  ylim(0, 2) +
  #theme_bw()
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text = element_text(size=12), axis.title = element_text(size=13), legend.position="top", legend.title = element_blank(), 
        legend.spacing.x = unit(0.2, 'cm'), legend.text = element_text(size=12), axis.line.x = element_line(color="black", size = 0.5))


#Histogram
plot.new()
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(fig=c(0,0.8,0,0.8), new=TRUE)
#plot(density(data$Bolength), xlim=c(0,2.0))
hist(data$Bolength, breaks = seq(0, 2.0, by=0.1), xlim=c(0,2.0), xlab = "Back-off distance [m]", main="Back-off distance")
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(data$Bolength, horizontal=TRUE,  outline=FALSE, ylim = c(0,2.0), notch = TRUE)


#Ein paar Werte zum Überblick
summary(data$Bolength)

XS <- subset(data, Robotcategory=="XS")
summary(XS$Bolength)
confiXS <- 1.58*IQR(XS$Bolength)/sqrt(50) #Überprüfung der Berechnung der Notches im boxplot

S <- subset(data, Robotcategory=="S")
summary(S$Bolength)
confiS <- 1.58*IQR(S$Bolength)/sqrt(50)

M <- subset(data, Robotcategory=="M")
summary(M$Bolength)
confiM <- 1.58*IQR(M$Bolength)/sqrt(50)

L <- subset(data, Robotcategory=="L")
summary(L$Bolength)
confiL <- 1.58*IQR(L$Bolength)/sqrt(50)

XL <- subset(data, Robotcategory=="XL")
summary(XL$Bolength)
confiXL <- 1.58*IQR(XL$Bolength)/sqrt(50)

H <- subset(data, View=="H")
HXS <- subset(H, Robotcategory=="XS")
HXL <- subset(H, Robotcategory=="XL")
summary(HXS$Bolength)
summary(HXL$Bolength)


V <- subset(data, View=="V")
VXS <- subset(V, Robotcategory=="XS")
VXL <- subset(V, Robotcategory=="XL")
summary(VXS$Bolength)
summary(VXL$Bolength)













#################  Back-Off Geschwindigkeit ###################
###############################################################


#### First example: linear regression to model the effect of (Robotcategory) and (View) on (Bolength)  ####

# graphical inspection
ggplot(data = data, aes(y = Bospeed, x = interaction(Robotcategory, View))) + geom_boxplot() + theme_bw()

# graphical check whether random slopes (cf. below) are justified for variable Scen
V <- subset(data, View=="V")
ggplot(data=V, aes(y=Bospeed, x=interaction(Robotlength), group = Participant, fill = Participant)) + geom_line(aes(color=Participant)) + theme_bw() + labs(y="Back-off speed [m/s] for viewpoint straight")
H <- subset(data, View=="H")
ggplot(data=H, aes(y=Bospeed, x=interaction(Robotlength), group = Participant, fill = Participant)) + geom_line(aes(color=Participant)) + theme_bw() + labs(y="Back-off speed [m/s] for viewpoint side")

# package nlme
library(nlme)

# random intercept, no random slope
# main and interaction effects for Robotlength and View (only main effect: Robotlength + View)
m2 <- lme(Bospeed~Robotlength*View, random = ~1|Participant, data = data, method ="ML")
summary(m2)

# plot random effects
plot(ranef(m2))

# plot residuals
plot(residuals(m2))
plot(m2)
#Shapiro Wilk Test for normal distribution of residuals: significant p value means data has no normal distribution
shapiro.test(residuals(m2))
hist(residuals(m2))

# check normal distribution of residuals
qqnorm(residuals(m2))
qqline(residuals(m2))



# Jetzt verändern wir das Modell und Vergleichen es mit dem ursprünglichen Modell um zu sehen welches am besten ist.
# assess significance of interaction effect by model comparison. Dazu wird ein Modell ohne Interaktionseffekt der main effects verglichen
# Der Vergleich ergibt keinen signifikanten Unterschied der Modelle, weshalb die Berücksichtigung des Interaktionseffekts die Modellgüte nicht verbessert 
# --> weiter mit m0 aktuell bestes Modell
m1 <- lme(Bospeed~Robotlength+View, random = ~1|Participant, data = data, method = "ML")
summary(m1)
anova(m2, m1)


# Check ob Probandengröße als random intercept das Modell verbessert
# mH schlechtes Modell, keine Berücksichtigung der Körpergröße empfohlen hohes AIC und BIC
m3 <- lme(Bospeed~Robotlength+View, random = ~1|Height, data = data, method = "ML")
summary(m3)
anova(m1, m3)


# check whether random slopes for View improve model fit ("Do people react differently to Viewpoint?")
# Vergleich ergibt keinen signifikanten Unterschied, was besagt, dass kein signifikanter Einfluss eines random Slope für untersch. View je Proband
# auf die Modellgüte vorliegt. Random Slope für View für Participants kann somit vernachlässigt werden
m4 <- lme(Bospeed~Robotlength+View, random = ~View|Participant, data = data, method = "ML")
summary(m4)
anova(m1, m4)

# check whether random slopes for Robotlength improve model fit ("Do people react differently to Robotlength?")
# Vergleich ergibt einen signifikanten Unerschied, was besagt, dass die Berücksichtigung des random slope für steigende Roboterlänge je Proband
# einen signifikanten Einfluss auf das Modell hat und berücksichtig werden sollte --> bestes Modell
m5 <- lme(Bospeed~Robotlength+View, random = ~Robotlength|Participant, data = data, method = "ML")
summary(m5)
anova(m1, m5)

anova(m5, m2)
anova(m5, m1)
anova(m5, m3)
anova(m5, m4)

# Die Coefficients ansehen. also die Intercepts und Slopes für jeden Probanden
coef(m5)


# nested random effects: include Bolegibility -> Sonja:"Eher nicht relevant. Das müsste ich nur beachten wenn..." 
mL <- lme(Bospeed~Robotlength+View, random = ~1|BOlegibility/Participant, data =data, method = "ML")
anova(m1, mL)

# Dieses Modell rechnet mit random slopes für View und Roboterlänge. Es konvergiert nicht, da evtl zu komplex, wird also nicht berücksichtigt.
#mB <- lme(Bospeed~Robotlength+View, random = ~(Robotlength+View)|Participant, data = data, method = "ML")
#summary(mB)
#anova(m, mB)





# Plot to support the mixed model
Rlength <- seq(from = 0.175, to = 1.204, length.out = 500)
# intercept and slope from linear mixed model
# Eine Gerade für View = H -> View = 0
View <- 0
BoDistRo <- 1.30-0.26*Rlength-0.02*View 
# Eine Gerade für View = V -> View = 1
View <- 1
BoDistRoVi <- 1.30-0.26*Rlength-0.02*View 

#detailed
ggplot(data = data, aes(y = Bospeed, x = Robotlength)) + 
  geom_point(alpha = 0.4, shape = 16, size=2) +
  geom_line(aes(x = Rlength, y = BoDistRo), color = "#0065BD", size=1.5, alpha = 0.9) +
  geom_line(aes(x = Rlength, y = BoDistRoVi), color = "#2a9f55", size=1.5, alpha = 0.9) +
  #geom_boxplot(data=data, aes(y = Bospeed, group = Robotcategory), notch=TRUE, color="black", fill='#0065BD' , alpha = 0.9, outlier.colour="black") + 
  #stat_summary(fun.y="mean", geom="point", shape=21, size=4, fill="#f2a10c", alpha = 1, aes(y=Bolength, group=Robotcategory)) + 
  #stat_summary(fun.y = mean, geom = "line", color = "#455B8F", aes(group="Robotcategory")) +
  labs(y="Back-off speed [m/s]") +
  labs(x="Robot length [m]") +
  #theme_bw()
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text = element_text(size=12), axis.title = element_text(size=13), legend.position="top", legend.title = element_blank(), 
        legend.spacing.x = unit(0.2, 'cm'), legend.text = element_text(size=12), axis.line.x = element_line(color="black", size = 0.5))

#plain
ggplot(data = data, aes(y = Bospeed, x = Robotlength)) + 
  #geom_point(alpha = 0.4, shape = 16, size=2) +
  geom_line(aes(x = Rlength, y = BoDistRo), color = "#000000", size=1.5, alpha = 0.9) +
  geom_line(aes(x = Rlength, y = BoDistRoVi), color = "#696969", size=1.5, alpha = 0.9) +
  #geom_boxplot(data=data, aes(y = Bospeed, group = Robotcategory), notch=TRUE, color="black", fill='#0065BD' , alpha = 0.9, outlier.colour="black") + 
  #stat_summary(fun.y="mean", geom="point", shape=21, size=4, fill="#f2a10c", alpha = 1, aes(y=Bolength, group=Robotcategory)) + 
  #stat_summary(fun.y = mean, geom = "line", color = "#455B8F", aes(group="Robotcategory")) +
  labs(y="Back-off speed [m/s]") +
  labs(x="Robot length [m]") +
  ylim(0, 2) +
  #theme_bw()
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text = element_text(size=12), axis.title = element_text(size=13), legend.position="top", legend.title = element_blank(), 
        legend.spacing.x = unit(0.2, 'cm'), legend.text = element_text(size=12), axis.line.x = element_line(color="black", size = 0.5))


#Histogram
plot.new()
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(fig=c(0,0.8,0,0.8), new=TRUE)
hist(data$Bospeed, breaks = seq(0, 2.0, by=0.1), xlim=c(0,2.0), xlab = "Back-off speed [m]", main="Back-off speed")
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(data$Bospeed, horizontal=TRUE,  outline=FALSE, ylim = c(0,2.0), notch = TRUE)


#Shapiro Wilk Test for normal distribution of BO distance: significant p value means data has no normal distribution
## Have a look at the densities
plot(density(data$Bospeed))
## Perform the test
shapiro.test(data$Bospeed)
## Plot using a qqplot
qqnorm(data$Bospeed);qqline(data$Bospeed)


summary(data$Bospeed)



XS <- subset(data, Robotcategory=="XS")
summary(XS$Bospeed)
confiXS <- 1.58*IQR(XS$Bospeed)/sqrt(50) #Überprüfung der Berechnung der Notches im boxplot

S <- subset(data, Robotcategory=="S")
summary(S$Bospeed)
confiS <- 1.58*IQR(S$Bospeed)/sqrt(50)

M <- subset(data, Robotcategory=="M")
summary(M$Bospeed)
confiM <- 1.58*IQR(M$Bospeed)/sqrt(50)

L <- subset(data, Robotcategory=="L")
summary(L$Bospeed)
confiL <- 1.58*IQR(L$Bospeed)/sqrt(50)

XL <- subset(data, Robotcategory=="XL")
summary(XL$Bospeed)
confiXL <- 1.58*IQR(XL$Bospeed)/sqrt(50)


H <- subset(data, View=="H")
HXS <- subset(H, Robotcategory=="XS")
HXL <- subset(H, Robotcategory=="XL")
summary(HXS$Bospeed)
summary(HXL$Bospeed)


V <- subset(data, View=="V")
VXS <- subset(V, Robotcategory=="XS")
VXL <- subset(V, Robotcategory=="XL")
summary(VXS$Bospeed)
summary(VXL$Bospeed)










##############  Back-Off Geschwindigkeit+Speed ################
###############################################################

# Korrelation
cor(data$Bolength, data$Bospeed, method = c("pearson"))
cor.test(data$Bolength, data$Bospeed, method=c("pearson"))

# + Robotlength + Legibility
ggplot(data=data, aes(x=Bolength, y=Bospeed)) +
  labs(x="Back-off distance [m]") +
  labs(y="Back-off speed [m/s]") +
  geom_point(alpha = 0.6, shape = 16, aes(color = factor(data$BOlegibility), size = factor(Robotcategory))) +
  #geom_bin2d(mapping = NULL, data = NULL, stat = "bin2d", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, bins = 6) +
  scale_color_manual(values=c("#d49f00", "#75A315", "#2a9f55")) +
  scale_x_continuous(breaks = seq(0, 2.0, 0.5), limits = c(0, 2.0)) +
  scale_y_continuous(breaks = seq(0, 2.0, 0.5), limits = c(0, 2.0)) + 
  coord_fixed(ratio = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.5), axis.text = element_text(size=12)) 

# simple
ggplot(data=data, aes(x=Bolength, y=Bospeed)) +
  labs(x="Back-off distance [m]") +
  labs(y="Back-off speed [m/s]") +
  geom_point(alpha = 0.6, shape = 16, size = 2) +
  #geom_bin2d(mapping = NULL, data = NULL, stat = "bin2d", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, bins = 6) +
  scale_color_manual(values=c("#d49f00", "#75A315", "#2a9f55")) +
  scale_x_continuous(breaks = seq(0, 2.0, 0.5), limits = c(0, 2.0)) +
  scale_y_continuous(breaks = seq(0, 2.0, 0.5), limits = c(0, 2.0)) + 
  coord_fixed(ratio = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black", size=0.5), axis.text = element_text(size=12)) 





#################### Back-Off time count #######################
###############################################################

ggplot(data = data, aes(x = Botime)) + 
  #geom_histogram(binwidth= 0.2, fill="#0065BD", colour="black", alpha = 0.9) +
  geom_histogram(aes(y=..density..), binwidth= 0.2, fill="#0065BD", colour="black", alpha = 0.9) +
  #geom_density(alpha=0.6) +
  #geom_boxplot(data=data) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text = element_text(size=12), axis.title = element_text(size=13), legend.position="top", legend.title = element_blank(), 
        legend.spacing.x = unit(0.2, 'cm'), legend.text = element_text(size=12), axis.line.x = element_line(color="black", size = 0.5))



ggplot(data = data, aes(x = Botime, fill = factor(data$Robotcategory))) + 
  geom_histogram(binwidth= 0.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text = element_text(size=12), axis.title = element_text(size=13), legend.position="top", legend.title = element_blank(), 
        legend.spacing.x = unit(0.2, 'cm'), legend.text = element_text(size=12), axis.line.x = element_line(color="black", size = 0.5))


ggplot(data = data, aes(x = Botime, fill = factor(data$Age))) + 
  geom_histogram(binwidth= 0.2) +
  #scale_fill_manual(values=c("#d49f00", "#75A315", "#2a9f55")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text = element_text(size=12), axis.title = element_text(size=13), legend.position="top", legend.title = element_blank(), 
        legend.spacing.x = unit(0.2, 'cm'), legend.text = element_text(size=12), axis.line.x = element_line(color="black", size = 0.5))

#Histogram
plot.new()
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(fig=c(0,0.8,0,0.8), new=TRUE)
hist(data$Botime, breaks = seq(0, 4.4, by=0.2), xlim=c(0,4.4), xlab = "Back-off time [s]", main="Back-off time")
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(data$Botime, horizontal=TRUE,  outline=FALSE, ylim = c(0,4.4), notch = TRUE)
 
# Histogram with normal distribution
# https://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r
#' @noRd
#' @exportMethod hist.default
#' @export
hist.default <- function(x,
                         breaks = "Sturges",
                         freq = NULL,
                         include.lowest = TRUE,
                         normalcurve = TRUE,
                         right = TRUE,
                         density = NULL,
                         angle = 45,
                         col = NULL,
                         border = NULL,
                         #main = paste("Histogram of", xname),
                         main = NULL,
                         ylim = NULL,
                         xlab = xname,
                         ylab = NULL,
                         axes = TRUE,
                         plot = TRUE,
                         labels = FALSE,
                         warn.unused = TRUE,
                         ...)  {
  
  # https://stackoverflow.com/a/20078645/4575331
  xname <- paste(deparse(substitute(x), 500), collapse = "\n")
  
  suppressWarnings(
    h <- graphics::hist.default(
      x = x,
      breaks = breaks,
      freq = freq,
      include.lowest = include.lowest,
      right = right,
      density = density,
      angle = angle,
      col = col,
      border = border,
      main = main,
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      axes = axes,
      plot = plot,
      labels = labels,
      warn.unused = warn.unused,
      ...
    )
  )
  
  if (normalcurve == TRUE & plot == TRUE) {
    x <- x[!is.na(x)]
    xfit <- seq(min(x), max(x), length = 40)
    yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
    if (isTRUE(freq) | (is.null(freq) & is.null(density))) {
      yfit <- yfit * diff(h$mids[1:2]) * length(x)
    }
    lines(xfit, yfit, col = "black", lwd = 2)
  }
  
  if (plot == TRUE) {
    invisible(h)
  } else {
    h
  }
}

plot.new()
nf <- layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,3))
par(fig=c(0,0.8,0,0.8), new=TRUE)
hist(data$Botime, breaks = seq(0, 4.4, by=0.2), xlim=c(0,4.4), xlab = "Back-off time [s]")
par(fig=c(0,0.8,0.55,1), new=TRUE)
boxplot(data$Botime, horizontal=TRUE,  outline=FALSE, ylim = c(0,4.4), notch = TRUE)

############## Robotlength -> Back-Off time ############## 
###############################################################


ggplot(data = data, aes(y = Botime, x = Robotlength)) + 
  geom_point(alpha = 0.4, shape = 16, size=2) +
  #geom_line(aes(x = Rlength, y = bo), color = "red", size=1.5, alpha = 0.5) +
  geom_boxplot(data=data, aes(y = Botime, group = Robotcategory), notch=TRUE, color="black", fill='#0065BD' , alpha = 0.9, outlier.colour="black") + 
  #stat_summary(fun.y="mean", geom="point", shape=21, size=4, fill="#f2a10c", alpha = 1, aes(y=Bolength, group=Robotcategory)) + 
  #stat_summary(fun.y = mean, geom = "line", color = "#455B8F", aes(group="Robotcategory")) +
  labs(y="Back-off time [s]") +
  labs(x="Robot length [m]") +
  #theme_bw()
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text = element_text(size=12), axis.title = element_text(size=13), legend.position="top", legend.title = element_blank(), 
        legend.spacing.x = unit(0.2, 'cm'), legend.text = element_text(size=12), axis.line.x = element_line(color="black", size = 0.5))



#Shapiro Wilk Test for normal distribution of BO time: significant p value means data has no normal distribution
## Have a look at the densities
ok <- na.omit(data$Botime)
plot(density(ok))
## Perform the test
shapiro.test(data$Botime)
## Plot using a qqplot
qqnorm(data$Botime);qqline(data$Botime)

# Art der schiefe der Verteilung finden -> vermutung rechtsschief
library(moments)
skewness(ok) 



summary(data$Botime)
sd(data$Botime, na.rm = TRUE)


# Mixed Effects Model
m <- lme(Botime~Robotlength*View, random = ~1|Participant, data = data, method ="ML", na.action = na.omit)
summary(m)


#################### Deskriptive Roboter und Back-Off Daten #######################
###################################################################################

mean(data$Robotlength)
mean(data$Robotwidth)
mean(data$Robotheight)

mean(data$Bolength)
sd(data$Bolength)
quantile(data$Bolength, c(.5))
#quantile(data$Bolength, c(.05)) 
quantile(data$Bolength, c(.95)) 

mean(data$Bospeed)
sd(data$Bospeed)
quantile(data$Bospeed, c(.5))

mean(data$Botime, na.rm = TRUE)
sd(data$Botime, na.rm = TRUE)
quantile(data$Botime, c(.5), na.rm = TRUE) 


# guten Back-off resufiltern
# nach length und time
data %>% filter(Bolength > quantile(data$Bolength, c(.95))) %>% filter(Botime > quantile(data$Botime, c(.5), na.rm = TRUE)-0.1 & Botime < quantile(data$Botime, c(.5), na.rm = TRUE)+0.1)
# nach length, speed und time
data %>% filter(Bolength > quantile(data$Bolength, c(.95))) %>% filter(Bospeed > quantile(data$Bospeed, c(.5))-0.1 & Bospeed < quantile(data$Bospeed, c(.5))+0.1) %>% filter(Botime > quantile(data$Botime, c(.5), na.rm = TRUE)-0.1 & Botime < quantile(data$Botime, c(.5), na.rm = TRUE)+0.1)

