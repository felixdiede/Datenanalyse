######### ====================== Datenanalyse SS 2022 ====================== #########

########### ---------------------- Loesung zum 10. Aufgabenzettel ---------------------- ##########

#### Aufgabe 27 ---- 

## Einlesen & Streudiagramm --

daten = data.frame(
  x=c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  y=c(1, 1, 4, 4, 6, 6, 8, 8, 9)
)

plot(daten)

## Regressionsgerade --

cf = lm(daten$y ~ daten$x)$coefficients

abline(cf, col="red" )

## Output interpretieren --

summary(lm(daten$y ~ daten$x))


#### Aufgabe 28 ---- 

## Einlesen --

covid_daily <- read.table(file = "covid_19_daily_reports_06-20-2022.csv",header = TRUE, sep = ";", dec = ".")
str(covid_daily)
summary(covid_daily)

meta <- read.table(file = "meta.csv",header = TRUE, sep = ";", dec = ",")
str(meta)
summary(meta)

## Verbinden der Datensätze --

covid <- merge(covid_daily, meta, by = "Country") 
covid$Rel.Confirmed = covid$Confirmed/covid$Population

str(covid)
summary(covid)

for(s in 2:ncol(covid)){
  if(class(covid[,s]) == "character"){
    covid[,s] = as.numeric(covid[,s])
  }
}


## Aufteilen nach Vorhandensein der Daten -- 

testing = covid[which(is.na(covid$Rel.Confirmed), arr.ind=TRUE),]
training = covid[which(!is.na(covid$Rel.Confirmed), arr.ind=TRUE),]


## Multiple Lineare Regression --

confirmed_LR_1 <- lm(Rel.Confirmed ~ Population+Median.Age+Life.Expectancy, data = training)
summary(confirmed_LR_1)

# Andere Variablen in Regression   
confirmed_LR_2 <- lm(Rel.Confirmed ~ Urban+Median.Age+Life.Expectancy, data = training)
summary(confirmed_LR_2)

# ALLE Variablen, die wir haben 

confirmed_LR_All <- lm(Rel.Confirmed ~ Population+Density+Urban+Median.Age+
                       Males.per.Female+Fertility+Life.Expectancy+GDP+
                       Median.wealth+Mean.wealth, data = covid)

summary(confirmed_LR_All)

# -> Alter scheint die Variable mit dem größten, nachhaltigstem Einfluss zu sein. 
# -> Einfach nur sinnlos Variablen in das Model zu packen scheint das R-Squared zu erhöhen, aber nicht das Adjusted-R-Squared 
# -> R-Squared ist niedrig: 
# (1) Die Lineare Regression ist einfach nicht geeignet für das Problem (Voraussetzungen nicht erfüllt)
# (2) Die Variablen, die mit einbezogen wurden, sind die falschen Variablen 
 

## Regression auf dem Training-Datensatz -- 

model <- lm(Rel.Confirmed ~ Median.Age, data = training)
summary(model)

model$coefficients

testing$Rel.Confirmed = predict.lm(model, testing)

##### Plotten der Ergebnisse ------

# Einlesen des vollständigen Datensatzes 
covid_vollständig <- read.table(file = "covid_19_daily_reports_06-20-2022_COMPLETE.csv",header = TRUE, sep = ";", dec = ".")
covid_vollständig = merge(covid_vollständig, meta, by = "Country") 

str(covid_vollständig)
summary(covid_vollständig)

for(s in 2:ncol(covid_vollständig)){
  if(class(covid_vollständig[,s]) == "character"){
    covid_vollständig[,s] = as.numeric(covid_vollständig[,s])
  }
}
covid_vollständig$Rel.Confirmed = covid_vollständig$Confirmed/covid_vollständig$Population


# Datenpunkte finden, die im ersten Datensatz fehlten 
index <- which(is.na(covid$Confirmed), arr.ind = T)

plot(x=training$Median.Age, y=training$Rel.Confirmed, 
     xlab="Median Alter",ylab="Bestätigte Fallzahlen", pch=16) 

# Regressionsgerade einzeichnen 
abline(model,col="red") 

# Die ersetzten fehlenden Daten eintragen 
points(x=testing$Median.Age, y=testing$Rel.Confirmed, col="blue", pch=16)

# Die wirklichen fehlenden Daten eintragen 
points(x=covid_vollständig$Median.Age[index],y=covid_vollständig$Rel.Confirmed[index],  col = "orange", pch=16)

legend("topleft", pch = 16, text.font = 1, cex=0.75, 
       legend = c("Beobachtungen", "Regressionsgerade", "Vorhergesagte Fallzahlen", "Wahre Fallzahlen"),
       col = c("black", "red", "blue", "orange"))


