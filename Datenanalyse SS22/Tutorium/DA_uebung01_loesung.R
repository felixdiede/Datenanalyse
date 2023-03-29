######### ====================== Datenanalyse SS 2021 ====================== #########

########### ---------------------- Loesung zum 2. Aufgabenzettel ---------------------- #########

#### Aufgabe 2 ---- 

# Aufgabe a) 

# Initialisierung des Zufallszahlengenerators
set.seed(0) 
# Stichprobenumfang
n=100 

# Anschauen des angegebenen Befehls 
?stats::rexp
X = rexp(n,1) # Erzeugt einen Vektor von 100 Realisierungen von u.i.v. Exp(1)-ZV

mean_x = cumsum(X)/(1:n) 
# cumsum bildet fortlaufende Summen (x1,x1+x2,x1+x2+x3,...,x1+...+x100), 
# der Quotient teilt komponentenweise durch 1,2,...100. D.h. fortlaufende Mittelwerte werden gebildet.

# Verbindet die fortlaufenden Mittelwerte mit einem Linienzug
plot(mean_x,type="o",pch=".",cex=0.2,xlab="",ylab="xquer") 
# hebt die Mittelwertpunkte auf dem Linienzug hervor
points(1:n,mean_x,cex=0.5) 
# Skizziert den Erwartungswert 1 einer Exp(1)-Verteilung
abline(1,0,col="blue") 

# Interpretation: Das Beispiel illustriert die Stabilisierung der fortlaufenden Mittelwerte 
# bei dem theoretischen Erwartungswert in Form des (starken) Gesetzes gro?er Zahlen.

# Aufgabe b) 
# Funktion zur Erzeugung eines einzelnen Stichprobenwertes
zgs.single.sim <- function(i, k){
  X = rexp(k,1)
  sqrt(k)*(mean(X)-1/1)*1
} 

set.seed(0)

Y = sapply(1:1000, zgs.single.sim, 100)

par(mfrow=c(1,2))
hist(Y,freq=FALSE,main="",ylim=c(0,0.4),xlab="x",ylab="rel. Häufigkeit")
curve(dnorm,from=-3,to=3,add=TRUE,col="red",ylim=c(0,0.6))
# Man erkennt, dass das Histogramm die Dichte der N(0,1)-Verteilung  ann?hert. 
# Das spricht daf?r, dass die Stichprobe n?herungseise einer N(0,1)-Verteilung folgt

plot.ecdf(Y, main="")
curve(pnorm,from=-3,to=3,add=TRUE,col="red")
# Auch die empirische VF entspricht ziemlich genau der VF Phi der N(0,1)-Verteilung
# Es zeigt sich hier die Stabilisierung des zentralen Grenzwertsatzes


#### Aufgabe 3 ---- 

# Einlesen des Datensatzes
covid <- read.table(file = "covid_19_daily_reports_04-12-2022.csv",header = TRUE, sep = " ", dec = ".")

str(covid)
summary(covid)

#### b) ---- 

covid_germany <- covid[covid$Country_Region == "Germany",c(1,3,4)]

distanz_matrix <- dist(covid_germany[,2:3])
clustern <- hclust(distanz_matrix, method = "ward.D2")
clustern$labels <- covid_germany$Province_State
plot(clustern, xlab = "Länder", ylab = "Distanz", main = "Dendrogramm")

#' Zwei Cluster scheinen angemessen zu sein 

#### c) ---- 
covid_agg <- aggregate(covid[,3:4], by = list(covid$Country_Region), FUN = sum) # 197 Länder sollten herauskommen 
colnames(covid_agg)[1] <- "Country"

#### d) ---- 
covid_agg_confirmed <- covid_agg[order(covid_agg$Confirmed),]

plot(covid_agg_confirmed$Confirmed, type = "b", 
     main = "Bestätigte COVID-19-Fälle vom 22.April 2022 pro Land", 
     xlab = "Index des Landes", ylab = "Absolute Fallzahlen",
     cex = 0.5)  
abline(a=median(covid_agg_confirmed$Confirmed),b=0, col="red")
abline(a=mean(covid_agg_confirmed$Confirmed),b=0, col="blue")

legend("topleft", pch = 16, title = "Farbcodes", text.font = 1, cex=0.75, 
       legend = c("Median", "Mittelwert"),
       col = c("red", "blue"))

#' Erkenntnisse: 
#' Man erkennt, dass einige wenige Länder hohe Fallzahlen haben als Außenseiter: 
#' covid_agg_confirmed[195:197,] -> Brasilien, Indien und die USA 
#' Die meisten Länder scheinen kaum bestätigte Corona-Fallzahlen zu haben. 
#' Da dieses Mal nur die absoluten Fallzahlen gegeben sind, kann man diesen Sachverhalt nicht näher untersuchen 
#' Der Median liegt niedriger als der Mittelwert, was auf eine schiefe Verteilung schließen lässt. 
#'    

#### e) ---- 

plot(covid_agg[,2:3])

#' Definition Bravais-Pearson-Korrelations-Koeffizient: Maß für die Stärke des linearen Zusammenhangs beider Merkmale

#' Geeignetes Korrelationsmaß: 
#' Man könnte über Pearsons-Korrelations-Koeffizienten nachdenken, da es einen linearen Zusammenhang zu geben scheint 

cor(covid_agg$Confirmed, covid_agg$Deaths, method = "pearson") #0.907031

# Zum Vergleich noch mal die Spearman-Rangkorrelation: 
cor(covid_agg$Confirmed, covid_agg$Deaths, method = "spearman") #0.9069328

#' -> Korrelation ist KEIN Beweis für Kausalität
#' Nur weil die Korrelation zwischen den bestätigten Fällen und Todesfällen hoch ist, heißt das nicht, dass das eine das andere bedingt. 
#' Hier ist auch das Alter der Betroffenen zum Beispiel zu beachten!

####  ende ----
