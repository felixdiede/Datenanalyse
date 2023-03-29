# Erfasse die Daten in einem Data Frame
tankdata=data.frame(
  x1=c(6000,2500,8500,6500,9500),
  x2=c(7000,6500,3000,7000,7500),
  y=c(3000,4000,2000,3000,3500)
)

# Berechnung des linearen Modells mit R
# mit x=TRUE wird auch die Modellmatrix X zugänglich gemacht (zu unterscheiden vom Data Frame)
lm.td<-lm(y~x1+x2,data=tankdata,x=TRUE)

# Ausgabe 
summary(lm.td)
#Residuals:
#  1       2       3       4       5 
#-391.00  337.20    5.45 -332.46  380.80 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) 2161.88938 1218.11016   1.775    0.218
#x1            -0.11708    0.09667  -1.211    0.350
#x2             0.27594    0.14324   1.926    0.194
#
#Residual standard error: 511 on 2 degrees of freedom
#Multiple R-squared:  0.7627,	Adjusted R-squared:  0.5253 
#F-statistic: 3.213 on 2 and 2 DF,  p-value: 0.2373
# Parametertests und Modelltests vgl. Folie 55, 59


# Rechnungen zu Fuß
# Die Modellmatrix X
xmat<-lm.td$x
# Die Matrix (X^T*X)
xxmat<-t(xmat)%*%xmat
# Die inverse Matrix (X^T*X)^{-1}
solve(xxmat)
# Der KQ-Schätzer
solve(xxmat,t(xmat)%*%tankdata$y)
      
# Konfidenzintervall für den erwarteten Umsatz, x1=7000, x2=6000, Folie 65
x0<-c(1,7000,6000)
yhat<-lm.td$coefficients%*%x0
sigmahat<-summary(lm.td)$sigma
stddev<-sigmahat*sqrt(x0%*%solve(xxmat)%*%x0)
c(yhat)+qt(0.975,df=2)*as.vector(stddev)*c(-1,1)
# KI mit R
predict.lm(lm.td,newdata=data.frame(x1=7000,x2=6000),interval="confidence")
# KI mit R
predict.lm(lm.td,newdata=data.frame(x1=6000,x2=7000),interval="confidence")

# Prognoseintervall für den erwarteten Umsatz, Folie 69
x0<-c(1,7000,6000)
stddev<-sigmahat*sqrt(1+x0%*%solve(xxmat)%*%x0)
c(yhat)+qt(0.975,df=2)*as.vector(stddev)*c(-1,1)
#PI mit R
predict.lm(lm.td,newdata=data.frame(x1=6000,x2=7000),interval="prediction")
# Diagnose-Plots
#Standardisierte Residuen
resstd<-lm.td$residuals/summary.lm(lm.td)$sigma

# qq-Plot
qqnorm(resstd)
qqline(resstd) # Ausgleichsgerade, erklärt  durch untere und obere Quartile
points(
  qnorm(c(0.25,0.75)),
  quantile(resstd,c(0.25,0.75)),
  col="red"
)
# direkte Konstruktion
plot(
  qnorm(c(0.1,0.3,0.5,0.7,0.9)),
  sort(resstd)
  )
qqline(resstd) # Ausgleichsgerade, erklärt  durch untere und obere Quartile
points(
  qnorm(c(0.25,0.75)),
  quantile(resstd,c(0.25,0.75)),
  col="red"
)

# plot fitted values versus standardized residuals
plot(lm.td$fitted.values,resstd)

## plot kann direkt die Schaubilder erzeugen, wenn ein lm-Objekt übergeben wird
plot(lm.td,which=1)  # residuals versus fitted
plot(lm.td,which=2)  # QQ-Plot

plot(lm.td)

