library(ggplot2)
herzdata=read.csv2("herzviertelmieten.csv")
herzdata$Zi=as.factor(herzdata$Zi)

herz.lm=lm(KM~qm+Zi,data=herzdata)
summary(herz.lm)
#Call:
#  lm(formula = KM ~ qm + Zi, data = herzdata)#

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-47.685 -17.510  -0.169  20.653  65.604 

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  222.2425    25.2884   8.788 4.06e-09 ***
#  qm            10.1528     0.5369  18.909 2.53e-16 ***
#  Zi2         -127.8967    18.6603  -6.854 3.49e-07 ***
#  Zi3         -351.5956    35.3833  -9.937 3.64e-10 ***
#  Zi4         -442.8894    42.4746 -10.427 1.37e-10 ***
#  ---

# Residual standard error: 31.82 on 25 degrees of freedom
# Multiple R-squared:  0.9711,	Adjusted R-squared:  0.9665 
# F-statistic: 210.1 on 4 and 25 DF,  p-value: < 2.2e-16

anova(herz.lm)
#Analysis of Variance Table

#Response: KM
#           Df Sum Sq Mean Sq F value  Pr(>F)    
#  qm        1 738943  738943 729.606 < 2e-16 ***
#  Zi        3 112367   37456  36.983 2.4e-09 ***
# Residuals 25  25320    1013 


herz.lm.interaction=lm(KM~qm*Zi,data=herzdata)
summary(herz.lm.interaction)
#Call:
#  lm(formula = KM ~ qm * Zi, data = herzdata)

# Residuals:
#  Min      1Q    Median      3Q     Max 
#-43.625 -13.899  -0.923  14.632  58.458 

#Coefficients:
#            Estimate  Std. Error t value Pr(>|t|)    
#(Intercept)   81.500     72.119   1.130   0.2706    
#qm            13.550      1.720   7.876 7.65e-08 ***
#Zi2          -22.750     85.065  -0.267   0.7916    
#Zi3         -142.454    103.526  -1.376   0.1827    
#Zi4            8.500    208.999   0.041   0.9679    
#qm:Zi2        -2.758      1.891  -1.459   0.1588    
#qm:Zi3        -4.078      1.870  -2.180   0.0402 *  
#qm:Zi4        -6.138      2.438  -2.518   0.0196 *  

#Residual standard error: 29.08 on 22 degrees of freedom
#Multiple R-squared:  0.9788,	Adjusted R-squared:  0.972 
#F-statistic: 144.9 on 7 and 22 DF,  p-value: < 2.2e-16

anova(herz.lm.interaction)
#Analysis of Variance Table

#Response: KM
#            Df Sum Sq Mean Sq  F value    Pr(>F)    
#  qm         1 738943  738943 873.7584 < 2.2e-16 ***
#  Zi         3 112367   37456  44.2893 1.715e-09 ***
#  qm:Zi      3   6714    2238   2.6465   0.07429 .  
#  Residuals 22  18606     846          

library(xtable)
xtable(herzdata[,c(2,3,4)])
ggplot(data=herzdata,aes(x=qm,y=KM,color=Zi))+geom_point()

attach(herzdata)
plot(qm,KM,col=Zi)
coeff=herz.lm.interaction$coefficients
inter=coeff[1]+c(0,coeff[3:5])
slope=coeff[2]+c(0,coeff[6:8])
for (i in 1:4){
  lines(
    c(min(qm[Zi==i]),max(qm[Zi==i])),
    inter[i]+slope[i]*c(min(qm[Zi==i]),max(qm[Zi==i])),
    col=i
    )}
detach(herzdata)
