
# Installerar nödvändiga paket inom tidsserier 

library(TSA)
library(nsRFA)
library(uroot)
library(tseries)
library(summarytools)

###################################################################################
###################################################################################
########################### 1. Korttidsprognos ###################################
###################################################################################


###################################################################################
                      ### Ta fram data ## 
abisko <- abiskodata_4man[abiskodata_4man$Tid..UTC. == "06:00:00",] # Väljer ut alla variabler där 06:00 ingår, dvs morgontemperatur
korttid.ts <- ts(abisko$Lufttemperatur, frequency = 1)  # number of observations per unit of time
plot(korttid.ts, type="o", ylab='Temperatur', xlab='Tid')




###################################################################################
                    ## Identifiera modell ######
### Kan det vara en random walk? 
model1=lm(korttid.ts~time(korttid.ts))
summary(model1)
plot(korttid.ts, type="o", ylab='Temperatur', xlab='Tid')
abline(modell.borjan) # add the fitted least square line from model1


##### Adjusted R-squared says that approx 43% of the variation in the random
# walk series is explained by the linear time trend. 


#Bygger s?songsmodell
korttid.sasong <- ts(abisko$Lufttemperatur, frequency = 5)  # sept-jan = 5 m?nader 
month.=season(korttid.sasong) # period added to improve table display
model2=lm(korttid.sasong~month.-1) # -1 removes the intercept term (if not doing so, january will fall off)
summary(model2)

# Enbart 18% av variationen i tidsserien f?rklaras av s?songsmodellen. Inte en bra modell. 

# Provar en cyklisk modell 
# ?ndrar  datan till 131 obs pe
korttid.cyklisk <- ts(abisko$Lufttemperatur, frequency = 7)  # number of observations per unit of time

har.=harmonic(korttid.cyklisk,1)
model4=lm(korttid.cyklisk~har.)
summary(model4)

# Minimal adj r-squared med denna modell. B?sta Adj R^2 blev med linj?r modell. 

###################################################################################
                    ## Skatta parametrar i modellen ######
par(mfrow=c(2,2))
plot(y=rstudent(model1),x=as.vector(time(korttid.ts)), 
     ylab='Standardized Residuals',xlab='Time',type='o')
plot(y=rstudent(model1),x=fitted(model1), 
     ylab='Standardized Residuals',xlab='Fitted Trend Line Values', 
     type='p')
acf(rstudent(model1))

runs(rstudent(model1)) # Testar oberoende i den stokastiska komponenten

qqnorm(korttid.ts); qqline(korttid.ts)

hist(rstudent(model1))

plot(y=rstudent(model2),x=as.vector(time(korttid.sasong)),
       xlab='Time',ylab='Standardized Residuals',type='o')

LB.test(model1, lag = 20, type= c("Ljung-Box"))

adf.test(korttid.ts) # Tidsserien verkar vara stationär. 

eacf(rstudent(model1)) # Enligt eacf en MA(1)-MA(5) process

arima(korttid.ts, order=c(0, 0, 2), method = c("CSS"))# MK-metoden
arima(korttid.ts, order=c(0, 0, 2), method = c("ML"))  # ML-metoden



###################################################################################
                           ##  Prediktion ######
## Linjär modell  ######
abisko.linjar <- abiskodata_4man[abiskodata_4man$Tid..UTC. == "06:00:00",]# Väljer ut alla variabler där 06:00 ingår, dvs morgontemperatur
abisko.linjar <- abisko.linjar[1:117,c("Lufttemperatur")] # Kapar vid 117 så att den predikterar från 118.
abisko.linjar <- ts(abisko.linjar, frequency = 1)  # number of observations per unit of time

linjar.pred=arima(abisko.linjar,order=c(0,0,4)) # Moving average med 4 lags, där förväntat värde blir 0 på 5> femte laget.
prediktion<- plot(linjar.pred, n.ahead=14, n1=c(117),type='b', xlab='Prediktion MA(4)')

### Cyklisk modell

korttid.ts <- ts(abisko$Lufttemperatur, frequency = 7)  # number of observations per unit of time
period.<-harmonic(korttid.ts) # Identifiera periodisk komponent.
minmodel=arima(korttid.ts,order=c(0,0,2),xreg=period.)
# kombinationen av ARIMA(p,d,q) + periodisk komponent.
newperiod.<-harmonic(ts(rep(1,14), start=c(1,131),freq=7),1)
# freq 7 -- veckoperiodisk
predresultat<-plot(minmodel,n.ahead=14,n1=c(0,1),newxreg=newperiod., type="o", xlab="14 dagar")
# Predresultat ger predikterat v\"{a}rde med \"{o}vre och nedre gr\"{a}ns i KI



###################################################################################
                    ##  Korsvalidera modellen  ######
# För att kunna jämföra predikterade och riktiga värden, ändrar jag så att 
# de predikterade värdena predikterar värden som finns i materialet startar från värde 117.

## Linjär modell  ######
abisko.linjar <- abiskodata_4man[abiskodata_4man$Tid..UTC. == "06:00:00",]# Väljer ut alla variabler där 06:00 ingår, dvs morgontemperatur
abisko.linjar <- abisko.linjar[1:117,c("Lufttemperatur")] # Kapar vid 117 så att den predikterar från 118.
abisko.linjar <- ts(abisko.linjar, frequency = 1)  # number of observations per unit of time

linjar.pred=arima(abisko.linjar,order=c(0,0,4)) # Moving average med 4 lags, där förväntat värde blir 0 på 5> femte laget.
prediktion<- plot(linjar.pred, n.ahead=14, n1=c(117),type='b', xlab='Prediktion MA(4)')

yhatt.linjar <-prediktion$pred
y.linjar <- abisko[118:131,c("Lufttemperatur")]

#Kollar så de har samma längd
length(yhatt.linjar)
length(y.linjar)

# Japp, båda är 14. Nu en loop som tar Summa(yhatt-y)^2 på varje rad. 

presstabell.linjar <- data.frame(y.linjar,yhatt.linjar) # Sätter samman dem till en dataframe

presstabell.linjar$pressfkn <- ((presstabell.linjar$y.linjar - presstabell.linjar$yhatt.linjar)^2) # Lägger till en ny rad med funktion

press.linjar <- sum(presstabell.linjar$pressfkn) # summerar pressvärdet

press.linjar


## Säsongsmodell  ######
abisko <- abiskodata_4man[abiskodata_4man$Tid..UTC. == "06:00:00",] # Väljer ut alla variabler där 06:00 ingår, dvs morgontemperatur
abisko.sasong <- ts(abisko$Lufttemperatur, frequency = 5)  # number of observations per unit of time

#period.<-season(abisko.sasong)# Identifiera periodisk komponent.
#minmodel=arima(abisko.sasong,order=c(0,0,4),xreg=period.);
# kombinationen av ARIMA(p,d,q) + periodisk komponent.
#> newperiod<-harmonic(ts(rep(1,14), start=c(veckonummer,dagiveckan),freq=7),1);
# freq 7 -- veckoperiodisk
#> predresultat<-plot(minmodel,n.ahead=14,n1=c(0,1),newxreg=newperiod.)
# Predresultat ger predikterat v\"{a}rde med \"{o}vre och nedre gr\"{a}ns i KI.


abisko.sasong <- abisko$Lufttemperatur 
abisko.sasong <- ts(abisko.sasong, frequency = 5)  # number of observations per unit of time
month.<-season(abisko.sasong) # Identifiera periodisk komponent.

model2=arima(abisko.sasong, order=c(0,0,4), xreg=month.)

prediktionsperiod<-season(ts(rep(1,14), start=c(1,131),freq=5),1)
# freq 7 -- veckoperiodisk
prediktion<-plot(model2, n.ahead=14,n1=c(131), newxreg=prediktionsperiod, type="l", xlab="14 dagar")


month.=season(abisko.sasong) # period added to improve table display
sasong.model=arima(abisko.sasong,order=c(0,0,2), xreg = month.)

# kombinationen av ARIMA(p,d,q) + periodisk komponent.

prognosperiod.<-season(ts(rep(1,14), start=c(1,117),freq=5),1)

prognosperiod.<-harmonic(ts(rep(1,14), start=c(1,117),freq=7),1)
# freq 7 -- veckoperiodisk
prediktion<-plot(prognosmodel,n.ahead=14,n1=c(1,117),newxreg=prognosperiod., type="o", xlab="14 dagar")
# Predresultat ger predikterat v\"{a}rde med \"{o}vre och nedre gr\"{a}ns i KI

yhatt.sasong <-sasong.pred$pred
y.sasong <- abisko[118:131,c("Lufttemperatur")]

#Kollar så de har samma längd
length(yhatt.sasong)
length(y.sasong)

# Japp, båda är 14. Nu en loop som tar Summa(yhatt-y)^2 på varje rad. 

presstabell.sasong <- data.frame(y.sasong,yhatt.sasong) # Sätter samman dem till en dataframe
presstabell.sasong$pressfkn <- ((presstabell.sasong$y.sasong - presstabell.sasong$yhatt.sasong)^2) # Lägger till en ny rad med funktion
press.sasong <- sum(presstabell.sasong) # summerar pressvärdet

press.sasong

### Cyklisk modell

abiskoprognos <- abiskodata[abiskodata$Tid..UTC. == "06:00:00",]# Väljer ut alla variabler där 06:00 ingår, dvs morgontemperatur
abiskoprognos <- abisko[1:117,c("Lufttemperatur")] # Kapar vid 117 så att den predikterar från 118.
korttid.pred <- ts(abiskoprognos, frequency = 7)  # number of observations per unit of time

periodprognos.<-harmonic(korttid.pred) # Identifiera periodisk komponent.
prognosmodel=arima(korttid.pred,order=c(0,0,2),xreg=periodprognos.)
# kombinationen av ARIMA(p,d,q) + periodisk komponent.
prognosperiod.<-harmonic(ts(rep(1,14), start=c(1,117),freq=7),1)
# freq 7 -- veckoperiodisk
prediktion<-plot(prognosmodel,n.ahead=14,n1=c(1,117),newxreg=prognosperiod., type="o", xlab="14 dagar")
# Predresultat ger predikterat v\"{a}rde med \"{o}vre och nedre gr\"{a}ns i KI

yhatt <-prediktion$pred
y <- abisko[118:131,c("Lufttemperatur")]

#Kollar så de har samma längd
length(yhatt)
length(y)

# Japp, båda är 14. Nu en loop som tar Summa(yhatt-y)^2 på varje rad. 

presstabell <- data.frame(y,yhatt) # Sätter samman dem till en dataframe

presstabell$pressfkn <- ((presstabell$y - presstabell$yhatt)^2) # Lägger till en ny rad med funktion

press.har <- sum(presstabell$pressfkn) # summerar pressvärdet



###################################################################################
###################################################################################
########################### 2. Prognoshorisont ###################################
###################################################################################


abisko <- abiskodata[abiskodata$Tid..UTC. == "06:00:00",] # Väljer ut alla variabler där 06:00 ingår, dvs morgontemperatur
korttid.ts <- ts(abisko$Lufttemperatur, frequency = 7)  # number of observations per unit of time


# Hela datamaterialet, 131 observationer
par(mfrow=c(2,2))
period.<-harmonic(korttid.ts) # Identifiera periodisk komponent.
minmodel=arima(korttid.ts,order=c(0,0,2),xreg=period.)
# kombinationen av ARIMA(p,d,q) + periodisk komponent.
newperiod.<-harmonic(ts(rep(1,7), start=c(1,131),freq=7),1)
# freq 7 -- veckoperiodisk
predresultat<-plot(minmodel,n.ahead=7,n1=c(1,131),newxreg=newperiod., type="o", xlab="131 observationer")
# Predresultat ger predikterat v\"{a}rde med \"{o}vre och nedre gr\"{a}ns i KI


## 100 observationer
period.<-harmonic(korttid.ts) # Identifiera periodisk komponent.
minmodel=arima(korttid.ts,order=c(0,0,2),xreg=period.)
# kombinationen av ARIMA(p,d,q) + periodisk komponent.
newperiod.<-harmonic(ts(rep(1,7), start=c(1,100),freq=7),1)
# freq 7 -- veckoperiodisk
predresultat<-plot(minmodel,n.ahead=7,n1=c(1,100),newxreg=newperiod., type="o", xlab="100 observationer")
# Predresultat ger predikterat v\"{a}rde med \"{o}vre och nedre gr\"{a}ns i KI

## 50 observationer
period.<-harmonic(korttid.ts) # Identifiera periodisk komponent.
minmodel=arima(korttid.ts,order=c(0,0,2),xreg=period.)
# kombinationen av ARIMA(p,d,q) + periodisk komponent.
newperiod.<-harmonic(ts(rep(1,7), start=c(1,50),freq=7),1)
# freq 7 -- veckoperiodisk
predresultat<-plot(minmodel,n.ahead=7,n1=c(1,50),newxreg=newperiod., type="o", xlab="50 observationer")
# Predresultat ger predikterat v\"{a}rde med \"{o}vre och nedre gr\"{a}ns i KI




###################################################################################
###################################################################################
########################### 3. Langtidsmatning av temperaturen ####################
###################################################################################


###################################################################################
### Ta fram data ## 
abisko_lang <- abisko_lang[abisko_lang$Tid..UTC. == "06:00:00",] # Väljer ut alla variabler där 06:00 ingår, dvs morgontemperatur
langtid.ts <- ts(abisko_lang$Lufttemperatur, frequency = 365, start=c(1966)) # number of observations per unit of time
plot(langtid.ts, ylab='Temperatur', type='b')

###################################################################################
## Identifiera modell ######

# Provar en cyklisk modell 

har.=harmonic(langtid.ts,1)
cykliskmodell=lm(langtid.ts~har.)
summary(cykliskmodell)
# Förklarar 66% 

plot(ts(fitted(cykliskmodell),freq=365,start=c(2019,1)), 
     ylab='Temperature',type='l', ylim=range(c(fitted(cykliskmodell),langtid.ts))); points(langtid.ts)


#Bygger sasongsmodell
abisko_lang <- abisko_lang[abisko_lang$Tid..UTC. == "06:00:00",] # Väljer ut alla variabler där 06:00 ingår, dvs morgontemperatur
langtid.ts <- ts(abisko_lang$Lufttemperatur, frequency = 365) # number of observations per unit of time
month.=season(langtid.ts) # period added to improve table display
model2=lm(langtid.ts~month.-1) # -1 removes the intercept term (if not doing so, january will fall off)
summary(model2)
# Förklarar 67% 


###################################################################################
## Skatta parametrar i modellen ######
par(mfrow=c(2,2))
runs(rstudent(model2)) # Testar oberoende i den stokastiska komponenten
eacf(rstudent(model2)) # Enligt eacf en ARMA(4,4) process
arima(month., order=c(4, 0, 4), method = c("ML"))# ML-metoden
adf.test(month.) # stationär, 26 lags.


###################################################################################
## Verifiera modellen ######
par(mfrow=c(4,2))
acf(rstudent(model2))
pacf(rstudent(model2))
plot(rstudent(model2))
qqnorm(rstudent(model2)); qqline(rstudent(model2))
hist(rstudent(model2))
plot(y=rstudent(model2),x=as.vector(time(langtid.ts)),
     xlab='Time',ylab='Standardized Residuals',type='o')

plot(y=rstudent(model2),x=as.vector(fitted(model2)), 
     xlab='Fitted Trend Values',
     ylab='Standardized Residuals',type='n')
points(y=rstudent(model2),x=as.vector(fitted(model2)), 
       pch=as.vector(season(langtid.ts)))

LB.test(model2, lag = 26, type= c("Ljung-Box")) # Box-Ljung test signifikant


###################################
### Använd modellen för att avgöra ifall det har skett någon signikant höjning
# av temperaturen sen mätningarna startade, och om så är fallet hur stor den är.
abisko_lang <- abisko_lang[abisko_lang$Tid..UTC. == "06:00:00",] # Väljer ut alla variabler där 06:00 ingår, dvs morgontemperatur
langtid.ts <- ts(abisko_lang$Lufttemperatur, frequency = 7) # number of observations per unit of time


## 50 observationer
period.<-harmonic(langtid.ts) # Identifiera periodisk komponent.
minmodel=arima(langtid.ts,order=c(2,0,2),xreg=period.)
# kombinationen av ARIMA(p,d,q) + periodisk komponent.
newperiod.<-harmonic(ts(rep(1,12), start=c(1,55),freq=365),1)
# freq 7 -- veckoperiodisk
predresultat<-plot(minmodel,n.ahead=55,n1=c(1,55),newxreg=newperiod., type="o", xlab="50 observationer")
# Predresultat ger predikterat v\"{a}rde med \"{o}vre och nedre gr\"{a}ns i KI




## 95% intervall på interceptet då och interceptet nu. Så my-my * 1,96*3/roten ur n. ? 



#Har man samma modell i början som i slutet av mätserien?
# Delar upp tidsserien i två delar.
Borjan <- abisko_lang[abisko_lang$Tid..UTC. == "06:00:00",] # Väljer ut alla variabler där 06:00 ingår, dvs morgontemperatur
Borjan <- ts(Borjan$Lufttemperatur, frequency = 365) # number of observations per unit of time
intercept.borjan <- Borjan[1:365]


month.=season(Borjan) # period added to improve table display
modell.borjan=lm(Borjan~month.-1) # -1 removes the intercept term (if not doing so, january will fall off)
summary(modell.borjan)


Tjugotalet <- abisko_lang[abisko_lang$Tid..UTC. == "06:00:00",] # Väljer ut alla variabler där 06:00 ingår, dvs morgontemperatur
Tjugotalet <- ts(Tjugotalet$Lufttemperatur, frequency = 12, start = 2021) # number of observations per unit of time
month2.=season(Tjugotalet) # period added to improve table display
model2=lm(Tjugotalet~month2.-1) # -1 removes the intercept term (if not doing so, january will fall off)
summary(model2)

### De två modellerna ger samma resultat i summary. 
## Verifiera modellerna ######
par(mfrow=c(3,2))
acf(rstudent(model2))
acf(rstudent(model3))

pacf(rstudent(model2))
pacf(rstudent(model3))

plot(y=rstudent(model3),x=as.vector(time(Sjuttio.Nittiotalet)),

         xlab='Time',ylab='Standardized Residuals',type='o')
plot(y=rstudent(model3),x=as.vector(time(Tjugotalet)),
     xlab='Time',ylab='Standardized Residuals',type='o')



plot(y=rstudent(model1),x=as.vector(fitted(model1)), 
     xlab='Fitted Trend Values',
     ylab='Standardized Residuals',type='n')
points(y=rstudent(model2),x=as.vector(fitted(model2)), 
       pch=as.vector(season(langtid.ts)))
qqnorm(rstudent(model2)); qqline(rstudent(model2))
hist(rstudent(model2))


plot(ts(fitted(cykliskmodell),freq=365,start=c(2019,1)), 
     ylab='Temperature',type='l', ylim=range(c(fitted(cykliskmodell),langtid.ts))); points(langtid.ts)



#Har sannolikheten för perioder av extrem kyla och värme ändrats under
#mätperioden? 
# Borde vara om konfidensintervallet ändrats under mätperioden. 

acf(as.vector(model2),lag.max=26) # Ser en autokorrelation
plot(diff(langtid.ts),ylab='1 diff temp Abisko',xlab='Tid')

plot(diff(diff(langtid.ts),lag=12),xlab='Time', 
     ylab='First and Seasonal Difference of CO2')

acf(as.vector(diff(diff(langtid.ts),lag=20)),lag.max=40,ci.type='ma')


