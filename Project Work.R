library(readxl)
mydata1 <- read_excel("mydata1.xlsx")
View(mydata1)

#Punto 1 - Missing o valori anomali nella variabile ETA
table(mydata1$ETA,useNA = "ifany") # non ci sono NA quindi no missing, ma si notano degli outliers
summary(mydata1$ETA) #uso summary per vederre minimo e massimo --> ci sono outliers
boxplot(mydata1$ETA) #noto che ci sono dei valori inferiori a 0 e superiori a 100,200,300 nel boxplot

#Punto 2
table(mydata1$STATOCIV, useNA="ifany") #frequenze assolute variabile STATOCIV
prop.table(table(mydata1$STATOCIV))    #frequenze relative
numero_sposati<-nrow(mydata1[mydata1$STATOCIV == 1, ]) #uso nrow per contare il numero di sposati (supponendo che 1 significhi sposato)
print(numero_sposati) #stampo numero sposati

#Punto 3
table(mydata1$PENSIONE, useNA = "ifany") # frequenze assolute (0=NO, 1=SI)
prop.table(table(mydata1$PENSIONE)) #frequenze relative
pensionati <- nrow(mydata1[mydata1$PENSIONE == 1, ]) #conto il numero dei pensionati con nrow
totale <- length(mydata1$SODDLAV) #length per trovare il totale (anche se sappiamo che è 6400 perchè sono tutte le variabili)
perc_pensionati <- (pensionati / totale) * 100 # trovo la percentuale di pensionati
print(perc_pensionati) #stampo la percentuale di pensionati

#Punto 4
table(mydata1$CATREDD) #frequenze assolute delle categorie di reddito
#Dalla funzione table notiamo che la classe con le frequenze assolute più alte è 25-49, quindi, essendo che le classi hanno stessa ampiezza, è la classe modale
which.max(table(mydata1$CATREDD)) # altrimenti possiamo usare questo comando, which.max, che ci consente di restituirci direttamente lui la classe modale.

#Punto 5
var(mydata1$REDDITO, na.rm = TRUE) #varianza
sd(mydata1$REDDITO, na.rm = TRUE) #sqr
sd(mydata1$REDDITO, na.rm = TRUE) / mean(mydata1$REDDITO, na.rm = TRUE) #coefficiente di variazione
#Uso questi indici per descrivere la variabilità della variabile REDDITO

#Punto 6
summary(mydata1$IMPIEGO) # la funzione summary mi consente di ottenre già i vari indici di posizione più rilventi
var(mydata1$IMPIEGO) # varianza
sd(mydata1$IMPIEGO) #sqr

#Punto 7 
eta2 <- subset(mydata1,mydata1$ETA >= 0 & mydata1$ETA <= 100,select= c(ETA)) #uso subset 2 per creare eta2 che non contiene gli outliers ma solo valori da 0 a 100
classi<-cut(eta2$ETA, breaks=c(0,25,50,75,100), include.lowest=TRUE, right=TRUE) # divido in classi, chiudendo a destra, separando ogni 25
table(classi) # frequenza assoluti classi
hist(eta2$ETA, breaks=c(0,25,50,75,100)) #istogramma per rappresentare la variabile REDDITO con la divisone in classi
barplot(table(classi)) #faccio lo stesso con barplot
min(eta2$ETA) #minimo
max(eta2$ETA) #massimo
mean(eta2$ETA) #media
median(eta2$ETA) #mediana
quantile(eta2$ETA) #quantile

#Punto 8
table(mydata1$SODDLAV) #distribuzione delle frequenze assolute di SODDLAV
non.sodd<-nrow(mydata1[mydata1$SODDLAV == -1 | mydata1$SODDLAV == -2,]) #uso nrow per trovare gli insoddisfatti del lavoro, ovvero SODDLAV = -1 e -2
totale <- length(mydata1$SODDLAV) #length per trovare il totale (anche se sappiamo che è 6400 perchè sono tutte le variabili)
perc.non.sodd <- (non.sodd/totale)*100 #trovo percentuale di insoddisfatti
print(perc.non.sodd) #stampo percentuale insoddisfatti

#Punto 9
table(mydata1$PC) #frequenze assolute PC (0=NO, 1=SI)

# grafico
pie(table(mydata1$PC),labels = c("No", "Sì"),col = c("red", "green")) #uso pie per fare un grafico a torta che rappresenti la percentuale di si e no. Labels mi consente di associare 0=NO 1=SI, e col di colorare per distinguere

#Punto 10
library(DescTools) #mi serve la library DescTools per poter usare l'indice di Gini direttamente
Gini(table(mydata1$SODDLAV)) #Gini di SODDLAV= 0.05992188 è maggiore di quello di PC --> presenta una maggiore eterogeneità
Gini(table(mydata1$PC)) #Gini di PC = 0.121875

#Punto 11 
table(mydata1$SESSO, mydata1$CATREDD) #creo tabella a doppia entrata
chisq.test(table(mydata1$SESSO, mydata1$CATREDD)) #test chi-quadro per l'indipendenza.P-value = 0.2691 è molto alto, quindi accetto Ho e quindi c'è indipendenza tra le due variabili

#Punto 12
# Estraggo i dati di maschi e femmine selezionando solo SESSO e REDDITO tramite subset
maschio<-subset(mydata1,mydata1$SESSO == 2,select= c(SESSO, REDDITO)) 
femmina<-subset(mydata1,mydata1$SESSO == 1,select= c(SESSO, REDDITO))
var.test(maschio$REDDITO, femmina$REDDITO,alternative="two.sided") #test di omoschedasticità. P-value = 2.642e-05, molto piccolo, quindi rifiuto HO e c'è eteroschedasticità

t.test(maschio$REDDITO, femmina$REDDITO, alternative= "two.sided", var.equal=FALSE) #P-VALUE= 0.4502 alto, quindi accetto H0 e non c'è una differenza statisticamente significativa tra il reddito medio di maschi e femmine.

#Punto 13
# Creazione dei sottogruppi di sposati e non sposati, selezionando solo SESSO e PC
sposato<-subset(mydata1,mydata1$STATOCIV == 1,select= c(SESSO, PC))
non.sposati<-subset(mydata1,mydata1$STATOCIV == 0,select= c(SESSO, PC))
# Conteggio del numero di individui che possiedono un PC in ciascun gruppo
sposati.con.pc <- nrow(subset(sposato, PC == 1))
non.sposati.con.pc <- nrow(subset(non.sposati, PC == 1))
#Test per il confronto di proporzioni per due popolazioni indipendenti
prop.test( x = c(sposati.con.pc, non.sposati.con.pc),n = c(nrow(sposato), nrow(non.sposati)),alternative = "two.sided") #p-value = 0.6386 è alto, accetto Ho e quindi non c'è una differenza signfiicativa tra le due proporzioni

#Punto 14
#QQ plot per verificare la normalità
qqnorm(mydata1$REDDITO) #dal grafico non si denota una distribuzione normale; ha l'andamento esponenziale
qqline(mydata1$REDDITO)
reddito.std<-(mydata1$REDDITO-mean(mydata1$REDDITO))/sqrt(var(mydata1$REDDITO)) #standardizzo il reddito
ks.test(reddito.std, "pnorm") #test di Kolmogorov-Smirnov per normalità. P-value= < 2.2e-16 molto basso, rifiuto Ho e quindi i dati non seguono una distribuzione normale

library(readxl)
mydata2 <- read_excel("mydata2.xlsx")
View(mydata2) 

#Punto 1 le variabili quantitative dipendenti sono: ETA, IMPIEGO E NFAM
cor(mydata2)
cor(mydata2[c("REDDITO", "ETA","IMPIEGO", "NFAM")]) #per creare la matrice di correlazione solo con le variabili quantitative
#La variabile IMPIEGO è la più correlata positivamente con REDDITO, seguita da ETA.
#La variabile NFAM mostra una correlazione molto bassa e negativa, quindi non è significativamente correlata con il reddito.
#la correlazione più alta con reddito è quella di impiego cor=0,57893146

#Punto 2
mod_mult1 <- lm(REDDITO ~ ETA + IMPIEGO + NFAM, data = mydata2)
summary(mod_mult1) #noto cheil p-value di NFAM è molto alto e quindi risulta essere non significativa, è meglio rimuoverla

mod_mult2 <- lm(REDDITO ~ ETA + IMPIEGO, data = mydata2) #copia e incolla
summary(mod_mult2)

#Punto 3
plot(mod_mult2)


#Ho verificato le ipotesi del modello REDDITO ~ ETA + IMPIEGO attraverso l’analisi grafica dei residui (con il comando plot(mod_mult2)) e i test F e t.
# Dal grafico residuals vs fitted value si nota una maggiore dispersione dei residui per valori più alti di REDDITO predetto (asse X). Questo suggerisce eteroscedasticità, ovvero che la varianza degli errori non è costante — un problema comune che viola le ipotesi della regressione lineare.
#I residui si discostano significativamente dalla linea, soprattutto nelle code (valori estremi).
# Questo suggerisce che i residui non seguono una distribuzione normale, in particolare mostrano code pesanti (heavy tails) a destra.
# La presenza di un aumento della dispersione dei residui al crescere dei valori predetti (come si nota nel grafico) indica eteroscedasticità: la varianza dei residui non è costante
# Il modello contiene alcuni outlier e punti con leverage elevato, ma nessuno sembra influenzare in modo eccessivo il modello. Tuttavia, vale la pena verificare e valutare questi casi più nel dettaglio
#Punto4
sd(mydata2$REDDITO)
sd(mydata2$ETA)
sd(mydata2$IMPIEGO)
# B0 --> esprime il reddito medio per una persona con età zero e anni di impiego paria a 0
# B1 --> esprime la riduzione del reddito medio a seguito di un aumento di 1 anno dell'età, a parità di anni di impiego
# B2 --> esprime l'aumento del reddito medio a seguito di un aumento di 1 anno di impiego, a parità di età.

beta.eta.std <- -0.2182 * (sd(mydata2$REDDITO) / sd(mydata2$ETA))
print(beta.eta.std)
beta.impegno.std <- 4.8582 * (sd(mydata2$REDDITO) / sd(mydata2$IMPIEGO))
print(beta.impegno.std)
# abbiamo normalizzato i coefficienti per capire quale sia il più importante, perchè sono su scale diverse

#Punto 5
# Crea la dummy per il sesso (uomo = 1, donna = 0)
mydata2$dummy_maschio <- ifelse(mydata2$SESSO == 2, 1, 0)

# Costruisci il modello con tutte le variabili dummy
mod_dummies <- lm(REDDITO ~ ETA + IMPIEGO + PENSIONE + CELLUL + TABLET + `LINEA WIFI` + LAPTOP + TV + `PC FISSO` + `E-BOOK` + `TEL FISSO` + DOMOTICA + dummy_maschio, data = mydata2)

# Visualizza l'output
summary(mod_dummies)


#Punto 6
mod_dummies2 <- lm(REDDITO ~ ETA + IMPIEGO + PENSIONE  + CELLUL + TABLET + `TEL FISSO` + DOMOTICA, data = mydata2)
summary(mod_dummies2)
#Il test F risulta essere significativo, infatti p-value < 2.2e-16. Mentre l'R quadro risulta essere molto basso = 0,07799

#Punto 7
# Ogni coefficiente indica quanto cambia il reddito atteso (in media) per chi possiede la caratteristica indicata (dummy = 1), rispetto a chi non la possiede (dummy = 0), mantenendo costanti le altre variabili
# Ad esempio:
#B1 (pensione):il reddito medio diminuisce di 43,832 (unità di misura) quando si passa da chi non è pensionato a chi è pensionato, a parità di altre variabili

#Punto 8
# Modello con ISTRUZ
mod_istruz <- lm(REDDITO ~ factor(ISTRUZ), data = mydata2)
summary(mod_istruz)

# Modello con SODDLAV
mod_soddlav <- lm(REDDITO ~ factor(SODDLAV), data = mydata2)
summary(mod_soddlav)
#Modello con STATOCIV
mod_statociv <- lm(REDDITO ~ factor(STATOCIV), data = mydata2)
summary(mod_statociv)

mydata2$SODD_M_1 <- ifelse(mydata2$SODDLAV == -1, 1, 0)
mydata2$SODD_0 <- ifelse(mydata2$SODDLAV == 0, 1, 0)
mydata2$SODD_P_1 <- ifelse(mydata2$SODDLAV == 1, 1, 0)
mydata2$SODD_P_2 <- ifelse(mydata2$SODDLAV == 2, 1, 0)


mod_politomica <- lm(REDDITO ~ ETA + IMPIEGO + PENSIONE + CELLUL + TABLET + `TEL FISSO` + DOMOTICA + SODD_M_1 + SODD_0 + SODD_P_1 + SODD_P_2, data = mydata2)
summary(mod_politomica)



#Il test F globale è altamente significativo (p-value ≪ 0.001), quindi almeno una delle variabili del modello spiega una quota significativa della variabilità di REDDITO.
#Quasi tutti i test t sui coefficienti sono significativi → queste variabili hanno un effetto individualmente rilevante su REDDITO
