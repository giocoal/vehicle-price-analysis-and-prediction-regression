# Foundation of Statistics
# Simulazione progetto
# 4 novembre 2021

path <- "/Users/mauge/Desktop/Università/DATA SCIENCE/Found. of Statistics/R/Data/housing.csv"
house <- read.delim(path, header = TRUE, sep=",")

# A ANALISI ESPLORATIVA DEI DATI
# 1 Outliers?
# 2 Missing values?
# 3 Analisi delle distribuzioni
# Indici di posizione e variabilità
# Istogrammi, boxplot, scatterplot
# 4 Analisi della correlazione

summary(house)
# Si tratta di un dataset con 10 variabili e 20640 osservazioni.
# La variabile total_bedrooms presenta 207 valori mancanti
# Si decide di eliminare quelle osservazioni.

# Rimozione dei valori na:
library(tidyr)
house <- house %>% drop_na()

# Estrazione delle variabili numeriche:
nums <- unlist(lapply(house, is.numeric))

# Prendo un campione di osservazioni per rendere più agile la rappresentazione grafica:
leggero <- house[sample(nrow(house), 500), ]

# Matrice dei diagrammi di dispersione:
pairs(leggero[, nums])

# Le correlazioni che appaiono subito molto marcate sono:
# longitude e latitude (negativa)
# total_rooms con total_bedrooms, population e households
# median_income e median_house_value (positiva)
# households e population (positiva)

# Calcolo la matrice di correlazione:
cor(house[,nums])

# Effettivamente ci sono dei valori molto elevati come quelli tra:
# latitude e longitude
# total_rooms e total_bedrooms
# total_rooms e population
# total_rooms e households
# total_bedrooms e households
# total_bedrooms e population
# population e households

# Decido di eliminarne alcune
# Elimino total_bedrooms (è quella col valore più elevato 0.98 circa)
house <- house[,-5]
nums <- unlist(lapply(house, is.numeric))
# Non sarebbe necessario ricalcolare le correlazioni, ma la ristampo per una questione di facilità di lettura:
cor(house[,nums])

# Elimino longitude (anche se latitude e longitude non è indispensabile eliminarle) -0.92
house <- house[,-1]
nums <- unlist(lapply(house, is.numeric))
# Non sarebbe necessario ricalcolare le correlazioni, ma la ristampo per una questione di facilità di lettura:
cor(house[,nums])

# Elimino households 0.92
house <- house[,-5]
nums <- unlist(lapply(house, is.numeric))
# Non sarebbe necessario ricalcolare le correlazioni, ma la ristampo per una questione di facilità di lettura:
cor(house[,nums])

# Elimino population
house <- house[,-4]
nums <- unlist(lapply(house, is.numeric))
# Non sarebbe necessario ricalcolare le correlazioni, ma la ristampo per una questione di facilità di lettura:
cor(house[,nums])
# Ora rimangono solo median_income e median_house_value con una correlazione abbastanza elevata (0.69) ma non troppo.

# Analizzo le variabili che rimangono
par(mfrow=c(2,3))
attach(house)
boxplot(latitude, main='Latitude')
boxplot(housing_median_age, main='Housing_median_age')
boxplot(total_rooms, main='Total_rooms')
boxplot(median_income, main='Median_income')
boxplot(median_house_value, main='Median_house_value')
detach(house)
par(mfrow=c(1,1))

# Latitude: abbastanza asimmetrica a destra
# Housing_median_age: sembra simmetrica
# Total_rooms: molto asimmetrica a destra, moltissimi outliers
# Median_income: asimmetrica a destra, molti outliers nella coda di destra
# Median_house_value: leggermente asimmetrica a destra, qualche outlier nella coda di destra

# Si può decidere di applicare la trasformazione logaritmica alle variabili molto asimmetriche:
house$log_total_rooms <- log(house$total_rooms)
house$log_median_income <- log(house$median_income)

par(mfrow=c(1,2))
boxplot(house$log_total_rooms, main='Log_total_rooms')
boxplot(house$log_median_income, main='Log_median_income')
par(mfrow=c(1,1))
# Mmmmh.. le variabili sono più simmetriche ma rimangono comunque moltissimi outliers.
# Quali considerare?
# In questo caso considero le variabili originali per comodità, ma non so se è giusto.
house <- house[,-7]
house <- house[,-7]
# Occhio! Hanno lo stesso numero ma non è un errore, è perchè il dataset si aggiorna dopo aver mandato in esecuzione il primo comando.

# Sarebbe interessante aggiungere anche un indicatore di asimmetria:
library(psych)
skew(house$latitude) #0.4649
skew(house$housing_median_age) #0.0616
skew(house$total_rooms) #4.1582
skew(house$median_income) #1.6443
skew(house$median_house_value) #0.9781
# Effettivamente le distribuzioni sono tutte asimmetriche a destra
# Ma quelle che presentano l'assimetria maggiore sono total_rooms e median_income.

# Per curiosità calcolo la skewness delle tarsformate logaritmiche di queste due variabili:
skew(log(house$total_rooms)) #-1.1030 è diventata asimmetria a sinistra!
skew(log(house$median_income)) #-0.1475 anche questa è diventata asimmetria a sinistra, ma più piccola

leggero <- house[sample(nrow(house), 500), ]
pairs(leggero[, nums])
# Si nota effettivamente che vi sono dei valori outliers
# Essi danno problemi nella visualizzazione degli scatterplot a causa della scala


# B INFERENZA E PREDIZIONE
# Training 80%, test 20%
library(caret)
index <- createDataPartition(house$median_house_value, p = .80, list = FALSE)
train <- house[index, ]
test <- house[-index, ]

dim(train) #16348 osservazioni
dim(test) # 4085 osservazioni

# 1 Regressione lineare multipla
modello_completo <- lm(median_house_value ~ ., data=train)
summary(modello_completo)
# Il valore di R^2 e di R^2_{adj} non sono eccezionali, entrambi intorno a 0.6
# Il t-test per il coefficiente della variabile latitude indica che la variabile è non significativa,
# mentre l'intercetta risulta significativa ma ad un livello maggiore rispetto alle altre variabili.
# Elimino la variabile latitude

modello1 <- lm(median_house_value ~ . - latitude, data=train)
summary(modello1)
# In questo modello tutti i coefficienti delle variabili risultano molto significativi
# Tuttavia i valori di R^2 e di R^2_{adj} rimangono non molto buoni

# 2 Confrontare più modelli con ANOVA
anova(modello_completo, modello1)
# Non c'è evidenza che un modello sia migliore dell'altro!
# Infatti anche i RSS risultano pressochè uguali nei due modelli

# Considero come "migliore" il modello1
# perché a differenza del modello completo non presenta coefficienti non significativi

# 3 Sul modello migliore:
# t-test dei coefficienti: tutti molto significativi (vedi sopra)

# test globale F:
# H0: tutti i coefficienti sono pari a 0
# H1: almeno un coefficiente è diverso da 1
# Il valore della statistica test F e del suo pvalue indicano che è giusto rifiutare H0 in favore di H1.

# R^2adj: (vedi sopra)

# analisi dei residui:
par(mfrow=c(2,2))
plot(modello1)
par(mfrow=c(1,1))

# C'è qualcosa che non va:
# Grafico 1: vi è un pattern particolare, i residui non sono distribuiti a nuvola!
# Si nota proprio una "retta" di punti, no buono.
# Grafico 2: i residui non si distribuiscono in modo normale.
# Specialmente la coda di destra dei quantili osservati è molto diversa dai quantili teorici. No buono!
# Grafico 3: si nota una cuspide, i punti non sono distribuiti in una nuvola casuale. No buono.
# Grafico 4: si nota che ci sono dei valori influenti, con un valore di leva importante.
# Questi punti andrebbero rimossi per poi ripetere il fitting del modello di regressione.


### INTERMEZZO TEST D'IPOTESI, IL MODELLO DI REGRESSIONE CONTINUA DOPO

# Poiché i coefficienti per i livelli della variabile ocean_proximity risultano tutti significativi,
# provo a vedere se la media di median_house_value è significativamente diversa per i diversi livelli della variabile.

boxplot(house$median_house_value ~ house$ocean_proximity, data=house)
# Graficamente le medie risultano molto diverse, effettuo un test per verificare questa ipotesi
# H0: la media di median_house_value è la stessa per ogni livello di ocean_proximity
# H1: almeno una delle medie è diversa dalle altre
test <- aov(house$median_house_value ~ house$ocean_proximity, data = house)
summary(test)
# Il livello della variabile ocean_proximity risulta significativamente influente sulla media della variabile median_house_income!
# Interessante!

### FINE INTERMEZZO


# Provo a plottare i valori reali della variabile target e quelli stimati dal modello
# per cercare di capire dove sta il problema.

plot(train$median_house_value, modello1$fitted.values)

# Sembra che nel dataset originale tutti i valori di median_house_value superiori ad un certo valore siano stati posti uguali ad un valore di default
# La soglia è 500k dollari (?)
hist(house$median_house_value, breaks=20)
# effettivamente è un comportamento molto anormale per una variabile.
# Probabilmente è stato imputato come 500K dollari il valore delle case che non lo presentavano
# e che presentano covariate molto diverse, cosa che "confonde" il modello.

# Si potrebbero eliminare queste osservazioni e re-fittare il modello.
train2 <- train[train$median_house_value < 500000,]

modello2 <- lm(median_house_value ~ . - latitude, data=train2)
summary(modello2)
# Strano: i valori di R^2 e R^2adj sono peggiorati leggermente :(
# Non so

# Plotto di nuovo i residui:
par(mfrow=c(2,2))
plot(modello2)
par(mfrow=c(1,1))
# I residui risultano ancora non normali (grafico 2) e vi sono ancora dei valori influenti (grafico 4),
# tuttavia i grafici 1 e 3 presentano delle nuvole di punti, come dovrebbe essere!

# PREDICT
#test <- test[,-5]
predict(modello2, new_data=test[1:100,c(1,2,3,4,6)])
# C'è qualcosa che non va ma non so cosa, scusate :(
# Non capisco perché il vettore calcolato ha 15k valori e non 4k come dovrebbe essere.

#### NOTE:
# Il pacchetto moments permette di calcolare simmetria e curtosi,
# con le funzioni skewness e curtosi

