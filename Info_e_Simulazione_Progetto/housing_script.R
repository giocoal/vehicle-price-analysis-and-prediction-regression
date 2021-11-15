########  Importo il dataset ########
## Uso readr (read_csv) che è più ottimizzato
install.packages('readr')
library(readr)
housing <- read_csv('housing.csv', col_names = TRUE, )
View(housing)

######## Statistica Descrittiva ########
######################################## 

###### Osservo il dataset (median_house_value TARGET) ########
class(housing)
dim(housing)
str(housing)
# Il summary oltreutto mi dice, per ogni colonna, se e quanti
# NA sono presenti, capisco quindi quale li ha
summary(housing) # total_bedrooms ha 207 NA
# Ocean Proximity è scritto come chr, lo trasformo in factor
housing$ocean_proximity <- as.factor(housing$ocean_proximity)
str(housing)

############## 1C Missing value ################
# 1) se sono pochi li elimino
# 2) se sono tanti li assegno alla media


### Li cerco
## is.na mi ha una lista di true/false
# NON applicarlo all'intero df!
## sum() mi da il numero di NA (che so gia da summary)
sum(is.na(housing$total_bedrooms))
## which mi da l'indice delle righe
which(is.na(housing$total_bedrooms)) 
### Assegno a nuovo dataset
housing_nona <- housing[-which(is.na(housing$total_bedrooms)), ]

############## 2C Outliers ##############
## Uso library(car) per i boxplot
install.packages('car')
library(car)
## summary() per vedere minimo e massimo
summary(housing_nona) # non ho particolari conoscenze derivate
## Osservo gli istogrammi # non noto particolari
## Osservo i boxplot 
# Hanno tutti outliers tranne prime 3 colonne
for (i in names(housing_nona)) {
  if (i == 'ocean_proximity') {
    break
  }
  else {
    Boxplot(housing_nona[,i], 
            main = i,
            id = list(n=1))
  }
}
# Le righe da eliminare sono troppe, non va bene
## Cosa posso fare per capire meglio questi outliers ?
# 1) Provo ad ottenere la trasformata delle mie variabili
# vedo se cambia qualcosa (dovrebbero diventare più simmetrici
# gli outliers rispetto alla media)
for (i in names(housing_nona)) {
  if (i == 'ocean_proximity') {
    break
  }
  else {
    Boxplot(log(housing_nona[,i], 
                main = i)
    hist(log(housing_nona[,i], 
                main = i))
  }
}
# 2) Essendo comunque molti gli outliers posso pensare a nuovi
# criteri per capire quali righe eliminare
# 2.1) Provo ad aumentare la maglia, anzi che Q[1] - 1.5*iqr
#      provo a fare 3* o 4* o 5*
# 2.2) Controllo quali righe (unità statistiche) hanno quasi
#      tutte le righe outliers
# 3) con eliminated <- subset()
# eliminano e tengo da parte queste righe
# 3.1) Le tengo da parte perchè poi alla fine posso valutare
# se questi sono contemplati comunque nel mio modello di regressione

############### 1C Distribution Analysis #################
# 1) Numerical Analysis: Statistical Indicators (mean, st. std, 1 quart, ...)
# lo faccio con summary()
# 2) Visual Analysis: Graph Rappresention (hist, boxplot, scatter plot, etc)
# 3) Valutare normalità (curtosi etc)
# 4) Valutare normalità di trasformate

############### 1D Correlation Analysis ############### 
# 1) Matrice di correlazione
res <- cor(housing_nona[, -c(10)])
round(res, 2)
# 2) Scatter plot a coppie
pairs(housing_nona[, -c(10)])
# Vedo se ho covariate con dipendenza tra loro (in questo caso ne ho due)
# Quando valuto la correlazione ne scelgo solo una delle due, QUELLA CHE HA 
# MASSIMA correlazione con la variabile target (ha un potere esplicativo maggiore)
# 3) Elimino dal dataset quella meno correlata 
# 4) Ripeto l'operazione se ci sono altre variabili
# 5) Controllo nuovamente il grafico pairs()
  # 5.1) Potrei notare che gli outliers possono modificare altamente la scala
       # degli scatter plot, da qui capisco se sono influenti



######## Inference & Prediction ########
########################################

######### Divido in training e test set (vedi appunti) #########

######### Regressione #########
# Parto con il modello lineare della variabile più correlata
