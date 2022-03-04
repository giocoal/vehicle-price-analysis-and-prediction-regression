## ----importazione librerie e opzioni di rendering, warning=FALSE, include=FALSE----------------------------------------
library(readr)
library(tibble)
library(dplyr)
library(stringr)
library(caret)
library(ggcorrplot)
library(GGally) #Per il ggcorr()
library(DiscriMiner) # Per il correlation ratio
library(psych)
library(ggrepel)
library(gridExtra)
library(glmnet)
library(xgboost)
library(ggplot2)
library(gridExtra)
library(MASS)
library(mice)


## ----importazione------------------------------------------------------------------------------------------------------
data_path <- "https://raw.githubusercontent.com/giocoal/datasets/main/Car%20details%20v3.csv"
car <- read_csv(data_path, col_types = 'ciiiffff????i')
head(data.frame(car))


## ----rimozione colonna torque e mileage--------------------------------------------------------------------------------
car <- dplyr::select(car, -c(torque,mileage))
head(car)


## ----da centesimo a dollaro--------------------------------------------------------------------------------------------
car$selling_price <- car$selling_price*0.01


## ----conversione year in age-------------------------------------------------------------------------------------------
car <- add_column(car, age = 2021 - car$year, .after = "year")
car <- dplyr::select(car, -c("year"))


## ----from 0 to NA------------------------------------------------------------------------------------------------------
car$max_power[car$max_power == 0 | car$max_power == "bhp"] <- NA


## ----Controllo unita di misura-----------------------------------------------------------------------------------------
all(grepl("CC", car$engine) == !is.na(car$engine))
all((grepl("bhp", car$max_power)) == !is.na(car$max_power))


## ----Eliminazione unità di misura--------------------------------------------------------------------------------------
car['engine_CC'] <- parse_number(car$engine)
car['max_power_bhp'] <- parse_number(car$max_power)
car <- dplyr::select(car, -c('engine', 'max_power'))


## ----colonna marca-----------------------------------------------------------------------------------------------------
car <- add_column(car, make = factor(word(car$name, 1)), .after = "name")


## ----missing value, echo = c(3,6)--------------------------------------------------------------------------------------
# md.pattern (del pacchetto mice), è una funzione di visualizzazione molto utile
# Permette di vedere la distribuzione di NA nel dataset
md.pattern(car, rotate.names=TRUE)
#numero di righe con almeno un NA (coincide con quanto visto in md.pattern)
# sum(!complete.cases(car))
car <- car[-which(!complete.cases(car)),]


## ----Estrazione delle variabili numeriche------------------------------------------------------------------------------
car_nums_colnames <- unlist(lapply(car, is.numeric))
car_num <- car[ , car_nums_colnames]
car_num <- dplyr::select(car_num, -c('seats'))


## ----------------------------------------------------------------------------------------------------------------------
summary(car_num)


## ----boxplot per outliers----------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
#summary(car_num)
for (i in names(car_num)) {
  boxplot(car_num[[i]], xlab = i, main = '')
}
par(mfrow=c(1,1))


## ----boxplot log-------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
for (i in names(car_num)) {
  boxplot(log10(car_num[[i]]), main = '', xlab = i, range = 3)
}
par(mfrow=c(1,1))



## ----------------------------------------------------------------------------------------------------------------------
car_num <- car_num[car_num$selling_price < 80000, ]
car_num <- car_num[car_num$engine_CC < 3000, ]
car_num <- car_num[car_num$max_power_bhp < 300, ]
car_num <- car_num[car_num$km_driven < 300000 & car_num$km_driven > 1, ]

car <- car[car$selling_price < 80000, ]
car <- car[car$engine_CC < 3000, ]
car <- car[car$max_power_bhp < 300, ]
car <- car[car$km_driven < 300000 & car$km_driven > 1, ]


## ----lettura-----------------------------------------------------------------------------------------------------------
summary(car)


## ----statistical indicators--------------------------------------------------------------------------------------------
summary(car$selling_price)


## ----istogramma selling price------------------------------------------------------------------------------------------
car %>% ggplot()+geom_histogram(aes(selling_price, ..density..), bins = 30, )+geom_density(aes(selling_price))+ggtitle('Istogramma di selling_price (prezzo di vendita auto usate)')+xlab('Selling Price')+ylab('Densità')


## ----------------------------------------------------------------------------------------------------------------------
print(paste("Indice di asimmetria della variabile response selling_price:", skew(car$selling_price), sep = " "))
print(paste("Indice di curtosi della variabile response selling_price:", kurtosi(car$selling_price), sep = " "))


## ----istogramma selling_price_log--------------------------------------------------------------------------------------
car %>% ggplot()+geom_histogram(aes(selling_price, ..density..), bins = 30, )+geom_density(aes(selling_price))+ggtitle('Istogramma del logaritmo di selling_price (prezzo di vendita auto usate)')+xlab('Selling Price')+ylab('Densità')+scale_x_log10()


## ----simmetria e curtosi log_selling_price-----------------------------------------------------------------------------
print(paste("Indice di asimmetria della variabile response selling_price:", skew(log10(car$selling_price)), sep = " "))
print(paste("Indice di curtosi della variabile response selling_price:", kurtosi(log10(car$selling_price)), sep = " "))


## ----indicatori statistici---------------------------------------------------------------------------------------------
summary(car_num)


## ----istogrammi--------------------------------------------------------------------------------------------------------
plot1 <- car_num %>% ggplot()+geom_histogram(aes(age, ..density..), bins=15)+geom_density(aes(age))+ylab('')

plot2 <- car_num %>% ggplot()+geom_histogram(aes(km_driven, ..density..), bins=30)+geom_density(aes(km_driven))+scale_x_continuous(labels = function(x) format(x, scientific = F))+ylab('')

plot3 <- car_num %>% ggplot()+geom_histogram(aes(engine_CC, ..density..), bins=30)+geom_density(aes(engine_CC))+ylab('')

plot4 <- car_num %>% ggplot()+geom_histogram(aes(max_power_bhp, ..density..), bins=25)+geom_density(aes(max_power_bhp))+ylab('')

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2,
             top = "Istogrammi delle distribuzioni delle variabili esplicative quantitative",
             left = "Densità"
             )


## ----indice di asimmetria----------------------------------------------------------------------------------------------
for (i in names(car_num)) {
  if (i == 'selling_price'){
    next
  }
  print(paste("Indice di asimmetria di Pearson di",i,":", skew(car_num[[i]]), sep = " "))
}


## ----indice di curtosi di Pearson--------------------------------------------------------------------------------------
for (i in names(car_num)) {
  if (i == 'selling_price'){
    next
  }
  print(paste("Indice di curtosi di Pearson di",i,":", kurtosi(car_num[[i]]), sep = " "))
}


## ----indice di simmetria di Pearson trasformazione logaritmica---------------------------------------------------------
for (i in names(car_num)) {
  if (i == 'selling_price'){
    next
  }
  print(paste("Indice di asimmetria di",i,":", skew(log10(car_num[[i]])), sep = " "))
}


## ----indice di curtosi di Pearson trasformazione logaritmica-----------------------------------------------------------
for (i in names(car_num)) {
  if (i == 'selling_price'){
    next
  }
  print(paste("Indice di curtosi di Pearson di",i,":", kurtosi(log10(car_num[[i]])), sep = " "))
}


## ----istogrammi log----------------------------------------------------------------------------------------------------
plot1 <- car_num %>% ggplot()+geom_histogram(aes(age, ..density..), bins=15)+geom_density(aes(age))+ylab('')+scale_x_log10()

plot2 <- car_num %>% ggplot()+geom_histogram(aes(km_driven, ..density..), bins=30)+geom_density(aes(km_driven))+theme(axis.text.x = element_text(angle = 10))+ylab('')+scale_x_log10()

plot3 <- car_num %>% ggplot()+geom_histogram(aes(engine_CC, ..density..), bins=30)+geom_density(aes(engine_CC))+ylab('')+scale_x_log10()

plot4 <- car_num %>% ggplot()+geom_histogram(aes(max_power_bhp, ..density..), bins=30)+geom_density(aes(max_power_bhp))+ylab('')+scale_x_log10()

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2,
             top = "Istogrammi delle distribuzioni delle variabili esplicative quantitative \n dopo l'applicazione della trasformazione logaritmica",
             left = "Densità"
             )


## ----trasformazione logaritmica----------------------------------------------------------------------------------------
car <- add_column(car, log_selling_price = log10(car$selling_price), .after = "selling_price")
car <- add_column(car, log_age = log10(car$age), .after = "age")
car <- add_column(car, log_max_power_bhp = log10(car$max_power_bhp), .after = "max_power_bhp")
car <- add_column(car, log_engine_CC = log10(car$engine_CC), .after = "engine_CC")

car_num <- add_column(car_num, log_selling_price = log10(car_num$selling_price), .after = "selling_price")
car_num <- add_column(car_num, log_age = log10(car_num$age), .after = "age")
car_num <- add_column(car_num, log_max_power_bhp = log10(car_num$max_power_bhp), .after = "max_power_bhp")
car_num <- add_column(car_num, log_engine_CC = log10(car_num$engine_CC), .after = "engine_CC")


## ----tipo di carburante------------------------------------------------------------------------------------------------
# coef = 3 indica la lunghezza dei baffi come multipli dell'IQR (porto da 1.5 a 4)
car %>% ggplot(aes(fuel, log_selling_price))+geom_boxplot(coef = 3)+geom_jitter(alpha=0.05)+theme(axis.text.x = element_text(angle = 0, hjust = 1))+stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")


## ----rapporto di correlaizone fuel-------------------------------------------------------------------------------------
corRatio(car$log_selling_price, car$fuel)


## ----indice di connessione fuel----------------------------------------------------------------------------------------
chisq_price_fuel <- chisq.test(car$log_selling_price, car$fuel, simulate.p.value = TRUE)
chisq_price_fuel$statistic


## ----tipo di proprietario----------------------------------------------------------------------------------------------
car %>% ggplot(aes(owner, log_selling_price))+geom_boxplot(coef = 3)+geom_jitter(alpha=0.05)+theme(axis.text.x = element_text(angle = 0, hjust = 1))+stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")+theme(axis.text.x = element_text(angle = 20, hjust = 1))


## ----rapporto di correlaizone owner------------------------------------------------------------------------------------
corRatio(car$log_selling_price, car$owner)


## ----indice di connessione owner---------------------------------------------------------------------------------------
chisq_price_owner <- chisq.test(car$log_selling_price, car$owner, simulate.p.value = TRUE)
chisq_price_owner$statistic


## ----make boxplot count------------------------------------------------------------------------------------------------
car %>% group_by(make) %>% count() %>% arrange(desc(n)) %>% ggplot() + geom_col(aes(x=n,y=reorder(make,n)), show.legend = F)+
labs(title = 'Distribuzione marche di auto ordinate per frequenza assoluta',
     subtitle = '',
     x= 'Frequenza Assoluta',
     y='make')


## ----selling_price make------------------------------------------------------------------------------------------------
car %>% ggplot(aes(reorder(make, selling_price, median), selling_price))+geom_boxplot()+geom_jitter(alpha=0.02)+geom_hline(aes(yintercept=median(selling_price)))+coord_flip()+xlab('make (marche ordinate per prezzo mediano)')+ylab('selling_price')+theme(aspect.ratio=1)+scale_y_log10()


## ----tipo di cambio----------------------------------------------------------------------------------------------------
car %>% ggplot(aes(transmission, log_selling_price))+geom_boxplot(coef = 3)+geom_jitter(alpha=0.05)+theme(axis.text.x = element_text(angle = 0, hjust = 1))+stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")


## ----rapporto di correlazione transmission-----------------------------------------------------------------------------
corRatio(car$log_selling_price, car$transmission)


## ----indice di connessione transmission--------------------------------------------------------------------------------
chisq_price_transmission <- chisq.test(car$log_selling_price, car$transmission, simulate.p.value = TRUE)
chisq_price_transmission$statistic


## ----tipo di seller----------------------------------------------------------------------------------------------------
car %>% ggplot(aes(seller_type, log_selling_price))+geom_boxplot(coef = 3)+geom_jitter(alpha=0.05)+theme(axis.text.x = element_text(angle = 0, hjust = 1))+stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")


## ----rapporto di correlazione seller-----------------------------------------------------------------------------------
corRatio(car$log_selling_price, car$seller_type)


## ----indice di connessione seller--------------------------------------------------------------------------------------
chisq_price_seller_type <- chisq.test(car$log_selling_price, car$seller_type, simulate.p.value = TRUE)
chisq_price_transmission$statistic


## ----correlazioni------------------------------------------------------------------------------------------------------
# Aggiorno il vettore delle colonne numeriche di car
car_nums_colnames_log <- c('log_age','log_selling_price','km_driven','log_engine_CC','log_max_power_bhp', 'seats')
# Prendo un campione di osservazioni per rendere più agile la rappresentazione grafica:
leggero <- car[sample(nrow(car), 500), ]
# Matrice dei diagrammi di dispersione: (non serve usare le variabili trasformate perchè la correlazione non cambia)
pairs(leggero[, car_nums_colnames_log])
cor(car[ ,car_nums_colnames_log])


## ----Pearson-----------------------------------------------------------------------------------------------------------
ggcorr(car[,car_nums_colnames_log], label = TRUE, label_size = 2.9, hjust = 1, layout.exp = 2)



## ----quantifico righe duplicare----------------------------------------------------------------------------------------
sum(duplicated(car))


## ----distribuzione selling_price---------------------------------------------------------------------------------------
car_noduplicati <- distinct(car)

plot1 <- car %>% ggplot()+geom_histogram(aes(log_selling_price, ..density..), bins = 30, )+geom_density(aes(log_selling_price))+ggtitle('Istogramma con righe duplicate')+xlab('Selling Price (log10)')+ylab('Densità')

plot2 <- car_noduplicati %>% ggplot()+geom_histogram(aes(log_selling_price, ..density..), bins = 30, )+geom_density(aes(log_selling_price))+ggtitle('Istogramma senza righe duplicate')+xlab('Selling Price (log10)')+ylab('Densità')

plot3 <- ggplot(car, aes(sample = log_selling_price)) + stat_qq() + stat_qq_line(col = "red")+ggtitle('Normal Q-Q con righe duplicate')+xlab('Theoretical Quantiles')+ylab('Sample Quantiles')

plot4 <- ggplot(car_noduplicati, aes(sample = log_selling_price)) + stat_qq() + stat_qq_line(col = "red")+ggtitle('Normal Q-Q senza righe duplicate')+xlab('Theoretical Quantiles')+ylab('Sample Quantiles')

grid.arrange(plot1, plot3, plot2, plot4, ncol = 2)


## ----conteggi per categorie make---------------------------------------------------------------------------------------
conteggi <- count(car_noduplicati, make, sort = TRUE)
drop<-conteggi[conteggi$n < 13,]
drop


## ----rimozione auto con marche poco rappresentate----------------------------------------------------------------------
car_noduplicati_nodrop <- droplevels(car_noduplicati[!(car_noduplicati$make %in% drop$make),])
str(car_noduplicati_nodrop$make)


## ----conteggi per categorie owner--------------------------------------------------------------------------------------
conteggi <- count(car_noduplicati, owner, sort = TRUE)
drop<-conteggi[conteggi$n < 13,]
drop


## ----rimozione auto con categorie owner poco rappresentate-------------------------------------------------------------
car_noduplicati_nodrop <- droplevels(car_noduplicati_nodrop[!(car_noduplicati_nodrop$owner %in% drop$owner),])
str(car_noduplicati_nodrop$owner)


## ----rimozione auto con owner poco rappresentate-----------------------------------------------------------------------
car_noduplicati_nodrop <- droplevels(car_noduplicati_nodrop[!(car_noduplicati_nodrop$owner %in% drop$owner),])
str(car_noduplicati_nodrop$owner)


## ----da variabili fattoriali a variabili dummy-------------------------------------------------------------------------

#subset delle variabili quantitative
car_dummy <- car_noduplicati_nodrop[,car_nums_colnames_log]

# Aggiungo la variabile fattoriale 'transmission' a car dummy e la converto in numerica (0 corrispode a 'automatic'):
car_dummy <- cbind(car_dummy,transmission = car_noduplicati_nodrop$transmission)
levels(car_dummy$transmission)<-c(1,0)
car_dummy$transmission <- as.numeric(levels(car_dummy$transmission))[car_dummy$transmission]

#converto la variabile fattoriale 'make' in dummy e la aggiungo a car_dummy:
dummy_temp <- data.frame(model.matrix( ~make, data = car_noduplicati_nodrop))[,-1]
car_dummy <-cbind(car_dummy,dummy_temp)

#converto la variabile fattoriale 'fuel' in dummy e la aggiungo a car_dummy:
dummy_temp <- data.frame(model.matrix( ~fuel, data = car_noduplicati_nodrop))[,-1]
car_dummy <-cbind(car_dummy,dummy_temp)

#converto la variabile fattoriale 'seller_type' in dummy e la aggiungo a car_dummy:
dummy_temp <- data.frame(model.matrix( ~seller_type, data = car_noduplicati_nodrop))[,-1]
car_dummy <-cbind(car_dummy,dummy_temp)

#converto la variabile fattoriale 'owner' in dummy e la aggiungo a car_dummy:
dummy_temp <- data.frame(model.matrix( ~owner, data = car_noduplicati_nodrop))[,-1]
car_dummy <-cbind(car_dummy,dummy_temp)


## ----split dataset in Train e Test sets--------------------------------------------------------------------------------
set.seed(100)
train_ind<-sample(1:nrow(car_dummy),0.8*nrow(car_dummy))

train_set <- car_dummy[train_ind,]
test_set <- car_dummy[-train_ind,]


## ----dimensioni train_set e test_set-----------------------------------------------------------------------------------
print(paste("Numero righe train_set:", nrow(train_set), sep = " "))
print(paste("Numero righe test_set: ", nrow(test_set), sep = " "))


## ----modello completo--------------------------------------------------------------------------------------------------
modello_completo <- lm(log_selling_price ~ ., data=train_set)
summary(modello_completo)


## ----rimuovo colonne seats e log_engine_CC-----------------------------------------------------------------------------
train_set <- dplyr::select(train_set, -c('log_engine_CC'))
test_set <- dplyr::select(test_set, -c('log_engine_CC'))


## ----primo modello con solo log_max_power_bhp--------------------------------------------------------------------------
modello_minimo <- lm(log_selling_price ~ log_max_power_bhp, data=train_set)
summary(modello_minimo)


## ----grafico modello semplificato--------------------------------------------------------------------------------------
ggplot(train_set, aes(x = log_max_power_bhp, y = log_selling_price)) + geom_point() + stat_smooth(formula = y ~ x, method = "lm", col = "red")


## ----modello stepAIC, include=FALSE, results='hide'--------------------------------------------------------------------
modello_completo <- lm(log_selling_price ~ ., data=train_set) #ricalcolo il modello perché ho rimosso la colonna log_engine_CC
modello_stepAIC <- stepAIC(modello_minimo, direction = "both", scope = formula(modello_completo))


## ----risultati modello stepAIC-----------------------------------------------------------------------------------------
summary(modello_stepAIC)


## ----modello stepAIC con parametro k, include=FALSE--------------------------------------------------------------------
k_value <- qchisq(0.001, 1, lower.tail = F)
modello_stepAIC_k <- stepAIC(modello_minimo, direction = "both", scope = formula(modello_completo), k = k_value)


## ----risultato modello stepAIC con parametro k-------------------------------------------------------------------------
summary(modello_stepAIC_k)


## ----confronto modello_stepAIC con modello_stepAIC_k-------------------------------------------------------------------
anova(modello_stepAIC_k, modello_stepAIC)


## ----plot modello stepAIC, warning=FALSE-------------------------------------------------------------------------------

plot1 <- qplot(.fitted, .resid, data = modello_stepAIC) + geom_hline(yintercept = 0) + geom_smooth(se = FALSE, col = "red") +ggtitle('Residuals vs Fitted')+ylab('Residuals')+xlab('Fitted values')

plot2 <- qplot(sample =.stdresid, data = modello_stepAIC, stat = "qq") + geom_abline(col = "red")+ggtitle('Normal Q-Q Plot')+ylab('Standardized Residuals')+xlab('Theoretical Quantiles') + geom_text(aes(label = '5491'), x=-3, y=-6.3)

plot3 <- qplot(.fitted, sqrt(abs(.stdresid)), data = modello_stepAIC) + geom_hline(yintercept = 1) + geom_smooth(se = FALSE,col = "red") +ggtitle('Scale - Location')+xlab('Fitted values')+ylab(expression(sqrt(abs('Standardized residuals '))))

plot4 <- qplot(.hat, .stdresid, data = modello_stepAIC) + geom_smooth(se = FALSE,col = "red")+ggtitle('Residuals vs Leverage')+xlab('Leverage')+ylab('Standardized residuals') + geom_hline(yintercept = 0)

grid.arrange(plot1,plot2,plot3,plot4)


## ----verifico outlier--------------------------------------------------------------------------------------------------
car_noduplicati_nodrop[row.names(car_noduplicati_nodrop) == 5491,]


## ----veirifico il costo delle altre Volkswagen Polo 1.5 TDI Comfortline------------------------------------------------
car_noduplicati_nodrop[car_noduplicati_nodrop$name == 'Volkswagen Polo 1.5 TDI Comfortline',]


## ----predizione sul test set-------------------------------------------------------------------------------------------
lm_pred <- predict(modello_stepAIC, newdata = test_set %>% dplyr::select(-log_selling_price))
R2(lm_pred, test_set$log_selling_price)


## ----valutazione RMSE--------------------------------------------------------------------------------------------------
RMSE(pred = modello_stepAIC$fitted.values, obs = train_set$log_selling_price)  # RMSE of train dataset
RMSE(pred = lm_pred, obs = test_set$log_selling_price)                         # RMSE of test dataset

