#regressioni lineari multiple

library(haven)
library(dplyr)
library(corrplot) 
library(lmtest)
library(regclass)
library(ISLR2)
library(precrec)
library(PRROC)
library(pROC)
library(ggplot2)



#prima di eseguire questo script occorre eseguire lo script precedente 
#riguardante la pulizia dei dati, che ci restituirà un dataset 
#"conviventi_e_sposati" con 7242 osservazioni e 10 variabili

#lo script sarà diviso in due parti: nella prima parte ci saranno delle regressioni
#lineari multiple piu generali, nella seconda parte verranno svolte le regressioni
#lineari multiple dei 20 gruppi piu presenti nella popolazione, con il fine di ottenere
#delle regressioni lineari multiple che riescano a coprire, ponendo che il nostro campione
#rappresenti fedelmente la popolazione, piu del 92% della popolazione in maniera 
#molto precisa partendo dall'informazione riguardante l'etnia, la religione, dove 
#vivono e se sono sposati o solo conviventi.

#PARTE 1: REGRESSIONI GENERALI

regress_multipla_tot=lm(anni_convivenza~ age+hhsizer+ppincome,
                        data=conviventi_e_sposati)

regress_multipla_conviventi=lm(anni_convivenza~ age + anni_istruzione,
                               data=subset(conviventi_e_sposati, mar==2))


regress_multipla_sposati=lm(anni_convivenza~ age+hhsizer,
                            data=subset(conviventi_e_sposati, mar==1))

#REGRESSIONE 1= regressione lineare multipla di tutte le unità nel dataset
#REGRESSIONE 2= regressione lineare multipla delle unità conviventi nel dataset
#REGRESSIONE 3= regressione lineare multipla delle unità sposate nel dataset
#ho gia eliminato dalle tre regressioni le variabili statisticamente non rilevanti


summary(regress_multipla_tot)
#in linea generale, è osservabile qui come l'età influenzi meno la longevità di un matrimonio
#o di una convivenza rispetto all'istruzione e/o alla grandezza del nucleo familiare. 


summary(regress_multipla_conviventi)
#in questa regressione si puo notare come l'età influenzi molto poco nelle convivenze. 
#Questo è però logico se si pensa che le coppie tendono a vivere un periodo di convivenza
#a prescindere dall'età che le unità hanno.


summary(regress_multipla_sposati)
#in questa regressione si puo notare come l'età acquisisca un coefficente piuttosto alto 
#rispetto alla precedente regressione, così come la grandezza del nucleo familiare
#diviene statisticamemte rilevante con un coefficente non così piccolo.


#PARTE 2: REGRESSIONI DEI 20 GRUPPI PIU NUMEROSI.


#le regressioni effettuate qui sotto sono le regressioni lineari multiple dei 20 gruppi
#piu popolosi nel nostro dataset conviventi_e_sposati, in base alle variabili 
#mar, geo2011, popgrp e rel. Ho anche, in ognuno dei blocchi, aggiunto alla fine 
#il numero di unità potenziali che è possibile andare ad analizzare con il modello appena 
#trovato. L'ultimo gruppo raccoglie in se tutte le unità statistiche che non hanno 
#trovato posto in nessuno dei gruppi precedenti.


# Totale unità statistiche nel dataset
total_units <- 7241
dataset_pulito$anni_mar_stimato=NA


# Blocco 1
group1_size <- sum(conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==2)
group1_size
# Gruppo 1 = 1385 unità statistiche, pari al 19.127% delle unità in popolazione.
percentage1 <- (group1_size / total_units) * 100
percentage1  
regress_multipla_1 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==1 & mar==1 & geo2011==2))
summary(regress_multipla_1)
# Eliminazione delle variabili non significative
regress_multipla_1 = lm(anni_convivenza ~ age + hhsizer+ anni_istruzione,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==1 & mar==1 & geo2011==2))
summary(regress_multipla_1)
r_quadro1 = summary(regress_multipla_1)$r.squared
# Numero di potenziali unità statistiche a cui il modello è applicabile
potential_group1_size = sum(dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer) & !is.na(dataset_pulito$anni_istruzione)
                            & !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                            na.rm=TRUE)
potential_group1_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 
                 & dataset_pulito$geo2011==2 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer) 
                 & !is.na(dataset_pulito$anni_istruzione) & !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                               dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 
                            & dataset_pulito$geo2011==2 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer) 
                            & !is.na(dataset_pulito$anni_istruzione) & !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Blocco 2
group2_size <- sum(conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==1)
group2_size
# Gruppo 2 = 1285 unità statistiche, pari al 17.746% delle unità in popolazione.
percentage2 <- (group2_size / total_units) * 100
percentage2
regress_multipla_2 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==1 & mar==1 & geo2011==1))
summary(regress_multipla_2)
# Eliminazione delle variabili non significative
regress_multipla_2 = lm(anni_convivenza ~ age + ppincome,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==1 & mar==1 & geo2011==1))
summary(regress_multipla_2)
r_quadro2 = summary(regress_multipla_2)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group2_size = sum(dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$ppincome)& !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                            na.rm=TRUE)
potential_group2_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$ppincome)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                               dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$ppincome)& !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Blocco 3
group3_size <- sum(conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==2 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==2)
group3_size
# Gruppo 3 = 748 unità statistiche, pari al 10.33% delle unità in popolazione.
percentage3 <- (group3_size / total_units) * 100
percentage3
regress_multipla_3 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==2 & mar==1 & geo2011==2))
summary(regress_multipla_3)
# Eliminazione delle variabili non significative
regress_multipla_3 = lm(anni_convivenza ~ age + hhsizer + ppincome,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==2 & mar==1 & geo2011==2))
summary(regress_multipla_3)
r_quadro3 = summary(regress_multipla_3)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group3_size = sum(dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer) & !is.na(dataset_pulito$ppincome)
                            & !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                            na.rm=TRUE)
potential_group3_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer) & !is.na(dataset_pulito$ppincome)
                 & !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                               dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer) & !is.na(dataset_pulito$ppincome)
                            & !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Blocco 4
group4_size <- sum(conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==2)
group4_size
# Gruppo 4 = 671 unità statistiche, pari al 9.253% delle unità in popolazione.
percentage4 <- (group4_size / total_units) * 100
percentage4
regress_multipla_4 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==1 & mar==2 & geo2011==2))
summary(regress_multipla_4)
# Eliminazione delle variabili non significative
regress_multipla_4 = lm(anni_convivenza ~ age + hhsizer,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==2 & mar==1 & geo2011==2))
summary(regress_multipla_4)
r_quadro4 = summary(regress_multipla_4)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group4_size = sum(dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer) & !is.na(dataset_pulito$anni_istruzione)
                            & !is.na(dataset_pulito$rel) & !is.na(dataset_pulito$ppincome)
                            & !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                            na.rm=TRUE)
potential_group4_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer) & !is.na(dataset_pulito$anni_istruzione)
                 & !is.na(dataset_pulito$rel) & !is.na(dataset_pulito$ppincome)
                 & !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer) & !is.na(dataset_pulito$anni_istruzione)
                            & !is.na(dataset_pulito$rel) & !is.na(dataset_pulito$ppincome)
                            & !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Blocco 5
group5_size <- sum(conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==4 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==2)
group5_size
# Gruppo 5 = 591 unità statistiche, pari al 8.162% delle unità in popolazione.
percentage5 <- (group5_size / total_units) * 100
percentage5
regress_multipla_5 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==4 & mar==1 & geo2011==2))
summary(regress_multipla_5)
# Eliminazione delle variabili non significative
regress_multipla_5 = lm(anni_convivenza ~ age + anni_istruzione,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==4 & mar==1 & geo2011==2))
summary(regress_multipla_5)
r_quadro5 = summary(regress_multipla_5)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group5_size = sum(dataset_pulito$rel==2 & dataset_pulito$popgrp==4 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                            na.rm=TRUE)
potential_group5_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==2 & dataset_pulito$popgrp==4 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==2 & dataset_pulito$popgrp==4 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Blocco 6
group6_size <- sum(conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==1)
group6_size
# Gruppo 6 = 438 unità statistiche, pari al 6.049% delle unità in popolazione.
percentage6 <- (group6_size / total_units) * 100
percentage6
regress_multipla_6 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==1 & mar==2 & geo2011==1))
summary(regress_multipla_6)
# Eliminazione delle variabili non significative
regress_multipla_6 = lm(anni_convivenza ~ age + ppincome,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==1 & mar==2 & geo2011==1))
summary(regress_multipla_6)
r_quadro6 = summary(regress_multipla_6)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group6_size = sum(dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$ppincome)& !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                            na.rm=TRUE)
potential_group6_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$ppincome)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$ppincome)& !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Blocco 7
group7_size <- sum(conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==2 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==2)
group7_size
# Gruppo 7 = 309 unità statistiche, pari al 4.267% delle unità in popolazione.
percentage7 <- (group7_size / total_units) * 100
percentage7
regress_multipla_7 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==2 & mar==2 & geo2011==2))
summary(regress_multipla_7)
# Eliminazione delle variabili non significative
regress_multipla_7 = lm(anni_convivenza ~ age,
                        data=subset(conviventi_e_sposati, rel==2 & popgrp==2 & mar==2 & geo2011==2))
summary(regress_multipla_7)
r_quadro7 = summary(regress_multipla_7)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group7_size = sum(dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2
                            & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                            na.rm=TRUE)
potential_group7_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2
                 & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2
                            & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Blocco 8
group8_size <- sum(conviventi_e_sposati$rel==6 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==1)
group8_size
# Gruppo 8 = 207 unità statistiche, pari al 2.859% delle unità in popolazione.
percentage8 <- (group8_size / total_units) * 100
percentage8 
regress_multipla_8 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                        data=subset(conviventi_e_sposati, rel==6 & popgrp==1 & mar==1 & geo2011==1))
summary(regress_multipla_8)
# Eliminazione delle variabili non significative
regress_multipla_8 = lm(anni_convivenza ~ age + anni_istruzione,
                        data=subset(conviventi_e_sposati, rel==6 & popgrp==1 & mar==1 & geo2011==1))
summary(regress_multipla_8)
r_quadro8 = summary(regress_multipla_8)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group8_size = sum(dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                            na.rm=TRUE)
potential_group8_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Blocco 9
group9_size <- sum(conviventi_e_sposati$rel==1 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==1)
group9_size
# Gruppo 9 = 131 unità statistiche, pari al 1.809% delle unità in popolazione.
percentage9 <- (group9_size / total_units) * 100
percentage9
regress_multipla_9 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                        data=subset(conviventi_e_sposati, rel==1 & popgrp==1 & mar==1 & geo2011==1))
summary(regress_multipla_9)
# Eliminazione delle variabili non significative
regress_multipla_9 = lm(anni_convivenza ~ age + hhsizer,
                        data=subset(conviventi_e_sposati, rel==1 & popgrp==1 & mar==1 & geo2011==1))
summary(regress_multipla_9)
r_quadro9 = summary(regress_multipla_9)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group9_size = sum(dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer)& !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                            na.rm=TRUE)
potential_group9_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1
                            & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer)& !is.na(dataset_pulito$rel) &
                              !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                              !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Blocco 10
group10_size <- sum(conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==3)
group10_size
# Gruppo 10 = 112 unità statistiche, pari al 1.547% delle unità in popolazione.
percentage10 <- (group10_size / total_units) * 100
percentage10
regress_multipla_10 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                         data=subset(conviventi_e_sposati, rel==2 & popgrp==1 & mar==1 & geo2011==3))
summary(regress_multipla_10)
# Eliminazione delle variabili non significative
regress_multipla_10 = lm(anni_convivenza ~ age + anni_istruzione,
                         data=subset(conviventi_e_sposati, rel==2 & popgrp==1 & mar==1 & geo2011==3))
summary(regress_multipla_10)
r_quadro10 = summary(regress_multipla_10)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group10_size = sum(dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==3
                             & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                             na.rm=TRUE)
potential_group10_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==3
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==3
                             & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Blocco 11
group11_size <- sum(conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==2 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==3)
group11_size
# Gruppo 11 = 102 unità statistiche, pari al 1.409% delle unità in popolazione.
percentage11 <- (group11_size / total_units) * 100
percentage11
regress_multipla_11 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                         data=subset(conviventi_e_sposati, rel==2 & popgrp==2 & mar==1 & geo2011==3))
summary(regress_multipla_11)
# Eliminazione delle variabili non significative
regress_multipla_11 = lm(anni_convivenza ~ age,
                         data=subset(conviventi_e_sposati, rel==2 & popgrp==2 & mar==1 & geo2011==3))
summary(regress_multipla_11)
r_quadro11 = summary(regress_multipla_11)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group11_size = sum(dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==1 & dataset_pulito$geo2011==3
                             & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                             na.rm=TRUE)
potential_group11_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==1 & dataset_pulito$geo2011==3
                 & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==1 & dataset_pulito$geo2011==3
                             & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Gruppo 12
group12_size <- sum(conviventi_e_sposati$rel==6 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==2)
group12_size
# Gruppo 12 = 101 unità statistiche, pari al 1.395% delle unità in popolazione.
percentage12 <- (group12_size / total_units) * 100
percentage12
regress_multipla_12 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                         data=subset(conviventi_e_sposati, rel==6 & popgrp==1 & mar==1 & geo2011==2))
summary(regress_multipla_12)
# Eliminazione delle variabili non significative
regress_multipla_12 = lm(anni_convivenza ~ age + anni_istruzione,
                         data=subset(conviventi_e_sposati, rel==6 & popgrp==1 & mar==1 & geo2011==2))
summary(regress_multipla_12)
r_quadro12 = summary(regress_multipla_12)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group12_size = sum(dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                             & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                             na.rm=TRUE)
potential_group12_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                             & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Gruppo 13
group13_size <- sum(conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==3)
group13_size
# Gruppo 13 = 101 unità statistiche, pari al 1.395% delle unità in popolazione.
percentage13 <- (group13_size / total_units) * 100
percentage13
regress_multipla_13 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                         data=subset(conviventi_e_sposati, rel==2 & popgrp==1 & mar==2 & geo2011==3))
summary(regress_multipla_13)
# Eliminazione delle variabili non significative
regress_multipla_13 = lm(anni_convivenza ~ age + hhsizer,
                         data=subset(conviventi_e_sposati, rel==2 & popgrp==1 & mar==2 & geo2011==3))
summary(regress_multipla_13)
r_quadro13 = summary(regress_multipla_13)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group13_size = sum(dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==3
                             & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                             na.rm=TRUE)
potential_group13_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==3
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==3
                             & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Gruppo 14
group14_size <- sum(conviventi_e_sposati$rel==1 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==1)
group14_size
# Gruppo 14 = 87 unità statistiche, pari al 1.201% delle unità in popolazione.
percentage14 <- (group14_size / total_units) * 100
percentage14
regress_multipla_14 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                         data=subset(conviventi_e_sposati, rel==1 & popgrp==1 & mar==2 & geo2011==1))
summary(regress_multipla_14)
# Eliminazione delle variabili non significative
regress_multipla_14 = lm(anni_convivenza ~ age,
                         data=subset(conviventi_e_sposati, rel==1 & popgrp==1 & mar==2 & geo2011==1))
summary(regress_multipla_14)
r_quadro14 = summary(regress_multipla_14)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group14_size = sum(dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1
                             & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                             na.rm=TRUE)
potential_group14_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1
                 & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1
                             & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Gruppo 15
group15_size <- sum(conviventi_e_sposati$rel==1 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==2)
group15_size
# Gruppo 15 = 83 unità statistiche, pari al 1.146% delle unità in popolazione.
percentage15 <- (group15_size / total_units) * 100
percentage15
regress_multipla_15 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                         data=subset(conviventi_e_sposati, rel==1 & popgrp==1 & mar==2 & geo2011==2))
summary(regress_multipla_15)
# Eliminazione delle variabili non significative
regress_multipla_15 = lm(anni_convivenza ~ age,
                         data=subset(conviventi_e_sposati, rel==1 & popgrp==1 & mar==2 & geo2011==2))
summary(regress_multipla_15)
r_quadro15 = summary(regress_multipla_15)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group15_size = sum(dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2
                             & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                             na.rm=TRUE)
potential_group15_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2
                 & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2
                             & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Gruppo 16
group16_size <- sum(conviventi_e_sposati$rel==1 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==2)
group16_size
# Gruppo 16 = 77 unità statistiche, pari al 1.063% delle unità in popolazione.
percentage16 <- (group16_size / total_units) * 100
percentage16
regress_multipla_16 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                         data=subset(conviventi_e_sposati, rel==1 & popgrp==1 & mar==1 & geo2011==2))
summary(regress_multipla_16)
r_quadro16 = summary(regress_multipla_16)$r.squared
# Eliminazione delle variabili non significative
regress_multipla_16 = lm(anni_convivenza ~ age + hhsizer,
                         data=subset(conviventi_e_sposati, rel==1 & popgrp==1 & mar==1 & geo2011==2))
summary(regress_multipla_16)
# Numero di potenziali unità statistiche il modello è applicabile
potential_group16_size = sum(dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                             & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                             na.rm=TRUE)
potential_group16_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                             & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Gruppo 17
group17_size <- sum(conviventi_e_sposati$rel==6 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==1)
group17_size
# Gruppo 17 = 76 unità statistiche, pari al 1.049% delle unità in popolazione.
percentage17 <- (group17_size / total_units) * 100
percentage17
regress_multipla_17 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                         data=subset(conviventi_e_sposati, rel==6 & popgrp==1 & mar==2 & geo2011==1))
summary(regress_multipla_17)
# Eliminazione delle variabili non significative
regress_multipla_17 = lm(anni_convivenza ~ age + anni_istruzione,
                         data=subset(conviventi_e_sposati, rel==6 & popgrp==1 & mar==2 & geo2011==1))
summary(regress_multipla_17)
r_quadro17 = summary(regress_multipla_17)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group17_size = sum(dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1
                             & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                             na.rm=TRUE)
potential_group17_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1
                             & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Gruppo 18
group18_size <- sum(conviventi_e_sposati$rel==6 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==2)
group18_size
# Gruppo 18 = 70 unità statistiche, pari al 0.967% delle unità in popolazione.
percentage18 <- (group18_size / total_units) * 100
percentage18
regress_multipla_18 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                         data=subset(conviventi_e_sposati, rel==6 & popgrp==1 & mar==2 & geo2011==2))
summary(regress_multipla_18)
# Eliminazione delle variabili non significative
regress_multipla_18 = lm(anni_convivenza ~ age + anni_istruzione,
                         data=subset(conviventi_e_sposati, rel==6 & popgrp==1 & mar==2 & geo2011==2))
summary(regress_multipla_18)
r_quadro18 = summary(regress_multipla_18)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group18_size = sum(dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2
                             & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                             na.rm=TRUE)
potential_group18_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2
                 & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2
                             & !is.na(dataset_pulito$age) & !is.na(dataset_pulito$anni_istruzione)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Gruppo 19
group19_size <- sum(conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==2 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==3)
group19_size
# Gruppo 19 = 66 unità statistiche, pari al 0.911% delle unità in popolazione.
percentage19 <- (group19_size / total_units) * 100
percentage19
regress_multipla_19 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                         data=subset(conviventi_e_sposati, rel==2 & popgrp==2 & mar==2 & geo2011==3))
summary(regress_multipla_19)
# Eliminazione delle variabili non significative
regress_multipla_19 = lm(anni_convivenza ~ age,
                         data=subset(conviventi_e_sposati, rel==2 & popgrp==2 & mar==2 & geo2011==3))
summary(regress_multipla_19)
r_quadro19 = summary(regress_multipla_19)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group19_size = sum(dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==2 & dataset_pulito$geo2011==3
                             & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                             na.rm=TRUE)
potential_group19_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==2 & dataset_pulito$geo2011==3
                 & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==2 & dataset_pulito$geo2011==3
                             & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


# Gruppo 20
group20_size <- sum(conviventi_e_sposati$rel==5 & conviventi_e_sposati$popgrp==3 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==2)
group20_size
# Gruppo 20 = 57 unità statistiche, pari al 0.787% delle unità in popolazione.
percentage20 <- (group20_size / total_units) * 100
percentage20
regress_multipla_20 = lm(anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
                         data=subset(conviventi_e_sposati, rel==5 & popgrp==3 & mar==1 & geo2011==2))
summary(regress_multipla_20)
# Eliminazione delle variabili non significative
regress_multipla_20 = lm(anni_convivenza ~ age,
                         data=subset(conviventi_e_sposati, rel==5 & popgrp==3 & mar==1 & geo2011==2))
summary(regress_multipla_20)
r_quadro20 = summary(regress_multipla_20)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group20_size = sum(dataset_pulito$rel==5 & dataset_pulito$popgrp==3 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                             & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1 & dataset_pulito$age>0,
                             na.rm=TRUE)
potential_group20_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(dataset_pulito$rel==5 & dataset_pulito$popgrp==3 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                 & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                   !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                   !is.na(dataset_pulito$mar) & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              dataset_pulito$rel==5 & dataset_pulito$popgrp==3 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2
                             & !is.na(dataset_pulito$age)& !is.na(dataset_pulito$rel) &
                               !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) &
                               !is.na(dataset_pulito$mar) & dataset_pulito$married==1))


#con i 20 gruppi qui sopra abbiamo coperto il 92,474% della popolazione
total_percentage = percentage1 + percentage2 + percentage3 + percentage4 + percentage5 +
  percentage6 + percentage7 + percentage8 + percentage9 + percentage10 +
  percentage11 + percentage12 + percentage13 + percentage14 + percentage15 +
  percentage16 + percentage17 + percentage18 + percentage19 + percentage20
total_percentage
#con il gruppo successivo andiamo a creare un modello di regressione lineare multipla
#per le unità rimanenti non appartenenti a nessun gruppo.


# Gruppo 21:
# Gruppo unità rimanenti:
# Calcolo delle unità rimanenti escludendo i primi 20 gruppi specificati
group21_size <- sum(!(
  (conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==2) |
    (conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==1) |
    (conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==2 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==2) |
    (conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==2) |
    (conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==4 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==2) |
    (conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==1) |
    (conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==2 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==2) |
    (conviventi_e_sposati$rel==6 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==1) |
    (conviventi_e_sposati$rel==1 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==1) |
    (conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==3) |
    (conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==2 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==3) |
    (conviventi_e_sposati$rel==6 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==2) |
    (conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==3) |
    (conviventi_e_sposati$rel==1 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==1) |
    (conviventi_e_sposati$rel==1 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==2) |
    (conviventi_e_sposati$rel==1 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==2) |
    (conviventi_e_sposati$rel==6 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==1) |
    (conviventi_e_sposati$rel==6 & conviventi_e_sposati$popgrp==1 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==2) |
    (conviventi_e_sposati$rel==2 & conviventi_e_sposati$popgrp==2 & conviventi_e_sposati$mar==2 & conviventi_e_sposati$geo2011==3) |
    (conviventi_e_sposati$rel==5 & conviventi_e_sposati$popgrp==3 & conviventi_e_sposati$mar==1 & conviventi_e_sposati$geo2011==2)
))
group21_size
# 545 unità statistiche rimanenti, pari al 7.527% delle unità in popolazione.
percentage21 <- (group21_size / total_units) * 100
percentage21
regress_multipla_21 = lm(
  anni_convivenza ~ age + hhsizer + anni_istruzione + ppincome,
  data = subset(conviventi_e_sposati, 
                !((mar == 1 & rel == 2 & popgrp == 1 & geo2011 == 2) # Gruppo 1
                  | (mar == 1 & rel == 2 & popgrp == 1 & geo2011 == 1) # Gruppo 2
                  | (mar == 1 & rel == 2 & popgrp == 2 & geo2011 == 2) # Gruppo 3
                  | (mar == 2 & rel == 2 & popgrp == 1 & geo2011 == 2) # Gruppo 4
                  | (mar == 1 & rel == 2 & popgrp == 4 & geo2011 == 2) # Gruppo 5
                  | (mar == 2 & rel == 2 & popgrp == 1 & geo2011 == 1) # Gruppo 6
                  | (mar == 2 & rel == 2 & popgrp == 2 & geo2011 == 2) # Gruppo 7
                  | (mar == 1 & rel == 6 & popgrp == 1 & geo2011 == 1) # Gruppo 8
                  | (mar == 1 & rel == 1 & popgrp == 1 & geo2011 == 1) # Gruppo 9
                  | (mar == 1 & rel == 2 & popgrp == 1 & geo2011 == 3) # Gruppo 10
                  | (mar == 1 & rel == 2 & popgrp == 2 & geo2011 == 3) # Gruppo 11
                  | (mar == 1 & rel == 6 & popgrp == 1 & geo2011 == 2) # Gruppo 12
                  | (mar == 2 & rel == 2 & popgrp == 1 & geo2011 == 3) # Gruppo 13
                  | (mar == 2 & rel == 1 & popgrp == 1 & geo2011 == 1) # Gruppo 14
                  | (mar == 2 & rel == 1 & popgrp == 1 & geo2011 == 2) # Gruppo 15
                  | (mar == 1 & rel == 1 & popgrp == 1 & geo2011 == 2) # Gruppo 16
                  | (mar == 2 & rel == 6 & popgrp == 1 & geo2011 == 1) # Gruppo 17
                  | (mar == 2 & rel == 6 & popgrp == 1 & geo2011 == 2) # Gruppo 18
                  | (mar == 2 & rel == 2 & popgrp == 2 & geo2011 == 3) # Gruppo 19
                  | (mar == 1 & rel == 5 & popgrp == 3 & geo2011 == 2) # Gruppo 20
                )))
summary(regress_multipla_21)
# Eliminazione delle variabili non significative
regress_multipla_21 = lm(
  anni_convivenza ~ age + hhsizer,
  data = subset(conviventi_e_sposati, 
                !((mar == 1 & rel == 2 & popgrp == 1 & geo2011 == 2) # Gruppo 1
                  | (mar == 1 & rel == 2 & popgrp == 1 & geo2011 == 1) # Gruppo 2
                  | (mar == 1 & rel == 2 & popgrp == 2 & geo2011 == 2) # Gruppo 3
                  | (mar == 2 & rel == 2 & popgrp == 1 & geo2011 == 2) # Gruppo 4
                  | (mar == 1 & rel == 2 & popgrp == 4 & geo2011 == 2) # Gruppo 5
                  | (mar == 2 & rel == 2 & popgrp == 1 & geo2011 == 1) # Gruppo 6
                  | (mar == 2 & rel == 2 & popgrp == 2 & geo2011 == 2) # Gruppo 7
                  | (mar == 1 & rel == 6 & popgrp == 1 & geo2011 == 1) # Gruppo 8
                  | (mar == 1 & rel == 1 & popgrp == 1 & geo2011 == 1) # Gruppo 9
                  | (mar == 1 & rel == 2 & popgrp == 1 & geo2011 == 3) # Gruppo 10
                  | (mar == 1 & rel == 2 & popgrp == 2 & geo2011 == 3) # Gruppo 11
                  | (mar == 1 & rel == 6 & popgrp == 1 & geo2011 == 2) # Gruppo 12
                  | (mar == 2 & rel == 2 & popgrp == 1 & geo2011 == 3) # Gruppo 13
                  | (mar == 2 & rel == 1 & popgrp == 1 & geo2011 == 1) # Gruppo 14
                  | (mar == 2 & rel == 1 & popgrp == 1 & geo2011 == 2) # Gruppo 15
                  | (mar == 1 & rel == 1 & popgrp == 1 & geo2011 == 2) # Gruppo 16
                  | (mar == 2 & rel == 6 & popgrp == 1 & geo2011 == 1) # Gruppo 17
                  | (mar == 2 & rel == 6 & popgrp == 1 & geo2011 == 2) # Gruppo 18
                  | (mar == 2 & rel == 2 & popgrp == 2 & geo2011 == 3) # Gruppo 19
                  | (mar == 1 & rel == 5 & popgrp == 3 & geo2011 == 2) # Gruppo 20
                )))
summary(regress_multipla_21)
r_quadro21 = summary(regress_multipla_21)$r.squared
# Numero di potenziali unità statistiche il modello è applicabile
potential_group21_size <- sum(!(
  (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==4 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==3) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==1 & dataset_pulito$geo2011==3) |
    (dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==3) |
    (dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==2 & dataset_pulito$geo2011==3) |
    (dataset_pulito$rel==5 & dataset_pulito$popgrp==3 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2)) &
    !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer) & !is.na(dataset_pulito$rel) & 
    !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) & !is.na(dataset_pulito$mar) 
  & dataset_pulito$married==1 & dataset_pulito$age>0, na.rm=TRUE)
potential_group21_size
#indice di righe per cui si puo stimare la lunghezza del matrimonio
indices <- which(!(
  (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==4 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==3) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==1 & dataset_pulito$geo2011==3) |
    (dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==3) |
    (dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==2 & dataset_pulito$geo2011==3) |
    (dataset_pulito$rel==5 & dataset_pulito$popgrp==3 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2)) &
    !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer) & !is.na(dataset_pulito$rel) & 
    !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) & !is.na(dataset_pulito$mar) 
  & dataset_pulito$married==1)
#stima della lunghezza del matrimonio in anni_mar_stimato
dataset_pulito$anni_mar_stimato[indices] <- predict(regress_multipla_1, newdata = subset(dataset_pulito,
                              !(
  (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==4 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==3) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==1 & dataset_pulito$geo2011==3) |
    (dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==3) |
    (dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==1 & dataset_pulito$popgrp==1 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==1) |
    (dataset_pulito$rel==6 & dataset_pulito$popgrp==1 & dataset_pulito$mar==2 & dataset_pulito$geo2011==2) |
    (dataset_pulito$rel==2 & dataset_pulito$popgrp==2 & dataset_pulito$mar==2 & dataset_pulito$geo2011==3) |
    (dataset_pulito$rel==5 & dataset_pulito$popgrp==3 & dataset_pulito$mar==1 & dataset_pulito$geo2011==2)) &
    !is.na(dataset_pulito$age) & !is.na(dataset_pulito$hhsizer) & !is.na(dataset_pulito$rel) & 
    !is.na(dataset_pulito$popgrp) & !is.na(dataset_pulito$geo2011) & !is.na(dataset_pulito$mar) 
  & dataset_pulito$married==1))


#i modelli che abbiamo trovato hanno un r quadro medio di 0.5279853, che è un valore
#abbastanza buono. Abbiamo fatto la media ponderata perchè ognuno degli r_quadro 
#"pesa" in maniera diversa sulla popolazione perchè il modello di regressione da cui è
#stato preso rappresenta solo una parte della popolazione.
#Ricordo che inoltre si è assunto che il nostro campione rappresenti perfettamente 
#la nostra popolazione di riferimento.


somma_r_quadro = r_quadro1 * percentage1 + r_quadro2 * percentage2 + r_quadro3 * percentage3 + r_quadro4 * percentage4 + r_quadro5 * percentage5 +
  r_quadro6 * percentage6 + r_quadro7 * percentage7 + r_quadro8 * percentage8 + r_quadro9 * percentage9 + r_quadro10 * percentage10 +
  r_quadro11 * percentage11 + r_quadro12 * percentage12 + r_quadro13 * percentage13 + r_quadro14 * percentage14 + r_quadro15 * percentage15 +
  r_quadro16 * percentage16 + r_quadro17 * percentage17 + r_quadro18 * percentage18 + r_quadro19 * percentage19 +
  r_quadro20 * percentage20 + r_quadro21 * percentage21
r_quadro_medio = somma_r_quadro/100
r_quadro_medio


#se dovessimo escudere i valori r quadro estremi 
r_quadro = c ( r_quadro1 , r_quadro2 , r_quadro3 , r_quadro4, r_quadro5 ,
               r_quadro6 , r_quadro7 , r_quadro8 , r_quadro9 , r_quadro10 ,
               r_quadro11 , r_quadro12 , r_quadro13 , r_quadro14 , r_quadro15 ,
               r_quadro16 , r_quadro17 , r_quadro18 , r_quadro19 , r_quadro20 ,
               r_quadro21)
sd (r_quadro, na.rm=TRUE)
r_quadro_medio + 2.5 * sd (r_quadro, na.rm=TRUE)
r_quadro_medio - 2.5 * sd (r_quadro, na.rm=TRUE)


#i valori estremi, cioè quelli che differiscono di piu di 2.5 volte la deviazione 
#standard dalla media, sono solo r_quadro18. se lo eliminiamo la nostra media di 
#r quadro risulta essere il seguente, mostrandoci come esso non cambia significativamente

somma_r_quadro = r_quadro1 * percentage1 + r_quadro2 * percentage2 + r_quadro3 * percentage3 + r_quadro4 * percentage4 + r_quadro5 * percentage5 +
  r_quadro6 * percentage6 + r_quadro7 * percentage7 + r_quadro8 * percentage8 + r_quadro9 * percentage9 + r_quadro10 * percentage10 +
  r_quadro11 * percentage11 + r_quadro12 * percentage12 + r_quadro13 * percentage13 + r_quadro14 * percentage14 + r_quadro15 * percentage15 +
  r_quadro16 * percentage16 + r_quadro17 * percentage17 + r_quadro19 * percentage19 +
  r_quadro20 * percentage20 + r_quadro21 * percentage21
r_quadro_medio = somma_r_quadro/(100 - percentage18)
r_quadro_medio

#andiamo ora ad unire la colonna anni di convivenza e anni mar stimato in un unica colonna
dataset_pulito$anni_conv_matrim=coalesce(dataset_pulito$anni_convivenza, abs(ceiling(dataset_pulito$anni_mar_stimato)))

#sono presenti unità statistiche che hanno durata del matrimonio negativa
#il che significa che non sarebbero dovuto essere sposati secondo le nostre 
#regressioni ma lo sono.
sum(dataset_pulito$anni_mar_stimato<0, na.rm=TRUE)

#come possiamo vedere dalla somma di tutte le percentage i nostri 21 modelli coprono
#il 100% delle nostre unità statistiche.
total_percentage = percentage1 + percentage2 + percentage3 + percentage4 + percentage5 +
  percentage6 + percentage7 + percentage8 + percentage9 + percentage10 +
  percentage11 + percentage12 + percentage13 + percentage14 + percentage15 +
  percentage16 + percentage17 + percentage18 + percentage19 + percentage20 +
  percentage21
total_percentage

#andremo ora a creare un grafico a torta con le diverse percentuali dei diversi gruppi,
#per dare un idea di come le unità siano distribuite nei diversi gruppi.
#per facilità il gruppo percentage_remaining verrà visualizzato nel grafico come
#gruppo numero 21, e si è preferito non inserire nessun titolo senno il gruppo 
#numero 2 e la relativa percentuale non era di facile lettura.
#affinche il codice seguente stampi il grafico è necessario ingrandire quanto
#piu possibile la sezione plots di r, altrimenti si puo diminuire i valori in par
#ma piu essi divengono piccoli e piu il grafico sarà, nell parte in basso a destra, 
#di difficile lettura.

percentages = c(percentage1, percentage2, percentage3, percentage4, percentage5,
                 percentage6, percentage7, percentage8, percentage9, percentage10,
                 percentage11, percentage12, percentage13, percentage14, percentage15,
                 percentage16, percentage17, percentage18, percentage19,
                percentage20, percentage21)

par(mar = c(0.1, 0.1, 0.1, 0.1))    
par(pin = c(6.816, 6.816))        

labels <- paste(1:21, " (", percentages, "%)", sep="")
pie(percentages, 
    labels = labels,
    col = rainbow(length(percentages)), radius=1)



#andando ora a confrontare le unità prese in considerazione nel modello iniziale con le 
#unità di cui adesso possiamo andare ad applicare un modello di regressione notiamo 
#come il numero complessivo di unità a cui è applicabile un modello è ovviamente maggiore
#del numero di unità prese da noi in considerazione inizialmente.

unita_modello_iniziale <- group1_size + group2_size + group3_size + group4_size + group5_size + 
  group6_size + group7_size + group8_size + group9_size + group10_size + group11_size + 
  group12_size + group13_size + group14_size + group15_size + group16_size + group17_size +
  group18_size + group19_size + group20_size + group21_size
unita_modello_iniziale


unità_finali_analizzabili <- potential_group1_size + potential_group2_size + potential_group3_size + 
  potential_group4_size + potential_group5_size + potential_group6_size + 
  potential_group7_size + potential_group8_size + potential_group9_size + 
  potential_group10_size + potential_group11_size + potential_group12_size + 
  potential_group13_size + potential_group14_size + potential_group15_size + 
  potential_group16_size + potential_group17_size + potential_group18_size + 
  potential_group19_size + potential_group20_size + potential_group21_size
unità_finali_analizzabili

