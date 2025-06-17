#data cleaning

library(haven)
library(dplyr)
library(corrplot) 
library(lmtest)
library(regclass)
library(ISLR2)
library(precrec)
library(PRROC)
library(pROC)


dataset_pulito = data.frame(
  pid = data.sel$pid,
  married = data.sel$married, 
  popgrp = data.sel$popgrp,
  mar = data.sel$mar, 
  mary_m = data.sel$mary_m, 
  mary_l = data.sel$mary_l, 
  em1 = data.sel$em1,
  rel = data.sel$rel,  
  geo2011 = data.sel$geo2011,
  best_edu = data.sel$best_edu,
  ppincome = data.sel$hhincome/data.sel$hhsizer,#questa variabile indica l'income per persona
  hhsizer = data.sel$hhsizer, 
  monthlyhours = data.sel$monthlyhours, 
  age = data.sel$age
)

dataset_pulito$anni_convivenza=coalesce(dataset_pulito$mary_m, dataset_pulito$mary_l)


#andiamo ora ad inserire nel dataset la colonna riguardante gli anni di istruzione,
#gestiti come segue:
#1(Grade 1 (Previously Sub A/Class 1)) = 1 anno di istruzione
#2(Grade 2 (Previously Sub B/Class 2)) = 2 anni di istruzione
#3(Grade 3 (Std. 1)) = 3 anni di istruzione
#4(Grade 4 (Std. 2)) = 4 anni di istruzione
#5(Grade 5 (Std. 3)) = 5 anni di istruzione
#6(Grade 6 (Std. 4)) = 6 anni di istruzione
#7(Grade 7 (Std. 5)) = 7 anni di istruzione
#8(Grade 8 (Std. 6/Form 1)) = 8 anni di istruzione
#9(Grade 9 (Std. 7/Form 2)) = 9 anni di istruzione
#10(Grade 10 (Std. 8/Form 3)) = 10 anni di istruzione
#11(Grade 11 (Std. 9/Form 4)) = 11 anni di istruzione
#13(NTC 1/NCV 2) = 10 anni di istruzione 
#14(NTC 2/CV 3) = 11 anni di istruzione 
#15(NTC 3/NCV 4) = 12 anni di istruzione 
#16(Certificate not requiring Grade 12/Std. 10) = 9.5 anni di istruzione 
#17(Diploma not requiring Grade 12/Std. 10) = 10.5 anni di istruzione
#18(Certificate requiring Grade 12/Std. 10) = 12.5 anni di istruzione
#19(Diploma requiring Grade 12/Std. 10) = 13.5 anni di istruzione
#20(Bachelors Degree) = 15 anni di istruzione
#21(Bachelors Degree and diploma)=16.5 anni di istruzione
#22(Honours Degree) = 16 anni di istruzione
#23(Higher Degree (Masters, Doctorate))=18.5 anni di istruzione
#24(Others (Specify))= eliminerò le 21 unità statistiche che presentano questo valore 
#perchè non sono in possesso della specifica da essi inserita nel dizionario. inoltre 
#risulterebbe potrebbe rivelarsi essere complicato individuare la quantità precisa
#di anni di studio pur avendo la specifica fornita dagli intervistati
#25(No Schooling) = 0 anni di istruzione
#27(National Certificate Vocational 2 (NCV 2)) = 10 anni di istruzione
#28(National Certificate Vocational 3 (NCV 3)) = 11 anni di istruzione
#29(National Certificate Vocational 4 (NCV 4)) = 12 anni di istruzione
#30(N1 (NATED)/NTC 1) = 10 anni di istruzione
#31(N2 (NATED)/NTC 2) = 11 anni di istruzione
#32(N3 (NATED)/NTC 3) = 12 anni di istruzione
#33(N4 (NATED)) = 13 anni di istruzione
#34(N5 (NATED)) = 14 anni di istruzione
#34(N6 (NATED)) = 15 anni di istruzione

dataset_pulito$anni_istruzione <- ifelse(dataset_pulito$best_edu == 0, 0,
                              ifelse(dataset_pulito$best_edu == 1, 1,
                              ifelse(dataset_pulito$best_edu == 2, 2,
                              ifelse(dataset_pulito$best_edu == 3, 3,
                              ifelse(dataset_pulito$best_edu == 4, 4,
                              ifelse(dataset_pulito$best_edu == 5, 5,
                              ifelse(dataset_pulito$best_edu == 6, 6,
                              ifelse(dataset_pulito$best_edu == 7, 7,
                              ifelse(dataset_pulito$best_edu == 8, 8,
                              ifelse(dataset_pulito$best_edu == 9, 9,
                              ifelse(dataset_pulito$best_edu == 10, 10,
                              ifelse(dataset_pulito$best_edu == 11, 11,
                              ifelse(dataset_pulito$best_edu == 12, 12,
                              ifelse(dataset_pulito$best_edu == 13, 10,
                              ifelse(dataset_pulito$best_edu == 14, 11,
                              ifelse(dataset_pulito$best_edu == 15, 12,
                              ifelse(dataset_pulito$best_edu == 16, 9.5,
                              ifelse(dataset_pulito$best_edu == 17, 10.5,
                              ifelse(dataset_pulito$best_edu == 18, 12.5,
                              ifelse(dataset_pulito$best_edu == 19, 13.5,
                              ifelse(dataset_pulito$best_edu == 20, 15,
                              ifelse(dataset_pulito$best_edu == 21, 16.5,
                              ifelse(dataset_pulito$best_edu == 22, 16,
                              ifelse(dataset_pulito$best_edu == 23, 18.5,
                              ifelse(dataset_pulito$best_edu == 25, 0,
                              ifelse(dataset_pulito$best_edu == 27, 10,
                              ifelse(dataset_pulito$best_edu == 28, 11,
                              ifelse(dataset_pulito$best_edu == 29, 12,
                              ifelse(dataset_pulito$best_edu == 30, 10,
                              ifelse(dataset_pulito$best_edu == 31, 11,
                              ifelse(dataset_pulito$best_edu == 32, 12,
                              ifelse(dataset_pulito$best_edu == 33, 13,
                              ifelse(dataset_pulito$best_edu == 34, 14,
                              ifelse(dataset_pulito$best_edu == 35, 15,NA
                              ))))))))))))))))))))))))))))))))))



#lo scopo di questa nostra analisi è riuscire a stimare la durata del matrimonio/convivenza delle unità 
#a partire da fattori quali il grado di studio, l'income, la grandezza della famiglia e l'età,
#l etnia dell'unità statistica, la sua religione o il posto di residenza .
#volevo anche inserire tra le variabili la quantità di lavoro in un mese, ma
#da un analisi da me precedentemente effettuata (lo script progetto 1)) sulle variabili 
#che gia avevano i suddetti dati (quindi senza andarci a stimare la variabile mar che
#a noi manca e che ora con il processo di regressione logistica andreamo a trovare
#per le variabili che hanno tutte le variabili di cui sopra tranne appunto la variabile
#mar) nonostante avessi inserito nei modelli di regresssione multipla anche le ore
#lavorate, questa variabile non era statisticamente significative in nessun caso.
#ho quindi reputato opportuna eliminarla dalla regressione logistica e dalle regressioni
#multiple che a essa seguiranno, visto anche che quando si richiede che monthlyhours
#sia !=NA il nostro campione statistico si riduce di montissimo, di piu del 50%.
#questa significativa riduzione si puo vedere dalle seguenti due somme:


sum(dataset_pulito$married==1 &
      !is.na(dataset_pulito$ppincome)&!is.na(dataset_pulito$hhsizer)&
      !is.na(dataset_pulito$age)&!is.na(dataset_pulito$anni_convivenza)&
      !is.na(dataset_pulito$anni_istruzione) &
      !is.na(dataset_pulito$monthlyhours))
sum(dataset_pulito$married==1 &
      !is.na(dataset_pulito$ppincome)&!is.na(dataset_pulito$hhsizer)&
      !is.na(dataset_pulito$age)&!is.na(dataset_pulito$anni_convivenza)&
      !is.na(dataset_pulito$anni_istruzione))



#studiando il nostro dataset, notiamo che sono presenti unità di cui noi sappiamo il valore della 
#variabile married, ma non sappiamo il valore di mar. molte delle unità infatti non
#hanno la variabile mar perche non sono sposate ne conviventi (se married è 0 il valore
#di mar è il piu delle volte na), ma ci sono tante unità statistiche (1665) che nonostante 
#abbiano un valore married=1 ("Married or living with partner") non hanno alcun valore mar.

sum(dataset_pulito$married==1 & is.na(dataset_pulito$mar))

#tra le variabili non numeriche, l unica binomiale che ci puo interessare andarci a 
#stimare partendo da questi fattori è, sapendo che le unità statistiche sono 
#sposate o convivono (quindi married=1), l'informazione riguardo l'essere sposato
#o semplicemente convivente dell'unita statistica in questione.
#ci interesssa quindi sapere se la variabile mar assume valori 1 (formalmente
#sposati) o 2 (conviventi). la possibilita che mar=3 (non vivono insieme) la scarterò,
#e per ulteriori dettagli rimando al paper di accompagnamento allo script.
#mar verrà quindi trattata come una binomiale.
#mi limiterò, con la sum qui sotto, a far notare come l'eventualità di unità sposata 
#(married=1) ma non convivente (mar=3) sia remota (0,04884% delle unità totali):
sum(dataset_pulito$married==1 & dataset_pulito$mar==3, na.rm=TRUE)


#per predirre se le unità siano mar=1 oppure mar=2 (se married=1)
#andramo ad effettuare una regressione logistica, utilizzando il seguente dataset
#(levando da dataset pulito molte colonne, facendo in particolare rimanere in 
#sposati_e_non_logistica solo variabili numeriche su cui, date le nostra conoscenze,
#possiamo applicere la regessione logistica.)
#aggiamo anche impoto che l'età sia maggiore di zero, perchè ci sono dei valori
#negativi nel dataset pulito (sono 19).

sposati_e_non_logistica=select(filter(dataset_pulito,!is.na(mar) & married==1 & 
                                 (mar==1 | mar==2) &
                                 !is.na(ppincome) & !is.na(hhsizer) & !is.na(age) & 
                                 !is.na(anni_convivenza) & !is.na(anni_istruzione) 
                                 &age>0),
                               -mary_m, -mary_l, -best_edu, -monthlyhours, -popgrp,
                               -pid, -em1, -rel, -geo2011, -best_edu, -married)


#il processo di regressione logistica che ora andremo ad effettuare ci permetterà di stimare
#lo stato della variabile mar di 867 variabili (come dimostrato dalla somma qui sotto)
sum(dataset_pulito$married==1 & is.na(dataset_pulito$mar) & !is.na(dataset_pulito$ppincome) & 
      !is.na(dataset_pulito$hhsizer) & !is.na(dataset_pulito$anni_convivenza) &
      !is.na(dataset_pulito$anni_istruzione) & !is.na(dataset_pulito$age))


regressione_logistica=glm(mar-1~ppincome + hhsizer +
                            age + anni_convivenza + anni_istruzione, 
                          data=sposati_e_non_logistica, family = binomial, 
                          control = list(maxit = 1000000000000000))
summary(regressione_logistica)

#andiamo ora a trovarsi la soglia di p che massimizza sia la specificità che la
#sensibilità, e questo è stato fatto tramite lo script della regressione logistica
#messo a disposizione dal professor Dotto.

predict.probs = predict(regressione_logistica,type="response")

p.soglia    = 0.5

predict.class = ifelse(predict.probs>=p.soglia,1,0)

conf.mat = table(Predetti=predict.class,
                 Osservati = as.numeric(sposati_e_non_logistica$mar)-1)


TN = (conf.mat[1,1])
TP = (conf.mat[2,2])

FN = conf.mat[1,2]
FP = conf.mat[2,1]

sensitivity.vec = TP/(TP+FN)
specificity.vec = TN/(TN+FP)

TP;FN
table(Default$default)

RROC_obj <- roc.curve(scores.class0  = regressione_logistica$fitted.values, 
                      weights.class0 = as.numeric(sposati_e_non_logistica$mar)-1,
                      curve=TRUE)
plot(RROC_obj)
names(RROC_obj)


p.soglia.vec    = seq(from=1,to=0,length.out = 300)
sensitivity.vec = c()
specificity.vec = c()

for(j in 1:length(p.soglia.vec)){
  
  predict.class = ifelse(predict.probs>=p.soglia.vec[j],1,0)
  predict.class = c(0,1,predict.class)
  
  predict.class = as.factor(predict.class)
  levels(predict.class) = c("0","1")
  predict.class = predict.class[-c(1,2)]
  conf.mat = table(Predetti=as.factor(predict.class),
                   Osservati = as.factor(as.numeric(sposati_e_non_logistica$mar)-1))
  
  TN = (conf.mat[1,1])
  TP = (conf.mat[2,2])
  
  FN = conf.mat[1,2]
  FP = conf.mat[2,1]
  
  sensitivity.vec[j] = TP/(TP+FN)
  specificity.vec[j] = TN/(TN+FP)
  
}

plot(RROC_obj)


res.mat = cbind(sensitivity.vec,specificity.vec,p.soglia.vec)
diff = abs(res.mat[,1]-res.mat[,2])
p.opt = p.soglia.vec[which.min(diff)]

#la soglia ottimale è quindi la seguente p.opt
p.opt
#andiamo ora, tramite la soglia p.opt, a predirre i valori di mar.
#se il risultato della regressione logistica viene maggiore di p allora è piu 
#probabile che l unità stia semplicemente convivendo rispetto a che sia sposata.
#i valori sono poi stati inseriti nella colonna mar_stimato

sposati_e_non_logistica$mar_stimato=ifelse(predict(regressione_logistica, 
                                                   type="response")>=p.opt,2,1)

#andiamo a vedere ora sensibilità e specificità del nostro modello
#per calcolare true positive/true negative noi ipotizziamo che l essere conviventi (2)
#rappresenti positive e l'essere sposati (1) rappresenti negative.

table(mar=sposati_e_non_logistica$mar,mar_stimato=sposati_e_non_logistica$mar_stimato)

sensibilità=sum(sposati_e_non_logistica$mar_stimato==2 & sposati_e_non_logistica$mar==2)/
  sum(sposati_e_non_logistica$mar==2)

specificità=sum(sposati_e_non_logistica$mar_stimato==1 & sposati_e_non_logistica$mar==1)/
  sum(sposati_e_non_logistica$mar==1)

sensibilità

specificità

summary(regressione_logistica)

#avendo ora questo modello, è ora possibile andare a stimare, con una mediocre
#sicurezza (sensibilità al 74,53% e specificità al 74,72%) chi, tra quelli che
#abitano insieme ma di cui non sappiamo se sono formalmente sposati o solo conviventi,
#sono sposati oppure no. grazie alla regressione logistica appena creata sarà possibile 
#andare ad aggiungere 867 unità statistiche alla nostra analisi (lo abbiamo calcolato
#prima) che corrispondono a tutte quelle unità statistiche di cui si ha a disposizione tutti
#i dati che servono nella nostra regressione ad eccezione di mar, e ora che ne andiamo a
#stimare il valore è possibile aggiungere anche loro all'analisi. Le righe di codice
#sottostanti aggiungono dove possibile nella colonna mar la stima di mar


dataset_pulito$mar=ifelse(is.na(dataset_pulito$mar) & 
                            !is.na(dataset_pulito$ppincome) &
                            !is.na(dataset_pulito$hhsizer) &
                            !is.na(dataset_pulito$age) & 
                            !is.na(dataset_pulito$anni_convivenza) &
                            !is.na(dataset_pulito$anni_istruzione), 
                          ifelse(predict(regressione_logistica,
                                         newdata=subset(dataset_pulito,
                                                        is.na(dataset_pulito$mar) & 
                                                          !is.na(dataset_pulito$ppincome) &
                                                          !is.na(dataset_pulito$hhsizer) &
                                                          !is.na(dataset_pulito$age) & 
                                                          !is.na(dataset_pulito$anni_convivenza) &
                                                          !is.na(dataset_pulito$anni_istruzione)),
                                         type="response")>=p.opt,2,1),
                          dataset_pulito$mar)


#notiamo ora che la somma delle unità per cui tutti i valori è aumentata

sum(!is.na(dataset_pulito$mar) & dataset_pulito$married==1 &
      !is.na(dataset_pulito$ppincome) &
      !is.na(dataset_pulito$hhsizer) &
      !is.na(dataset_pulito$age) & 
      !is.na(dataset_pulito$anni_convivenza) &
      !is.na(dataset_pulito$anni_istruzione))

#se ci si stesse chiedendo a questo punto se si possa fare un ragionamento simile
#anche per stimare em1 per i valori na rimando al paper allegato allo script.


#creo ora un dataset con le unità statistiche che sono sposate o conviventi (married=1)  
#e che hanno tutti i dati per le altre variabili (nessun na).
#non inserisco nell analisi coloro i quali risultano essere married=0 e mar=1
#perche le due cose sono in contrasto (married=0 indica il non essere 
#sposato o convivente e mar=1 l essere sposato) e non vi sono casi nella popolazione.
sum(dataset_pulito$married==0 & dataset_pulito$mar==1, na.rm=TRUE)
#vi sono in popolazione casi in cui married =0 e mar =2, evenienza però illogica 
#dato che mar=2 significa che si convive senza essere sposati mentre 
#married=0 significa che non si ricade nella categoria married=1 (che appunto è
#quella di coloro che o sono sposati o vivono insieme).
#per questo motivo (perche appunto sono dati illogici e contraddittori), ho eliminato
#implicitamente dall'analisi (imponendo married==1) questo caso qui riportato.
sum(dataset_pulito$married==0 & dataset_pulito$mar==2, na.rm=TRUE)
#il caso in cui non sono ne sposati ne conviventi (married=0) e non abitano insieme (mar=3), 
#non è nella nostra popolazione target, cosi come non è nel nostro target coloro che
#sono sposati ma non vivono insieme (vi sono, come si puo vedere, dei casi), perchè 
#noi vogliamo studiare la durata di una convivenza (sia da sposati sia da non sposati)
sum(dataset_pulito$married==0 & dataset_pulito$mar==3, na.rm=TRUE)
#come detto in apertura elimineremo le unità che presentano married=1 e mar==3, perchè 
#sono una porzione di popolazione molto molto esigua (appena 20 osservazioni40944 toteli) e perchè
#abbiamo assunto, prima della regressione, che se married=1 allora mar era uguale o a 1 o a 2.

conviventi_e_sposati=select(filter(dataset_pulito, 
                                   (married==1 & (mar==1 | mar==2)),
                                   !is.na(pid),
                                   !is.na(rel),
                                   !is.na(married),
                                   !is.na(popgrp),
                                   !is.na(mar),
                                   !is.na(geo2011),
                                   !is.na(ppincome),
                                   !is.na(hhsizer),
                                   !is.na(age),
                                   !is.na(anni_convivenza),
                                   !is.na(anni_istruzione),
                                   age>0), 
                            -em1, -married, -mary_m, -mary_l, -monthlyhours,
                            -best_edu)

#ho eliminato la variabile married perchè per ipotesi è sempre uguale ad 1
#ho eliminato le variabili mary_m e mary_l perche sintetizzate in anni convivenza
#ho eliminato la variabile best_edu perche sintetizzata in anni istruzione
#vedere paper per il motivo per cui ho eliminato monthlyhours non tenendolo in considerazioe
#vedere paper per il motivo per cui ho eliminato em1 non tenendolo in considerazioe

#il dataset conviventi e sposati è ora pulito ed è pronto per essere utilizzato 
#nei nostri modelli di regressione lineare multipla, presenti nello script successivo


