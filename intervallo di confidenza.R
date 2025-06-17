#PARTE 3

#ci andiamo in questa parte a cercare l'intervallo di confidenza
#per andare ad inquadrare la media degli anni di convivenza della popolazione 
#partendo dalla media del nostro campione.
#ho usato il dataset in cui abbiamo integrato i dati tramite le regressioni multiple
#trovate nello script precedente.
#nel dataset interv_conf_sposati ho eliminato le unità con un età minore di 0 perchè
#sono chiaramente unità che non possono realmente esistere (nessuno puo avere 
#un età negativa)


#intervallo di confidenza sposati 

interv_conf_sposati=select(filter(dataset_pulito, 
                                   married==1, 
                                   mar==1,
                                   !is.na(pid),
                                   !is.na(married),
                                   !is.na(mar),
                                   !is.na(anni_conv_matrim),
                                   age>0), 
                            -em1, -married, -mary_m, -mary_l, -monthlyhours,
                            -best_edu, -anni_convivenza, -anni_mar_stimato,
                           -rel, -popgrp, -geo2011, -ppincome, -hhsizer,
                           -anni_istruzione, -mar, -age)

#imponiamo alfa uguale a 0,05
a=0.05

#mi calcolo ora la media del mio campione degli anni di matrimonio
media_sposati= mean(interv_conf_sposati$anni_conv_matrim)

#deviazione standard degli anni di matrimonio
dev_stand_sposati=sd(interv_conf_sposati$anni_conv_matrim)

#definiamo z alfa mezzi
z_alfa_mezzi=qnorm(1-(a/2))

#calcoliamo il limite inferiore e superiore del nostro intervallo di confidenza
limite_inferiore_sposati_1=media_sposati-
  (z_alfa_mezzi*(dev_stand_sposati/sqrt(nrow(interv_conf_sposati))))
limite_superiore_sposati_1=media_sposati+
  (z_alfa_mezzi*(dev_stand_sposati/sqrt(nrow(interv_conf_sposati))))

#imponiamo a=0.00001 e studiamo come cambia l'intervallo di confidenza
a_2=0.00001
z_alfa_mezzi_2=qnorm(1-(a_2/2))
limite_inferiore_sposati_2=media_sposati-(z_alfa_mezzi_2*(dev_stand_sposati/sqrt(nrow(interv_conf_sposati))))
limite_superiore_sposati_2=media_sposati+(z_alfa_mezzi_2*(dev_stand_sposati/sqrt(nrow(interv_conf_sposati))))

#E' quindi probabile al 99.99999% che un intervallo tra i 20 anni e 2 mesi 
#(limite_inferiore_sposati_2) e i 21 anni e 9 mesi (limite_superiore_sposati_2) 
#comprenda la durata media del matrimonio dell'intera popolazione



#intervallo di confidenza conviventi 

interv_conf_conviventi=select(filter(dataset_pulito, 
                                  married==1, 
                                  mar==2,
                                  !is.na(pid),
                                  !is.na(married),
                                  !is.na(mar),
                                  !is.na(anni_conv_matrim),
                                  age>0), 
                           -em1, -married, -mary_m, -mary_l, -monthlyhours,
                           -best_edu, -anni_convivenza, -anni_mar_stimato,
                           -rel, -popgrp, -geo2011, -ppincome, -hhsizer,
                           -anni_istruzione, -mar, -age)

#imponiamo alfa uguale a 0.05
a=0.05

#mi calcolo ora la media del mio campione degli anni di convivenza
media_conviventi = mean(interv_conf_conviventi$anni_conv_matrim)

#deviazione standard degli anni di matrimonio/convivenza
dev_stand_conviventi = sd(interv_conf_conviventi$anni_conv_matrim)

#definiamo z alfa mezzi
z_alfa_mezzi=qnorm(1-(a/2))

#calcoliamo il limite inferiore e superiore del nostro intervallo di confidenza
limite_inferiore_conviventi_1=media_conviventi-
  (z_alfa_mezzi*(dev_stand_conviventi/sqrt(nrow(interv_conf_conviventi))))
limite_superiore_conviventi_1=media_conviventi+
  (z_alfa_mezzi*(dev_stand_conviventi/sqrt(nrow(interv_conf_conviventi))))

#imponiamo a=0.00001 e studiamo come cambia l'intervallo di confidenza
a_2=0.00001
z_alfa_mezzi_2=qnorm(1-(a_2/2))
limite_inferiore_conviventi_2=media_conviventi-(z_alfa_mezzi_2*(dev_stand_conviventi/sqrt(nrow(interv_conf_conviventi))))
limite_superiore_conviventi_2=media_conviventi+(z_alfa_mezzi_2*(dev_stand_conviventi/sqrt(nrow(interv_conf_conviventi))))

#E' quindi probabile al 99.99999% che un intervallo tra i 9 anni e 8 mesi (limite_inferiore_conviventi_2)
# e i 11 anni e 7 mesi (limite_superiore_conviventi_2) comprenda la durata media 
#della convivenza dell'intera popolazione.
