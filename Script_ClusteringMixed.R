set.seed(1680)
library(dplyr)   
library(cluster)
library(ggplot2) 


if(Sys.info()[names(Sys.info())=="user"]=="Dotto"){
  setwd("C:/Users/Dotto/Dropbox/Università/Corsi/Didattica/Tesi/Suriano/Script/Script")
  }else if(Sys.info()[names(Sys.info())=="user"]=="2003l"){
  setwd("C:\\Users\\2003l\\Dropbox\\Suriano\\Script\\Script")
  
}

#data cleaning
data1 <- read.csv("customer_data11.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "NA")
data1 <- na.omit(data1)
data1 <- data1 %>% select(-id)
#data1 <- data1 %>% select(-satisfaction_score)
v_cat <- c("gender", "education", "region", "loyalty_status", "purchase_frequency", "product_category")
v_num <- c("age", "purchase_amount","income","promotion_usage","satisfaction_score")
data1[v_cat] <- lapply(data1[v_cat], as.factor)


#Riduzione dimensione dataset a 40000 unità
data0 <- data1[sample(1:nrow(data1), 40000), ]




#DIstanza di gower
gower_dist <- daisy(data0, metric = "gower", stand = TRUE)



mod.list = list()

#silhouette width
sil_width <- c(NA)
for(i in 2:10){pam_fit <- pam(gower_dist, diss = TRUE, k = i)
mod.list[[i]] = pam_fit
  sil_width[i] <- pam_fit$silinfo$avg.width}
#grafico
plot(1:10, sil_width,
     xlab = "N cluster",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
#optimal_k <- which.max(sil_width)
optimal_k <- 2

cat("Numero ottimale di cluster:", optimal_k, "\n")
save(mod.list,file="mod_list.rda")

mod.scelto = mod.list[[2]]

plot(data0$income,data0$purchase_amount,col=data0$Cluster)

plot(data0$income, data0$purchase_amount, 
     col=data0$Cluster,
     pch = 16,
     xlab = "Income", 
     ylab = "Purchase Amount",
     cex.lab = 1.4, 
     cex.main = 1.6, 
     cex.axis = 1.2) 
legend("topleft", 
       legend = c("Cluster 1", "Cluster 2"), 
       col=data0$Cluster, 
       pch = 16, 
       cex = 1.2, 
       bty = "n")

correlation <- cor(data0$income, data0$purchase_amount, method = "pearson")
print(correlation)


#Algoritmo PAM con gower distance
pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
pam_results <- data0 %>%
  mutate(Cluster = pam_fit$clustering) %>%
  group_by(Cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary
#Medoidi risultanti per numero di cluster ottimali
data0[pam_fit$medoids, ]

data0 <- data0 %>% mutate(Cluster = factor(pam_fit$clustering))





tab <- table(data0$Cluster, data0$promotion_usage)
tab



n1 <- sum(data0$Cluster == 1)
n2 <- sum(data0$Cluster == 2)





#Summary delle variabili numeriche per cluster
summary_numeric <- data0 %>% 
  group_by(Cluster) %>%
  summarise(across(all_of(v_num), 
                   list(mean = mean, median = median, sd = sd),
                   .names = "{.col}_{.fn}"))
print(summary_numeric)

#Summary delle variabili categoriche per cluster
for(var in v_cat) {
  cat(var)
  print(table(data0[[var]], data0$Cluster))
}


cor(data0 %>% filter(Cluster == 1) %>% select(all_of(v_num)))
cor(data0 %>% filter(Cluster == 2) %>% select(all_of(v_num)))

table(data0$gender,data0$education)

gc.cat = paste(data0$gender,data0$Cluster)
data0$gc.cat = gc.cat
boxplot(data0$income~data0$gc.cat)

boxplot(data0$income~data0$education)

boxplot(data0$purchase_amount~data0$Cluster)
boxplot(data0$purchase_amount~data0$region)
table(data0$education,data0$Cluster)
table(data0$gender,data0$Cluster)
table(data0$gender,data0$region)
table(data0$gender,data0$education)


# Livelli di educazione e genere nei diversi cluster

t2.ed = table(data0$education[which(data0$Cluster==2)],
      data0$gender[which(data0$Cluster==2)])
t2.tot.ed = apply(t2.ed,2,sum)
t2.rel.ed = sweep(t2.ed,2,t2.tot.ed,"/")

t1.ed = table(data0$education[which(data0$Cluster==1)],
           data0$gender[which(data0$Cluster==1)])
t1.tot.ed = apply(t1.ed,2,sum)
t1.rel.ed = sweep(t1.ed,2,t1.tot.ed,"/")
cbind(t1.rel.ed,t2.rel.ed)



# Tipo di acquisto per genere nei diversi cluster

t2.cat = table(data0$product_category[which(data0$Cluster==2)],
           data0$gender[which(data0$Cluster==2)])
t2.tot.cat = apply(t2.cat,2,sum)
t2.rel.cat = sweep(t2.cat,2,t2.tot.cat,"/")

t1.cat = table(data0$product_category[which(data0$Cluster==1)],
           data0$gender[which(data0$Cluster==1)])
t1.tot.cat = apply(t1.cat,2,sum)
t1.rel.cat = sweep(t1.cat,2,t1.tot.cat,"/")
cbind(t1.rel.cat,t2.rel.cat)

# Regione per genere nei diversi cluster

t2.reg = table(data0$region[which(data0$Cluster==2)],
               data0$gender[which(data0$Cluster==2)])
t2.tot.reg = apply(t2.reg,2,sum)
t2.rel.reg = sweep(t2.reg,2,t2.tot.reg,"/")


t1.reg = table(data0$region[which(data0$Cluster==1)],
               data0$gender[which(data0$Cluster==1)])
t1.tot.reg = apply(t1.reg,2,sum)
t1.rel.reg = sweep(t1.reg,2,t1.tot.reg,"/")
cbind(t1.rel.reg,t2.rel.reg)



boxplot(data0$purchase_amount~data0$gc.cat)


conteggio <- table(data0$product_category, data0$gender, data0$Cluster)

conteggio


pam_results <- data0 %>%
  mutate(Cluster = pam_fit$clustering) %>%
  group_by(Cluster) %>%
  do(the_summary = summary(select(., -satisfaction_score, -Cluster)))

pam_results$the_summary


library(ggplot2)
library(dplyr)

# Creazione del grafico a barre per promotion_usage
ggplot(data0, aes(x = Cluster, fill = as.factor(promotion_usage))) +
  geom_bar(position = "dodge") +
  labs(title = "Promotion Usage",
       x = "Cluster",
       y = "Frequenza",
       fill = "Promotion Usage") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )


# Boxplot per variabili numeriche
for(var in v_num) {
  p <- ggplot(data0, aes_string(x = "Cluster", y = var)) +
    geom_boxplot() +
    labs(title = var,
         x = "Cluster",
         y = var) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    )
  print(p)
}

# Barplot per variabili categoriche
for(var in v_cat) {
  p <- ggplot(data0, aes_string(x = "Cluster", fill = var)) +
    geom_bar(position = "dodge") +
    labs(title = var,
         x = "Cluster",
         y = "Frequenza") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12)
    )
  print(p)
}



promotion_percentages <- data0 %>%
  group_by(Cluster, gender) %>%
  summarise(Count = n()) %>%
  group_by(Cluster) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))

print(promotion_percentages)
