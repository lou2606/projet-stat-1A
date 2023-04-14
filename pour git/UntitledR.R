## importation des donnees et selections des donnees importantes

library("dplyr")
library("FactoMineR")
library("readxl")
require("openxlsx")
Database <- read_excel("resultats-par-niveau-subcom-t2-france-entiere.xlsx")
region_paca <- Database[ (Database[,1]== '04' | Database[,1]== '05' | Database[,1]== '06' | Database[,1]== '13' | Database[,1]== '83' | Database[,1]== '84'),]


## debut etude univarie

summary(region_paca)
"
indicateur interessant : par des emploi dans chaque secteur
                         medianne de revennu  
                         
"
?cat
revenu <- read_excel("revenu.xlsx")
for ( i in 1:34965) revenu[i,5] <-substr(revenu[i,1],1,2)
revenu_region_paca <- revenu[ (revenu[,5]== '04' | revenu[,5]== '05' | revenu[,5]== '06' | revenu[,5]== '13' | revenu[,5]== '83' | revenu[,5]== '84'),]
for (i in 1:946) {region_paca[i,34]<- paste(region_paca[i,1], region_paca[i,3], sep = "")}
colnames(region_paca)[34]<- "Code com"
test <- merge( x = region_paca, y =  revenu_region_paca, by.x = "Code com", by.y = "Code")


colnames(test)[31]<- "Voix marine"
colnames(test)[32]<- "% Voix marine / inscrit"
colnames(test)[33]<- "% Voix marine / exprime"
colnames(test)[25]<- "Voix macron"
colnames(test)[26]<- "% Voix macron / inscrit"
colnames(test)[27]<- "% Voix macron / exprime"
colnames(test)[24]<- "tt"

colnames(revenu_region_paca)[3]<- "med"
colnames(revenu)[3]<- "med"
tem <- c(which(revenu_region_paca$med != "N/A - résultat non disponible"))
rpaca <- revenu_region_paca[c(which(revenu_region_paca$med != "N/A - résultat non disponible")),]
rfr <- revenu[c(which(revenu$med != "N/A - résultat non disponible")),]
revenu_region_paca_bis <- transform(rpaca, med = as.integer(med))
revenubis <- transform(rfr, med = as.numeric(med))
?transform
boxplot(revenu_region_paca_bis[3], main = "Revenu median PACA", ylab = "revenu med", ylim = c(10000,55000))
boxplot(revenubis[3], main = "Revenu median FR", ylab = "revenu med", ylim = c(10000,55000))
hist(revenu_region_paca_bis$med,freq = FALSE)
hist(revenubis$med, freq = FALSE)
?hist
?boxplot
revenu <- transform(revenu, med = as.integer(med))
tresetrevpaca <- merge(rpaca, region_paca, by.x = "Code", by.y = "Code com")
tresetrevpaca <- transform(tresetrevpaca, med = as.integer(med))
tresetrevpaca <- transform(tresetrevpaca, med = (round(med, digits = -3)))
tresetrevpaca <- transform(tresetrevpaca, X..Abs.Ins = (round(X..Abs.Ins, digits = 0)))
tresetrevpaca <- transform(tresetrevpaca, X..Voix.Exp = (round(X..Voix.Exp, digits = 0)))
tresetrevpaca <- transform(tresetrevpaca, X..Exp.Vot = (round(X..Exp.Vot, digits = 0)))
tresetrevpaca <- transform(tresetrevpaca, X..Blancs.Vot = (round(X..Blancs.Vot, digits = 0)))
test <- data.frame( tresetrevpaca[31],  tresetrevpaca[3],tresetrevpaca[13],tresetrevpaca[18] )
test2 <- data.frame( tresetrevpaca[31],  tresetrevpaca[13],tresetrevpaca[18])
test3 <- data.frame( tresetrevpaca[18],  tresetrevpaca[3])
plot(test2)
plot(test3)
plot( test, label = c( "pourcentage de voix Macron", " revenu median" , "pourcentage abstention" , "pourcentage de vote blanc"))
q <- PCA(test, scale.unit = TRUE, graph = TRUE, axes=c(1,3))
plot.PCA(a, select='cos2  0.5')
plot.PCA(a, choix = "var")
PCA(test2, scale = TRUE, graph = TRUE)
PCA(test3, scale = TRUE, graph = TRUE)
?PCA
cor(test)
?cor
cov(test)

write.xlsx(region_paca, file ="resultat_paca_2022.xls")










#creation table revenu X resultat niveau France
for (i in 1:34965) {Database[i,34]<- paste(Database[i,1], Database[i,3], sep = "")}
colnames(Database)[34]<- "Code com"
tresetrevFR <- merge(revenu, Database, by.x = "Code", by.y = "Code com")
for ( i in 1:34965) tresetrevFR[i,38] <-substr(tresetrevFR[i,1],1,2)
tresetrevFR[,39] <- ifelse((tresetrevFR[,38]== '04' | tresetrevFR[,38]== '05' | tresetrevFR[,38]== '06' | tresetrevFR[,38]== '13' | tresetrevFR[,38]== '83' | tresetrevFR[,38]== '84'),"A","B")

tresetrevFR <- transform(tresetrevFR, V39 = as.integer(V39))


i = 3



# Agréger les résultats des élections présidentielles par revenu médian des communes
election_results_by_income <- tresetrevpaca %>%
  group_by(med) %>%
  summarize(total_votes = sum(Voix),
            candidate_votes = sum(Voix),
            candidate_percent = X..Voix.Exp)

# Trier la table par ordre croissant de revenu médian
election_results_by_income <- election_results_by_income %>%
  arrange(med)

# Identifier la première tranche de revenu pour laquelle le pourcentage de vote est supérieur ou égal à 50%
income_threshold <- election_results_by_income %>%
  filter(candidate_percent >= 60) %>%
  slice(1) %>%
  pull(med)
election_results_by_income <- arrange(election_results_by_income, med, candidate_percent)
