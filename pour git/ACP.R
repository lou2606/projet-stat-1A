require(openxlsx)
library("dplyr")
library("FactoMineR")
library("readxl")
require("openxlsx")
table_paca <- read.csv("tableau_paca.csv", header = TRUE, sep = ";", encoding = "UTF-8")
table_nationale <- read.csv("tableau_national.csv", header = TRUE, sep = ";", encoding = "UTF-8")
table_age <- read.xlsx("pop_age.xlsx", sep = ";")
table_comp <- read.xlsx("tablecomplementaire.xlsx", sep = ";")
logement_soc <- read.xlsx("tdm.xlsx", sep = ";")
mont_lit <- read.xlsx("litt_mont.xlsx", sep = ";")
vote_2017 <- read.csv("vote_2017_nettoye.csv", sep=",")
mont_lit[,3] <- as.factor(mont_lit[,3])
mont_lit[,4] <- as.factor(mont_lit[,4])

table_comp[,3:22] <- lapply(table_comp[,3:22], as.numeric)
table_age$ratio_0_14 <- table_age$`population;des;0-14;ans;2015` / table_age$`population;2015`
table_age$ratio_15_29 <- table_age$`population;des;15-29;ans;2015` / table_age$`population;2015`
table_age$ratio_30_44 <- table_age$`population;des;30-44;ans;2015` / table_age$`population;2015`
table_age$ratio_45_59 <- table_age$`population;des;45-59;ans;2015` / table_age$`population;2015`
table_age$ratio_60_74 <- table_age$`population;des;60-74;ans;2015` / table_age$`population;2015`
table_age$ratio_75_pl <- table_age$`population;des;75;ans;ou;plus;2015` / table_age$`population;2015`
table_comp <- merge(x = table_comp, y = logement_soc, by = "Code")
table_comp$Part_logement_sociaux <- table_comp$`Nombre;de;logements;sociaux;(RPLS);2019.y` / table_comp$`Nombre;de;logements;2019`

table_paca$codgeo <- sprintf("%05d",table_paca$codgeo )
table_int <- merge(x = table_age[,-2], y = table_comp, by.x = "Code", by.y = "Code", all.x = TRUE) 

table_int$part_actif <- table_int$`Nombre;d'actifs;de;15-64;ans;en;emploi;à;temps;partiel;2019_x000D_;Ensemble`/ table_int$`population;2015`

table_tout <- merge(table_paca, table_int, by.x = "codgeo", by.y = "Code")
table_tout <- table_tout [,-26]
table_tout_sans_na <- na.omit(table_tout)
table_tout[,72]<- table_paca$...3
table_tout[,72]<- as.factor(table_tout[,72])
table_tout <- merge(x = table_tout, y = mont_lit[,-2], by.x = 'codgeo', by.y = 'Code')

vote_2017$codgeo <- sprintf("%05d",vote_2017$codgeo )
table_tout <- merge(x = table_tout, y = vote_2017, by = 'codgeo')

## PCA 2022 originale
q <- PCA(table_tout, scale.unit = TRUE, graph = TRUE, axes=c(1,3))
table_act <- table_tout[, c(6:14, 25, 41, 44, 47, 53, 58,60,64, 70, 42,43,45,50:52,54:57,59,61:63,65,71, 72)]
table_sup <- table_tout[,c(42,43,45,50:52,54:57,59,61:63,65,71)]

q <- PCA(table_act, scale.unit = TRUE, graph = TRUE, axes=c(1,2), quanti.sup = c(19:34, 38:65), quali.sup = c(35:37))
q <- PCA(table_act, scale.unit = TRUE, graph = TRUE, axes=c(1,2), ind.sup = c(15:23))
?PCA

barplot(q$eig[,1], main="Valeurs propres",
        names.arg = paste0("dim", 1:nrow(q$eig)))
# fonction pour plot le graph des individus sans les valeurs supplémentaires
plot.PCA(q, choix = "ind",  invisible =c("ind.sup", "quali.sup"))
# fonction pour plot le graph des individus avec les valeurs supplémentaires sans les autres
plot.PCA(q, choix = "ind",  invisible ="ind")
# fonction pour plot le graph des individus avec les valeurs qualitative supplémentaires sans les autres
plot.PCA(q, choix = "ind",  invisible =c("ind.sup", "ind"))

# PCA 2022 vote 2017
q <- PCA(table_tout, scale.unit = TRUE, graph = TRUE, axes=c(1,3))
table_act <- table_tout[, c(6:14, 25, 41, 44, 47, 53, 58,60,64, 70, 75:102)]
table_sup <- table_tout[,c(42,43,45,50:52,54:57,59,61:63,65,71)]

q <- PCA(table_act, scale.unit = TRUE, axes=c(1,2), quanti.sup = c(19:46), graph = FALSE)
q <- PCA(table_act, scale.unit = TRUE, graph = TRUE, axes=c(1,2), ind.sup = c(15:23))
?PCA

barplot(q$eig[,1], main="Valeurs propres",
        names.arg = paste0("dim", 1:nrow(q$eig)))


# fonction pour plot le graph des individus sans les valeurs supplémentaires
plot.PCA(q, choix = "var",  invisible ="quanti.sup", axes = c(1,4))
# fonction pour plot le graph des individus avec les valeurs qualitative supplémentaires sans les autres
plot.PCA(q, choix = "var", invisible = c("var"), max.overlaps = 200, label = "quanti.sup", cex=0.4)
?plot.PCA
library(missMDA)
# Impute missing values using the 'imputePCA' function
table_act_imputed <- imputePCA(table_act)$completeObs

# Afficher le cercle des corrélations avec les variables supplémentaires
fviz_pca_var(pca_suppl, col.var.sup = "blue")
library(factoextra)

# Affichage des noms des flèches sur le premier plan factoriel
fviz_pca_ind(q, geom.ind = "point", col.ind = table_tout$V72, 
             palette = c("blue", "red", "green", "yellow","purple","cyan"), axes=c(2,3), addEllipses = TRUE, ellipse.type = "convex")
?fviz_pca_ind

## PCA 2017
q <- PCA(table_tout, scale.unit = TRUE, graph = TRUE, axes=c(1,3))

q <- PCA(table_tout[, c(15:23, 25:26)], scale.unit = TRUE, graph = TRUE, axes=c(1,2))


library(factoextra)

# Affichage des noms des flèches sur le premier plan factoriel
fviz_pca_ind(q, geom.ind = "point", col.ind = 72, 
             palette = c("blue", "red", "green", "yellow","purple","cyan"), axes=c(1,2), addEllipses = TRUE, ellipse.type = "convex")


cor(table_tout[, c(6:14, 25, 41, 44, 47, 53, 58,60,64, 70)])
image(cor(table_tout[, c(6:14, 25, 41, 44, 47, 53, 58,60,64, 70)])[,ncol(table_tout[, c(6:14, 25, 41, 44, 47, 53, 58,60,64, 70)]):1])

               