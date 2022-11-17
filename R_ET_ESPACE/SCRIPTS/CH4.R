## CHAPITRE 4 : ANALYSE UNIVARIEE

## ----- OBJECTIFS DU CHAPITRE
## - réaliser une analyse univariée et produire des graphiques associés
## - prendre des mesures de dispersion et de position
## - usages des pkg : base, stats et classInt


## ----- PARAMETRER ENVIRONNEMENT DE TRAVAIL ET 
##+ IMPORTER LES DONNEES

## Définir l'espace de travail :
setwd("C:\\Users\\will\\Documents\\GitHub\\livres_numeriques\\R_ET_ESPACE")

popCom3608 <- read.csv(".\\RetEspace_Donnees\\PopCom3608.csv", sep=";", encoding="utf-8", stringsAsFactors=FALSE)

socEco9907 <- read.csv(".\\RetEspace_Donnees\\socEco9907.csv", sep=";", encoding="utf-8", stringsAsFactors=FALSE)

## ----- CHARGER LES PACKAGES
library(base)
library(stats) ## analyse statistiques
install.packages("classInt")
library(classInt) ## dicrétiser des variables continues

## ----- CALCULS SIMPLES ET RECODAGE

## calculer la densité de population
##+ le taux d'emploi
##+ le taux de variation de la population

## utiliser la commande with() pour ne pas répéter la référence au tableau :
popCom3608$EVOLPOP <- with(popCom3608,
					POP2008 / POP1936 - 1)
popCom3608$DENSPOP <- with(popCom3608,
					POP2008 / SURF)
socEco9907$TXEMPLOI <- with(socEco9907,
					EMPLOI06 / ACTOCC06)

## Ensuite on va créer deux nouvelles variables pour 
##+ analyse les structures spatiales à Paris et dans
##+ la petite couronne.
## CODDEP (code départemental) et DISTCONT (distance à Paris)
## extraction des deux premières valeurs pour avoir le CODDEP
socEco9907$CODDEP <- substr(socEco9907$CODGEO, 1, 2)

## calcul de la distance à Paris
## on va devoir diviser par 10 pour obtenir
##+ une valeur en km (le syst de proj étant en hectomètre)
##+ on considère Paris 1e comme le barycentre :

## on commence par stocker les coordonnées de Paris 1er :
coordPremier <- as.numeric(socEco9907[1, 3:4])
## Ensuite on calcule la distance à paris :
socEco9907$DISTCONT <- 0.1 * sqrt(
				(socEco9907$X - coordPremier[1]) ** 2 +
				(socEco9907$Y - coordPremier[2]) ** 2
			)
## ensuite discrétisation de la variable DISTCLASS
## plusieurs méthodes sont proposées
##+ la meilleure étant d'utiliser cut() après avoir
##+ définis des breaks à lui passer pour qu'elle 
##+ puisse découper la variable continue en seuils :

breaksDist <- c(0, 5, 10, 15, max(socEco9907$DISTCONT))
socEco9907$DISTCLASS <- cut(socEco9907$DISTCONT,
					breaks = breaksDist,
					include.lowest = TRUE)

## ---- RESUMES STATISTIQUES

## utiliser la fonction summary() pour un résumé statistique.
## peu de modules pour la représentation des données.
## On utilisera Hmisc, 
##+ avec ses fonctions wtd.table(), wtd.mean(), wtd.quantile().
## Dans le cadre d'une enquête, on pourra utiliser le module survey.

## faire la moyenne de la proportion des cadres dans les communes
##+ reviendrait à attribuer à chaque commune le même poids,
##+ donc toutes les communes auraient le même nb de cadre.

## on peut retrouver le nombre de cadre à partir des actifs
##+ et de la proportion de cadre par commune.
## ensuite on somme les valeurs issus de la multiplication.
## le rapport entre cette valeur et la somme des actifs occupés
##+ nous donnera la proportion de cadre dans l'espace d'étude.

## sinon on peut utiliser wtd.mean()
##+ qui permet de calculer la moyenne des proportions de cadre
##+ pondérée par l'effectif de la population active

library(Hmisc)

wtd.mean(socEco9907$PCAD99 , weights=socEco9907$ACTOCC99)

## ---- REPRESENTATION GRAPHIQUE DES DISTRIBUTIONS STATISTIQUES




