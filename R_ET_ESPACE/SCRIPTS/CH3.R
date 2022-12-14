## R et espace
## CH 3 : introduction à la programmation

## ----- OBJECTIFS DU CHAPITRE
## - utiliser et créer des fonctions.
## - utiliser les boucles.
## - utiliser les fonctions de la famille apply()
##+ pour gagner en efficacité.
## - faire des représentations graphiques (mono, multigraphiques).
## - calculer la distance entre deux communes, dans un ensemble
##+ de communes.
## - utiliser l'équilibre de Wardrop.
## - appeler le test de shapiro.

## Définir l'espace de travail :
setwd("C:\\Users\\will\\Documents\\GitHub\\livres_numeriques\\R_ET_ESPACE")

## ----- LES BOUCLES

## script pour essayer les structures de ocntrôles R :

for (i in 1:9){
	print("première boucle for !")
}

print("et usage du while : ")

i <- 0
while (i != 10){
	print("hello world")
	i <- i + 1
}

## R préfèrera les boucles while aux boucles for.

## ----- USAGE DES BOUCLES

## ouvrir un jeu de données :

popCom3608 <- read.csv(".\\RetEspace_Donnees\\PopCom3608.csv", sep=";", encoding="utf-8", stringsAsFactors=FALSE)

socEco9907 <- read.csv(".\\RetEspace_Donnees\\socEco9907.csv", sep=";", encoding="utf-8", stringsAsFactors=FALSE)

## le fichier n'est pas construit pour être lu par read.table()

## Afficher des graphiques pour chaque colonne :
## on va afficher 9 graphiques ont doit donc découper la zone d'affichage 
##+ en 9 (3*3)
par(mfrow=c(3,3)) ## découpage en 3x3
for (i in 3:11){
	popAnnee <- colnames(popCom3608) ## permet d'avoir les noms des colonnes
	hist(popCom3608[, i], breaks=20, main=popAnnee[i])
}

## cas de double boucle for : le distancier

## on va chercher à calculer la distance entre chaque commune.
## on a besoin de la double boucle for pour calculer la distance entre
##+ chaque commune.

## on commence par créer trois vecteurs pour contenir les libellés 
##+ et distance aux communes
print("----- CALCUL DE DISTANCE ENTRE LES COMMUNES -----")
## Création de 3 vecteurs pour contenir différentes valeurs
##+ nom des communes et la distance entre les deux.
com_a <- c() ## on utilise c() pour combine. Permet de créer un vecteur
com_b <- c()
dist_ab <- c()

k <- 1 ## la valeur qui compte et indice les vecteurs :
## bien indiquer les parenthèses !
## calculer la distance entre chaque commune :
for (i in 1:(nrow(socEco9907) - 1) ){ 
	for (j in i+1:nrow(socEco9907)){
		com_a[k] <- socEco9907$NOM[i]
		com_b[k] <- socEco9907$NOM[j]
		dist <- sqrt( ( socEco9907$X[j] - socEco9907$X[i] )^2 + ( socEco9907$Y[j] - socEco9907$Y[i ])^2 )
		## print(i)
		dist_ab[k] <- dist
		k <- k + 1
	}
}

head(com_a)


## ensuite mettre les résultats dans un dataframe (tableau)
df_distAB <- data.frame(COMMA = com_a, COMMB = com_b, DIST_AB = dist_ab)
dim(df_distAB) ## 20306 3 attendu

## ----- LES CONDITIONS/TESTS

## on a l'instruction ifelse() qui permet d'assigner une variable selon un test
## on a le test if ... else (if) pour tester
## applicable à plusieurs objets de R
## généralement utilisé dans les fonctions

## ----- LES FONCTIONS

## utilisées pour éviter de répéter du code
## elles peuvent être mises à disposition d'autre utilisateur
## souvent dans le cas d'usage générique
## rarement propre à un cas.

## les fonctions peuvent admettre 0 ou plusieurs paramètres.
## Lorsqu'un argument n'est pas indiqué, R va rechercher cet arg dans l'env global
## Il faut donc faire attention au nommage pour éviter toutes mauvaises surprises ...


## exemple :
fn <- function(a, b){
	print(a)
	print(b)
}

fn("hello", 4)

## créer une fonction qui retourne un résultat :
sommer <- function(a,b){
	res <- a + b
	return(res)
}

t <- sommer(2,3)
print(t)

## la fonction methods renvoie les méthodes d'une fonction.
## certaines fonctions détectent le type de données
##+ et agissent en conséquence. C'est le cas de plot().

## Exemple : la fonction curve()
curve(
	expr=dnorm,
	from=-10,
	to=10,
	main="Distribution normale"
)

## on peut également dériver des fonctions
##+ avec la fonction D()

## ---- CREATION D'UNE FONCTION DE DISCRETISATION
## la discrétisation est très utilisée en cartographie.
##+ elle dépend de la distribution de la variable.
## quand la distribution est normale => dicrétisation par la moyenne et la std
## quand la distribution est dissymétrique => dicrétisation par les quantiles

## créer la fonction de discrétisation
Discretisation <- function(vec){
	normTest <- shapiro.test(vec)
	if (normTest$p.value > 0.1){ ## revoir le test de shapiro
		print("Discrétisation autour de la moyenne")
		valBreaks <- c(min(vec), mean(vec) - sd(vec), mean(vec), mean(vec) + sd(vec), max(vec))
		varDiscret <- cut(vec, breaks=valBreaks, include.lowest=TRUE, right=FALSE)
	} else {
		print("Discrétisation en quantiles")
		valBreaks <- quantile(vec, probs=c(0, 0.25, 0.5, 0.75, 1))
		varDiscret <- cut(vec, breaks=valBreaks, include.lowest=TRUE, right=FALSE)
	}
	return(varDiscret)
}
## dans cette fonction, on voit qu'il n'y a pas de portée des variables.
## une variable locale à un if statement devient globale à la fonction.


popCom3608$POPDISCR36 <- Discretisation (popCom3608$POP1936)

## ---- EQUILIBRE DE WARDROP
## consiste en la recherche d'un temps de parcours minimum
##+ sur 2 routes et trouver le flux optimal pour ces deux routes

## facteurs du temps de trajet : temps à vide, cap de la route et congestion.
## le flux de la route correspond au nombre de voiture
## donc, les flux q1 et q2 sont conditonnés : q1 + q2 = nombre de voitures max
## avec le nb de voiture max = 4000 ici
## la congestion est donnée par :
## tx = tps * (1 * 0.15 * (qx / capx)^4)
## avec capx la capacité de la route x
## les informations sur les routes sont données par le manuel
## dans la config de Wardrop, q1 = q2, on cherche donc une variable qui assure cet équilibre
## on cherche finalement q1 - q2 = 0, donc on va chercher un x qui assure l'égalité.
## on appelera la combinaison des deux fonctions la fonction f(x).

## on commence par faire une estimation x0.
##+ puis, par application de la méthode de Newton, on va faire une estimation
##+ plus précise, depuis x0.
## la méthode de newton :
## x(n+1) = xn - ( f(xn) / f'(xn) )
## avec f' la dérivée de f

## APPLICATION
## on commence par définir les constantes

alpha <- 0.15
beta <- 4
t1 <- 10
t2 <- 30
c1 <- 1000
c2 <- 3000
Q <- 4000

## ensuite on défini les fonctions
f1 <- function(x){
	t1 * ( 1 + alpha * ( x / c1 )^beta ) -
	t2 * (1 + alpha * ( (Q - x) / c2 )^beta )
}

f2 <- function(xx){
	eval( {x <- xx ;
	(D(expression(
	t1 * ( 1 + alpha * ( x / c1 )^beta ) -
	t2 * (1 + alpha * ( (Q - x) / c2 )^beta )),
	"x"))	
})
}

f3 <- function(x){
	x - f1(x) / f2(x)
}

## RESOLUTION NUMERIQUE
## on décide arbitrairement d'une première estimation
##+ du nb de voitures qui emprunteront la route 1 à l'équilibre,
##+ par exemple 1000 véhicules.
## On calcule x1 selon la méthode de Newton à partir de cette première
##+ estimation, puis x2 et ainsi de suite
## L'algorithme ci-dessous calcule ainsi les dix premiers 
##+ termes de cette suite

seqConv <- NULL
seqConv[1] <- 1000

for (i in 2:10){
	seqConv[i] <- f3(seqConv[i - 1])
}

## représenter la convergence de suite vers le trafic
##+ à l'équilibre et reporter le temps de trajet dans cette
##+ configuration

plot(
	seqConv,
	type="b",
	pch=19,
	col="dimgrey",
	ylim=c(0, 4000),
	xlab="Nombre d'itération",
	ylab="x_n"
)

## Dans cet exemple, la valeur qui satisfait l’équilibre de Wardrop est de
##+ 2 000 véhicules sur chacune des deux routes.

## ---- VOL DE SYRACUSE

## définition : quand une valeur est paire, elle est divisée par deux
##+ quand une valeur est impaire, elle est multipliée par 3, puis +1

## normalement on fini par tomber sur 1.
## on va implémenter cet algo (pour faire des conditions et des boucles) :
## cet algo recherche à atteindre 1 depuis une valeur quelconque.

syracuse <- function(nb){
	## nb : une valeur numérique à ajouter à uneséquence initialisée
	## la séquence qui va contenir nos valeurs :
	syr <- nb ## on crée un vecteur
	
	i <- 1
	while (syr[i] != 1){
		if (syr[i] %% 2 == 0){
			syr[i + 1] <- syr[i] / 2
		} else if (syr[i] %% 2 != 0){
			syr[i + 1] <- 3 * syr[i] + 1
		}
		i <- i + 1
		
	}
	return(syr)
}

## Maintenant, il faut connaître le nombre de coups :
dureeSyr <- function(n, fct){
	## en gros, on va créer une fonction qui va lancer une fonction
	##+ et quand celle-ci est résolue, afficher la longueur du vecteur crée
	length(fct(n))

}


## le vol de Syracuse c'est justement la longueur du vecteur (le nb de coups
##+ pour atteindre la valeur 1).

## Là on va appliquer le vol de Syracuse
##+ au df de la popualtion communale 2008
## On va donc voir s'il existe un lien entre la taille de la
##+ population et la durée du vol :

## on utilisera apply()
val <- popCom3608$POP2008
syr_dur <- sapply(val,
			dureeSyr,
			fct=syracuse,
			simplify=TRUE)
plot(val, syr_dur, pch=19, xlab="Population communale", ylab="Durée de vol de Syracuse")

## finalement, pas de lien apparent entre la valeur de départ et 
##+ la durée du vol.

max(syr_dur)
## afficher la commune avec le plus d'itération :
popCom3608$LIBELLE[which.max(syr_dur)]

## Afficher le vol de syracuse d'une commune :
plot(syracuse(val[which.max(syr_dur)]),
	type="l",
	log="y",
	xlab="nb d'essai",
	ylab="u_n"
	)

## ----- APPLICATION DE FONCTIONS SUR DES ENSEMBLES
## précédemment on a appliqué des fonctions
##+ sur des ensembles pour produire des résumés
##+ numériques selon une variables d'aggrégation.
## Toutes les fonctions de la famille apply()
##+ ne servent pas qu'à produire ce genre de résumé.
##+ dans cette partie, on va regarder quelques fonctions
##+ de la famille apply().

## le fonctionnement :
## - inspirées du paradigme de la prog fonctionnelle
##+ qui fonctionne à l'origine sur des ensembles.
## - plus rapide et moins gourmande en ressource que les boucles
## - retourne un type matrix/array. Si df, 
##+ alors l'objet est transformé.
## - contrairement aux boucles, possibilité d'être
##+ exécutées en parallèle (notamment avec les pkg
parallel et multicore).

## syntaxe générale :
## apply(tab, dimensions, fonction)
## avec :
## - tab : matrix, array ou df (transformé automatiquement)
## - dimensions : la dimension sur laquelle doit être appliquée 
##+ la fonction. Soit 1 = ligne, 2 = colonne (pour un tab en 2D).
##+ on peut dépasser la 2e dimension si nécessaire.
## - fonction : la fonction qui va être appliquée. Elle peut
##+ être native, ou bien créer par l'utilisateur.
## exemple :
sumPop <- apply(popCom3608[ , 3:11], 2, sum)
sumPop

## ----- A LA FIN DE CE CHAPITRE JE SAIS
## ----- OBJECTIFS DU CHAPITRE
## - utiliser et créer des fonctions.
## - utiliser les boucles.
## - utiliser les fonctions de la famille apply()
##+ pour gagner en efficacité.
## - faire des représentations graphiques (mono, multigraphiques).
## - calculer la distance entre deux communes, dans un ensemble
##+ de communes.
## - utiliser l'équilibre de Wardrop.
## - appeler le test de shapiro.






















