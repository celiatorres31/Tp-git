
# COURS 2 : CLUSTERING ----------------------------------------------------

library(tidyverse)
library(ggplot2)
library(skimr)

library(cluster)    # librairie de base pour la clusterisation
library(factoextra) # visualisations améliorées

# PRÉPARATION À LA CLUSTERISATION -----------------------------------------

data(iris)
skim(iris)

######  1) vérifions qu'il n'y ait pas de valeurs manquantes. 

#S'il y en a, on impute ou on efface les lignes avec des valeurs manquantes. On peut utiliser drop_na() sur les colonnes qu'on veut retenir pour clusteriser

#Dans ce jeu de données, il n'y a pas de valeurs manquantes alos on n'impute pas

--------------------

iris[1,1] <- NA # je crée une valeur manquante

iris %>% drop_na() -> iris_sans_na
skim(iris_sans_na)

library(missMDA)
imputeFAMD(iris)$completeObs -> iris_complete # FAMD s'applique à variables quanti et quali
skim(iris_complete)

#La machine a deviné la valeur manquante pour combler les NA. 

--------------------

###### 2) vérifions que les colonnes quantitatives sont à peu près dans la même échelle de valeurs. 

#Si elles ne le sont pas, on utilise mutate_if(is.numeric, scale) pour les toutes les normaliser.

iris_complete %>% mutate_if(is.numeric, scale) -> iris_standardisé
skim(iris_standardisé)

####### 3) je choisis des colonnes pour la clusterisation

#iris2 <- iris_standardisé %>% select(-Species) : on utilise cette formule si jamais on avait eu besoin de standardiser ou d'imputer des NA
iris2 <- iris %>% select(-Species) # on n'utilise pas la variable Species

# RÉALISER UNE CLUSTERISATION HIÉRARCHIQUE (DIANA ET AGNES) -----------------------------

### DIANA - clusterisation hiérarchique descendante

iris2 %>% diana() -> iris_diana # réalisons la clusterisation
fviz_dend(iris_diana, k = 3) # je colorie cinq clusters

### AGNES - clusterisation hiérarchique ascendante

iris2 %>% agnes() -> iris_agnes # réalisons la clusterisation
fviz_dend(iris_agnes, k = 3) # je colorie trois clusters

### Je sauvegarde le clustering que je préfère

factor(cutree(iris_agnes, 3)) -> iris$clusters # je choisis de tailler l'arbre à trois clusters

# Validation interne (coefficients) ---------------------------------------

# Ici, il s'agit de vérifier quel algorithme est le plus adapté : 

iris_diana$dc # le coefficient de division est de 0.95 : normalement, c'est Diana le meilleur, mais il existe d'autres techniques pour Agnès qui nous permettra de mieux voir et d'avoir peut être un meilleur résultat
iris_agnes$ac # le coefficient agglomératif est de 0.93

### d'autres méthodes pour AGNES

iris2 %>% agnes(method = "single") -> iris_agnes
iris_agnes$ac # agglomerative coefficient 0.85

iris2 %>% agnes(method = "average") -> iris_agnes
iris_agnes$ac # agglomerative coefficient 0.93 (méthode par default)

iris2 %>% agnes(method = "weighted") -> iris_agnes
iris_agnes$ac # agglomerative coefficient 0.94

iris2 %>% agnes(method = "complete") -> iris_agnes
iris_agnes$ac # agglomerative coefficient 0.96

iris2 %>% agnes(method = "gaverage") -> iris_agnes
iris_agnes$ac # agglomerative coefficient 0.98

iris2 %>% agnes(method = "ward") -> iris_agnes
iris_agnes$ac # agglomerative coefficient 0.99 c'est le gagnant !

# attention : ces valeurs me parlent de l'arbre tout entier et pas du nombre de clusters que je choisis

# Validation externe (vérité de terrain) ----------------------------------

### matrice de confusion

table(iris$Species, iris$clusters)

### visualisation comparée

clustering_iris_agnes <- ggplot(iris) + 
  aes(x = Petal.Length, y = Petal.Width, color = clusters) + 
  geom_point() + 
  ggtitle("Clusters - AGNES") # clusters

especes_iris <- ggplot(iris) + 
  aes(x = Petal.Length, y = Petal.Width, color = Species) + 
  geom_point() + 
  ggtitle("Espèces d'Iris") # vérité de terrain

# On vérifie entre les variables que l'on a et les clusters que l'on a obtenus (espèces et clusters en l'occurence)

library(cowplot)
plot_grid(clustering_iris_agnes, especes_iris) # pour mettre deux diagrammes l'un à côté de l'autre


# Validation manuelle / experte -------------------------------------------

# On vérifie à la mano toutes les varaibles pour voir les clusters : 

### moyennes des variables par cluster

iris %>% 
  group_by(clusters) %>% 
  summarise(petales_longueur_moyenne = mean(Petal.Length),
            petales_largeur_moyenne = mean(Petal.Width),
            sepales_longueur_moyenne = mean(Sepal.Length),
            sepales_largeur_moyenne = mean(Sepal.Width))

### visualisation des moyennes par cluster

iris %>% 
  group_by(clusters) %>% 
  summarise(petales_longueur_moyenne = mean(Petal.Length),
            petales_largeur_moyenne = mean(Petal.Width),
            sepales_longueur_moyenne = mean(Sepal.Length),
            sepales_largeur_moyenne = mean(Sepal.Width)) %>% 
  pivot_longer(!clusters) %>% # astuce !
  ggplot() +
    aes(x = name, y = value, fill = name) +
    geom_col() +
    facet_grid(cols = vars(clusters)) +
    scale_fill_brewer(palette = "Paired") +
    ggtitle("Profils des trois clusters obtenus avec AGNES") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.title = element_blank())

### est-ce que la largeur des sépales est significativement différente entre les clusters 2 et 3 ?
iris %>% 
  filter(clusters != 1) %>% 
  summarise(resultat_du_t_test = t.test(Sepal.Length ~ clusters)$p.value) # oui car très significatif

# Exercice ----------------------------------------------------------------

pokemon <- read_csv("https://query.data.world/s/5c53w5t6xhwciujbc4kwqac5s5spig")

# This dataset is about the pokemon games (NOT pokemon cards or Pokemon Go).
# Number: The ID for each pokemon
# Name: The name of each pokemon
# Type 1: Each pokemon has a type, this determines weakness/resistance to attacks
# Type 2: Some pokemon are dual type and have 2
# Total: Sum of all stats that come after this, a general guide to how strong a pokemon is
# HP: Hit points, or health, defines how much damage a pokemon can withstand before fainting
# Attack: The base modifier for normal attacks (eg. Scratch, Punch)
# Defense: The base damage resistance against normal attacks
# SP Atk: Special attack, the base modifier for special attacks (e.g. fire blast, bubble beam)
# SP Def: Special defense, the base damage resistance against special attacks
# Speed: Determines which pokemon attacks first each round
# Generation: The generation of games where the pokemon was first introduced
# Legendary: Some pokemon are much rarer than others, and are dubbed "legendary"


#Réalisez une clusterisation et interprétez-la 

skim(pokemon)

# D'abord, on choisit les variables que l'on souhaite analyser. Ici : hp, attack, defense, speed. Sur ces variables, il n'y a pas de NA alors pas besoin d'imputer. De même, toutes les données sont quanti et de même échelle donc pas besoin de les standardiser. 

# Maintenant, nous pouvons effectuer la clusterisation : 

pokemon2 <- pokemon %>% 
  select(hp, attack, defense, speed)

# Notre nouveau jeu de données est enregistré alors on passe à la clusterisation avec ces données : 

#DIANA
pokemon2 %>% diana() -> pokemon_diana
fviz_dend(pokemon_diana, k=4) # Ici, j'ai choisi de faire 4 clusters 

#AGNES
pokemon2 %>% agnes() -> pokemon_agnes
fviz_dend(pokemon_agnes, k = 4) # je colorie quatre clusters

### Je sauvegarde le clustering que je préfère

factor(cutree(pokemon_diana, 4)) -> pokemon$clusters


# Validation interne (coefficients) ---------------------------------------

# Ici, il s'agit de vérifier quel algorithme est le plus adapté : 

pokemon_diana$dc # le coefficient de division est de 0.96, c'est le meilleur car le plus proche de 1

#Validation manuelle / experte : 

pokemon %>% 
  group_by(groupes) %>% 
  summarise(m_attack = mean(attack), 
            m_defense = mean(defense), 
            m_hp = mean(hp),
            m_speed = mean(speed),
            n_legendes = sum(legendary),
            m_total = mean(total))

#ce qu'il a fait : le groupe 1 = les faibles, le groupe 2 = les forts, le groupe 4 = pokemon trop fort, valeur atypique il est trop fort pour être rangé avec les autres, groupe 3 = les hp les plus forts 
#il y a 108 légendes dans le groupe 2, donc dans le groupe le plus fort 
#le total est le plus fort dans le groupe 2 (et 4 parce que de toute façon il est plus fort que les autres)
