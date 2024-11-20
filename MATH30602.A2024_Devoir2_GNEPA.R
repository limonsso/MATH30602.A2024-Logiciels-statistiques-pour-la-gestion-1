#####################################################################################################
######################################################################################################
#                          Math30602 Logiciels statistiques en gestion

#									        Devoir 1

#                          Poly Franck Ezechiel Gnepa
#                          11299554
#                          poly-franck-ezechiel.gnepa@hec.ca
                          
######################################################################################################
######################################################################################################
#####################################################################################################

setwd("/Users/zecklimonsso/Library/CloudStorage/OneDrive-Personnel/Documents/Cours/HEC/MATH30602/devoir2")
getwd()

###################Partie 1

#####Question1 – Importation des données

#Importation des données

actes_criminels <- read.csv("actes-criminels_in_.csv", 
                                   header= T, 
                                   sep=",")
install.packages("readxl")
library(readxl)

pdq_descriptions <- as.data.frame(read_excel('pdq_description.xlsx')) 

#####Question2 – Jointure

crimes <- merge(actes_criminels, pdq_descriptions, by.x = "PDQ", by.y = "code_PDQ", all.x = TRUE)

#####Question3 – Test sur la jointure
#a
crimes_sans_desc <- crimes[is.na(crimes$Description), ]
#b
nbr_crimes_sans_desc <-  nrow(crimes_sans_desc)
print(nbr_crimes_sans_desc)
#c
crimes <- crimes[!is.na(crimes$Description), ]
#d
install.packages("dplyr")
library(dplyr)

pdq_count <- crimes %>%
  count(PDQ) %>%
  arrange(desc(n))

pdq_max = pdq_descriptions[pdq_descriptions$code_PDQ == pdq_count[1,]$PDQ, ]
pdq_min = pdq_descriptions[pdq_descriptions$code_PDQ == pdq_count[nrow(pdq_count),]$PDQ, ]

cat("\nPDQ avec le plus grand nombre de crimes :", pdq_max$code_PDQ, "-", pdq_max$Description," \n")
cat("PDQ avec le plus faible nombre de crimes :", pdq_min$code_PDQ, "-", pdq_min$Description,"\n")

#####Question4 – Boucle de partage par description

crimes_by_PDQ <- list()

descriptions <- unique(crimes$Description)

for(description in descriptions){
  crimes_by_PDQ[[description]] <- crimes[crimes$description == description, ]
}
all_col <- colnames(crimes)

#####Question5 – Généralisation de la fonction
split_df_to_list <- function(entr_df,split_var,selected_var=NULL){
  
  all_col <- colnames(entr_df)
  if (!split_var %in% all_col) {
    stop("La colonne", split_var," n'existe pas dans le dataframe.")
  }
  
  if(is.null(selected_var)){
    selected_var <- all_col
  }
  result <- list()
  
  valeurs <- unique(entr_df[,split_var])
  
  for(val in valeurs){
    result[[val]] <- entr_df[entr_df[,split_var] == val, selected_var]
  }
  return(result)
}
#####Question6 – Testez votre fonction 

crimes_by_CATEGORIES <- split_df_to_list(crimes, "CATEGORIE",c("LATITUDE","LONGITUDE","DATE","QUART"))

###################Partie 2

#####Question1 – Adaptation de code

# La fonction get_circle_points génère un ensemble de points équidistants formant
# un cercle autour d’un centre défini par une latitude et une longitude. Ces points
# peuvent être utilisés pour visualiser un cercle géographique ou effectuer des
# opérations spatiales, comme filtrer des données à l'intérieur ou autour du cercle.
# Argument	  Type	  Description
# center_long numeric	Longitude du centre du cercle.
# center_lat  numeric	Latitude du centre du cercle.
# radius      numeric	Rayon du cercle (en kilomètres ou une autre unité compatible).
# num_points	integer	Nombre de points utilisés pour approximer le cercle. Par défaut : 100.
# Un data.frame contenant les coordonnées géographiques des points du cercle avec les colonnes suivantes :
# LATITUDE : Latitude des points sur le périmètre du cercle.
# LONGITUDE : Longitude des points sur le périmètre du cercle.
# geographical_quarter fait généralement référence à une classification des points
# géographiques en fonction de leur position relative à un centre (latitude et longitude). 
# Typiquement, cette classification divise un espace en quatre quarts : Nord-Est, Nord-Ouest, Sud-Est, Sud-Ouest
get_circle_points <- function(center_long,
                              center_lat,
                              radius_in_km,
                              nb_of_points){
  # Complétez les fonctions avec les bons paramètres d’entrée
  radiusLon = 1 / (111.319 * cos(center_lat * (pi / 180))) * radius_in_km
  radiusLat = 1 / 110.574 * radius_in_km
  
  dTheta = 2 * pi / nb_of_points
  theta = 0
  # Un data.frame vide dans lequel on va stocker le résultat au fur et à mesure des itérations de la boucle for
  points_on_circle = data.frame()
  
  for (i in 1:nb_of_points){
    # Contanez les résultats correctement
    # rbind() pour ajouter une nouvelle ligne au dataframe
    points_on_circle <- rbind(points_on_circle,
                              data.frame(center_long + radiusLon * cos(theta),
                                center_lat + radiusLat * sin(theta)
                            ))
    theta = theta + dTheta
  }
  points_on_circle <- data.frame(points_on_circle)
  # LATITUDE : la latitude des points.
  # LONGITUDE : la longitude des points.
  # geographical_quarter : une étiquette descriptive.
  #Ajustez le nom des colonnes du dataframe de sortie
  colnames(points_on_circle) <- c('LONGITUDE', 'LATITUDE')
  
  # vérifier que la latitude est inférieure au center_lat et que la longitude est supérieure au center_long
  # en fonction des coordonnées géographiques 
  points_on_circle$geographical_quarter <- 'West - South'
  points_on_circle$geographical_quarter[points_on_circle$LATITUDE >= center_lat & points_on_circle$LONGITUDE
                                        <= center_long] <- 'West - North'
  points_on_circle$geographical_quarter[points_on_circle$LATITUDE >= center_lat & points_on_circle$LONGITUDE >
                                          center_long] <- 'East - North'
  # En vous inspirant de ce qui est fait dans les 2 lignes au dessus, écrivez la dernière condition
  points_on_circle$geographical_quarter[points_on_circle$LATITUDE < center_lat & points_on_circle$LONGITUDE >
                                          center_long] <- 'East - South'
  # Ajoutez le point central défini en paramètre d’entrée
  points_on_circle = rbind(points_on_circle, c(center_lat, center_long, 'Center'))
  # Ajustez les formats à numérique
  points_on_circle$LONGITUDE <- as.numeric(points_on_circle$LONGITUDE)
  points_on_circle$LATITUDE <- as.numeric(points_on_circle$LATITUDE)
  # On veut retournez le dataframe que l’on a construit dans la fonction
  return(points_on_circle)
} 

#####Question2 – Testez votre fonction
LONGITUDE <- -73.6212999
LATITUDE <- 45.4983503
# Étape 1 : Générer les points du cercle dans un périmètre de x
# kilomètre autour de ce point central 
test1km <- get_circle_points(LONGITUDE, LATITUDE, 1, 100)

plot(LONGITUDE, LATITUDE , col = as.factor(test1km$geographical_quarter))

#####Question3

filter_by_circle_points <- function(circle_points,data_points){
  points_in_circle = data.frame()
  # Étape 3 : Filtrage dans les limites géographiques et l'exclusion des valeurs NA
  # na.omit() : Cela supprime toutes les lignes contenant au moins un NA dans le dataframe
  points_in_circle <- na.omit(data_points[
    data_points$LATITUDE >= min(circle_points$LATITUDE) & data_points$LATITUDE <= max(circle_points$LATITUDE) &
      data_points$LONGITUDE >= min(circle_points$LONGITUDE) & data_points$LONGITUDE <= max(circle_points$LONGITUDE),
  ])
  return(points_in_circle)
}

crimes_in_circle_points <- filter_by_circle_points(test1km,crimes)

#####Question4 - Identifiez les points à l’intérieur du périmètre voulu.

#Vérifie si des points donnés (latitude, longitude) se trouvent à l'intérieur d'un cercle.
in_the_cercle_v2 <- function(x, circle_point){
  
  res <- all(c(
    ## Condition 1 : Quart "West - South"  
    any(circle_point[circle_point$geographical_quarter == 'West - South', c('LONGITUDE')]<=
                     as.numeric(x[['LONGITUDE']]) & circle_point[circle_point$geographical_quarter == 'West - South',c('LATITUDE')] <
                     as.numeric(x[['LATITUDE']])),
    # Condition 2 : Quart "West - North"
    any(circle_point[circle_point$geographical_quarter == 'West - North',c('LONGITUDE')]<= as.numeric(x[['LONGITUDE']])
                   & circle_point[circle_point$geographical_quarter == 'West - North',c('LATITUDE')]> as.numeric(x[['LATITUDE']])),
    # Condition 3 : Quart "East - North"
   any(circle_point[circle_point$geographical_quarter == 'East - North',c('LONGITUDE')]>= as.numeric(x[['LONGITUDE']])
                   & circle_point[circle_point$geographical_quarter == 'East - North',c('LATITUDE')]> as.numeric(x[['LATITUDE']])),
    # Condition 3 : Quart "East - North"
   any(circle_point[circle_point$geographical_quarter == 'East - South',c('LONGITUDE')]>= as.numeric(x[['LONGITUDE']])
                   & circle_point[circle_point$geographical_quarter == 'East - South',c('LATITUDE')]> as.numeric(x[['LATITUDE']]))
  ))
  return(res)
}

#####Question5 – Rassemblez toutes les pièces.

merge_all <-function(data_center_long, data_center_lat, entire_spatial_DB, radius_in_km,
                     nb_radius_points=100){
  
  # Étape 1 : Générer les points du cercle
  radius_points <- get_circle_points(data_center_long,data_center_lat,radius_in_km,nb_radius_points)
  
  # Étape 2 : Filtrer les données spatiales 
  entire_spatial_DB_int <- filter_by_circle_points(radius_points, entire_spatial_DB)
  
  # Appliquer de façon récursive la fonction sur toutes les observations d’intérêt
  res <- data.frame(apply(entire_spatial_DB_int, 1,function(row) {
    in_the_cercle_v2(as.list(row), circle_point = radius_points)
  }))
  return(list(res, entire_spatial_DB_int,radius_points))
}

Latitude <- 45.50333
longitude <- -73.6206714
testRes <- merge_all(Latitude, longitude, crimes, 1.5, 100)
plot(testRes[[2]]$LONGITUDE, testRes[[2]]$LATITUDE, col = as.factor(testRes[[1]][,1]))

