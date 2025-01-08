rm(list = ls())

# Chargement des packages nécessaires
library(httr)
library(jsonlite)
library(dplyr)

# Fonction pour récupérer les cocktails par leur première lettre
get_cocktails_by_letter <- function(letter) {
  url <- paste0("https://www.thecocktaildb.com/api/json/v1/1/search.php?f=", letter)
  response <- GET(url)
  if (response$status_code == 200) {
    data <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(data, flatten = TRUE)
    return(json_data$drinks)
  } else {
    return(NULL)
  }
}

# Obtenir tous les cocktails
all_cocktails <- lapply(letters, get_cocktails_by_letter)
all_cocktails <- bind_rows(all_cocktails)  

# Nettoyer et structurer les données
all_cocktails <- all_cocktails %>% 
  select(-idDrink, -strDrinkAlternate, -strTags, -strVideo, -strIBA, 
         -strInstructionsDE, -strInstructionsES, -strInstructionsFR, 
         -strInstructionsIT, -`strInstructionsZH-HANS` , -`strInstructionsZH-HANT`, 
         -strDrinkThumb, -strIngredient12, -strIngredient13, -strIngredient14, -strIngredient15, 
         -strImageSource, -strImageAttribution, -strCreativeCommonsConfirmed, -dateModified,
         -strMeasure12, -strMeasure13, -strMeasure14, -strMeasure15) %>% 
  rename(cocktail_name = strDrink,
         category = strCategory,
         alcoholic = strAlcoholic,
         glass = strGlass,
         instructions = strInstructions,
         ingredient_1 = strIngredient1,
         ingredient_2 = strIngredient2,
         ingredient_3 = strIngredient3,
         ingredient_4 = strIngredient4,
         ingredient_5 = strIngredient5,
         ingredient_6 = strIngredient6,
         ingredient_7 = strIngredient7,
         ingredient_8 = strIngredient8,
         ingredient_9 = strIngredient9,
         ingredient_10 = strIngredient10,
         ingredient_11 = strIngredient11,
         measure_1 = strMeasure1,
         measure_2 = strMeasure2,
         measure_3 = strMeasure3,
         measure_4 = strMeasure4,
         measure_5 = strMeasure5,
         measure_6 = strMeasure6,
         measure_7 = strMeasure7,
         measure_8 = strMeasure8,
         measure_9 = strMeasure9,
         measure_10 = strMeasure10,
         measure_11 = strMeasure11)

# Fonction pour trouver un cocktail en fonction des goûts
find_by_taste <- function(category) {
  cocktails <- all_cocktails %>% filter(grepl(category, category, ignore.case = TRUE))
  if (nrow(cocktails) == 0) {
    return("Aucun cocktail trouvé pour cette catégorie.")
  } else {
    return(cocktails$cocktail_name)
  }
}

# Fonction pour trouver un cocktail en fonction des ingrédients
find_by_ingredients <- function(available_ingredients) {
  matching_cocktails <- all_cocktails %>%
    rowwise() %>%
    filter(any(c_across(starts_with("ingredient_")) %in% available_ingredients)) %>%
    ungroup()
  
  if (nrow(matching_cocktails) == 0) {
    return("Aucun cocktail trouvé avec ces ingrédients.")
  } else {
    return(matching_cocktails$cocktail_name)
  }
}

# Fonction principale pour interagir avec l'utilisateur
main_function <- function() {
  cat("Bienvenue dans le système de recommandation de cocktails !\n")
  choice <- readline(prompt = "Souhaitez-vous trouver un cocktail basé sur vos goûts (1) ou sur vos ingrédients disponibles (2) ? Entrez 1 ou 2 : ")
  
  if (choice == "1") {
    category <- readline(prompt = "Entrez une catégorie (par exemple : Tropical, Classic, etc.) : ")
    result <- find_by_taste(category)
    cat("Voici les cocktails correspondants :\n", paste(result, collapse = ", "), "\n")
  } else if (choice == "2") {
    ingredients <- readline(prompt = "Entrez vos ingrédients disponibles, séparés par des virgules : ")
    ingredients_list <- strsplit(ingredients, ",")[[1]]
    ingredients_list <- trimws(ingredients_list)  # Supprimer les espaces
    result <- find_by_ingredients(ingredients_list)
    cat("Voici les cocktails correspondants :\n", paste(result, collapse = ", "), "\n")
  } else {
    cat("Choix invalide. Veuillez redémarrer.\n")
  }
}

# Lancer la fonction principale
main_function()

