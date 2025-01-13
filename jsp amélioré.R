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
         -strInstructionsIT, -`strInstructionsZH-HANS`, -`strInstructionsZH-HANT`, 
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

# Obtenir une liste unique d'ingrédients
unique_ingredients <- unique(unlist(all_cocktails %>% select(starts_with("ingredient_"))))
unique_ingredients <- unique_ingredients[!is.na(unique_ingredients)] # Retirer les valeurs NA

# Fonction pour trouver un cocktail en fonction des ingrédients
find_by_ingredients <- function(selected_ingredients) {
  matching_cocktails <- all_cocktails %>%
    rowwise() %>%
    filter(all(selected_ingredients %in% c_across(starts_with("ingredient_")))) %>%
    ungroup()
  
  if (nrow(matching_cocktails) == 0) {
    return("Aucun cocktail trouvé avec ces ingrédients.")
  } else {
    return(matching_cocktails)
  }
}

# Fonction pour afficher les détails d'un cocktail
show_cocktail_details <- function(cocktail_data, cocktail_name) {
  cocktail <- cocktail_data %>% filter(cocktail_name == !!cocktail_name)
  if (nrow(cocktail) == 1) {
    cat("\nDétails du cocktail : ", cocktail_name, "\n")
    cat("Catégorie : ", cocktail$category, "\n")
    cat("Type : ", cocktail$alcoholic, "\n")
    cat("Verre recommandé : ", cocktail$glass, "\n")
    cat("Instructions : ", cocktail$instructions, "\n")
    cat("Ingrédients et mesures : \n")
    ingredients <- cocktail %>% select(starts_with("ingredient_")) %>% unlist() %>% na.omit()
    measures <- cocktail %>% select(starts_with("measure_")) %>% unlist() %>% na.omit()
    for (i in seq_along(ingredients)) {
      cat("- ", ingredients[i], ": ", ifelse(i <= length(measures), measures[i], "À votre convenance"), "\n")
    }
  } else {
    cat("Aucune information disponible pour ce cocktail.\n")
  }
}

# Fonction principale pour interagir avec l'utilisateur
main_function <- function() {
  cat("Bienvenue dans le système de recommandation de cocktails !\n")
  choice <- readline(prompt = "Souhaitez-vous trouver un cocktail basé sur vos goûts (1) ou sur vos ingrédients disponibles (2) ? Entrez 1 ou 2 : ")
  
  
  if (choice == "1") {
    category <- readline(prompt = "Entrez une catégorie (par exemple : Tropical, Classic, etc.) : ")
    result <- all_cocktails %>% 
      filter(grepl(category, category, ignore.case = TRUE)) %>% 
      pull(cocktail_name)
    if (length(result) == 0) {
      cat("Aucun cocktail trouvé pour cette catégorie.\n")
    } else {
      cat("Voici les cocktails correspondants :\n", paste(result, collapse = ", "), "\n")
    }
  } else if (choice == "2") {
    selected_indices <- menu(unique_ingredients, title = "Sélectionnez vos ingrédients (appuyez sur 0 pour terminer) :", graphics = FALSE)
    if (selected_indices == 0) {
      cat("Aucun ingrédient sélectionné. Veuillez redémarrer.\n")
      return()
    }
    selected_ingredients <- unique_ingredients[selected_indices]
    matching_cocktails <- find_by_ingredients(selected_ingredients)
    if (is.character(matching_cocktails)) {
      cat(matching_cocktails, "\n")
    } else {
      cat("Cocktails trouvés :\n")
      cocktail_list <- matching_cocktails$cocktail_name
      selected_cocktail <- menu(cocktail_list, title = "Choisissez un cocktail pour voir les détails :")
      if (selected_cocktail > 0) {
        show_cocktail_details(matching_cocktails, cocktail_list[selected_cocktail])
      } else {
        cat("Aucun cocktail sélectionné. Veuillez redémarrer.\n")
      }
    }
  } else {
    cat("Choix invalide ou fonctionnalité non implémentée.\n")
  }
}

# Lancer la fonction principale
main_function()

View(all_cocktails)
