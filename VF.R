# Clear the environment
rm(list = ls())

# Load necessary packages
library(httr)
library(jsonlite)
library(dplyr)

# Function to retrieve cocktails by their first letter
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

# Fetch all cocktails
all_cocktails <- lapply(letters, get_cocktails_by_letter)
all_cocktails <- bind_rows(all_cocktails)

# Clean and structure the data
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

# Get a unique list of ingredients 
unique_ingredients <- unique(unlist(all_cocktails %>% select(starts_with("ingredient_"))))
unique_ingredients <- unique_ingredients[!is.na(unique_ingredients)] # Remove NA values
unique_ingredients <- unique(tolower(unique_ingredients)) # Convert to lowercase to avoid duplicates
unique_ingredients <- sort(unique_ingredients)

# Get a unique list of categories
unique_categories <- unique(all_cocktails$category)
unique_categories <- unique_categories[!is.na(unique_categories)] # Remove NA values

# Function to find cocktails by ingredients, and count the missing ingredients
find_by_ingredients <- function(selected_ingredients) {
  matching_cocktails <- all_cocktails %>%
    mutate(across(starts_with("ingredient_"), tolower)) %>% # Convert ingredients in the dataset to lowercase
    rowwise() %>%
    mutate(
      present_ingredients = sum(c_across(starts_with("ingredient_")) %in% selected_ingredients, na.rm = TRUE), # Count how many of the selected ingredients are present in the cocktail
      missing_ingredients = sum(!c_across(starts_with("ingredient_")) %in% selected_ingredients & !is.na(c_across(starts_with("ingredient_"))), na.rm = TRUE)       # Count the missing ingredients
    ) %>%
    ungroup() %>%
    # Only keep cocktails that contain at least one selected ingredient
    filter(present_ingredients > 0)
  
  if (nrow(matching_cocktails) == 0) {
    return("No cocktails found with these ingredients.")
  } else {
    matching_cocktails <- matching_cocktails %>%
      mutate(display_name = paste(cocktail_name, "(", missing_ingredients, " ingredient(s) missing)", sep = ""))
    return(matching_cocktails)
  }
}

# Function to find cocktails by category
find_by_category <- function(selected_categories) {
  matching_cocktails <- all_cocktails %>%
    filter(category %in% selected_categories)
  
  if (nrow(matching_cocktails) == 0) {
    return("No cocktails found for these categories.")
  } else {
    matching_cocktails <- matching_cocktails %>%
      mutate(display_name = cocktail_name)
    return(matching_cocktails)
  }
}


# Function to show cocktail details
show_cocktail_details <- function(cocktail_data, cocktail_name) {
  cocktail <- cocktail_data %>% filter(cocktail_name == !!cocktail_name)
  if (nrow(cocktail) == 1) {
    cat("\nCocktail Details: ", cocktail_name, "\n")
    cat("Category: ", cocktail$category, "\n")
    cat("Type: ", cocktail$alcoholic, "\n")
    cat("Recommended Glass: ", cocktail$glass, "\n")
    cat("Instructions: ", cocktail$instructions, "\n")
    cat("Ingredients and Measures: \n")
    ingredients <- cocktail %>% select(starts_with("ingredient_")) %>% unlist() %>% na.omit()
    measures <- cocktail %>% select(starts_with("measure_")) %>% unlist() %>% na.omit()
    for (i in seq_along(ingredients)) {
      cat("- ", ingredients[i], ": ", ifelse(i <= length(measures), measures[i], "To taste"), "\n")
    }
  } else {
    cat("No information available for this cocktail.\n")
  }
}

# Function to explore matching cocktails (for ingredients)
explore_cocktails_by_ingredients <- function(matching_cocktails) {
  cocktail_list <- matching_cocktails$display_name
  repeat {
    selected_cocktail <- menu(cocktail_list, title = "Select a cocktail to view details (press 0 to quit):")
    if (selected_cocktail == 0) {
      cat("Thank you for using our application! See you next time :) \n")
      break
    }
    selected_cocktail_name <- matching_cocktails$cocktail_name[selected_cocktail]
    show_cocktail_details(matching_cocktails, selected_cocktail_name)
    another <- readline(prompt = "Would you like to see another cocktail? (yes/no): ")
    if (tolower(another) != "yes") {
      cat("Thank you for using our application! See you next time:) \n")
      break
    }
  }
}

# Function to explore matching cocktails (for categories)
explore_cocktails_by_category <- function(matching_cocktails) {
  cocktail_list <- matching_cocktails$cocktail_name
  repeat {
    selected_cocktail <- menu(cocktail_list, title = "Select a cocktail to view details (press 0 to quit):")
    if (selected_cocktail == 0) {
      cat("Thank you for using our application! See you next time :) \n")
      break
    }
    selected_cocktail_name <- matching_cocktails$cocktail_name[selected_cocktail]
    show_cocktail_details(matching_cocktails, selected_cocktail_name)
    another <- readline(prompt = "Would you like to see another cocktail? (yes/no): ")
    if (tolower(another) != "yes") {
      cat("Thank you for using our application! See you next time :) \n")
      break
    }
  }
}


# Main function
main_function <- function() {
  cat("Welcome to the Cocktail Recommendation System!\n")
  choice <- readline(prompt = "Would you like to find a cocktail based on your tastes (1) or your available ingredients (2)? Enter 1 or 2: ")
  
  if (choice == "1") {
    selected_indices <- menu(unique_categories, title = "Select one categorie:", graphics = FALSE)
    if (selected_indices == 0) {
      cat("No categories selected. Please restart.\n")
      return()
    }
    selected_categories <- unique_categories[selected_indices]
    matching_cocktails <- find_by_category(selected_categories)
    if (is.character(matching_cocktails)) {
      cat(matching_cocktails, "\n")
    } else {
      explore_cocktails_by_category(matching_cocktails)
    }
  } else if (choice == "2") {
    selected_indices <- menu(unique_ingredients, title = "Select your ingredients:", graphics = FALSE)
    if (selected_indices == 0) {
      cat("No ingredients selected. Please restart.\n")
      return()
    }
    selected_ingredients <- unique_ingredients[selected_indices]
    matching_cocktails <- find_by_ingredients(selected_ingredients)
    if (is.character(matching_cocktails)) {
      cat(matching_cocktails, "\n")
    } else {
      explore_cocktails_by_ingredients(matching_cocktails)
    }
  } else {
    cat("Invalid choice. Please restart.\n")
  }
}

# Launch the main function
main_function()



