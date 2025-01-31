rm(list = ls())

#loading of packages
library(httr)
library(jsonlite)
library(dplyr)

#function to scrap from an API cocktails by their first letter
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

all_cocktails <- lapply(letters, get_cocktails_by_letter)
all_cocktails <- bind_rows(all_cocktails)  

all_cocktails = as.data.frame(all_cocktails)

all_cocktails = all_cocktails %>% 
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

View(all_cocktails)


