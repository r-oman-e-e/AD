# Chargement des packages nécessaires
library(shiny)
library(dplyr)
library(shinythemes)

# Fonction pour analyser les cocktails et identifier les ingrédients manquants
analyser_ingredients <- function(data, ingredients_dispo) {
  data %>%
    rowwise() %>%
    mutate(
      ingredients_necessaires = list(na.omit(c(ingredient_1, ingredient_2, ingredient_3, ingredient_4, ingredient_5, ingredient_6, ingredient_7, ingredient_8, ingredient_9, ingredient_10, ingredient_11))),
      mesures = list(na.omit(c(measure_1, measure_2, measure_3, measure_4, measure_5, measure_6, measure_7, measure_8, measure_9, measure_10, measure_11))),
      ingredients_manquants = list(setdiff(unlist(ingredients_necessaires), ingredients_dispo)),
      nb_manquants = length(ingredients_manquants)
    ) %>%
    ungroup() %>%
    select(cocktail_name, category, glass, instructions, ingredients_necessaires, mesures, ingredients_manquants, nb_manquants)
}

# Interface utilisateur
ui <- fluidPage(
  themeSelector(),
  titlePanel("Quel cocktail peut-on préparer ?"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("searchCocktail", "Chercher un cocktail :", value = ""),
      actionButton("searchButton", "Trouver"),
      selectInput("category", "Quel type de cocktail préférez-vous ?",
                  choices = unique(all_cocktails$category),
                  selected = NULL, multiple = FALSE),
      actionButton("prefButton", "Quels ingrédients avez-vous ?"),
      uiOutput("ingredientSelector"),
      actionButton("goButton", "Trouver un cocktail"),
      actionButton("surpriseMe", "Surprenez-moi !")
    ),
    
    mainPanel(
      uiOutput("cocktailsList"),
      uiOutput("detailsSection"),
      uiOutput("suggestionSection"),
      uiOutput("shoppingListSection")
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Suggérer des cocktails dynamiquement au fur et à mesure que l'utilisateur tape
  observeEvent(input$searchCocktail, {
    search_suggestions <- all_cocktails %>%
      filter(grepl(input$searchCocktail, cocktail_name, ignore.case = TRUE)) %>%
      pull(cocktail_name)
    
    updateSelectInput(session, "searchCocktail", choices = search_suggestions)
  })
  
  # Recherche par nom
  observeEvent(input$searchButton, {
    search_result <- all_cocktails %>%
      filter(grepl(input$searchCocktail, cocktail_name, ignore.case = TRUE))
    
    output$cocktailsList <- renderUI({
      if (nrow(search_result) > 0) {
        tagList(
          tags$h3("Résultats de la recherche :"),
          tags$ul(
            lapply(1:nrow(search_result), function(i) {
              cocktail <- search_result[i, ]
              tags$li(
                tags$p(paste("Catégorie :", cocktail$category)),
                tags$p(paste("Verre :", cocktail$glass)),
                actionButton(paste0("details_", i), "Voir les détails", class = "btn-primary")
              )
            })
          )
        )
      } else {
        tags$p("Aucun cocktail trouvé avec ce nom.")
      }
    })
  })
  
  # Filtrer les ingrédients par catégorie
  observeEvent(input$prefButton, {
    selected_category <- input$category
    suggested_ingredients <- all_cocktails %>%
      filter(category == selected_category) %>%
      select(starts_with("ingredient_")) %>%
      unlist() %>%
      unique() %>%
      na.omit()
    
    output$ingredientSelector <- renderUI({
      selectInput("ingredients", "Sélectionnez vos ingrédients :",
                  choices = suggested_ingredients,
                  selected = NULL, multiple = TRUE)
    })
  })
  
  # Trouver un cocktail avec les ingrédients sélectionnés
  observeEvent(input$goButton, {
    ingredients <- input$ingredients
    
    # Analyser les cocktails
    cocktails_analyse <- analyser_ingredients(all_cocktails, ingredients)
    cocktails_disponibles <- cocktails_analyse %>% filter(nb_manquants == 0)
    cocktails_suggestions <- cocktails_analyse %>% filter(nb_manquants > 0) %>% arrange(nb_manquants)
    
    # Afficher les cocktails réalisables
    output$cocktailsList <- renderUI({
      if (nrow(cocktails_disponibles) > 0) {
        tagList(
          tags$h3("Cocktails réalisables :"),
          tags$ul(
            lapply(1:nrow(cocktails_disponibles), function(i) {
              cocktail <- cocktails_disponibles[i, ]
              tags$li(
                tags$p(paste("Nom :", cocktail$cocktail_name)),
                tags$p(paste("Catégorie :", cocktail$category)),
                tags$p(paste("Verre :", cocktail$glass)),
                actionButton(paste0("details_", i), "Voir les détails", class = "btn-primary")
              )
            })
          )
        )
      } else {
        tags$p("Aucun cocktail n'est réalisable avec vos ingrédients.")
      }
    })
    
    # Afficher une suggestion avec des ingrédients manquants
    output$suggestionSection <- renderUI({
      if (nrow(cocktails_suggestions) > 0) {
        cocktail <- cocktails_suggestions[1, ]
        tagList(
          tags$h3("Suggestion :"),
          tags$p(paste("Le cocktail le plus proche est :", cocktail$cocktail_name)),
          tags$p(paste("Ingrédient(s) manquant(s) :", paste(unlist(cocktail$ingredients_manquants), collapse = ", "))),
          actionButton("generateList", "Générer une liste de courses", class = "btn-warning")
        )
      }
    })
  })
  
  # Générer une liste de courses
  observeEvent(input$generateList, {
    if (exists("cocktails_suggestions") && nrow(cocktails_suggestions) > 0) {
      cocktail <- cocktails_suggestions[1, ]
      output$shoppingListSection <- renderUI({
        tagList(
          tags$h3("Liste de courses :"),
          tags$ul(lapply(cocktail$ingredients_manquants[[1]], tags$li))
        )
      })
    } else {
      output$shoppingListSection <- renderUI({
        tags$p("Pas de suggestions disponibles pour générer une liste de courses.")
      })
    }
  })
  
  lapply(1:nrow(all_cocktails), function(i) {
    observeEvent(input[[paste0("details_", i)]], {
      # Extraire le cocktail correspondant à l'indice 'i'
      cocktail <- all_cocktails %>% slice(i)
      
      # Mettre à jour la section de détails
      output$detailsSection <- renderUI({
        tagList(
          tags$h3("Détails du cocktail :"),
          tags$p(paste("Catégorie :", cocktail$category)),
          tags$p(paste("Verre :", cocktail$glass)),
          tags$p(paste("Instructions :", cocktail$instructions)),
          tags$h4("Ingrédients :"),
          tags$ul(
            # Parcourir les ingrédients nécessaires pour ce cocktail
            lapply(1:11, function(j) {
              ingredient <- cocktail[[paste0("ingredient_", j)]]
              measure <- cocktail[[paste0("measure_", j)]]
              # Afficher seulement si l'ingrédient existe
              if (!is.na(ingredient) && ingredient != "") {
                tags$li(paste(ingredient, ":", ifelse(!is.na(measure), measure, "Quantité non spécifiée")))
              }
            })
          )
        )
      })
    })
  })
  
  
  # Surprenez-moi
  observeEvent(input$surpriseMe, {
    cocktail_random <- all_cocktails %>% sample_n(1)
    output$cocktailsList <- renderUI({
      tagList(
        tags$h3("Cocktail surprise :"),
        tags$p(paste("Catégorie :", cocktail_random$category)),
        tags$p(paste("Verre :", cocktail_random$glass)),
        tags$p(paste("Instructions :", cocktail_random$instructions))
      )
    })
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)

