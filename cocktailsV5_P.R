
# Fonction pour analyser les cocktails et identifier les ingrédients manquants
analyser_ingredients <- function(data, ingredients_dispo) {
  cocktails_analyse <- data %>%
    rowwise() %>%
    mutate(
      ingredients_necessaires = list(na.omit(c(ingredient_1, ingredient_2, ingredient_3, ingredient_4, ingredient_5, ingredient_6, ingredient_7, ingredient_8, ingredient_9, ingredient_10, ingredient_11))),
      mesures = list(na.omit(c(measure_1, measure_2, measure_3, measure_4, measure_5, measure_6, measure_7, measure_8, measure_9, measure_10, measure_11))),
      ingredients_manquants = list(setdiff(unlist(ingredients_necessaires), ingredients_dispo)),
      nb_manquants = length(ingredients_manquants)
    ) %>%
    ungroup() %>%
    select(cocktail_name, category, glass, instructions, ingredients_necessaires, mesures, ingredients_manquants, nb_manquants)
  return(cocktails_analyse)
}

# Définir l'interface utilisateur
ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("On boit quoi ??"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("ingredients", "Quels ingrédients t'as ?",
                  choices = unique(c(all_cocktails$ingredient_1, all_cocktails$ingredient_2, all_cocktails$ingredient_3, all_cocktails$ingredient_4, all_cocktails$ingredient_5, all_cocktails$ingredient_6, all_cocktails$ingredient_7, all_cocktails$ingredient_8, all_cocktails$ingredient_9, all_cocktails$ingredient_10, all_cocktails$ingredient_11)),
                  selected = NULL, multiple = TRUE),
      actionButton("goButton", "Vamos")
    ),
    
    mainPanel(
      uiOutput("cocktailsList"),
      uiOutput("detailsSection")
    )
  )
)

# Définir le serveur
server <- function(input, output, session) {
  
  observeEvent(input$goButton, {
    # Récupérer les ingrédients disponibles
    ingredients <- input$ingredients
    
    # Analyser les cocktails
    cocktails_analyse <- analyser_ingredients(all_cocktails, ingredients)
    
    # Séparer les cocktails réalisables et ceux nécessitant des ingrédients
    cocktails_disponibles <- cocktails_analyse %>%
      filter(nb_manquants == 0)
    
    cocktails_manquants <- cocktails_analyse %>%
      filter(nb_manquants > 0) %>%
      arrange(nb_manquants)
    
    # Afficher les cocktails réalisables ou proposer le plus proche
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
    
    # Afficher les détails pour le cocktail sélectionné
    lapply(1:nrow(cocktails_disponibles), function(i) {
      observeEvent(input[[paste0("details_", i)]], {
        cocktail <- cocktails_disponibles[i, ]
        output$detailsSection <- renderUI({
          tagList(
            tags$h3("Détails du cocktail :"),
            tags$p(paste("Nom :", cocktail$cocktail_name)),
            tags$p(paste("Catégorie :", cocktail$category)),
            tags$p(paste("Verre :", cocktail$glass)),
            tags$p("Ingrédients :"),
            tags$ul(
              lapply(1:length(cocktail$ingredients_necessaires[[1]]), function(j) {
                tags$li(paste(cocktail$ingredients_necessaires[[1]][j], "-", cocktail$mesures[[1]][j]))
              })
            ),
            tags$p(paste("Instructions :", cocktail$instructions))
          )
        })
      })
    })
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
