library(shiny)
library(readxl)
library(dplyr)
library(shinythemes)

# Charger le fichier Excel fourni

cocktails <- read_excel("C:/Users/andre/OneDrive/Documentos/GitHub/AD/cocktails_pour_party.xlsx")

# Fonction pour analyser les cocktails et identifier les ingrédients manquants
analyser_ingredients <- function(data, alcools_dispo, softs_dispo, sirops_dispo, autres_dispo) {
  cocktails_analyse <- data %>%
    rowwise() %>%
    mutate(
      ingredients_necessaires = list(na.omit(c(`alcool 1`, `alcool 2`, `alcool 3`, `soft 1`, `soft 2`, sirop, autre))),
      ingredients_manquants = list(
        setdiff(unlist(ingredients_necessaires), c(na.omit(alcools_dispo), na.omit(softs_dispo), na.omit(sirops_dispo), na.omit(autres_dispo)))
      ),
      nb_manquants = length(ingredients_manquants)
    ) %>%
    ungroup() %>%
    select(`nom du cocktail`, note, ingredients_necessaires, ingredients_manquants, nb_manquants)
  return(cocktails_analyse)
}

# Définir l'interface utilisateur
ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("On boit quoi ??"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("alcools", "Quelles bouteilles t'as ?",
                  choices = unique(c(cocktails$`alcool 1`, cocktails$`alcool 2`, cocktails$`alcool 3`)),
                  selected = NULL, multiple = TRUE),
      selectInput("softs", "Quels softs t'as ?",
                  choices = unique(c(cocktails$`soft 1`, cocktails$`soft 2`)),
                  selected = NULL, multiple = TRUE),
      selectInput("sirops", "Quels sirops t'as ?",
                  choices = unique(cocktails$sirop),
                  selected = NULL, multiple = TRUE),
      selectInput("autres", "Et il reste quoi dans ton frigo ??",
                  choices = unique(cocktails$autre),
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
    alcools <- input$alcools
    softs <- input$softs
    sirops <- input$sirops
    autres <- input$autres
    
    # Analyser les cocktails
    cocktails_analyse <- analyser_ingredients(cocktails, alcools, softs, sirops, autres)
    
    # Séparer les cocktails réalisables et ceux nécessitant des ingrédients
    cocktails_disponibles <- cocktails_analyse %>%
      filter(nb_manquants == 0)
    
    cocktails_manquants <- cocktails_analyse %>%
      filter(nb_manquants > 0) %>%
      arrange(nb_manquants)
    
    # Afficher les cocktails réalisables ou proposer le plus proche
    output$cocktailsList <- renderUI({
      if (nrow(cocktails_disponibles) > 0) {
        cocktail_proche <- cocktails_disponibles[1, ]
        tagList(
          tags$h3("Cocktail réalisable :"),
          tags$p(paste("Nom :", cocktail_proche$`nom du cocktail`)),
          tags$p(paste("Note :", round(cocktail_proche$note, 1))),
          actionButton("showDetails", "Voir les détails", style = "background-color: red; color: white;")
        )
      } else {
        tags$p("Aucun cocktail n'est réalisable avec vos ingrédients.")
      }
    })
    
    # Afficher les détails pour le cocktail sélectionné
    output$detailsSection <- renderUI({
      req(input$showDetails)
      if (nrow(cocktails_disponibles) > 0) {
        cocktail_proche <- cocktails_disponibles[1, ]
        tagList(
          tags$h3("Détails du cocktail :"),
          tags$p(paste("Nom :", cocktail_proche$`nom du cocktail`)),
          tags$p("Ingrédients nécessaires :"),
          tags$ul(
            lapply(unlist(cocktail_proche$ingredients_necessaires), function(ingredient) {
              tags$li(ingredient)
            })
          )
        )
      }
    })
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)



##############


library(shiny)
library(readxl)
library(dplyr)
library(shinythemes)

# Charger le fichier Excel fourni
cocktails <- read_excel("/mnt/data/cocktails_def_pour la party.xlsx")

# Fonction pour analyser les cocktails et identifier les ingrédients manquants
analyser_ingredients <- function(data, alcools_dispo, softs_dispo, sirops_dispo, autres_dispo) {
  cocktails_analyse <- data %>%
    rowwise() %>%
    mutate(
      ingredients_necessaires = list(na.omit(c(`alcool 1`, `alcool 2`, `alcool 3`, `soft 1`, `soft 2`, sirop, autre))),
      ingredients_manquants = list(
        setdiff(unlist(ingredients_necessaires), c(na.omit(alcools_dispo), na.omit(softs_dispo), na.omit(sirops_dispo), na.omit(autres_dispo)))
      ),
      nb_manquants = length(ingredients_manquants)
    ) %>%
    ungroup() %>%
    select(`nom du cocktail`, note, ingredients_necessaires, ingredients_manquants, nb_manquants)
  return(cocktails_analyse)
}

# Définir l'interface utilisateur
ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("On boit quoi ??"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("alcools", "Quelles bouteilles t'as ?",
                  choices = unique(c(cocktails$`alcool 1`, cocktails$`alcool 2`, cocktails$`alcool 3`)),
                  selected = NULL, multiple = TRUE),
      selectInput("softs", "Quels softs t'as ?",
                  choices = unique(c(cocktails$`soft 1`, cocktails$`soft 2`)),
                  selected = NULL, multiple = TRUE),
      selectInput("sirops", "Quels sirops t'as ?",
                  choices = unique(cocktails$sirop),
                  selected = NULL, multiple = TRUE),
      selectInput("autres", "Et il reste quoi dans ton frigo ??",
                  choices = unique(cocktails$autre),
                  selected = NULL, multiple = TRUE),
      actionButton("goButton", "Vamos")
    ),
    
    mainPanel(
      uiOutput("cocktailsList"),
      uiOutput("detailsSection"),
      uiOutput("suggestionSection")
    )
  )
)

# Définir le serveur
server <- function(input, output, session) {
  
  observeEvent(input$goButton, {
    # Récupérer les ingrédients disponibles
    alcools <- input$alcools
    softs <- input$softs
    sirops <- input$sirops
    autres <- input$autres
    
    # Analyser les cocktails
    cocktails_analyse <- analyser_ingredients(cocktails, alcools, softs, sirops, autres)
    
    # Séparer les cocktails réalisables et ceux nécessitant des ingrédients
    cocktails_disponibles <- cocktails_analyse %>%
      filter(nb_manquants == 0)
    
    cocktails_manquants <- cocktails_analyse %>%
      filter(nb_manquants > 0) %>%
      arrange(nb_manquants)
    
    # Afficher les cocktails réalisables ou proposer le plus proche
    output$cocktailsList <- renderUI({
      if (nrow(cocktails_disponibles) > 0) {
        cocktail_proche <- cocktails_disponibles[1, ]
        tagList(
          tags$h3("Cocktail réalisable :"),
          tags$p(paste("Nom :", cocktail_proche$`nom du cocktail`)),
          tags$p(paste("Note :", round(cocktail_proche$note, 1))),
          actionButton("showDetails", "Voir les détails", style = "background-color: red; color: white;")
        )
      } else {
        tags$p("Aucun cocktail n'est réalisable avec vos ingrédients.")
      }
    })

    # Afficher les détails pour le cocktail sélectionné
    output$detailsSection <- renderUI({
      req(input$showDetails)
      if (nrow(cocktails_disponibles) > 0) {
        cocktail_proche <- cocktails_disponibles[1, ]
        tagList(
          tags$h3("Détails du cocktail :"),
          tags$p(paste("Nom :", cocktail_proche$`nom du cocktail`)),
          tags$p("Ingrédients nécessaires :"),
          tags$ul(
            lapply(unlist(cocktail_proche$ingredients_necessaires), function(ingredient) {
              tags$li(ingredient)
            })
          )
        )
      }
    })

    # Afficher une suggestion avec l'ingrédient à ajouter
    output$suggestionSection <- renderUI({
      if (nrow(cocktails_disponibles) == 0 && nrow(cocktails_manquants) > 0) {
        cocktail_suggestion <- cocktails_manquants[1, ]
        tagList(
          tags$h3("Suggestion :"),
          tags$p(paste("Le cocktail le plus proche est :", cocktail_suggestion$`nom du cocktail`)),
          tags$p(paste("Ingrédient à ajouter :", unlist(cocktail_suggestion$ingredients_manquants)[1]))
        )
      } else if (nrow(cocktails_disponibles) > 0 && nrow(cocktails_manquants) > 0) {
        cocktail_suggestion <- cocktails_manquants[1, ]
        tagList(
          tags$h3("Autre suggestion :"),
          tags$p(paste("Vous pouvez presque réaliser :", cocktail_suggestion$`nom du cocktail`)),
          tags$p(paste("Ajoutez :", paste(unlist(cocktail_suggestion$ingredients_manquants), collapse = ", ")))
        )
      }
    })
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)

############


library(shiny)
library(readxl)
library(dplyr)
library(shinythemes)

# Charger le fichier Excel fourni


cocktails <- read_excel("C:/Users/andre/OneDrive/Documentos/GitHub/AD/cocktails_pour_party.xlsx")

# Fonction pour analyser les cocktails et identifier les ingrédients 
manquants
analyser_ingredients <- function(data, alcools_dispo, softs_dispo, sirops_dispo, autres_dispo) {
  cocktails_analyse <- data %>%
    rowwise() %>%
    mutate(
      ingredients_necessaires = list(na.omit(c(`alcool 1`, `alcool 2`, `alcool 3`, `soft 1`, `soft 2`, sirop, autre))),
      ingredients_manquants = list(
        setdiff(unlist(ingredients_necessaires), c(na.omit(alcools_dispo), na.omit(softs_dispo), na.omit(sirops_dispo), na.omit(autres_dispo)))
      ),
      nb_manquants = length(ingredients_manquants)
    ) %>%
    ungroup() %>%
    select(`nom du cocktail`, note, ingredients_necessaires, ingredients_manquants, nb_manquants)
  return(cocktails_analyse)
}

# Définir l'interface utilisateur
ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("On boit quoi ??"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("alcools", "Quelles bouteilles t'as ?",
                  choices = unique(c(cocktails$`alcool 1`, cocktails$`alcool 2`, cocktails$`alcool 3`)),
                  selected = NULL, multiple = TRUE),
      selectInput("softs", "Quels softs t'as ?",
                  choices = unique(c(cocktails$`soft 1`, cocktails$`soft 2`)),
                  selected = NULL, multiple = TRUE),
      selectInput("sirops", "Quels sirops t'as ?",
                  choices = unique(cocktails$sirop),
                  selected = NULL, multiple = TRUE),
      selectInput("autres", "Et il reste quoi dans ton frigo ??",
                  choices = unique(cocktails$autre),
                  selected = NULL, multiple = TRUE),
      actionButton("goButton", "Vamos")
    ),
    
    mainPanel(
      uiOutput("cocktailsList"),
      uiOutput("detailsSection"),
      uiOutput("suggestionSection"),
      uiOutput("nextSuggestionSection")
    )
  )
)

# Définir le serveur
server <- function(input, output, session) {
  
  observeEvent(input$goButton, {
    # Récupérer les ingrédients disponibles
    alcools <- input$alcools
    softs <- input$softs
    sirops <- input$sirops
    autres <- input$autres
    
    # Analyser les cocktails
    cocktails_analyse <- analyser_ingredients(cocktails, alcools, softs, sirops, autres)
    
    # Séparer les cocktails réalisables et ceux nécessitant des ingrédients
    cocktails_disponibles <- cocktails_analyse %>%
      filter(nb_manquants == 0)
    
    cocktails_manquants <- cocktails_analyse %>%
      filter(nb_manquants > 0) %>%
      arrange(nb_manquants)
    
    # Afficher les cocktails réalisables ou proposer le plus proche
    output$cocktailsList <- renderUI({
      if (nrow(cocktails_disponibles) > 0) {
        cocktail_proche <- cocktails_disponibles[1, ]
        tagList(
          tags$h3("Cocktail réalisable :"),
          tags$p(paste("Nom :", cocktail_proche$`nom du cocktail`)),
          tags$p(paste("Note :", round(cocktail_proche$note, 1))),
          actionButton("showDetails", "Voir les détails", style = "background-color: red; color: white;")
        )
      } else {
        tags$p("Aucun cocktail n'est réalisable avec vos ingrédients.")
      }
    })
    
    # Afficher les détails pour le cocktail sélectionné
    output$detailsSection <- renderUI({
      req(input$showDetails)
      if (nrow(cocktails_disponibles) > 0) {
        cocktail_proche <- cocktails_disponibles[1, ]
        tagList(
          tags$h3("Détails du cocktail :"),
          tags$p(paste("Nom :", cocktail_proche$`nom du cocktail`)),
          tags$p("Ingrédients nécessaires :"),
          tags$ul(
            lapply(unlist(cocktail_proche$ingredients_necessaires), function(ingredient) {
              tags$li(ingredient)
            })
          )
        )
      }
    })
    
    # Afficher une suggestion avec l'ingrédient à ajouter
    output$suggestionSection <- renderUI({
      if (nrow(cocktails_disponibles) == 0 && nrow(cocktails_manquants) > 0) {
        cocktail_suggestion <- cocktails_manquants[1, ]
        tagList(
          tags$h3("Suggestion :"),
          tags$p(paste("Le cocktail le plus proche est :", cocktail_suggestion$`nom du cocktail`)),
          tags$p(paste("Ingrédient à ajouter :", unlist(cocktail_suggestion$ingredients_manquants)[1])),
          actionButton("nextSuggestion", "Voir une autre suggestion", style = "background-color: blue; color: white;")
        )
      } else if (nrow(cocktails_disponibles) > 0 && nrow(cocktails_manquants) > 0) {
        cocktail_suggestion <- cocktails_manquants[1, ]
        tagList(
          tags$h3("Autre suggestion :"),
          tags$p(paste("Vous pouvez presque réaliser :", cocktail_suggestion$`nom du cocktail`)),
          tags$p(paste("Ajoutez :", paste(unlist(cocktail_suggestion$ingredients_manquants), collapse = ", "))),
          actionButton("nextSuggestion", "Voir une autre suggestion", style = "background-color: blue; color: white;")
        )
      }
    })
    
    # Afficher une autre suggestion lorsqu'on clique sur le bouton
    observeEvent(input$nextSuggestion, {
      if (nrow(cocktails_manquants) > 1) {
        cocktail_suivant <- cocktails_manquants[2, ]
        output$nextSuggestionSection <- renderUI({
          tagList(
            tags$h3("Nouvelle suggestion :"),
            tags$p(paste("Le cocktail suivant est :", cocktail_suivant$`nom du cocktail`)),
            tags$p(paste("Ingrédients à ajouter :", paste(unlist(cocktail_suivant$ingredients_manquants), collapse = ", ")))
          )
        })
      } else {
        output$nextSuggestionSection <- renderUI({
          tags$p("Aucune autre suggestion disponible.")
        })
      }
    })
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)



