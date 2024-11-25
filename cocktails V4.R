library(shiny)
library(readxl)
library(dplyr)
library(shinythemes)

# lire le fichier Excel
cocktails <- read_excel("~/perso/cocktails.xlsx")

# fonction pour filtrer les cocktails en fonction des ingrédients disponibles
filtrer_cocktails_par_ingredients <- function(data, alcools, softs, sirops, autres) {
  data %>%
    filter((`alcool 1` %in% alcools | is.na(`alcool 1`)) &
             (`alcool 2` %in% alcools | is.na(`alcool 2`)) &
             (`alcool 3` %in% alcools | is.na(`alcool 3`)) &
             (`soft 1` %in% softs | is.na(`soft 1`)) &
             (`soft 2` %in% softs | is.na(`soft 2`)) &
             (sirop %in% sirops | is.na(sirop)) &
             (autre %in% autres | is.na(autre)))
}

alcools_uniques <- unique(c(cocktails$`alcool 1`, cocktails$`alcool 2`, cocktails$`alcool 3`))
softs_uniques <- unique(c(cocktails$`soft 1`, cocktails$`soft 2`))
sirops_uniques <- unique(cocktails$sirop)
autres_uniques <- unique(cocktails$autre)


# Définir l'interface utilisateur
ui <- fluidPage(
  theme = shinytheme("journal"),
  titlePanel("On boit quoi ??"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("alcools", "quelles bouteills t'as ?",
                  choices = alcools_uniques, selected = NULL, multiple = TRUE),
      selectInput("softs", "quels softs t'as ?",
                  choices = softs_uniques, selected = NULL, multiple = TRUE),
      selectInput("sirops", "quels sirops t'as ?",
                  choices = sirops_uniques, selected = NULL, multiple = TRUE),
      selectInput("autres", "et il reste quoi dans ton frigo ??",
                  choices = autres_uniques, selected = NULL, multiple = TRUE),
      actionButton("goButton", "vamos")
    ),
    
    mainPanel(
      uiOutput("cocktailsList"),
      uiOutput("ingredientsInfo")
    )
  )
)

# Définir le serveur
server <- function(input, output, session) {
  
  observeEvent(input$goButton, {
    
    # Extraire les ingrédients des entrées utilisateur
    alcools <- input$alcools
    softs <- input$softs
    sirops <- input$sirops
    autres <- input$autres
    
    # Filtrer les cocktails selon les ingrédients disponibles
    cocktails_possibles <- filtrer_cocktails_par_ingredients(cocktails, alcools, softs, sirops, autres)
    
    # Remplacer les noms manquants par "Nom manquant"
    cocktails_possibles <- cocktails_possibles %>%
      mutate(`nom du cocktail` = ifelse(is.na(`nom du cocktail`), "Nom manquant", `nom du cocktail`)) %>%
      filter(!is.na(`alcool 1`)) %>%
      mutate(note = round(note, 1))
    
    # Créer une liste de cocktails avec un bouton pour chaque cocktail
    output$cocktailsList <- renderUI({
      if (nrow(cocktails_possibles) == 0 ) {
        return (tags$p("j'ai rien pour toi ma puce, y'a besoin d'aller faire les courses là !"))
      }
      cocktails_list <- lapply(1:nrow(cocktails_possibles), function(i) {
        cocktail <- cocktails_possibles[i, ]
        tagList(
          tags$div(
            tags$p(tags$strong(paste(cocktail$`nom du cocktail`))),
            tags$p(paste("Note : ", cocktail$note)),
            actionButton(paste0("btn_", i), "Voir les détails", class = "btn btn-primary"),
            tags$hr()
          )
        )
      })
      do.call(tagList, cocktails_list)
    })
    
    # Afficher les détails des ingrédients lorsqu'un bouton est cliqué
    observe({
      lapply(1:nrow(cocktails_possibles), function(i) {
        observeEvent(input[[paste0("btn_", i)]], {
          cocktail <- cocktails_possibles[i, ]
          output$ingredientsInfo <- renderUI({
            info_list <- c()
            
            if (!is.na(cocktail$`alcool 1`) && cocktail$`alcool 1` != "") {
              info_list <- c(info_list, paste("Alcool 1 : ", cocktail$`alcool 1`))
            }
            if (!is.na(cocktail$`alcool 2`) && cocktail$`alcool 2` != "") {
              info_list <- c(info_list, paste("Alcool 2 : ", cocktail$`alcool 2`))
            }
            if (!is.na(cocktail$`alcool 3`) && cocktail$`alcool 3` != "") {
              info_list <- c(info_list, paste("Alcool 3 : ", cocktail$`alcool 3`))
            }
            if (!is.na(cocktail$`soft 1`) && cocktail$`soft 1` != "") {
              info_list <- c(info_list, paste("Soft 1 : ", cocktail$`soft 1`))
            }
            if (!is.na(cocktail$`soft 2`) && cocktail$`soft 2` != "") {
              info_list <- c(info_list, paste("Soft 2 : ", cocktail$`soft 2`))
            }
            if (!is.na(cocktail$sirop) && cocktail$sirop != "") {
              info_list <- c(info_list, paste("Sirop : ", cocktail$sirop))
            }
            if (!is.na(cocktail$autre) && cocktail$autre != "") {
              info_list <- c(info_list, paste("Autre : ", cocktail$autre))
            }
            
            tagList(
              tags$p(tags$strong(paste("Ingrédients pour :", cocktail$`nom du cocktail`))),
              tags$ul(lapply(info_list, function(info) tags$li(info)))
            )
          })
        })
      })
    })
  })
}
# lancer l'application
shinyApp(ui = ui, server = server)

