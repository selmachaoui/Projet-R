#installation de packages

#shiny [1] : Il permettra de construire l’application web

#shinydashboard [2]: Il permettra de créer une architecture dynamique à la page web avec une zone de titre, une menu rabattable et une zone principale

#shinyWidgets [3] : Il permettra de mettre un message d’alerte pour confirmer la lecture correcte du tableau

#DT [4] : Il permettra de créer un tableau dynamique avec de la coloration conditionnelle

#plotly [5] , ggplot2 [6] et googleVis [7] : Ils nous permettront de réaliser des graphiques

#colourpicker et esquisse [8] : Il permettra à l’utilisateur de sélectionner une couleur.

#dplyr [9]: est une extension facilitant le traitement et la manipulation de données contenues dans une ou plusieurs tables (qu’il s’agisse de data frame ou de tibble).




install.packages("anyLib")
anyLib::anyLib(c("shiny", "shinydashboard","dplyr"))

install.packages("DT")
install.packages("ggplot2")
install.packages("colourpicker")
install.packages("plotly")
install.packages("googleVis")
install.packages("googleVis")

#Pachages a installé
install.packages("shinyWidgets")
install.packages("esquisse")


#importation de données

setwd("G:/_ BUT STID/STID 2/S 1/Projet R/Fichiers CSv")
influenceur = read.csv2("Influencer.csv", h=T, sep=";")
categorie = read.csv2("Categories.csv", h=T, sep=";")
pays = read.csv2("Pays_Audience.csv", h=T, sep=";")
appartient=read.csv2("Appartiens.csv", h=T, sep=";")
instagram=read.csv2("instagram.csv", h=T, sep=";")
continent=read.csv2("Continent.csv", h=T, sep=";")

#création de liaisons/ jointures

jointure <- merge(x = appartient, y = influenceur, by = "ID_Influencer", all.x = TRUE)
jointure <- merge(x = appartient, y = categorie, by = "ID_Category", all.x = TRUE)
jointure <- merge(x = appartient, y = pays, by = "ID_Pays", all.x = TRUE)


#Changement de variables 



# Palette de couleur
couleurs <- c( noir = "#412a1e", 
              jaune = "#f8de3c",
              rouge = "#c8472c",
              blanc = "#fefefe",
              bleu_c = "#58acf4",
              bleu_d = "#105edd",
              bleu_f = "#0000FF",
              vert   = "#009999")



#Esquisser qui permet de faire des graphique et de copier le codes et les afficher sur shny
#Faut l'exécuter suelement pour faire les graphiques 
esquisser()





#Library

library(shiny)
library(shinydashboard)
library(esquisse)
library(ggplot2)
library(dplyr)




#SERVER
#Server d�finit le fonctionnement de l'application.

server <- function(input, output) { 

  output$dataTable = DT::renderDataTable({
        req(input$dataFile)
    
    data = reactiveValues()
    
    observeEvent(input$actBtnVisualisation, {
      data$table = read.csv(input$dataFile$datapath,
                            header = as.logical(input$header),
                            sep = input$sep,
                            quote = input$quote,
                            nrows=10)
      
      sendSweetAlert(
        session = session,
        title = "Done !",
        text = "Le fichier a bien été lu !",
        type = "success"
        
      )
    })
    
    
  },  options = list(scrollX = TRUE , dom = 't'))
  
  # Calcule de KPI 
  total.influenceur= nrow(x = influenceur) 
  total.categorie = nrow(x = categorie) 
  total.pays = nrow(x = pays) 
  #A rajouter
  #total.continent = nrow(x = continent)
  
  
  #Nombre total des influenceurs
  output$KPI1 <- renderValueBox({
    valueBox(
      total.influenceur, "Nombre total des influenceurs", icon = icon("user"),
      color = "blue")
  })
  
  #Nombre total des catégories
  output$KPI2 <- renderValueBox({
    valueBox(
      total.categorie, "Nombre total des catégories", icon = icon("folder-open"),
      color = "red")
  })
  
  #Nombre total des pays d'audience
  output$KPI3 <- renderValueBox({
    valueBox(
      total.pays, "Nombre total des pays d'audience", icon = icon("location-dot"),
      color = "green")
  })
 
}
  
# LIBRAIRIES
library(shinydashboard)
library(esquisse)
library(ggplot2)
library(dplyr)


# UI 
#Ui d�finit l'apparence de l'application


ui <- dashboardPage(
  dashboardHeader(title = "Instagram influencers",
                  titleWidth = 300),
  skin ="green",
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Lecture des données", tabName = "readData", icon = icon("readme")),
      menuItem("Visualisation des KPI",tabName = "kpi",icon=icon("signal")),
      menuItem("Visualisation des données", tabName = "visualization", icon = icon("poll")),
      menuItem("Tableaux",tabName = "tableau",icon=icon("signal")),
      menuItem("Graphiques",tabName = "graphique",icon=icon("signal"))
      
    )
  ),
  
  dashboardBody( 
 #les deux lignes ne sont pas relues   
    tags$br(),
    div(actionButton(inputId = "actBtnVisualisation", label = "Visualisation",icon = icon("play") ), align = "center"),
    
    tabItems(
      # Read data
      tabItem(tabName = "readData",
              h1("Lecture des données"),
              fileInput("dataFile",label = NULL,
                        buttonLabel = "Télécharger...",
                        placeholder = "Aucun fichier sélectionné"),
              
              fluidRow(
                column(3,
                      h3("Paramètres"),
                      
                      # Input: Checkbox if file has header
                      radioButtons(inputId = "header", 
                                   label = "Titre",
                                   choices = c("Oui" = TRUE,
                                               "Non" = FALSE),
                                   selected = TRUE, inline=T),
                      
                      # Input: Select separator ----
                      radioButtons(inputId = "sep", 
                                   label = "Séparateur",
                                   choices = c("Virgule" = ",",
                                               "Point-virgule" = ";",
                                               "Tabulation" = "\t"),
                                   selected = "\t", inline=T),
                      
                      # Input: Select quotes ----
                      radioButtons(inputId = "quote", #indication, citation
                                   label= "Quote(')",
                                   choices = c(Aucun = "",
                                               "Double Quote" = '"',
                                               "Simple Quote" = "'"),
                                   selected = "", inline=T)),
              column(9,
                    h3("Fichier aperçu/File preview"),
                    dataTableOutput(outputId = "preview")
      ))),
              
      
      # visualization
      tabItem(tabName = "visualization",
              h1("Visualisation des données")
      ),
      
      # tableaux
      tabItem(tabName = "tableau",
              h1("Tableaux")
      ),
    
      #KPI
      tabItem(tabName = "kpi",
              h1("KPI"),
              h2("Informations principales"),
              fluidRow(
                
                # Affichage des KPI
                valueBoxOutput("KPI1"),
                valueBoxOutput("KPI2"),
                valueBoxOutput("KPI3")),
      #Graphques
      tabItem(tabName = "graphique",
              h1("Graphiques")
      )
            
        
      )
    )
  )
)
  


   
    
  



shinyApp(ui,server)