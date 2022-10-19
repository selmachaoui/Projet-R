##installation de packages

#shiny [1] : Il permettra de construire lâapplication web

#shinydashboard [2]: Il permettra de crÃ©er une architecture dynamique Ã  la page web avec une zone de titre, une menu rabattable et une zone principale

#shinyWidgets [3] : Il permettra de mettre un message dâalerte pour confirmer la lecture correcte du tableau

#DT [4] : Il permettra de crÃ©er un tableau dynamique avec de la coloration conditionnelle

#plotly [5] , ggplot2 [6] et googleVis [7] : Ils nous permettront de rÃ©aliser des graphiques

#colourpicker et esquisse [8] : Il permettra Ã  lâutilisateur de sÃ©lectionner une couleur.

#dplyr [9]: est une extension facilitant le traitement et la manipulation de donnÃ©es contenues dans une ou plusieurs tables (quâil sâagisse de data frame ou de tibble).



install.packages("anyLib")
anyLib::anyLib(c("shiny", "shinydashboard", "shinyWidgets", "DT", "plotly", "ggplot2", "googleVis", "colourpicker"))




#importation de donnÃ©es

setwd("E:/_ BUT STID/STID 2/S 1/Projet R/Fichiers CSv")
influenceur = read.csv2("Influencer.csv", h=T, sep=";")
categorie = read.csv2("Categories.csv", h=T, sep=";")
pays = read.csv2("Pays_Audience.csv", h=T, sep=";")
appartient=read.csv2("Appartiens.csv", h=T, sep=";")
instagram=read.csv2("instagram.csv", h=T, sep=";")
continent=read.csv2("Continent.csv", h=T, sep=";")
influenceur2 = read.csv2("influenceur2.csv", h=T, sep=";")

#crÃ©ation de liaisons/ jointures

jointure <- merge(x = appartient, y = influenceur, by = "ID_Influencer", all.x = TRUE)
jointure <- merge(x = appartient, y = categorie, by = "ID_Category", all.x = TRUE)
jointure <- merge(x = appartient, y = pays, by = "ID_Pays", all.x = TRUE)


#Changement de variables 

influenceur$Followers<-as.numeric(influenceur$Followers,na.rm=F)
influenceur$Authentic.engagement<-as.numeric(influenceur$Authentic.engagement,na.rm=F)
influenceur$Engagement.avg<-as.numeric(influenceur$Engagement.avg,na.rm=F)

influenceur2$Engagement.avg=as.factor(influenceur$Engagement.avg)
influenceur2$Engagement.avg=as.numeric(influenceur$Engagement.avg)
influenceur2$Authentic.engagement=as.factor(influenceur$Authentic.engagement)
influenceur2$Authentic.engagement=as.numeric(influenceur$Authentic.engagement)
influenceur2$Followers=as.numeric(influenceur$Followers)



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
#Faut l'exÃ©cuter suelement pour faire les graphiques 
esquisser()


#Les couleurs qui marchent pour les KPI: red, yellow, aqua, blue, 
#light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, 
#maroon, black



#Library

library(shiny)
library(shinydashboard)
library(esquisse)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(DT)
library(plotly)
library(ggplot2)
library(googleVis)
library(colourpicker)




#SERVER
#Server definit le fonctionnement de l'application.

server <- function(input, output, session) { 
  
  output$preview <-  renderDataTable({
    
    req(input$dataFile)
    
    df <- read.csv(input$dataFile$datapath,
                   header = as.logical(input$header),
                   sep = input$sep,
                   quote = input$quote,
                   nrows=10
    )
  },  options = list(scrollX = TRUE , dom = 't'))
  
  data = reactiveValues()
  
  observeEvent(input$actBtnVisualisation, {
    data$table = read.csv(input$dataFile$datapath,
                          header = as.logical(input$header),
                          sep = input$sep,
                          quote = input$quote,
                          nrows=10)
  })
  observeEvent(input$actBtnVisualisation, {
    data$table = read.csv(input$dataFile$datapath,
                          header = as.logical(input$header),
                          sep = input$sep,
                          quote = input$quote,
                          nrows=10)
    sendSweetAlert(
      session = session,
      title = "Done !",
      text = "Le fichier a été bien lu !",
      type = "succès"
    )  
  })
  #changer de page une fois que le fichier est lu pour arriver sur la page de visualisation.
  updateTabItems(session=session, "tabs", selected = "tableau")
  
  #envoyer le contenu de notre fichier dans ce tableau par le biais de la reactiveValue. Ainsi, le tableau sera automatiquement mis ? jour si un nouveau fichier est lu.
  output$dataTable = DT::renderDataTable(data$table)



options = list(scrollX = TRUE , dom = 't')

output$import_data<-DT::renderDataTable({})

# Calcule de KPI 
total.influenceur= nrow(x = influenceur) 
total.categorie = nrow(x = categorie) 
total.pays = nrow(x = pays) 
moyennefollowers=round(mean(influenceur$Followers,na.rm=T))
moyennevue=round(mean(influenceur$Engagement.avg,na.rm=T))
moyennelike=round(mean(influenceur$Authentic.engagement,na.rm=T))

#Nombre total des influenceurs
output$KPI1 <- renderValueBox({
  valueBox(
    total.influenceur,"Nombre total des influenceurs", icon = icon ("user"),
    color = "fuchsia") 
})

#Nombre total des catÃ©gories
output$KPI2 <- renderValueBox({
  valueBox(
    total.categorie, "Nombre total des catégories", icon = icon("folder-open"),
    color = "blue")
})

#Nombre total des pays d'audience
output$KPI3 <- renderValueBox({
  valueBox(
    total.pays, "Nombre total des pays d'audience", icon = icon("map-marker"), #glyphicon-carte-marqueur
    color = "aqua")
})

#Moyenne de followers
output$KPI4 <- renderValueBox({
  valueBox(
    moyennefollowers, "Moyenne de followers", icon = icon("thumbs-up"),#folder-open
    color = "fuchsia")
  
})

#Moyenne de like
output$KPI5 <- renderValueBox({
  valueBox(
    moyennelike, "Moyenne de like", icon = icon("heart"),
    color = "blue")
  
})

#Moyenne de vues
output$KPI6 <- renderValueBox({
  valueBox(
    moyennevue, "Moyenne de vues", icon = icon("eye"),
    color = "aqua")
  
})
#glyphicon glyphicon-pouce levé
#glyphicon glyphicon-oeil-ouvert
#glyphicon enregistré par glyphicon
#glyphicon glyphicon-cœur
}


# UI 
#Ui d?finit l'apparence de l'application


ui <- dashboardPage(
  dashboardHeader(title = "Instagram influencers",
                  titleWidth = 300),
  skin ="purple",
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Lecture des donnees", tabName = "readData", icon = icon("readme")),
      menuItem("Visualisation des KPI",tabName = "kpi",icon=icon("chart-pie")),
      menuItem("Tableaux",tabName = "tableau",icon=icon("table")),
      menuItem("Graphiques",tabName = "graphique",icon=icon("signal"))
      
    )
  ),
  
  dashboardBody( 
    tags$br(),
    div(actionButton(inputId = "actBtnVisualisation", label = "Visualisation",icon = icon("play") ), align = "center"),
    
    tabItems(
      # Read data
      tabItem(tabName = "readData",
              h1("Lecture des donnees"),
              fileInput("dataFile",label = NULL,
                        buttonLabel = "Télécharger...",
                        placeholder = "Aucun fichier sélectionner"),
              
              fluidRow(
                column(3,
                       h3("Paramétres"),
                       
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
                                    selected = "", inline=T),
                column(9,
                       h3("Fichier aperçu/File preview"),
                       dataTableOutput(outputId = "preview")
                )
                ))),
      #KPI
      tabItem(tabName = "kpi",
              h1("KPI"),
              h2("Informations principales"),
              fluidRow(
                
                # Affichage des KPI
                valueBoxOutput("KPI1"),
                valueBoxOutput("KPI2"),
                valueBoxOutput("KPI3"),
                valueBoxOutput("KPI4"),
                valueBoxOutput("KPI5"),
                valueBoxOutput("KPI6")),
              ),    

      
      # tableaux
      tabItem(tabName = "tableau",
              h1("Tableaux")
      ),
      
      
      # Graphiques
      tabItem(tabName = "graphique",
              h1("Graphiques")
                      
              )
      )))
      
      
    
  

    
    
    
    
    
    
    
    
    
    
    shinyApp(ui,server)