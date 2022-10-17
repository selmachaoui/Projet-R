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

setwd("G:/_ BUT STID/STID 2/S 1/Projet R/Fichiers CSv")
influenceur = read.csv2("Influencer.csv", h=T, sep=";")
categorie = read.csv2("Categories.csv", h=T, sep=";")
pays = read.csv2("Pays_Audience.csv", h=T, sep=";")
appartient=read.csv2("Appartiens.csv", h=T, sep=";")
instagram=read.csv2("instagram.csv", h=T, sep=";")
continent=read.csv2("Continent.csv", h=T, sep=";")

#crÃ©ation de liaisons/ jointures

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
#Faut l'exÃ©cuter suelement pour faire les graphiques 
esquisser()





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
#Server d?finit le fonctionnement de l'application.

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
      text = "Le fichier a bien ?t? lu !",
      type = "success"
    )  
  })
  #changer de page une fois que le fichier est lu pour arriver sur la page de visualisation.
  updateTabItems(session=session, "tabs", selected = "visualization")
  
  #envoyer le contenu de notre fichier dans ce tableau par le biais de la reactiveValue. Ainsi, le tableau sera automatiquement mis ? jour si un nouveau fichier est lu.
  output$dataTable = DT::renderDataTable(data$table)
}


options = list(scrollX = TRUE , dom = 't')

# Calcule de KPI 
total.influenceur= nrow(x = influenceur) 
total.categorie = nrow(x = categorie) 
total.pays = nrow(x = pays) 
#A rajouter
#total.continent = nrow(x = continent)


#Nombre total des influenceurs
output$KPI1 <- renderValueBox({
  valueBox(
    total.influenceur,"Nombre total des influenceurs", icon = icon ("user"),
    color = "blue") #user
})

#Nombre total des catÃ©gories
output$KPI2 <- renderValueBox({
  valueBox(
    total.categorie, "Nombre total des catÃ©gories", icon = icon("folder-open"),
    color = "red")
})

#Nombre total des pays d'audience
output$KPI3 <- renderValueBox({
  valueBox(
    total.pays, "Nombre total des pays d'audience", icon = icon("location-dot"),
    color = "green")
  
  
  
  
})





# UI 
#Ui d?finit l'apparence de l'application


ui <- dashboardPage(
  dashboardHeader(title = "Instagram influencers",
                  titleWidth = 300),
  skin ="purple",
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Lecture des donnÃ©es", tabName = "readData", icon = icon("chart-pie")),
      menuItem("Visualisation des KPI",tabName = "kpi",icon=icon("signal")),
      menuItem("Visualisation des donnÃ©es", tabName = "visualization", icon = icon("readme")), #poll
      menuItem("Tableaux",tabName = "tableau",icon=icon("table")),
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
              h1("Lecture des donnÃ©es"),
              fileInput("dataFile",label = NULL,
                        buttonLabel = "TÃ©lÃ©charger...",
                        placeholder = "Aucun fichier sÃ©lectionnÃ©"),
              
              fluidRow(
                column(3,
                       h3("ParamÃ¨tres"),
                       
                       # Input: Checkbox if file has header
                       radioButtons(inputId = "header", 
                                    label = "Titre",
                                    choices = c("Oui" = TRUE,
                                                "Non" = FALSE),
                                    selected = TRUE, inline=T),
                       
                       # Input: Select separator ----
                       radioButtons(inputId = "sep", 
                                    label = "SÃ©parateur",
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
                       h3("Fichier aperÃ§u/File preview"),
                       dataTableOutput(outputId = "preview")
                ))),
      
      
      # visualization
      tabItem(tabName = "visualization",
              h1("Visualisation des donnÃ©es"),
              h2("Exploration du tableau"),
              dataTableOutput('dataTable')
      ),
      
      # tableaux
      tabItem(tabName = "tableau",
              h1("Tableaux")
      ),
      
      
      # Graphiques
      tabItem(tabName = "graphique",
              h1("Graphiques"),
              fluidRow(
                
                
                # Boite Ã  moustache sur la moyenne de l'audience:
                
                
                #Boxplot de comparaison:
                box(boxplot(influenceur2$Authentic.engagement,influenceur2$Engagement.avg,names=c("Mention Like sur une photo","Visite du profil"),col=c("blue","red"), main="Moyenne du nombre de clics et du nombre de likes sur un profil", horizontal=F)),
                
                # Cette boÃ®te Ã  moustache permet de comparer la moyenne du nombre de "likes" par photo et la moyenne des visites de profil d'un influenceur Instagram. On voit 450 000 "likes" en moyenne sur une photo d'un influenceur et lÃ©gÃ¨rement plus de 450 000 visites de profil.
                
                
                #Histogramme sur le nombre de followers par continent:
                
                count2<-table(influenceur2$Continent),
                par(mfrow=c(1,2)),
                bx(barplot(sort(count2, decreasing = TRUE)), 
                   horiz = FALSE, las = 2, 
                   col = "palegreen1", col.main = "black",
                   main = "Nombre d'influenceurs par continent"),
                
                # Cet histogramme permet de montrer quels sont les continents qui ont les influenceurs les plus "populaires" et inversement, quels continents en ont le moins. On remarque que l'AmÃ©rique et l'Asie sont les 2 continents ayant le plus de followers, avec plus de 500 followers pour l'AmÃ©rique, soit plus de la moitiÃ© du nombre total d'influenceurs et plus de 300 followers pour l'Asie ( soit plus d'un tiers).  
                
                
                # top 5 influenceurs qui ont le plus de followers
                
                
                # 1 ere Ã©tape :tri avec dyplyr
                
                library(dplyr),
                box(influenceur2bis<-arrange(influenceur2,desc(Followers),Account)),
                
                # 2 Ã¨me Ã©tape: top 5 des influenceurs:
                
                box(df<-influenceur2bis[1:6,]),
                
                
                # 3 eme Ã©tape: Suppression de la colonne dÃ©part
                
                box(dft<-df[-c(1), ]),
                
                
                # 4 eme Ã©tape: diagramme barres top5
                
                count<-table(dft$Account),
                par(mfrow=c(1,2)),
                barplot(count,cex.names=0.4),
                box(barplot(sort(count, decreasing = TRUE)), 
                    horiz = TRUE, las = 2, 
                    col = "red", col.main = "black",
                    main = "Top 5 des comptes les plus visitÃ©s"),
                
                # Ce graphique est un diagramme en barres, avec la mÃªme Ã©chelle sur l'axe des absisses. Il montre le top 5 des influenceurs.   
                
                
                #Pour le filte
                box(title = "Filtre", status = "danger", solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    selectInput("Pays", "Audience.Country", 
                                choices = c("Tous les pays", unique(pays$Audience.Country))),
                    
                    
                )
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
    
    
    
    
    
    
    
    
    
    
    shinyApp(ui,server)