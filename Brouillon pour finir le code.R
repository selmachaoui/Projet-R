#j'ai enlevé ces lignes qui me permettait d'avoir un tablea dans la partie KPI


column(12,
       tableOutput('table'),
       DT::dataTableOutput("import_data"),
       div(style = "display:inline-block; float:right"),)



#graphiques

#table ...
table<- table(appartient$Category,appartient$Audience.Country)

dimnames(table)
names(dimnames(table)) <- c("Catégories","Pays d'audience")
table <- addmargins(table)
table


#graphiques de catégories par continent  (Le probleme ce que le continent de chaque influenceur apparait autant de fois que l'influenceur apparait. Sachant que l'influenceur apparait autant de fois que le nombre de catégorie qu'il fait)

ggplot(appartient) +
  aes(x = ID_Category, fill = Continent) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()


#comparaison de followers par continent (boxplot) (même problèeme)

ggplot(appartient) +
  aes(x = ID_Influencer, y = Continent) +
  geom_boxplot(fill = "#112446") +
  theme_minimal()



#Diagramme en barre sur le nombre d’influenceurs par pays (le nombre d'audience par pays) (même problèeme)

ggplot(appartient) +
  aes(x = Audience.Country) +
  geom_bar(fill = "#112446") +
  theme_minimal()


#Diagramme en barre sur le nombre d’influenceurs par cont (même problème)

ggplot(appartient) +
  aes(x = Continent) +
  geom_bar(fill = "#112446") +
  theme_minimal()


#Diagramme en barre sur le nombre de followers par continent (même problème)

ggplot(appartient) +
  aes(x = Continent, weight = Followers) +
  geom_bar(fill = "#112446") +
  theme_minimal()

#Histogramme sur le nombre de catégories par pays 


#Diagramme circulaire sur le nombre d’influenceurs par catégorie


