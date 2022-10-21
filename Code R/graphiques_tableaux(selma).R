
#Les graphiques

#Tp 3 des pays avec bcp d'infl
tables_pays=table(instagram$Audience.Country)
table_pays_order=tables_pays[order(tables_pays,decreasing = T)]
top3<-barplot(table_pays_order[1:3],main="Top 3 de pays qui a bcp d'influeunceurs")


# top 5 influenceurs qui ont le plus de followers
top5<-instagram %>%
  filter(Rank >= 996L & Rank <= 1000L) %>%
  ggplot() +
  aes(x = Title, y = Engagement.avg) +
  geom_col(fill = "#112446") +
  theme_minimal()


#Top 5 des comptes les plus visitÃ©s

# 1 ere Ã©tape :tri avec dyplyr

library(dplyr)
influenceurbis<-arrange(influenceur,desc(Followers),Account)

# 2 Ã¨me Ã©tape: top 5 des influenceurs:

df<-influenceurbis[1:6,]


# 3 eme Ã©tape: Suppression de la colonne dÃ©part

dft<-df[-c(1), ]


# 4 eme Ã©tape: diagramme barres top5

count<-table(dft$Account)
par(mfrow=c(1,2))
graph3<-(barplot(sort(count, decreasing = TRUE), 
                 horiz = TRUE, las = 2, 
                 col = "red", col.main = "black",
                 main = "Top 5 des comptes les plus visitÃ©s"))



#Les tableaux 

table1<- c(3,4,6,7,8)
influe_top<- instagram[,table1]

#Pour afficher ce tableau dans shiny
output$tableau1 <- renderTable(
  influe_top %>%
    arrange(desc(influe_top$Followers)) %>%
    slice(1:3)
)





table2<- c(3,4,6,7,8)
influe_pastop<- instagram[,table2]

#Pour afficher ce tableau dans shiny

output$tableau2 <- renderTable(
  influe_pastop %>%
    arrange(desc(influe_pastop$Followers)) %>%
    slice(1000:998)
)


