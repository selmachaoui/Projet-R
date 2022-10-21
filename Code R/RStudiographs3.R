influenceur<-read.csv(file="D:/RShiny/Tables/influenceur2.csv",sep=";",dec=".")


influenceur$Engagement.avg=as.factor(influenceur$Engagement.avg)
influenceur$Engagement.avg=as.numeric(influenceur$Engagement.avg)
influenceur$Authentic.engagement=as.factor(influenceur$Authentic.engagement)
influenceur$Authentic.engagement=as.numeric(influenceur$Authentic.engagement)
influenceur$Followers=as.numeric(influenceur$Followers)


# Boite à moustache sur la moyenne de l'audience:



#Boxplot de comparaison:
boxplot(influenceur$Authentic.engagement,influenceur$Engagement.avg,names=c("Mention Like sur une photo","Visite du profil"),col=c("blue","red"), main="Moyenne du nombre de clics et du nombre de likes sur un profil", horizontal=F)

# Cette boîte à moustache permet de comparer la moyenne du nombre de "likes" par photo et la moyenne des visites de profil d'un influenceur Instagram. On voit 450 000 "likes" en moyenne sur une photo d'un influenceur et légèrement plus de 450 000 visites de profil.


#Histogramme sur le nombre de followers par continent:
  
count2<-table(influenceur$Continent)
par(mfrow=c(1,2))
barplot(sort(count2, decreasing = TRUE), 
        horiz = FALSE, las = 2, 
        col = "palegreen1", col.main = "black",
        main = "Nombre d'influenceurs par continent")

# Cet histogramme permet de montrer quels sont les continents qui ont les influenceurs les plus "populaires" et inversement, quels continents en ont le moins. On remarque que l'Amérique et l'Asie sont les 2 continents ayant le plus de followers, avec plus de 500 followers pour l'Amérique, soit plus de la moitié du nombre total d'influenceurs et plus de 300 followers pour l'Asie ( soit plus d'un tiers).  


# top 5 influenceurs qui ont le plus de followers


# 1 ere étape :tri avec dyplyr

library(dplyr)
influenceurbis<-arrange(influenceur,desc(Followers),Account)

# 2 ème étape: top 5 des influenceurs:

df<-influenceurbis[1:6,]


# 3 eme étape: Suppression de la colonne départ

dft<-df[-c(1), ]


# 4 eme étape: diagramme barres top5

count<-table(dft$Account)
par(mfrow=c(1,2))
barplot(count,cex.names=0.4)
barplot(sort(count, decreasing = TRUE), 
        horiz = TRUE, las = 2, 
        col = "red", col.main = "black",
        main = "Top 5 des comptes les plus visités")

# Ce graphique est un diagramme en barres, avec la même échelle sur l'axe des absisses. Il montre le top 5 des influenceurs.   

# 1 er tableau:

continent<-c("Europe","Amerique","Amerique","Amerique","Asie")
catégorie<-c("Sport","Fashion","Sport","Music","Cinema")
mydata<-data.frame(continent,catégorie, row.names=c("Christiano Ronaldo","Kylie Jenner","Lionel Messi","Selena Gomez","Dwayne Johnson"))
mydata
fix(mydata)

#Ce tableau vise à montrer dans quel continent et dans quelle catégorie les influenceurs les plus connus se situent, afin de pouvoir voir quel catégorie et quel continent contiennent le plus de followers.  

#2ème tableau
nombre_followers<-c(10000000,10000000,10000000,10000000,10000000)
nombre_moy_likes_par_photo<-c(1600000,520100,474100,343800,244100)
mydata2<-data.frame(nombre_followers,nombre_moy_likes_par_photo, nom.du.compte=c("wi__wi__wi","sandrinna_11","vousmevoyez","dean.schneider","francety"))
mydata2
fix(mydata2)

# Parmi les 1000 influenceurs les moins connus, nous nous intéressons cette-fois aux 5 influenceurs avec un nombre de followers moins important ainsi que leur nombre moyen de "like" par photo.