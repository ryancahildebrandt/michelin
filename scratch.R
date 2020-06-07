# Doc Setup -----
library(plotly)
library(DataExplorer)
library(esquisse)
library(data.table)
library(psych)
library(MASS)
library(klaR)
library(tidyverse)
library(magrittr)
library(readr)
library(NbClust)
library(ggmap)
library(maps)
#esquisse::esquisser(michelin)
#rpivotTable::rpivotTable()

# Read in & Join----

one_star_michelin_restaurants <- read_csv("one-star-michelin-restaurants.csv", 
                                          col_types = cols(latitude = col_character(), 
                                                           longitude = col_character(), 
                                                           year = col_integer(), 
                                                           zipCode = col_character())) %>%
  mutate(., stars=1)
two_star_michelin_restaurants <- read_csv("two-stars-michelin-restaurants.csv", 
                                          col_types = cols(latitude = col_character(), 
                                                           longitude = col_character(), 
                                                           year = col_integer(), 
                                                           zipCode = col_character()))  %>%
  mutate(., stars=2)
three_star_michelin_restaurants <- read_csv("three-stars-michelin-restaurants.csv", 
                                          col_types = cols(latitude = col_character(), 
                                                           longitude = col_character(), 
                                                           year = col_integer(), 
                                                           zipCode = col_character()))  %>%
  mutate(., stars=3,)

michelin<-one_star_michelin_restaurants %>% 
  bind_rows(., two_star_michelin_restaurants) %>%
  bind_rows(., three_star_michelin_restaurants) %>%
  mutate(., 
         city=factor(.$city),
         region=factor(.$region),
         zipCode=factor(.$zipCode),
         cuisine=factor(.$cuisine),
         price=factor(str_length(.$price)),
         stars=factor(.$stars),
         latitude=as.numeric(.$latitude),
         longitude=as.numeric(.$longitude))%>%
           mutate(.,
                  cuisine.collapsed=fct_collapse(michelin$cuisine,
             American= c("American","Californian"),
             Australian= c("Australian"),
             Chinese= c("Cantonese","Hang Zhou","Sichuan-Huai Yang",
                        "Cantonese Roast Meats","Fujian","Hunanese and Sichuan",
                        "Chinese","Shanghainese","Taiwanese","Dim Sum","Sichuan","Taizhou","Noodles and congee"),
             French= c("Classic French","French contemporary","Creative French","French","Modern French"),
             British= c("Creative British","Traditional British","Modern British"),
             European= c("European","Austrian","European contemporary","Danish",
                         "Finnish"),
             Meats=c("Meats and grills","Barbecue"),
             Modern=c("modern","Contemporary","creative","Fusion","Modern cuisine","Creative",
                      "Gastropub","Innovative"),
             Other= c("International","Street Food",
                      "Temple cuisine","Seafood","Vegetarian",
                      "Classic cuisine","Market cuisine","Regional cuisine","Steakhouse"),
             Japanese= c("Japanese contemporary","Sushi","Teppanyaki","Japanese"),
             Moroccan= c("Moroccan"),
             Scandinavian= c("Scandinavian"),
             Asian= c("Asian","Asian contemporary","Asian influences"),
             Italian= c("Italian","Italian contemporary"),
             Korean= c("Korean","Korean contemporary"),
             Mediterranean= c("Mediterranean","Mediterranean cuisine"),
             Thai= c("Southern Thai","Thai","Thai Contemporary"),
             Indian= c("Indian"),
             Malaysian= c("Peranakan"),
             Spanish= c("Spanish"),
             Mexican= c("Mexican")))

create_report(michelin)

# K-Modes Clustering----
michelin.kmodes <- michelin %>%
  mutate_if(sapply(., is.factor), as.numeric)%>%
  dplyr::select(.,c("cuisine.collapsed","price","stars"))

NbClust(michelin.kmodes,
        distance="euclidean",
        min.nc=2,
        max.nc=20,
        method="ward.D")

kmode.4<-kmodes(michelin.kmodes,
                     4,
                     iter.max = 10,
                     weighted = FALSE)

cluster.info<-kmode.4$modes %>%
  mutate(., 
         cuisine.collapsed=levels(michelin$cuisine.collapsed)[.$cuisine.collapsed],
         price=strrep("$",.$price))

michelin$cluster<-kmode.4$cluster


#Plots----
plot(michelin.kmodes,col=kmode.4$cluster)

michelin.kmodes %>% 
  mutate(.,cluster=kmode.4$cluster) %>% 
  ggplot(.,aes(x=stars,y=price,color=cluster,fill=cluster)) +
  geom_point(size=2) +
  geom_jitter()

michelin.kmodes %>% 
  mutate(.,cluster=kmode.4$cluster) %>% 
  ggplot(.,aes(x=cuisine.collapsed,y=price,color=cluster,fill=cluster)) +
  geom_point(size=2) +
  geom_jitter()

michelin.kmodes %>% 
  mutate(.,cluster=kmode.4$cluster) %>% 
  ggplot(.,aes(x=cuisine.collapsed,y=stars,color=cluster,fill=cluster)) +
  geom_point(size=2) +
  geom_jitter()
#----
michelin.kmodes %>%
  mutate(.,cluster=kmode.4$cluster) %>% 
  plot_ly(x=.$cuisine.collapsed, 
          y=.$price, 
          z=.$stars, 
          type="scatter3d", 
          mode="markers", 
          color=.$cluster,
          showlegend=FALSE,
          hoverinfo="text",
          hovertext=paste(michelin$name,":",
      "A",strrep("$",michelin$price),
      michelin$stars,"Michelin star",
      michelin$cuisine.collapsed,"restaurant",
      "in",michelin$city))

# Map ----
Sys.setenv("MAPBOX_TOKEN"="pk.eyJ1IjoicnlhbmNhaGlsZGVicmFuZHQiLCJhIjoiY2tiNWd0MzJmMTN5MzJybXZ0cnp2N2c0MSJ9.qh0GjKns3qfkdZFxLlG4Lw")
plot_mapbox(maps::world.cities) %>%
  add_markers(
    x = michelin$longitude, 
    y = michelin$latitude, 
    size = as.numeric(michelin$price)**5, 
    color = michelin$stars,
    hoverinfo="text",
    hovertext=paste(michelin$name,":",
      "A",strrep("$",michelin$price),
      michelin$stars,"Michelin star",
      michelin$cuisine.collapsed,"restaurant",
      "in",michelin$city))

       
          






