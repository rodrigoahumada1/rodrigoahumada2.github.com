library(leaflet)
library(sp)
library(rgdal)
library(leaflet.extras)
library(leaflet.extras2)
library(sf)
library(leafpop)
library(dplyr)
library(htmlwidgets)

setwd("C:/Users/Usuario/Desktop/PracticaGeomatica/RespaldoCapas/Consolidado")
proyectos = readOGR(dsn = ".", layer = "Consolidado_v5")
proyectos = spTransform(proyectos, CRS("+proj=longlat +datum=WGS84 +no_defs"))
proyectos$AreaHa = round(proyectos$AreaHa,2)

setwd("C:/Users/Usuario/Desktop/PracticaGeomatica/RespaldoCapas/Consolidado/CapasBase")
comunas = readOGR(dsn = ".", layer = "comunas_definitivo")
comunas = spTransform(comunas, CRS("+proj=longlat +datum=WGS84 +no_defs"))
regiones = readOGR(dsn = ".", layer = "Regional")
regiones = spTransform(regiones, CRS("+proj=longlat +datum=WGS84 +no_defs"))
sitiosprioritarios = readOGR(dsn = ".", layer = "SitiosPrioritarios")
sitiosprioritarios = spTransform(sitiosprioritarios, CRS("+proj=longlat +datum=WGS84 +no_defs"))


setwd("C:/Users/Usuario/Desktop/PracticaGeomatica/RespaldoCapas/Consolidado")
#Despliege de información
popup = paste("<b>", "Codigo Proyecto: ", "</b>", as.character(proyectos$COD), 
              "<br>", "<b>", "Sub Codidgo Proyecto: ", "</b>", as.character(proyectos$SubCOD), 
              "<br>", "<b>", "Nombre Proyecto: ", "</b>", as.character(proyectos$NomProyect), 
              "<br>", "<b>", "Tipo de Proyecto ", "</b>", as.character(proyectos$TipoProyec), 
              "<br>", "<b>", "Area (Ha): ", "</b>",(proyectos$AreaHa), 
              "<br>","<b>", "Región: ", "</b>", as.character(proyectos$Region), 
              "<br>", "<b>", "Comuna: ", "</b>", as.character(proyectos$Comuna), 
              "<br>",  "<b>", "Cliente: ", "</b>", as.character(proyectos$Cliente),
              "<br>",  "<b>", "Tipologia: ", "</b>", as.character(proyectos$Tipologia),
              "<br>",  "<b>", "Ficha SEIA: ", "</b>", '<a href=',proyectos$LinkSEIA,'>Link</a>')


#Paleta de colores para la leyenda
palfac = colorFactor("viridis", domain =proyectos$NomProyect)

mapa = leaflet(proyectos, options = leafletOptions(minZoom = 3)) %>%
  addResetMapButton() %>%
  setView(lng = -71.542969, lat = -35.675147, zoom = 5)%>%
  addPolygons(data= regiones,color = "black", smoothFactor = 1, fillOpacity = 0,label = regiones$Region, weight = 3, group = "Regiones") %>%
  addPolygons(data= comunas,color = "black", smoothFactor = 1, fillOpacity = 0,label = comunas$Comuna_1, weight = 1, group = "Comunas") %>%
  addPolygons(data= sitiosprioritarios,color = "green", smoothFactor = 1, fillOpacity = 0.5,label = sitiosprioritarios$NOMBRE, weight = 1, group = "SitiosPrioritarios") %>%
  addPolygons(data= proyectos, color = "#444444", weight = 1,smoothFactor = 0.5, fillOpacity = 0.5,fillColor = ~palfac(proyectos$NomProyect) ,popup = popup, label = proyectos$NomProyect, group ="Proyectos") %>%
  addDrawToolbar(targetGroup="proyectos",editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%  
  addMiniMap(tiles = providers$OpenStreetMap,toggleDisplay = TRUE)%>% 
  addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>% 
  addProviderTiles("Esri.WorldImagery",group = "Esri") %>% 
  addLayersControl(baseGroups = c("OpenStreetMap", "Esri"),overlayGroups = c("Proyectos","SitiosPrioritarios","Comunas","Regiones"),options = layersControlOptions(collapsed = FALSE))%>% 
  addMeasure(position = "bottomleft",primaryLengthUnit = "meters",primaryAreaUnit = "hectare",activeColor = "#3D535D",completedColor = "#7D4479", localization = "es") %>% 
  addSearchFeatures(targetGroups = "Proyectos",options = searchFeaturesOptions(zoom= 14, hideMarkerOnCollapse=TRUE))%>% 
  addScaleBar( position = "bottomright")%>% 
  hideGroup("Regiones")%>%hideGroup("Comunas")%>%hideGroup("SitiosPrioritarios")
  
  mapa

#Guardar el mapa
htmlwidgets::saveWidget(mapa, "MapaV5.html")

#para sacar capas o leyendas se usa en la fila de addlayerscontrol
#overlayGroups = c("proyectos","Leyenda")

addPolygons(data= sitiosprioritarios,color = "green", smoothFactor = 1, fillOpacity = 0.5,label = sitiosprioritarios$NOMBRE, weight = 1, group = "SitiosPrioritarios") %>%
addLegend("topright", pal = palfac, opacity = 5.0 ,values = ~proyectos$TipoProyec, title = "Tipo de Proyecto", group = "Leyenda")%>% 
palfac = colorFactor("viridis", domain =proyectos$TipoProyec)
