#Importación de librerias
library(leaflet)
library(sp)
library(rgdal)
library(leaflet.extras)
library(leaflet.extras2)
library(sf)
library(leafpop)
library(dplyr)
library(htmlwidgets)

#Se determina la carpeta de trabajo, se abre el archivo shape de proyectos y luego se pasa a coordenadas geograficas para mapearlo con leaflet
setwd("C:/Users/Usuario/Desktop/PracticaGeomatica/RespaldoCapas/Consolidado")
proyectos = readOGR(dsn = ".", layer = "Consolidado_v5")
proyectos = spTransform(proyectos, CRS("+proj=longlat +datum=WGS84 +no_defs"))
#Se aproxima el campo "AreaHa" a dos decimales
proyectos$AreaHa = round(proyectos$AreaHa,2)

#Se determina la 2da carpeta de trabajo donde estan los archivos shapes bases,
#se abren los archivos shapes con las capas bases relevantes  y luego se pasan a coordenadas geograficas para mapearlo con leaflet
setwd("C:/Users/Usuario/Desktop/PracticaGeomatica/RespaldoCapas/Consolidado/CapasBase")
comunas = readOGR(dsn = ".", layer = "comunas_definitivo")
comunas = spTransform(comunas, CRS("+proj=longlat +datum=WGS84 +no_defs"))
regiones = readOGR(dsn = ".", layer = "Regional")
regiones = spTransform(regiones, CRS("+proj=longlat +datum=WGS84 +no_defs"))
sitiosprioritarios = readOGR(dsn = ".", layer = "SitiosPrioritarios")
sitiosprioritarios = spTransform(sitiosprioritarios, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#Se vuelve a determinar la primera carpeta donde se exportará el mapa en html
setwd("C:/Users/Usuario/Desktop/PracticaGeomatica/RespaldoCapas/Consolidado")

#Se crea una variable popup, la cual desplegará la información relevante de cada proyecto
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

#Se crea una paleta de colores en la cual cada proyecto tendrá un color distinto
palfac = colorFactor("viridis", domain =proyectos$NomProyect)

#Se empieza a crear el mapa en leaflet
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

#Exportar el mapa a un archivo html
htmlwidgets::saveWidget(mapa, "MapaV5.html")

#Lo siguientes son extractos de codigo que se podrian usar en un futuro
#para sacar capas o leyendas se usa en la fila de addlayerscontrol
#overlayGroups = c("proyectos","Leyenda")

addLegend("topright", pal = palfac, opacity = 5.0 ,values = ~proyectos$TipoProyec, title = "Tipo de Proyecto", group = "Leyenda")%>% 
palfac = colorFactor("viridis", domain =proyectos$TipoProyec)
