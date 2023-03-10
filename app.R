#
# Dashboard for Pillars of Ciudad Verde
#


# PACKAGES
library(dbplyr)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(colorspace)


# FUNCTIONS

get_quartiles_labels <- function(x) {
  # Returns a vector with the labels of the quartiles ranges
  
  r1 <- paste0(as.character(round(x[1], 1)), " - ", as.character(round(x[2], 1)))
  r2 <- paste0(as.character(round(x[2], 1)), " - ", as.character(round(x[3], 1)))  
  r3 <- paste0(as.character(round(x[3], 1)), " - ", as.character(round(x[4], 1)))  
  r4 <- paste0(as.character(round(x[4], 1)), " - ", as.character(round(x[5], 1)))  
  
  return(c(r1, r2, r3, r4))
}


# DATASETS

# Data sources
DSN_CANTONS <- "data/metricas_ejemplo_tablero_080223.geojson"

# Cantons and their indicators of ecosystem services
cantons <- st_read(dsn = DSN_CANTONS, quiet = TRUE)

# Data columns
COLUMN_CANTON_NAME <- cantons$canton
COLUMN_CANTON_NAME_2 <- "canton"

COLUMN_GREENAREA <- cantons$supverd.ha
COLUMN_TRAILSLONGITUDE <- cantons$km_sendero
COLUMN_OPENSPACES <- cantons$numeros_es

COLUMN_CULTIVATEDLAND <- cantons$tierrasc_h
COLUMN_CULTIVATEDPASTURES <- cantons$pastos_ha
COLUMN_SHADECOFFEE <- cantons$cafe_ha
COLUMN_PERMANENTCROPS <- cantons$perenne_ha
COLUMN_ANNUALCROPS <- cantons$anuales_ha


# CONSTANTS

# Indicators
INDICATOR_GREENAREA <- "Superficie verde por habitante"
INDICATOR_TRAILSLONGITUDE <- "Longitud de senderos para caminar per cápita"
INDICATOR_OPENSPACES <- "Número de espacios públicos para recreación"

INDICATOR_CULTIVATEDLAND <- "Superficie de tierra cultivada"
INDICATOR_CULTIVATEDPASTURES <- "Superficie de pastos cultivados"
INDICATOR_SHADECOFFEE <- "Superficie de café con sombra"
INDICATOR_PERMANENTCROPS <- "Superficie de cultivos perennes"
INDICATOR_ANNUALCROPS <- "Superficie de cultivos anuales"

# Classes for simbology in maps
QUANTILES_GREENAREA_VALUES <- fivenum(COLUMN_GREENAREA)
QUANTILES_GREENAREA_LABELS <- get_quartiles_labels(QUANTILES_GREENAREA_VALUES)

QUANTILES_TRAILSLONGITUDE_VALUES <- fivenum(COLUMN_TRAILSLONGITUDE)
QUANTILES_TRAILSLONGITUDE_LABELS <- get_quartiles_labels(QUANTILES_TRAILSLONGITUDE_VALUES)

QUANTILES_OPENSPACES_VALUES <- fivenum(COLUMN_OPENSPACES)
QUANTILES_OPENSPACES_LABELS <- get_quartiles_labels(QUANTILES_OPENSPACES_VALUES)

QUANTILES_CULTIVATEDLAND_VALUES <- fivenum(COLUMN_CULTIVATEDLAND)
QUANTILES_CULTIVATEDLAND_LABELS <- get_quartiles_labels(QUANTILES_CULTIVATEDLAND_VALUES)

QUANTILES_CULTIVATEDPASTURES_VALUES <- fivenum(COLUMN_CULTIVATEDPASTURES)
QUANTILES_CULTIVATEDPASTURES_LABELS <- get_quartiles_labels(QUANTILES_CULTIVATEDPASTURES_VALUES)

QUANTILES_SHADECOFFEE_VALUES <- fivenum(COLUMN_SHADECOFFEE)
QUANTILES_SHADECOFFEE_LABELS <- get_quartiles_labels(QUANTILES_SHADECOFFEE_VALUES)

# QUANTILES_PERMANENTCROPS_VALUES <- fivenum(COLUMN_PERMANENTCROPS)
# QUANTILES_PERMANENTCROPS_LABELS <- get_quartiles_labels(QUANTILES_PERMANENTCROPS_VALUES)
# To avoid error: 'breaks' are not unique
QUANTILES_PERMANENTCROPS_VALUES <- c(0, 0.7, 5.25, 99.967)
QUANTILES_PERMANENTCROPS_LABELS <- c("0 - 0.7", "0.7 - 5.25", "5.25 - 99.97")

QUANTILES_ANNUALCROPS_VALUES <- fivenum(COLUMN_ANNUALCROPS)
QUANTILES_ANNUALCROPS_LABELS <- get_quartiles_labels(QUANTILES_ANNUALCROPS_VALUES)


# Color palettes
PALETTE_GREENAREA_COLOR <- rgb(175, 255, 175, maxColorValue = 255) # CORINE CR - Zonas verdes urbanas
PALETTE_GREENAREA_START_COLOR <- lighten(PALETTE_GREENAREA_COLOR, 0.4)
PALETTE_GREENAREA_END_COLOR <- darken(PALETTE_GREENAREA_COLOR, 0.4)
PALETTE_GREENAREA <- colorBin(
  bins = QUANTILES_GREENAREA_VALUES,   
  palette = c(PALETTE_GREENAREA_START_COLOR, PALETTE_GREENAREA_END_COLOR),
  na.color = NA
)
PALETTE_TRAILSLONGITUDE_COLOR <- rgb(248, 0, 0, maxColorValue = 255) # CORINE CR - Red vial
PALETTE_TRAILSLONGITUDE_START_COLOR <- lighten(PALETTE_TRAILSLONGITUDE_COLOR, 0.4)
PALETTE_TRAILSLONGITUDE_END_COLOR <- darken(PALETTE_TRAILSLONGITUDE_COLOR, 0.4)
PALETTE_TRAILSLONGITUDE <- colorBin(
  bins = QUANTILES_TRAILSLONGITUDE_VALUES, 
  palette = c(PALETTE_TRAILSLONGITUDE_START_COLOR, PALETTE_TRAILSLONGITUDE_END_COLOR),
  na.color = NA
)

PALETTE_OPENSPACES_COLOR <- rgb(230, 0, 0, maxColorValue = 255) # CORINE CR - Instalaciones recreativas
PALETTE_OPENSPACES_START_COLOR <- lighten(PALETTE_OPENSPACES_COLOR, 0.4)
PALETTE_OPENSPACES_END_COLOR <- darken(PALETTE_OPENSPACES_COLOR, 0.4)
PALETTE_OPENSPACES <- colorBin(
  bins = QUANTILES_OPENSPACES_VALUES, 
  palette = c(PALETTE_OPENSPACES_START_COLOR, PALETTE_OPENSPACES_END_COLOR),
  na.color = NA
)

PALETTE_CULTIVATEDLAND_COLOR <- rgb(190, 205, 0, maxColorValue = 255) # CORINE CR - Mosaico de cultivos
PALETTE_CULTIVATEDLAND_START_COLOR <- lighten(PALETTE_CULTIVATEDLAND_COLOR, 0.4)
PALETTE_CULTIVATEDLAND_END_COLOR <- darken(PALETTE_CULTIVATEDLAND_COLOR, 0.4)
PALETTE_CULTIVATEDLAND <- colorBin(
  bins = QUANTILES_CULTIVATEDLAND_VALUES, 
  palette = c(PALETTE_CULTIVATEDLAND_START_COLOR, PALETTE_CULTIVATEDLAND_END_COLOR),
  na.color = NA
)

PALETTE_CULTIVATEDPASTURES_COLOR <- rgb(255, 255, 166, maxColorValue = 255) # CORINE CR - Pastos limpios
PALETTE_CULTIVATEDPASTURES_START_COLOR <- lighten(PALETTE_CULTIVATEDPASTURES_COLOR, 0.4)
PALETTE_CULTIVATEDPASTURES_END_COLOR <- darken(PALETTE_CULTIVATEDPASTURES_COLOR, 0.4)
PALETTE_CULTIVATEDPASTURES <- colorBin(
  bins = QUANTILES_CULTIVATEDPASTURES_VALUES, 
  palette = c(PALETTE_CULTIVATEDPASTURES_START_COLOR, PALETTE_CULTIVATEDPASTURES_END_COLOR),
  na.color = NA
)

PALETTE_SHADECOFFEE_COLOR <- rgb(115, 38, 0, maxColorValue = 255) # CORINE CR - Café
PALETTE_SHADECOFFEE_START_COLOR <- lighten(PALETTE_SHADECOFFEE_COLOR, 0.4)
PALETTE_SHADECOFFEE_END_COLOR <- darken(PALETTE_SHADECOFFEE_COLOR, 0.4)
PALETTE_SHADECOFFEE <- colorBin(
  bins = QUANTILES_SHADECOFFEE_VALUES, 
  palette = c(PALETTE_SHADECOFFEE_START_COLOR, PALETTE_SHADECOFFEE_END_COLOR),
  na.color = NA
)

PALETTE_PERMANENTCROPS_COLOR <- rgb(255, 210, 125, maxColorValue = 255) # CORINE CR - Otros cultivos permanentes
PALETTE_PERMANENTCROPS_START_COLOR <- lighten(PALETTE_PERMANENTCROPS_COLOR, 0.4)
PALETTE_PERMANENTCROPS_END_COLOR <- darken(PALETTE_PERMANENTCROPS_COLOR, 0.4)
PALETTE_PERMANENTCROPS <- colorBin(
  bins = QUANTILES_PERMANENTCROPS_VALUES, 
  palette = c(PALETTE_PERMANENTCROPS_START_COLOR, PALETTE_PERMANENTCROPS_END_COLOR),
  na.color = NA
)

PALETTE_ANNUALCROPS_COLOR <- rgb(168, 112, 0, maxColorValue = 255) # CORINE CR - Otros cultivos anuales
PALETTE_ANNUALCROPS_START_COLOR <- lighten(PALETTE_ANNUALCROPS_COLOR, 0.4)
PALETTE_ANNUALCROPS_END_COLOR <- darken(PALETTE_ANNUALCROPS_COLOR, 0.4)
PALETTE_ANNUALCROPS <- colorBin(
  bins = QUANTILES_ANNUALCROPS_VALUES, 
  palette = c(PALETTE_ANNUALCROPS_START_COLOR, PALETTE_ANNUALCROPS_END_COLOR),
  na.color = NA
)


# USER INTERFACE
ui <-
  fluidPage(theme = "bootstrap",
    tags$head(
      tags$style(
        HTML(
          '/* Radio buttons size */
          #radiobuttons_indicators_recreation label {
            font-size: 18px;
          }
          #radiobuttons_indicators_food label {
            font-size: 18px;
          }
          .texto_agradecimiento_logos {
            text-align: center;
          }'
        )
      )
    ),    
    navbarPage(title = "Pilares Ciudad Verde", 
               theme = shinytheme("lumen"), 

               # Pilar Ciudad verde: Salud y bienestar
               navbarMenu("Salud y bienestar", icon = icon("globe-americas"),
                          
                          # Dimensión: Recreación - Servicio ecosistémico: Cultural
                          tabPanel("Recreación", fluid = TRUE, icon = icon("globe-americas"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       fluidRow(h2(strong("Pilar Ciudad Verde"), br(), "Salud y bienestar")),
                                       fluidRow(h3(strong("Dimensión"), br(), "Recreación")),
                                       fluidRow(h4(strong("Meta aspiracional"), br(), em("La ciudad brinda a sus habitantes espacios verdes públicos y de calidad, para la recreación y la salud mental."))),
                                       fluidRow(h4(strong("Servicio ecosistémico"), br(), "Cultural")),
                                       fluidRow(h4(strong("Indicadores"))),
                                       fluidRow(
                                         radioButtons("radiobuttons_indicators_recreation",
                                                      label = "",
                                                      choices = c(
                                                        INDICATOR_GREENAREA,
                                                        INDICATOR_TRAILSLONGITUDE,
                                                        INDICATOR_OPENSPACES
                                                      ),
                                                      selected = INDICATOR_GREENAREA
                                         )
                                       )
                                     ),
                                     mainPanel(
                                       fluidRow(h4(strong(textOutput("header_recreation")))),
                                       fluidRow(withSpinner(leafletOutput("map_recreation"))),
                                       fluidRow(withSpinner(plotlyOutput("barplot_recreation")))
                                     )
                                   )
                          ),
                          
                          # Dimensión: Alimento para la población - Servicio ecosistémico: Aprovisionamiento
                          tabPanel("Alimento para la población", fluid = TRUE, icon = icon("globe-americas"),
                                   sidebarPanel(
                                     fluidRow(h2(strong("Pilar Ciudad Verde"), br(), "Salud y bienestar")),
                                     fluidRow(h3(strong("Dimensión"), br(), "Alimento para la población")),
                                     fluidRow(h4(strong("Meta aspiracional"), br(), em("La ciudad ofrece alimentos frescos, orgánicos de producción sostenible y local a la población."))),
                                     fluidRow(h4(strong("Servicio ecosistémico"), br(), "Aprovisionamiento")),  
                                     fluidRow(h4(strong("Indicadores"))),
                                     fluidRow(
                                       radioButtons("radiobuttons_indicators_food",
                                                    label = "",
                                                    choices = c(
                                                      INDICATOR_CULTIVATEDLAND,
                                                      INDICATOR_CULTIVATEDPASTURES,
                                                      INDICATOR_SHADECOFFEE,
                                                      INDICATOR_PERMANENTCROPS,
                                                      INDICATOR_ANNUALCROPS
                                                    ),
                                                    selected = INDICATOR_CULTIVATEDLAND
                                       )
                                     )            
                                   ),
                                   mainPanel(
                                     fluidRow(h4(strong(textOutput("header_food")))),
                                     fluidRow(withSpinner(leafletOutput("map_food"))),
                                     fluidRow(withSpinner(plotlyOutput("barplot_food")))
                                   )
                          )
               ),
               h3(class = "texto_agradecimiento_logos", strong("Este proyecto es posible gracias a")),
               fluidRow(h1(column(width = 12))),
               fluidRow(
                 column(width = 4, img(src = "logo-gcr20222026.png", height = 90)),
                 column(width = 4, img(src = "logo-minae.png", height = 90)),
                 column(width = 4, img(src = "logo-sinac.jpg", height = 90)),
                 class = "text-center"
               ),
               fluidRow(h1(column(width = 12))),
               fluidRow(
                 column(width = 4, img(src = "logo-catie.jpeg", height = 90)),
                 column(width = 4, img(src = "logo-giz.png", height = 90)),
                 column(width = 4, img(src = "logo-minambientealemania-iki.png", height = 90)),
                 class = "text-center"
               ),
               fluidRow(h1(column(width = 12))),
               fluidRow(h1(column(width = 12)))
    )
  )


# SERVER LOGIC
server <- function(input, output) {
  # Recreation indicators header
  output$header_recreation <- renderText(
    if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
      INDICATOR_GREENAREA
    } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
      INDICATOR_TRAILSLONGITUDE
    } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
      INDICATOR_OPENSPACES
    }
  )
  
  # Food indicators header
  output$header_food <- renderText(
    if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
      INDICATOR_CULTIVATEDLAND
    } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
      INDICATOR_CULTIVATEDPASTURES
    } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
      INDICATOR_SHADECOFFEE
    } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
      INDICATOR_PERMANENTCROPS
    } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
      INDICATOR_ANNUALCROPS
    }
  )  
  
  
  # Recreation indicators map
  output$map_recreation <- renderLeaflet({
    # cantons <- filter_cantons()
    
    # Column with value of indicator in dataframe
    indicator_column <-
      if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
        COLUMN_GREENAREA
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
        COLUMN_TRAILSLONGITUDE
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
        COLUMN_OPENSPACES
      }
    
    # Group in map
    indicator_group <-
      if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
        INDICATOR_GREENAREA
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
        INDICATOR_TRAILSLONGITUDE
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
        INDICATOR_OPENSPACES
      }
    
    # Title in map legend
    indicator_legend_title <-
      if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
        INDICATOR_GREENAREA
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
        INDICATOR_TRAILSLONGITUDE
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
        INDICATOR_OPENSPACES
      }
    
    # Labels in map legend
    indicator_legend_labels <-
      if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
        QUANTILES_GREENAREA_LABELS
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
        QUANTILES_TRAILSLONGITUDE_LABELS
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
        QUANTILES_OPENSPACES_LABELS
      }     
    
    # Units of measurement of the indicator (NOT IN USE)
    indicator_unit <-
      if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
        "m2"
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
        "km"
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
        ""
      }
    
    # Fill color of the map
    indicator_fillColor <-
      if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
        ~ PALETTE_GREENAREA(supverd.ha)
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
        ~ PALETTE_TRAILSLONGITUDE(km_sendero)
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
        ~ PALETTE_OPENSPACES(numeros_es)
      }
    
    # Color palette
    indicator_palette <-
      if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
        PALETTE_GREENAREA
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
        PALETTE_TRAILSLONGITUDE
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
        PALETTE_OPENSPACES
      }
    
    # Map
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") |>
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") |>
      addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") |>   
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") |>
      addPolygons(data = cantons,
                  fillOpacity = ifelse(is.na(indicator_column), 0, 0.7),
                  stroke = TRUE,
                  color = "Black",
                  fillColor = indicator_fillColor,
                  weight = 1,
                  popup = paste(
                    paste("<strong>Cantón:</strong>",  cantons[[COLUMN_CANTON_NAME_2]]),
                    paste(
                      paste0("<strong>", indicator_group, ":</strong>"), 
                      indicator_column
                    ),
                    sep = '<br/>'
                  ),
                  label = paste(
                    paste("Cantón:",  cantons[[COLUMN_CANTON_NAME_2]]),
                    paste(paste0(indicator_group, ":"), indicator_column),
                    sep = ' - '
                  ),
                  group = indicator_group
      ) |>
      addLegend(
        position = "bottomright",
        pal = indicator_palette,
        values = indicator_column,
        labFormat = function(type, cuts, p) {
          paste0(indicator_legend_labels)
        },
        group = indicator_group,
        title = indicator_legend_title
      ) |>
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap",
          "CartoDB Dark Matter",
          "Stamen Toner Lite",
          "ESRI World Imagery"
        ),
        overlayGroups = c(indicator_group),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(imperial = FALSE)
      ) |>
      addMouseCoordinates() |>
      addSearchOSM() |>
      addResetMapButton() |>
      addFullscreenControl()
  })
  
  # Food indicators map
  output$map_food <- renderLeaflet({
    # cantons <- filter_cantons()
    
    # Column with value of indicator in dataframe
    indicator_column <-
      if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
        COLUMN_CULTIVATEDLAND
      } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
        COLUMN_CULTIVATEDPASTURES
      } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
        COLUMN_SHADECOFFEE
      } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
        COLUMN_PERMANENTCROPS
      } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
        COLUMN_ANNUALCROPS
      }
    
    # Group in map
    indicator_group <-
      if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
        INDICATOR_CULTIVATEDLAND
      } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
        INDICATOR_CULTIVATEDPASTURES
      } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
        INDICATOR_SHADECOFFEE
      } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
        INDICATOR_PERMANENTCROPS
      } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
        INDICATOR_ANNUALCROPS
      }
    
    # Title in map legend
    indicator_legend_title <-
      if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
        INDICATOR_CULTIVATEDLAND
      } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
        INDICATOR_CULTIVATEDPASTURES
      } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
        INDICATOR_SHADECOFFEE
      } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
        INDICATOR_PERMANENTCROPS
      } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
        INDICATOR_ANNUALCROPS
      }
    
    # Labels in map legend
    indicator_legend_labels <-
      if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
        QUANTILES_CULTIVATEDLAND_LABELS
      } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
        QUANTILES_CULTIVATEDPASTURES_LABELS
      } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
        QUANTILES_SHADECOFFEE_LABELS
      } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
        QUANTILES_PERMANENTCROPS_LABELS
      } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
        QUANTILES_ANNUALCROPS_LABELS
      }
    
    # Units of measurement of the indicator (NOT IN USE)
    indicator_unit <-
      if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
        "ha"
      } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
        "ha"
      } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
        "ha"
      } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
        "ha"
      } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
        "ha"
      }
    
    # Fill color of the map
    indicator_fillColor <-
      if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
        ~ PALETTE_CULTIVATEDLAND(tierrasc_h)
      } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
        ~ PALETTE_CULTIVATEDPASTURES(pastos_ha)
      } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
        ~ PALETTE_SHADECOFFEE(cafe_ha)
      } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
        ~ PALETTE_PERMANENTCROPS(perenne_ha)
      } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
        ~ PALETTE_ANNUALCROPS(anuales_ha)
      }
    
    # Color palette
    indicator_palette <-
      if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
        PALETTE_CULTIVATEDLAND
      } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
        PALETTE_CULTIVATEDPASTURES
      } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
        PALETTE_SHADECOFFEE
      } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
        PALETTE_PERMANENTCROPS
      } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
        PALETTE_ANNUALCROPS
      }
    
    # Map
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap") |>
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB Dark Matter") |>
      addProviderTiles(providers$Stamen.TonerLite, group = "Stamen Toner Lite") |>   
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") |>
      addPolygons(data = cantons,
                  fillOpacity = ifelse(is.na(indicator_column), 0, 0.7),
                  stroke = TRUE,
                  color = "Black",
                  fillColor = indicator_fillColor,
                  weight = 1,
                  popup = paste(
                    paste("<strong>Cantón:</strong>",  {{COLUMN_CANTON_NAME_2}}),
                    paste(
                      paste0("<strong>", indicator_group, ":</strong>"), 
                      indicator_column
                    ),
                    sep = '<br/>'
                  ),
                  label = paste(
                    paste("Cantón:",  {{COLUMN_CANTON_NAME_2}}),
                    paste(paste0(indicator_group, ":"), indicator_column),
                    sep = ' - '
                  ),
                  group = indicator_group
      ) |>
      addLegend(
        position = "bottomright",
        pal = indicator_palette,
        values = indicator_column,
        labFormat = function(type, cuts, p) {
          paste0(indicator_legend_labels)
        },
        group = indicator_group,
        title = indicator_legend_title
      ) |>
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap",
          "CartoDB Dark Matter",
          "Stamen Toner Lite",
          "ESRI World Imagery"
        ),
        overlayGroups = c(indicator_group),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions(imperial = FALSE)
      ) |>
      addMouseCoordinates() |>
      addSearchOSM() |>
      addResetMapButton() |>
      addFullscreenControl()
  })
  
  
  # Recreation indicator bar plot
  output$barplot_recreation <- renderPlotly({
    # cantons <- filter_cantons()
    
    # Column with value of indicator in dataframe
    indicator_column <-
      if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
        COLUMN_GREENAREA
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
        COLUMN_TRAILSLONGITUDE
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
        COLUMN_OPENSPACES
      }
    
    # Label for bars
    indicator_geom_col_label <-
      if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
        INDICATOR_GREENAREA
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
        INDICATOR_TRAILSLONGITUDE
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
        INDICATOR_OPENSPACES
      }
    
    # Y axis label
    indicator_y_axis_label <-
      if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
        INDICATOR_GREENAREA
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
        INDICATOR_TRAILSLONGITUDE
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
        INDICATOR_OPENSPACES
      }
    
    # Fill color of bars
    indicator_geom_col_fill <-
      if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
        PALETTE_GREENAREA_END_COLOR
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
        PALETTE_TRAILSLONGITUDE_END_COLOR
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
        PALETTE_OPENSPACES_END_COLOR
      }  
    
    # Units of measurement of the indicator (NOT IN USE)
    indicator_unit <-
      if (input$radiobuttons_indicators_recreation == INDICATOR_GREENAREA) {
        "m2"
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_TRAILSLONGITUDE) {
        "km"
      } else if (input$radiobuttons_indicators_recreation == INDICATOR_OPENSPACES) {
        ""
      }
    
    # Ggplot2 plot
    barplot_recreation_ggplot2 <-
      cantons |>
      ggplot(
        aes(x = reorder(!!sym(COLUMN_CANTON_NAME_2),-indicator_column), y = indicator_column)
      ) +
      geom_col(
        aes(
          text = paste0(
            "Cantón: ", !!sym(COLUMN_CANTON_NAME_2), "\n",
            indicator_geom_col_label, ": ", indicator_column
          )
        ),
        fill = indicator_geom_col_fill
      ) +
      xlab("Cantón") +
      ylab(indicator_y_axis_label) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1),
        legend.position = "none"
      )
    
    # Plotly plot
    barplot_recreation_ggplot2 |>
      ggplotly(tooltip = "text") |>
      config(locale = 'es')
  })
  
  # Food indicator bar plot
  output$barplot_food <- renderPlotly({
    # cantons <- filter_cantons()
    
    # Column with value of indicator in dataframe
    indicator_column <-
      if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
        COLUMN_CULTIVATEDLAND
      } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
        COLUMN_CULTIVATEDPASTURES
      } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
        COLUMN_SHADECOFFEE
      } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
        COLUMN_PERMANENTCROPS
      } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
        COLUMN_ANNUALCROPS
      }
    
    # Label for bars
    indicator_geom_col_label <-
      if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
        INDICATOR_CULTIVATEDLAND
      } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
        INDICATOR_CULTIVATEDPASTURES
      } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
        INDICATOR_SHADECOFFEE
      } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
        INDICATOR_PERMANENTCROPS
      } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
        INDICATOR_ANNUALCROPS
      }
    
    # Y axis label
    indicator_y_axis_label <-
      if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
        INDICATOR_CULTIVATEDLAND
      } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
        INDICATOR_CULTIVATEDPASTURES
      } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
        INDICATOR_SHADECOFFEE
      } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
        INDICATOR_PERMANENTCROPS
      } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
        INDICATOR_ANNUALCROPS
      }
    
    # Fill color of bars
    indicator_geom_col_fill <-
      if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
        PALETTE_CULTIVATEDLAND_END_COLOR
      } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
        PALETTE_CULTIVATEDPASTURES_END_COLOR
      } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
        PALETTE_SHADECOFFEE_END_COLOR
      } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
        PALETTE_PERMANENTCROPS_END_COLOR
      } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
        PALETTE_ANNUALCROPS_END_COLOR
      }
    
    # Units of measurement of the indicator (NOT IN USE)
    indicator_unit <-
      if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDLAND) {
        "ha"
      } else if (input$radiobuttons_indicators_food == INDICATOR_CULTIVATEDPASTURES) {
        "ha"
      } else if (input$radiobuttons_indicators_food == INDICATOR_SHADECOFFEE) {
        "ha"
      } else if (input$radiobuttons_indicators_food == INDICATOR_PERMANENTCROPS) {
        "ha"
      } else if (input$radiobuttons_indicators_food == INDICATOR_ANNUALCROPS) {
        "ha"
      }
    
    # Ggplot2 plot
    barplot_recreation_ggplot2 <-
      cantons |>
      ggplot(
        aes(x = reorder(COLUMN_CANTON_NAME,-indicator_column), y = indicator_column)
      ) +
      geom_col(
        aes(
          text = paste0(
            "Cantón: ", COLUMN_CANTON_NAME, "\n", 
            indicator_geom_col_label, ": ", indicator_column
          )
        ),
        fill = indicator_geom_col_fill
      ) +
      xlab("Cantón") +
      ylab(indicator_y_axis_label) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1),
        legend.position = "none"
      )
    
    # Plotly plot
    barplot_recreation_ggplot2 |>
      ggplotly(tooltip = "text") |>
      config(locale = 'es')
  })     
}


# RUN APPLICATION
shinyApp(ui = ui, server = server)