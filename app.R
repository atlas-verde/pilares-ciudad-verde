#
# Ecosystem services dashboard
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


# CONSTANTS

# Indicators
INDICATOR_GREENAREA <- "Superficie verde por habitante"
INDICATOR_TRAILSLONGITUDE <- "Longitud de senderos para caminar per cápita"
INDICATOR_OPENSPACES <- "Número de espacios públicos para recreación"

# Data sources
DSN_CANTONS <- "data/metricas_ejemplo_tablero_080223.geojson"

# Classes
QUANTILES_GREENAREA_VALUES <- c(17, 99, 264, 563, 1393)
QUANTILES_GREENAREA_LABELS <- c("17 - 99", "100 - 264", "265 - 563", "564 - 1393")

QUANTILES_TRAILSLONGITUDE_VALUES <- c(0.00000, 0.04350, 0.12400, 0.30750, 2.08500)
QUANTILES_TRAILSLONGITUDE_LABELS <- c("0.00000 - 0.04350", "0.04351 - 0.12400", "0.12401 - 0.30750", "0.30751 - 2.08500")

QUANTILES_OPENSPACES_VALUES <- c(5, 12, 21, 38, 156)
QUANTILES_OPENSPACES_LABELS <- c("5 - 12", "13 - 21", "22 - 38", "39 - 156")

# Color palettes
PALETTE_GREENAREA_START_COLOR <- "#90ee90"
PALETTE_GREENAREA_END_COLOR <- "#013220"
PALETTE_GREENAREA <- colorBin(
  bins = QUANTILES_GREENAREA_VALUES, 
  palette = c(PALETTE_GREENAREA_START_COLOR, PALETTE_GREENAREA_END_COLOR),
  na.color = NA
)

PALETTE_TRAILSLONGITUDE_START_COLOR <- "#b5651d"
PALETTE_TRAILSLONGITUDE_END_COLOR <- "#654321"
PALETTE_TRAILSLONGITUDE <- colorBin(
  bins = QUANTILES_TRAILSLONGITUDE_VALUES, 
  palette = c(PALETTE_TRAILSLONGITUDE_START_COLOR, PALETTE_TRAILSLONGITUDE_END_COLOR),
  na.color = NA
)

PALETTE_OPENSPACES_START_COLOR <- "#add8e6"
PALETTE_OPENSPACES_END_COLOR <- "#00008b"
PALETTE_OPENSPACES <- colorBin(
  bins = QUANTILES_OPENSPACES_VALUES, 
  palette = c(PALETTE_OPENSPACES_START_COLOR, PALETTE_OPENSPACES_END_COLOR),
  na.color = NA
)


# DATASETS

# Cantons and their indicators of ecosystem services
cantons <- st_read(dsn = DSN_CANTONS, quiet = TRUE)


# USER INTERFACE
ui <-
  fluidPage(
    tags$head(
      tags$style(
        HTML(
          "/* Radio buttons size */
          #radiobuttons_indicator label {
            font-size: 18px;
          }"
        )
      )
    ),    
    navbarPage("Pilares Ciudad Verde", theme = shinytheme("lumen"),
      navbarMenu("Salud y bienestar", icon = icon("chart-bar"),
        tabPanel("Recreación", fluid = TRUE, icon = icon("globe-americas"),
          sidebarLayout(
            sidebarPanel(
              fluidRow(h1(strong("Pilar Ciudad Verde"), br(), "Salud y bienestar")),
              fluidRow(h2(strong("Dimensión"), br(), "Recreación")),
              fluidRow(h3(strong("Meta aspiracional"), br(), em("La ciudad brinda a sus habitantes espacios verdes públicos y de calidad, para la recreación y la salud mental."))),
              fluidRow(h3(strong("Servicio ecosistémico"), br(), "Cultural")),
              fluidRow(h3(strong("Indicadores"))),
              fluidRow(
                radioButtons("radiobuttons_indicator",
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
              fluidRow(h3(strong(textOutput("header_recreation")))),
              fluidRow(withSpinner(leafletOutput("map_recreation"))),
              fluidRow(withSpinner(plotlyOutput("barplot_recreation")))
            )
          )
        )
      )
    )
  )


# SERVER LOGIC
server <- function(input, output) {
  # Reactive function for data fitering
  filter_cantons <- reactive({
    filtered_cantons <- cantons
    
    filtered_cantons <-
      filtered_cantons |>
      filter(supverd.ha >= input$slider_greenarea[1] & supverd.ha <= input$slider_greenarea[2]) |>
      filter(km_sendero >= input$slider_trailslongitude[1] & km_sendero <= input$slider_trailslongitude[2])      
    
    return(filtered_cantons)
  })  
  
  # Header
  output$header_recreation <- renderText(
    if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
      INDICATOR_GREENAREA
    } else if (input$radiobuttons_indicator == INDICATOR_TRAILSLONGITUDE) {
      INDICATOR_TRAILSLONGITUDE
    } else {
      INDICATOR_OPENSPACES
    }   
  )
  
  # Recreation indicator map
  output$map_recreation <- renderLeaflet({
    # cantons <- filter_cantons()
    
    # Column with value of indicator in dataframe
    indicator_column <-
      if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
        cantons$supverd.ha
      } else if (input$radiobuttons_indicator == INDICATOR_TRAILSLONGITUDE) {
        cantons$km_sendero
      } else {
        cantons$numeros_es
      }
    
    # Group in map
    indicator_group <-
      if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
        INDICATOR_GREENAREA
      } else if (input$radiobuttons_indicator == INDICATOR_TRAILSLONGITUDE) {
        INDICATOR_TRAILSLONGITUDE
      } else {
        INDICATOR_OPENSPACES
      }
    
    # Title in map legend
    indicator_legend_title <-
      if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
        INDICATOR_GREENAREA
      } else if (input$radiobuttons_indicator == INDICATOR_TRAILSLONGITUDE) {
        INDICATOR_TRAILSLONGITUDE
      } else {
        INDICATOR_OPENSPACES
      }
    
    # Labels in map legend
    indicator_legend_labels <-
      if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
        QUANTILES_GREENAREA_LABELS
      } else if (input$radiobuttons_indicator == INDICATOR_TRAILSLONGITUDE) {
        QUANTILES_TRAILSLONGITUDE_LABELS
      } else {
        QUANTILES_OPENSPACES_LABELS
      }     
    
    # Units of measurement of the indicator (NOT IN USE)
    indicator_unit <-
      if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
        "m2"
      } else {
        "km"
      }
    
    # Fill color of the map
    indicator_fillColor <-
      if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
        ~ PALETTE_GREENAREA(supverd.ha)
      } else if (input$radiobuttons_indicator == INDICATOR_TRAILSLONGITUDE) {
        ~ PALETTE_TRAILSLONGITUDE(km_sendero)
      } else {
        ~ PALETTE_OPENSPACES(numeros_es)
      }
    
    # Color palette
    indicator_palette <-
      if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
        PALETTE_GREENAREA
      } else if (input$radiobuttons_indicator == INDICATOR_TRAILSLONGITUDE) {
        PALETTE_TRAILSLONGITUDE
      } else {
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
          paste("<strong>Cantón:</strong>",  cantons$canton),
          paste(
            paste0("<strong>", indicator_group, ":</strong>"), 
            indicator_column
          ),
          sep = '<br/>'
        ),
        label = paste(
          paste("Cantón:",  cantons$canton),
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
      if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
        cantons$supverd.ha
      } else if (input$radiobuttons_indicator == INDICATOR_TRAILSLONGITUDE) {
        cantons$km_sendero
      } else {
        cantons$numeros_es
      }
    
    # Label for bars
    indicator_geom_col_label <-
      if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
        INDICATOR_GREENAREA
      } else if (input$radiobuttons_indicator == INDICATOR_TRAILSLONGITUDE) {
        INDICATOR_TRAILSLONGITUDE
      } else {
        INDICATOR_OPENSPACES
      }
    
    # Y axis label
    indicator_y_axis_label <-
      if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
        INDICATOR_GREENAREA
      } else if (input$radiobuttons_indicator == INDICATOR_TRAILSLONGITUDE) {
        INDICATOR_TRAILSLONGITUDE
      } else {
        INDICATOR_OPENSPACES
      }
    
    # Fill color of bars
    indicator_geom_col_fill <-
      if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
        PALETTE_GREENAREA_END_COLOR
      } else if (input$radiobuttons_indicator == INDICATOR_TRAILSLONGITUDE) {
        PALETTE_TRAILSLONGITUDE_END_COLOR
      } else {
        PALETTE_OPENSPACES_END_COLOR
      }  
    
    # Units of measurement of the indicator (NOT IN USE)
    indicator_unit <-
      if (input$radiobuttons_indicator == INDICATOR_GREENAREA) {
        "m2"
      } else {
        "km"
      }    
    
    # Ggplot2 plot
    barplot_recreation_ggplot2 <-
      cantons |>
      ggplot(
        aes(x = reorder(canton,-indicator_column), y = indicator_column)
      ) +
      geom_col(
        aes(
          text = paste0(
            "Cantón: ", canton, "\n", 
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