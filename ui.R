
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


  
  header <- dashboardHeader(title = "Crimen en CDMX")
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Análisis espacial", tabName = "Espacial", icon = icon("map-marker")),
      menuItem("Análisis temporal", tabName = "Temporal", icon = icon("calendar")),
      menuItem("Análisis por delito", tabName = "Delito", icon = icon("bars")),
      menuItem("General", tabName = "Cruce", icon = icon("area-chart"))
    )
  )
  
  body <- dashboardBody(
    
    
    tabItems(
      
    
    ## First tab
    tabItem(tabName = "Espacial",
            br(),
            fluidRow(
              box(title = "Análisis espacial",
                  status = "primary",
                  solidHeader = F,
                  width = 12,
                  h5("Datos de crímenes en la Ciudad de México de enero 2013 a septiembre 2016."),
                  h5("El mapa muestra el total de delitos cometidos por sector de la ciudad."),
                  h5("(Mientras más oscuro, mayor el total de delitos)"),
                  br(),
                  actionButton("go_esp", h5("Actualizar", icon("refresh"))),
                  br(),
                  br(),
                  selectInput("esp_crime", label = "Seleccionar tipo de delito", 
                              choices = tipo_crimen, 
                              selected = tipo_crimen[1]),
                  br(),
                  leafletOutput("mapa_leaf")
              )
            )
            # fluidRow(
            #   br(),
            #   box(title = "Filtros",
            #       status = "warning",
            #       solidHeader = F,
            #       actionButton("go_esp", h5("Actualizar", icon("refresh"))),
            #       selectInput("esp_crime", label = "Tipo de delito", 
            #                   choices = tipo_crimen, 
            #                   selected = tipo_crimen[1]))
            ),
    
    ## Second tab
    tabItem(tabName = "Temporal",
            fluidRow(
              br(),
              box(title = "Análisis temporal",
                  width = 12,
                  status = "primary",
                  solidHeader = F,

                  column(width = 3,
                         actionButton("go_temp", h5("Actualizar", icon("refresh"))),
                         br(),
                         br(),
                         selectInput("select_crime", label = "Seleccionar tipo de delito", 
                                     choices = tipo_crimen, 
                                     selected = tipo_crimen[1]),
                         checkboxGroupInput("checkGroup0",
                                            label = "Seleccionar municipios",
                                            choices = muns,
                                            selected = muns[1])),
                  column(width = 9,
                         br(),
                         h5("Frecuencia de delitos por municipio"),
                         br(),
                         plotOutput("graf_meses"),
                         br(),
                         h5("Frecuencia total de delitos en la ciudad"),
                         plotOutput("graf_tot_meses")))
            )),
    
    ## Third tab
    tabItem(tabName = "Delito",
            column(width = 5,
              br(),
              box(title = "Análisis por tipo de delito",
                  status = "primary", solidHeader = F,
                  width = 12,
                  # height = 600,
                  actionButton("go_tipo", h5("Actualizar", icon("refresh"))),
                  br(),
                  br(),
                  checkboxGroupInput("checkGroup",
                                     label = "Seleccionar tipo de delito",
                                     choices = tipo_crimen,
                                     selected = tipo_crimen[1]),
                  h4("Frecuencia total por tipo de delito"),
                  plotlyOutput("graf_frec_tipo"))),
            
            column(width = 7,
              br(),
              box(title = NULL,
                  status = "primary",
                  solidHeader = F,
                  width = 12,
                  h4("Delitos por hora"),
                  h5("Porcentaje por hora del día"),
                  plotOutput("graf_horas"),
                  h4("Delitos por día de la semana"),
                  h5("Porcentaje por día de la semana"),
                  plotOutput("graf_dias"))
            )
            ),
    
    ## Fourth tab
    tabItem(tabName = "Cruce",
            br(),
            fluidRow(
              br(),
              box(title = "Análisis general",
                  status = "primary",
                  solidHeader = F,
                  width = 4,
                  h5("Serie de tiempo de la frecuencia de delitos durante el año."),
                  h5("El mapa muestra el total anual de delitos cometidos por sector de la ciudad."),
                  h5("(Mientras más oscuro, mayor el total de delitos)"),
                  br(),
                  actionButton("go_cruce", h5("Actualizar", icon("refresh"))),
                  br(),
                  br(),
                  selectInput("cruce_crime", label = "Seleccionar tipo de delito", 
                              choices = tipo_crimen, 
                              selected = tipo_crimen[1]),
                  selectInput("cruce_year", label = "Seleccionar año", 
                              choices = year, 
                              selected = year[1])
                  ),
              box(title = "Serie anual",
                  status = "primary",
                  solidHeader = F,
                  width = 8,
                  plotOutput("graf_cruce_mes"))
              
            ),
            fluidRow(
              br(),
              box(width = 12,
                leafletOutput("mapa_cruce_leaf")
              )
            ))
    
    
    )
    
  )
  
  dashboardPage(header, sidebar, body, skin = "green")
  



