
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


shinyServer(function(input, output) {
  

    
  

# Outputs espacial -----------------------------------------------
  
  puntos_mapa <- eventReactive(input$go_esp, {
    
    sectores_tmp <- sectores_json
    
    dat_mapita <- cuenta_sector %>%
      filter_(paste("crime == '", input$esp_crime, "'", sep = '')) %>%
      left_join(coordenadas_labels, by = c("sector" = "id"))
    
    sectores_tmp@data <- sectores_tmp@data %>% 
      left_join(dat_mapita)
    
    
    
    return(sectores_tmp)
    
  })
  
  output$mapa_leaf <- renderLeaflet({
    
    labels <- sprintf(
      "<strong>%s</strong><br/> Municipio: %s<br/>Total delitos: %g <br/>Población: %g",
      puntos_mapa()@data$sector, puntos_mapa()@data$municipio, 
      puntos_mapa()@data$total, puntos_mapa()@data$population
    ) %>% lapply(htmltools::HTML)
    
    pal <-colorNumeric(
      palette = "Reds",
      domain = unique(puntos_mapa()@data$total))
    
    mapita <- leaflet() %>%
      addTiles() %>%
      setView(lng = long_media, lat = lat_media, zoom = 10) %>%
      addPolygons(data = puntos_mapa(), 
                  color = "#444444",
                  weight = 1, 
                  smoothFactor = 0.2,
                  opacity = 1.0, 
                  fillOpacity = 0.7,
                  fillColor = ~colorNumeric("YlOrRd", total)(total),
                  
                  highlightOptions = highlightOptions(color = "black", weight = 2,bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"),
                  group="polygon"
                 ) 
    
    mapita
  })


# Outputs temporal --------------------------------------------------------

  tabla_meses <- eventReactive(input$go_temp, {
    tab_meses <- res_cuadrantes %>%
      filter_(paste("crime == '", input$select_crime, "'", sep = '')) %>%
      filter_(paste("municipio %in% c('", paste(input$checkGroup0, collapse = "','"), "')", sep = ''))
  
  })
  
  
  output$graf_meses <- renderPlot({
    tabla_meses() %>%
      ggplot(aes(x = date, y = count, colour = municipio)) +
      geom_point(size = 2) +
      geom_line(size = 1) +
      theme_bw() +
      xlab("fecha") +
      ylab("número de delitos") +
      scale_colour_discrete(name = "Municipio") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
            axis.title = element_text(size = 14),
            axis.text.y = element_text(size = 14))
  })
  
  output$graf_tot_meses <- renderPlot({
    res_cuadrantes_tot %>% 
      ggplot(aes(x = date, y = count)) +
      geom_line(size = 1, colour = "forestgreen") +
      geom_point(size = 2, colour = "forestgreen") +
      theme_bw() +
      xlab("fecha") +
      ylab("número de delitos") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
            axis.title = element_text(size = 14),
            axis.text.y = element_text(size = 14))
  })

# Outputs tipo de delito --------------------------------------


  tabla_horas <- eventReactive(input$go_tipo, {
    tab_horas <- crimenes %>%
      filter_(paste("crime %in% c('", paste(input$checkGroup, collapse = "','"), "')", sep = '')) %>%
      group_by(crime, hora) %>%
      tally %>%
      ungroup() %>% 
      filter(!is.na(hora)) %>%
      group_by(crime) %>%
      mutate(prop = 100*n/sum(n)) %>%
      ungroup()
  })
  
  
  output$graf_horas <- renderPlot({
    tabla_horas() %>%
      ggplot(aes(x = as.factor(hora), y = prop, group = as.factor(crime),
                 colour = as.factor(crime))) +
      coord_polar() +
      geom_point(size = 2) +
      geom_polygon(fill = NA, size = 1) +
      theme_bw() +
      xlab("Hora") +
      ylab("% de delitos") +
      theme(legend.position = "none",
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12)) 
      
    
  })
  
  tabla_dias <- eventReactive(input$go_tipo, {

    tab_dias <- crimenes %>%
      filter_(paste("crime %in% c('", paste(input$checkGroup, collapse = "','"), "')", sep = '')) %>%
      group_by(crime, dia_semana) %>%
      tally %>%
      ungroup() %>% 
      filter(!is.na(dia_semana)) %>%
      group_by(crime) %>%
      mutate(prop = 100*n/sum(n)) %>%
      ungroup()
  })
  
  
  output$graf_dias <- renderPlot({
    tabla_dias() %>%
      mutate(dia_semana = factor(dia_semana, levels = c("Lunes", "Martes", "Miércoles",
                                                        "Jueves", "Viernes", "Sábado",
                                                        "Domingo"))) %>% 
      ggplot(aes(x = dia_semana, y = prop, group = as.factor(crime),
                 colour = as.factor(crime))) +
      geom_point(size = 2) +
      geom_line(size = 1) +
      theme_bw() +
      xlab("Día de la semana") +
      ylab("% de delitos") +
      scale_colour_discrete(name = "Tipo de delito") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
            axis.title = element_text(size = 14),
            axis.text.y = element_text(size = 14)) +
      ylim(0, max(tabla_dias()$prop) + 2)

  })
  
  tabla_frec_tipo <- reactive({
    tab_frec <- cuadrantes %>%
      group_by(crime) %>%
      summarise(total = sum(count)) %>%
      ungroup() %>%
      arrange(total) %>%
      mutate(crimen = factor(crime, levels = unique(crime))) %>%
      dplyr::select(-crime)
    
  })
  
  output$graf_frec_tipo <- renderPlotly({
    g1 <- tabla_frec_tipo() %>%
      ggplot(aes(x = crimen, y = total, fill = crimen)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5) )
      
    ggplotly(g1)
      
  })
  
  
  output$tabla <- renderTable({
    
    head(tabla_meses())

  })
  

# Output análisis general -------------------------------------------------

  tabla_cruce_mes <- eventReactive(input$go_cruce,{
    
    tab_cruce_mes <- cuenta_mes %>%
      filter_(paste("crime == '", input$cruce_crime, "'", sep = '')) %>%
      filter_(paste("year == '", input$cruce_year, "'", sep = ''))
      
    
  })
  
  output$graf_cruce_mes <- renderPlot({
    
    tabla_cruce_mes() %>% 
      ggplot(aes(x = date, y = total)) +
      geom_line(size = 1, colour = "forestgreen") +
      geom_point(size = 2, colour = "forestgreen") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
            axis.title = element_text(size = 14),
            axis.text.y = element_text(size = 12)) 
    
  })
  
  puntos_mapa_cruce <- eventReactive(input$go_cruce, {
    
    sectores_tmp <- sectores_json
    
    dat_mapita <- cuenta_sector_year %>%
      filter_(paste("crime == '", input$cruce_crime, "'", sep = '')) %>%
      filter_(paste("year == '", input$cruce_year, "'", sep = '')) %>% 
      left_join(coordenadas_labels, by = c("sector" = "id"))
    
    sectores_tmp@data <- sectores_tmp@data %>% 
      left_join(dat_mapita)
    
    
    
    return(sectores_tmp)
    
  })
  
  output$mapa_cruce_leaf <- renderLeaflet({
    
    labels <- sprintf(
      "<strong>%s</strong><br/> Municipio: %s<br/>Total delitos: %g <br/>Población: %g",
      puntos_mapa_cruce()@data$sector, puntos_mapa_cruce()@data$municipio, 
      puntos_mapa_cruce()@data$total, puntos_mapa_cruce()@data$population
    ) %>% lapply(htmltools::HTML)
    
    pal <-colorNumeric(
      palette = "Reds",
      domain = unique(puntos_mapa_cruce()@data$total))
    
    mapita <- leaflet() %>%
      addTiles() %>%
      setView(lng = long_media, lat = lat_media, zoom = 10) %>%
      addPolygons(data = puntos_mapa_cruce(), 
                  color = "#444444",
                  weight = 1, 
                  smoothFactor = 0.2,
                  opacity = 1.0, 
                  fillOpacity = 0.7,
                  fillColor = ~colorNumeric("YlOrRd", total)(total),
                  
                  highlightOptions = highlightOptions(color = "black", weight = 2,bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"),
                  group="polygon"
      ) 
    
    mapita
  })

})
