
shinyServer(function(input, output, session) {
  
  output$prueba <- renderText({
    paste0("You are viewing \"", input$tabs, "\"")
  })
  
  # Pestaña 1 ---------------------------------------------------------------
  
  datos <- eventReactive(input$go, {
    
    data <- df
    
    if(input$mensuales) {
      data <- data %>% 
        mutate(fecha = lubridate::floor_date(fecha, 'month')) %>% 
        group_by(fecha, region) %>% 
        summarise(ventas = sum(ventas),
                  dist = max(dist),
                  precio = mean(precio)) %>% 
        ungroup()
    }
    
    data <- data %>% 
      filter(region %in% input$region) %>% 
      select(fecha, region, input$variable)
    
    data
  })
  
  
  plot1 <- eventReactive(input$go, {
    
    var <- parse_expr(input$variable)
    varName <- vars %>%
      filter(short == input$variable) %>%
      pull(full)
    
    if(input$mediaMovil == 'No') {
      
      datos() %>% 
        ggplot() +
        geom_line(aes(fecha, !!var, color = region), size = 0.75) +
        scale_y_LQL('comma', 1) +
        scale_color_manual(values = c('#172744', '#DF2A35', '#666665', '#12A04B')) +
        labsLQL(title = varName, 
                y = input$variable) +
        themeLQL() +
        theme(axis.title.x = element_blank(),
              legend.title = element_blank())
      
    } else {
      datos() %>% 
        ggplot() +
        geom_line(aes(fecha, !!var, color = region), size = 0.25, linetype = 1) +
        geom_smooth(aes(fecha, !!var, color = region), 
                    size = 0.75,
                    linetype = 1,
                    se = FALSE, method = 'gam') +
        scale_y_LQL('comma', 1) +
        scale_color_manual(values = c('#172744', '#DF2A35', '#666665', '#12A04B')) +
        labsLQL(title = varName, 
                y = input$variable) +
        themeLQL() +
        theme(axis.title.x = element_blank(),
              legend.title = element_blank())
    }
    
  })
  
  
  output$plot <- renderPlot({ plot1() })
  
  
  # Pestaña 2 ---------------------------------------------------------------
  
  datosMod <- eventReactive(input$go2, {
    
    data <- df
    
    if(input$mensualesMod) {
      data <- data %>% 
        mutate(fecha = lubridate::floor_date(fecha, 'month')) %>% 
        group_by(fecha, region) %>% 
        summarise(ventas = sum(ventas),
                  dist = max(dist),
                  precio = mean(precio)) %>% 
        ungroup()
    }
    
    data %>% 
      mutate_at(c('ventas', 'dist', 'precio'), scale)
  })  
  
  modelo <- eventReactive(input$go2, {
    
    a <- dummyVars(~ ., data = datosMod(), fullRank = FALSE)
    b <- predict(a, datosMod()) %>% data.frame()
    
    mod.lm <- lm(parse_expr(paste0(input$varY, '~ .')),
                 data = select(b, -fecha))
    
    resumen <- data.frame(broom::tidy(mod.lm))
    
    resumen %>% 
      mutate(significancia = ifelse(p.value < 0.05, '*', '')) %>% 
      na.omit() %>% 
      mutate(term = str_remove(term, 'region.'))
    
  })
  
  output$table <- renderTable({ 
    modelo()
  })
  
  output$clip <- renderUI({
    if(input$go2 > 0) {
      downloadButton('descargar', 'Descargar resultados')
    }
  })
  
  output$descargar <- downloadHandler(
    filename = function() {
      'Resultados del modelo.csv'
    },
    content = function(file) {
      write.csv(modelo(), file, row.names = FALSE)
    }
  )
  
})
