
shinyServer(function(input, output, session) {
  
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
    
    if(input$mediaMovil == 'No') {
      
      datos() %>% 
        ggplot() +
        geom_line(aes(fecha, !!var, color = region), size = 0.75) +
        scale_y_LQL('dollar', 1) +
        scale_color_manual(values = c('#172744', '#DF2A35', '#666665', '#12A04B')) +
        labsLQL(title = 'Ventas por región',
                y = 'Ventas') +
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
                    size = 0.75, se = FALSE, method = 'gam') +
        scale_y_LQL('dollar', 1) +
        scale_color_manual(values = c('#172744', '#DF2A35', '#666665', '#12A04B')) +
        labsLQL(title = 'Ventas por región',
                y = 'Ventas') +
        themeLQL() +
        theme(axis.title.x = element_blank(),
              legend.title = element_blank())
    }
    
  })
    
  
  output$plot <- renderPlot({ plot1() })
  
  modelo <- eventReactive(input$go, {
    
    a <- dummyVars(~ ., data = datos(), fullRank = FALSE)
    b <- predict(a, datos()) %>% data.frame()
    
    mod.lm <- lm(ventas ~ ., data = select(b, -fecha))
    
    resumen <- data.frame(broom::tidy(mod.lm))
    
    resumen %>% 
      mutate(significancia = ifelse(p.value < 0.05, '*', '')) %>% 
      na.omit()
    
  })
  
  output$table <- renderTable({ 
    modelo()
  })
})
