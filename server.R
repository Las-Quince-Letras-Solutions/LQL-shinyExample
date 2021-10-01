

# shinyServer(function(input, output, session) {
server <- function(input, output, session) {
  output$prueba <- renderText({
    paste0("You are viewing \"", input$tabs, "\"")
  })
  
  # Pedo del login
  user_base_module_tbl <- tibble(user_name = "user",
                                 password  = "pw")
  
  validate_password_module <- callModule(
    module   = validate_pwd,
    id       = "module_login",
    data     = user_base_module_tbl,
    user_col = user_name,
    pwd_col  = password
  )
  
  
  # Contenido con contraseña ------------------------------------------------
  
  output$contenido <- renderUI({
    # req(validate_password_module())
    
    div(
      hr(),
      h4(icon('chevron-right'), 'Descripción de esta WebApp'),
      p(
        'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
         tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
         quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
         Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore
         eu fugiat nulla pariatur:'
      ),
      p(
        HTML(
          '<ul>
            <li>excepteur sint occaecat cupidatat non proident</li>
            <li>sunt in culpa qui officia deserunt mollit anim id est laborum</li>
          </ul>'
        )
      ),
      hr(),
      
      fluidRow(column(width = 3,
                      uiOutput('sidePanel')),
               column(
                 width = 9,
                 tabsetPanel(
                   type = "tabs",
                   id = 'tabs',
                   tabPanel(
                     title = "Gráficas",
                     value = 'Tab 1',
                     column(width = 9,
                            br(),
                            br(),
                            plotOutput("plot"))
                   ),
                   
                   tabPanel(
                     title = "Modelo",
                     value = 'Tab 2',
                     column(width = 9,
                            br(),
                            tableOutput("table"),
                            br(),
                            uiOutput('clip')),
                   ),
                   
                   tabPanel(
                     title = "Misc.",
                     value = 'Tab 3',
                     column(
                       width = 9,
                       br(),
                       p('Otras cosas que considero que vale la pena ver:'),
                       box(width = 12,
                           sliderInput('slider',
                                       h6('Elegir un número:'),
                                       min = 1,
                                       max = 100,
                                       step = 5,
                                       value = 50)),
                       br(),
                       valueBoxOutput("boxVers"),
                       valueBoxOutput("boxTasks"),
                       valueBoxOutput("boxMuestra"),
                     ),
                   )
                 )
               )),
      fluidRow(br())
      
      
    )
    
  })
  
  
  output$sidePanel <- renderUI({
    if (input$tabs == 'Tab 1') {
      div(
        width = 12,
        
        br(),
        h4(icon('chevron-right'), 'Parámetros'),
        box(
          width = 12,
          
          selectInput(
            "region",
            h6('Región'),
            choices = regiones,
            multiple = TRUE,
            selected = 'Bajío'
          ),
          
          radioButtons(
            'variable',
            h6('Variable'),
            choiceValues = c('ventas', 'dist', 'precio'),
            choiceNames = c('Ventas unitarias', 'Distribución', 'Precio unitario')
          ),
          
          h6('Datos'),
          switchInput(
            "mensuales",
            value = FALSE,
            onLabel = "Mensuales",
            offLabel = "Semanales"
          ),
          radioButtons(
            'mediaMovil',
            h6('¿Suavizar gráfica?'),
            choices = c('Sí', 'No'),
            inline = TRUE
          ),
          br(),
          actionButton('go', 'Actualizar')
        )
      )
    } else if (input$tabs == 'Tab 2') {
      div(
        br(),
        h4(icon('chevron-right'), 'Parámetros'),
        box(
          width = 12,
          
          radioButtons(
            'varY',
            h6('Variable a explicar'),
            choiceValues = c('ventas', 'dist', 'precio'),
            choiceNames = c('Ventas unitarias', 'Distribución', 'Precio unitario')
          ),
          
          h6('Datos'),
          switchInput(
            "mensualesMod",
            value = FALSE,
            onLabel = "Mensuales",
            offLabel = "Semanales"
          ),
          br(),
          actionButton('go2', 'Actualizar')
        ),
      )
    } else {
      div(br(),
          h4(icon('chevron-right'), 'Parámetros'),
          p('No aplica'))
    }
    
  })
  
  # Pestaña 1 ---------------------------------------------------------------
  
  datos <- eventReactive(input$go, {
    data <- df
    
    if (input$mensuales) {
      data <- data %>%
        mutate(fecha = lubridate::floor_date(fecha, 'month')) %>%
        group_by(fecha, region) %>%
        summarise(
          ventas = sum(ventas),
          dist = max(dist),
          precio = mean(precio)
        ) %>%
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
    
    if (input$mediaMovil == 'No') {
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
        geom_line(aes(fecha, !!var, color = region),
                  size = 0.25,
                  linetype = 1) +
        geom_smooth(
          aes(fecha, !!var, color = region),
          size = 0.75,
          linetype = 1,
          se = FALSE,
          method = 'gam'
        ) +
        scale_y_LQL('comma', 1) +
        scale_color_manual(values = c('#172744', '#DF2A35', '#666665', '#12A04B')) +
        labsLQL(title = varName,
                y = input$variable) +
        themeLQL() +
        theme(axis.title.x = element_blank(),
              legend.title = element_blank())
    }
    
  })
  
  
  output$plot <- renderPlot({
    plot1()
  })
  
  
  # Pestaña 2 ---------------------------------------------------------------
  
  datosMod <- eventReactive(input$go2, {
    data <- df
    
    if (input$mensualesMod) {
      data <- data %>%
        mutate(fecha = lubridate::floor_date(fecha, 'month')) %>%
        group_by(fecha, region) %>%
        summarise(
          ventas = sum(ventas),
          dist = max(dist),
          precio = mean(precio)
        ) %>%
        ungroup()
    }
    
    data %>%
      mutate_at(c('ventas', 'dist', 'precio'), scale)
  })
  
  modelo <- eventReactive(input$go2, {
    a <- dummyVars( ~ ., data = datosMod(), fullRank = FALSE)
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
    if (input$go2 > 0) {
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
  
  
  # Pestaña 3 ---------------------------------------------------------------
  
  output$boxVers <- renderValueBox({
    valueBox(round(rnorm(1, 20 + input$slider, sqrt(input$slider))), "Versiones del cuestionario",
             color = "purple")
  })
  
  output$boxTasks <- renderValueBox({
    valueBox(round(rnorm(1, 20 + input$slider, sqrt(input$slider/2))), "Número de tareas",
             color = "blue")
  })
  
  output$boxMuestra <- renderValueBox({
    valueBox(round(rnorm(1, 120 + input$slider, sqrt(2*input$slider))), "Personas en la muestra",
             color = "yellow")
  })
  
}

secure_server(server = server)