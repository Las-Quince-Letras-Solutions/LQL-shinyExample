
# Header ------------------------------------------------------------------

# header <- dashboardHeader(title = 'AHP por Las Quince Letras', titleWidth = 300)
header <- titlePanel(
  title = div(img(src = "logoLQL.png", height = '75px'),
              "Ejemplo de aplicación web")
)



# Sidebar -----------------------------------------------------------------

# sidebar <- dashboardSidebar(disable = T)

# Body --------------------------------------------------------------------

body <- mainPanel(
  width = 12,
  hr(),
  h4(icon('chevron-right'), 'Descripción de esta WebApp'),
  p('Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod 
    tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
    quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore 
    eu fugiat nulla pariatur:'),
  p(HTML('<ul>
            <li>excepteur sint occaecat cupidatat non proident</li>
            <li>sunt in culpa qui officia deserunt mollit anim id est laborum</li>
          </ul>')),
  hr(),
  
  fluidRow(
    tabsetPanel(type = "tabs",
                tabPanel("Gráficas", 
                         column(width = 3, align = 'left',
                                br(),
                                h4(icon('chevron-right'), 'Elegir filtros'),
                                box(width = 12,
                                    
                                    selectInput("region", h6('Región'),
                                                choices = regiones, multiple = TRUE,
                                                selected = 'Bajío'
                                    ),
                                    
                                    radioButtons('variable', h6('Variable'),
                                                 choiceValues = vars$short,
                                                 choiceNames = vars$full
                                    ),
                                    
                                    h6('Datos'),
                                    switchInput("mensuales", 
                                                value = FALSE,
                                                onLabel = "Mensuales",
                                                offLabel = "Semanales"
                                    ),
                                    radioButtons('mediaMovil', h6('¿Suavizar gráfica?'),
                                                 choices = c('Sí', 'No'), inline = TRUE),
                                    br(),
                                    actionButton('go', 'Actualizar')
                                ),
                                
                         ),
                         column(width = 9,
                                br(),
                                br(),
                                plotOutput("plot"))
                ),
                
                tabPanel("Modelo", 
                         column(width = 3, align = 'left',
                                br(),
                                h4(icon('chevron-right'), 'Elegir filtros'),
                                box(width = 12,
                                    
                                    radioButtons('varY', h6('Variable a explicar'),
                                                 choiceValues = vars$short,
                                                 choiceNames = vars$full
                                    ),
                                    
                                    h6('Datos'),
                                    switchInput("mensualesMod", 
                                                value = FALSE,
                                                onLabel = "Mensuales",
                                                offLabel = "Semanales"
                                    ),
                                    br(),
                                    actionButton('go2', 'Actualizar')
                                ),
                                
                         ),
                         column(width = 9,
                                br(),
                                tableOutput("table"),
                                br(),
                                uiOutput('clip'))
                )
    )
    # h4(icon('chevron-right'), 'Diseño'),
    # valueBoxOutput("boxVers"),
    # valueBoxOutput("boxTasks"),
    # valueBoxOutput("boxMuestra"),
    # uiOutput('clip'),
    # fluidRow(verbatimTextOutput('prueba'))
  ),
  fluidRow(br())
  
)

# Union -------------------------------------------------------------------

fluidPage(
  tags$style(boxCSS),
  tags$style(valueBoxCSS),
  tags$style(boxBodyCSS),
  
  setBackgroundColor("white"),
  useShinydashboard(),
  
  # Título del tab en el explorador
  tags$head(HTML("<title>Diseño de bloques incompletos</title> <link rel='icon' type='image/gif/png' href='imagotipo_claret.png'>"))  ,
  theme = shinytheme("yeti"),
  header,
  body
)
