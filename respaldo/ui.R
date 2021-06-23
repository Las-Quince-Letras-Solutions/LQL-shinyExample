div(
  id = "login-basic", 
  style = "width: 500px; max-width: 100%; margin: 0 auto;",
  
  div(
    class = "well",
    h4(class = "text-center", "Please login"),
    p(class = "text-center", 
      tags$small("First approach login form")
    ),
    
    textInput(
      inputId     = "ti_user_name_basic", 
      label       = tagList(icon("user"), 
                            "User Name"),
      placeholder = "Enter user name"
    ),
    
    passwordInput(
      inputId     = "ti_password_basic", 
      label       = tagList(icon("unlock-alt"), 
                            "Password"), 
      placeholder = "Enter password"
    ), 
    
    div(
      class = "text-center",
      actionButton(
        inputId = "ab_login_button_basic", 
        label = "Log in",
        class = "btn-primary"
      )
    )
  )
)
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
  # textOutput('prueba'),
  hr(),
  
  fluidRow(box(width = 12,
    tabsetPanel(type = "tabs", id = 'tabs',
                tabPanel(title = "Gráficas", value = 'Tab 1',
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
                
                tabPanel(title = "Modelo", value = 'Tab 2', 
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
  )),
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
