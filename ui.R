
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
  p('Esta aplicación optimiza el diseño de bloques incompletos a partir de los parámetros
    ingresados por el usuario. En caso de que éstos determinen un diseño no balanceado,
    el algoritmo los modificará para obtener'),
  p(HTML('<ul>
            <li>el número óptimo de tareas y/o versiones del cuestionario</li>
            <li>el mínimo tamaño de muestra requerido para significancia estadística en el análisis</li>
            <li>el diseño de bloques incompletos balanceados que atienda a los dos puntos anteriores</li>
          </ul>')),
  hr(),
  
  fluidRow(
    column(width = 3, align = 'left',
           h4(icon('chevron-right'), 'Elegir filtros'),
           box(width = 12,
               
               selectInput("region", h6('Región'),
                           choices = regiones, multiple = TRUE
               ),
               
               radioButtons('variable', h6('Variable'),
                            choiceValues = c('ventas', 'dist', 'precio'),
                            choiceNames = c('Ventas unitarias', 'Distribución', 'Precio unitario')
               ),
               
               h6('Datos'),
               switchInput("mensuales", 
                           value = FALSE,
                           onLabel = "Mensuales",
                           offLabel = "Semanales"
               ),
               
               br(),
               actionButton('go', 'Actualizar')
           ),
           
    ),
    column(width = 9,
           tabsetPanel(type = "tabs",
                       tabPanel("Gráficas", 
                                br(),
                                radioButtons('mediaMovil', h6('¿Graficar media móvil?'),
                                             choices = c('Sí', 'No'), inline = TRUE),
                                br(),
                                plotOutput("plot")
                       ),
                       
                       tabPanel("Modelo", 
                                br(),
                                tableOutput("table")
                       )
           )
           # h4(icon('chevron-right'), 'Diseño'),
           # valueBoxOutput("boxVers"),
           # valueBoxOutput("boxTasks"),
           # valueBoxOutput("boxMuestra"),
           # uiOutput('clip'),
           # fluidRow(verbatimTextOutput('prueba'))
    )
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
