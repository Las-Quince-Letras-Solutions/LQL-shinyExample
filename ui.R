
# Header ------------------------------------------------------------------

header <- titlePanel(
  title = div(img(src = "logoLQL.png", height = '75px'),
              "Ejemplo de aplicación web")
)

# Body --------------------------------------------------------------------

body <- mainPanel(
  column(
    width = 12,
    login_ui(id = "module_login", title = "Please login"),
    
    uiOutput(outputId = "contenido")
  )
)

# Union -------------------------------------------------------------------

fluidPage(
  tags$style(boxCSS),
  tags$style(valueBoxCSS),
  tags$style(boxBodyCSS),
  
  setBackgroundColor("white"),
  useShinydashboard(),
  useShinyjs(),  
  
  # Título del tab en el explorador
  tags$head(HTML("<title>Diseño de bloques incompletos</title> <link rel='icon' type='image/gif/png' href='imagotipo_claret.png'>"))  ,
  theme = shinytheme("yeti"),
  header,
  body
)
