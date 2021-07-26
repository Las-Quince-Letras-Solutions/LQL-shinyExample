
# Header ------------------------------------------------------------------

header <- titlePanel(
  title = div(img(src = "logoLQL.png", height = '75px'),
              "Ejemplo de aplicación web")
)

# Body --------------------------------------------------------------------

body <- mainPanel(
    width = 12,
  # column(
    # login_ui(id = "module_login", title = "Please login"),
    
    uiOutput(outputId = "contenido")
  # )
)

# Union -------------------------------------------------------------------

fluidPage(
  
  setBackgroundColor("white"),
  useShinydashboard(),
  useShinyjs(),  
  
  # Título del tab en el explorador
  tags$head(HTML("<title>Diseño de bloques incompletos</title> <link rel='icon' type='image/gif/png' href='imagotipo_claret.png'>"))  ,

  theme = shinytheme("yeti"),
  tags$link(rel = "stylesheet", type = "text/css", href = "estiloLQL.css"),
  header,
  body
)
