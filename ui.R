# TODO: cambiar a español botones y mensajes (con un fork maybe?)

# Header ------------------------------------------------------------------

header <- titlePanel(
  title = div(img(src = "logoLQL.png", height = '75px'),
              "Ejemplo de aplicación web")
)

# Body --------------------------------------------------------------------

body <- mainPanel(
  # column(
  width = 12,
  # login_ui(id = "module_login", title = "Inicio de sesión"),
  
  uiOutput(outputId = "contenido")
  # )
)

# Union -------------------------------------------------------------------

ui <- fluidPage(
  
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

# sign_in_ui_default(color = 'white')
secure_ui(
  ui = ui, 
  sign_in_page_ui = sign_in_ui_default(
    sign_in_module = sign_in_module_ui('sign_in', register_link = NULL),
    color = 'white',
    company_name = 'Las Quince Letras',
    logo_top = tags$img(src = "logoLQL.png",
                        style = "height: 75px; margin-top: 30px; margin-bottom: 30px;"),
    icon_href = 'imagotipo_claret.png',
    button_color = '#172744'
  )
)




