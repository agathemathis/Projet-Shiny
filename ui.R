

ui <- dashboardPage(
  dashboardHeader(title = "Romane & Agathe"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Lecture des données", tabName = "readData", icon = icon("readme")),
                menuItem("Visualisation des données", tabName = "visualization", icon = icon("poll"))
    )
  ),
  dashboardBody(
    
    tabItems(
      # Read data
      tabItem(tabName = "readData",
              h1("Lecture des données"),
              fileInput("dataFile",label = NULL,
                        buttonLabel = "Parcourir...",
                        placeholder = "Pas de fichier sélectionné"),
              
              fluidRow(
                column(3,
                       h3("Paramètres"),
                       
                       # Input: Checkbox if file has header
                       radioButtons(inputId = "header", 
                                    label = "Titres de colonnes",
                                    choices = c("Oui" = TRUE,
                                                "Non" = FALSE),
                                    selected = TRUE, inline=T),
                       
                       # Input: Select separator ----
                       radioButtons(inputId = "sep", 
                                    label = "Séparateur",
                                    choices = c(Comma = ",",
                                                Semicolon = ";",
                                                Tab = "\t"),
                                    selected = "\t", inline=T),
                       
                       # Input: Select quotes ----
                       radioButtons(inputId = "quote", 
                                    label= "Guillemets",
                                    choices = c(None = "",
                                                "Double Quote" = '"',
                                                "Single Quote" = "'"),
                                    selected = "", inline=T),
                       
                       #Input: Selection décimales
                       radioButtons(inputId = "dec", 
                                    label = "Décimales",
                                    choices = c("Point"=".",
                                                "Virgule"=","),
                                    selected = ".", inline=T)
                ),
                column(9,
                       h3("Prévisualisation"),
                       dataTableOutput(outputId = "preview")
                )
              ), 
              tags$br(),
              
              div(actionButton(inputId = "actBtnVisualisation", label = "Visualisation",icon = icon("play") ), align = "center")
              
              
              
      ),
      
      # visualization
      tabItem(tabName = "visualization",
              h1("Visualisation des données"),
              fluidRow(
                column(4, plotOutput("plotAvecR")),
                column(4, colourpicker::colourInput("colR", "Couleur graphique R", "black",allowTransparent = T),
                       sliderInput("cex", "Taille",
                                   min = 0.5, max = 3,
                                   value = 1,step = 0.2
                       )),
                column(4, selectInput(inputId = "pch", choices = 1:20, label = "Type de points",selected = 1),
                       textInput("title", "Titre", "Sepal length vs Petal length (R)") )
              ),
              tags$br(), 
              )
      )
    )
  )
