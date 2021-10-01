
server <- function(input, output) {
  
  data <- reactiveValues()
  
  #=============================================================================
  # Preview
  #=============================================================================
  output$preview <-  renderDataTable({
    
    req(input$dataFile)
    
    df <- read.csv(input$dataFile$datapath,
                   header = as.logical(input$header),
                   sep = input$sep,
                   quote = input$quote,
                   dec = input$dec,
                   nrows=10
    )
  },  options = list(scrollX = TRUE , dom = 't'))
  
  #=============================================================================
  # Lecture
  #=============================================================================
  observeEvent(input$actBtnVisualisation, {
    
    if(!is.null(input$dataFile$datapath)){
      data$table = read.csv(input$dataFile$datapath,
                            header = as.logical(input$header),
                            sep = input$sep,
                            quote = input$quote,
                            dec = input$dec)
      sendSweetAlert(
        session = session,
        title = "Done !",
        text = "Le fichier a bien été lu !",
        type = "success"
      )
      
      updateTabItems(session, "tabs", selected = "visualization")
    }
    
  })
  
  #=============================================================================
  # Exploration du tableau
  #=============================================================================
  
  output$dataTable = DT::renderDataTable({
    if(!is.null(data$table)){
      datatable(data$table, filter = 'top') %>% 
        formatStyle('Sepal.Length', 
                    background = styleColorBar(data$table, 'lightcoral'),
                    backgroundSize = '100% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'Sepal.Width',
          backgroundColor = styleInterval(c(3,4), c('white', 'red', "firebrick")),
          color = styleInterval(c(3,4), c('black', 'white', "white"))
        ) %>%
        formatStyle(
          'Petal.Length',
          background = styleColorBar(data$table$Petal.Length, 'lightcoral'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'Petal.Width',
          backgroundColor = styleInterval(c(1,2), c('white', 'red', "firebrick")),
          color = styleInterval(c(1,2), c('black', 'white', "white"))
        ) %>%
        formatStyle(
          'Species',
          backgroundColor = styleEqual(
            unique(data$table$diagnosis), c('lightblue', 'lightgreen', 'lavender')
          )
        )
    }else {
      NULL
    }
  })
  
  #=============================================================================
  # Graphiques
  #=============================================================================
  # dataTable_tab_rows_selected
  output$plotAvecR <- renderPlot({
    if(!is.null(data$table) && !is.null(input$dataTable_rows_all)){
      plot(data$table$Petal.Length[input$dataTable_rows_all],
           data$table$Sepal.Length[input$dataTable_rows_all], 
           main = input$title,
           ylab = "Sepal length",
           xlab = "Petal length",
           pch = as.numeric(input$pch),
           col = input$colR, 
           cex = input$cex)
    }else {
      NULL
    }
  })
  
  output$plotAvecGgplot2 <- renderPlot({
    if(!is.null(data$table)){
      ggplot(data=data$table[input$dataTable_rows_all,], aes(x = Sepal.Length, y = Sepal.Width)) + 
        geom_point(aes(color=Species, shape=Species)) +
        xlab("Sepal Length") +  ylab("Sepal Width") +
        ggtitle("Sepal Length-Width (ggplot2)")
    }else {
      NULL
    }
  })
  
  output$plotAvecPlotly <- renderPlotly({
    if(!is.null(data$table)){
      plot_ly(data = data$table[input$dataTable_rows_all,], x = ~Petal.Length, y = ~Petal.Width, color = ~Species)%>%
        layout(title = 'Petal Length-Width (plotly)',
               yaxis = list(title = "Petal width"),
               xaxis = list(title = "Petal length"))
    }else {
      NULL
    }
    
  })
  
  output$plotAvecGoogle <- renderGvis({
    if(!is.null(data$table)){
      gvisHistogram(as.data.frame(data$table$Petal.Width[input$dataTable_rows_all]),
                    options=list(title ="Petal width (Google)",
                                 height=400)
      )
    }else {
      NULL
    }
    
  })
  
}
