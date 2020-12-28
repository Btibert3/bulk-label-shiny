# libraries
library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
suppressPackageStartupMessages(library(dplyr))


# We'll use a subset of the mtcars data set, with fewer columns
# so that it prints nicely
mtcars2 <- mtcars[, c("mpg", "cyl", "disp", "hp", "wt", "am", "gear")]
mtcars2$label = NA
mtcars2$id = 1:nrow(mtcars2)


ui <- fluidPage(
  fluidRow(
    titlePanel("Bulk Annotate your data!"),
    column(width = 12,
           plotOutput("plot1", height = 300,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot1_click",
                      dblclick = "plot1_dblclick",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           )
    ),
    br(),
    h4("Double click on selection to zoom in, double click outside to reset.",style="text-align: center;"),
    hr()
  ),
  br(),
  fluidRow(
    column(width = 6,
           h4("Selected/Brushed points"),
           verbatimTextOutput("brush_info")
    ),
    column(width = 6,
           h4("Annotation Tool"),
           br(),
           textInput("tag", "Label to apply"),
           br(),
           actionButton('update', 'Add Label'),
           br(),
           br(),
           br(),
           downloadButton("downloadFile", "Download the dataset")
    )
  )
)

server <- function(input, output, session) {
  
  mt <- reactiveValues(data=mtcars2)
  filtered_df = reactive({mt$data %>% filter(is.na(label))})
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    ggplot(filtered_df(), aes(wt, mpg)) + 
      geom_point() + 
      coord_cartesian(xlim=ranges$x, 
                      ylim=ranges$y, 
                      expand = TRUE)
  })

  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
    
  
  output$brush_info <- renderPrint({
    brushedPoints(mtcars2, input$plot1_brush)
  })
  
  observeEvent(input$update, {
    df = brushedPoints(mtcars2, input$plot1_brush)
    mtcars2[mtcars2$id %in% df$id, "label"] <<- input$tag
    # clear out the input tag
    updateTextInput(session, "tag", value="")
    # reset the brush
    session$resetBrush("plot1_brush")
    
    #update the reactive dataframe
    mt$data <- mtcars2
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadFile <- downloadHandler(
    filename = function() {
      paste("data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(mtcars2, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)