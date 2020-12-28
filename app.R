## TODO: reactive dataframe, once have label, not shown in plot


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
    column(width = 12,
           plotOutput("plot1", height = 300,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           )
    )
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
           actionButton('update', 'Add Label')
    )
  )
)

server <- function(input, output, session) {
  
  filtered_df = reactive({mtcars2 %>% filter(is.na(label))})
  
  output$plot1 <- renderPlot({
    ggplot(filtered_df(), aes(wt, mpg)) + geom_point()
  })
  
  # output$click_info <- renderPrint({
  #   # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
  #   # were a base graphics plot, we'd need those.
  #   nearPoints(mtcars2, input$plot1_click, addDist = TRUE)
  # })
  
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
    
    mtcars2 <<- mtcars2
  })
  
  
}

shinyApp(ui, server)

