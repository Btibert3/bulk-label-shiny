# libraries
library(ggplot2)
#library(Cairo)   # For nicer ggplot2 output when deployed on Linux
suppressPackageStartupMessages(library(dplyr))



# hardcoded - the dataset that you want your app to use
# do any cleanup to test
df = read.csv("messages-to-label.csv")

## necessary for the app below
df$shiny_label = NA
df$shiny_id = 1:nrow(df)

## the columns from your dataset above that you want to show in the table
COLS = c("text", "shiny_label")


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
    column(width = 12,
           h4("Selected/Brushed points"),
           verbatimTextOutput("brush_info")
    )),
  fluidRow(
    column(width = 6,
           h4("Annotation Tool"),
           br(),
           textInput("tag", "Label to apply"),
           br(),
           actionButton('update', 'Add Label'),
           br(),
           br(),
           br(),
    column(widget=6, 
           downloadButton("downloadFile", "Download the dataset")),
           br(),
           br()
    )
  )
)

server <- function(input, output, session) {

  mt <- reactiveValues(data=df)
  filtered_df = reactive({mt$data %>% filter(is.na(shiny_label))})

  ranges <- reactiveValues(x = NULL, y = NULL)

  output$plot1 <- renderPlot({
    ggplot(filtered_df(), aes(x, y)) +
      geom_point(alpha=.5) +
      coord_cartesian(xlim=ranges$x,
                      ylim=ranges$y,
                      expand = TRUE) +
      theme_bw()
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
    brushedPoints(df, input$plot1_brush)[COLS]
  })

  observeEvent(input$update, {
    tmp = brushedPoints(df, input$plot1_brush)
    df[df$shiny_id %in% tmp$shiny_id, "shiny_label"] <<- input$tag
    # clear out the input tag
    updateTextInput(session, "tag", value="")
    # reset the brush
    session$resetBrush("plot1_brush")

    #update the reactive dataframe
    mt$data <- df
  })

  # Downloadable csv of selected dataset ----
  output$downloadFile <- downloadHandler(
    filename = function() {
      paste("data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df, file, row.names = FALSE)
    }
  )

}

shinyApp(ui, server)
