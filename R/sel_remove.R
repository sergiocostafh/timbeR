#' Remove unwanted data by selecting it
#'
#' Delete unwanted records from the dataset (e.g. outliers) by selecting them ia a scatter plot.
#'
#' @param data a data.frame.
#' @param xvar quoted name of the variable to be displayed in the x axis.
#' @param yvar quoted name of the variable to be displayed in the y axis.
#'
#' @return the data.frame given to the `data` argument, without the selected points.
#'
#' @export
sel_remove <- function(data, xvar, yvar) {

  shiny::runGadget(

    miniUI::miniPage(miniUI::gadgetTitleBar("Drag to delete points"),
                     shiny::plotOutput(outputId = "plot", height = '100%',brush = "brush")),

    server <- function(input, output, session) {

      data1 <-  shiny::reactiveValues(data = data)

      output$plot <- shiny::renderPlot({

        ggplot2::ggplot(data1$data, ggplot2::aes_string(xvar, yvar)) + ggplot2::geom_point()
      })

      shiny::observe({
        df = shiny::brushedPoints(data1$data,
                                  brush = input$brush,
                                  allRows = TRUE)
        data1$data <- df[df$selected_ == FALSE,]
      })

      shiny::observeEvent(input$done, {
        shiny::stopApp(data1$data)
      })
    }
  )

}
