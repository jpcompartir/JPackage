#' Builds a Shiny app that can be used to label embedded texts quickly
#'
#' @param data Your data frame, must have a V1 and V2 variable (2d UMAP output)
#' @param text_var Text variable
#' @param colour_var Variable colur should be mapped to
#' @param ... currently unused, may go to select ahead of `dataTableOutput()`
#' @param size The size of each point in the UMAP plot
#' @param umap_height total height of UMAP plot
#' @param type a string instructing what type of trace, "scattergl" can handle more points.
#'
#' @return shiny app
#' @export
#'

umap_shiny <- function(data,text_var, colour_var, size = 2, umap_height = 600, type = "scattergl",...){

  text_sym <- rlang::ensym(text_var)
  colour_sym <- rlang::ensym(colour_var)

  data <- dplyr::mutate(data, plot_id = dplyr::row_number())
  data <- dplyr::select(data, plot_id, V1, V2, {{text_var}}, {{colour_var}})
  data <- dplyr::rename(data, text_var = 4, colour_var = 5)

  ui <- shiny::fluidPage(

    shiny::downloadButton("downloadData", "Download"),
    shiny::hr(),

    shiny::fluidRow(
      shiny::column(7,
                    plotly::plotlyOutput("umapPlot")
      ),
      shiny::column(5,
                    DT::dataTableOutput("highlightedTable")
      )
    ),
  )


  server <- function(input, output, session){


    output$umapPlot = plotly::renderPlotly({
      #cluster can be changed
      plotly::plot_ly(data = data, x = ~V1, y = ~V2, type = type, color = ~colour_var,
                      #make sure mention_content = text variable of your data
                      text = ~paste("<br> Post:", text_var),
                      hoverinfo = "text", marker = list(size = size), height = umap_height) %>%
        plotly::layout(dragmode = "select") %>%
        plotly::event_register(event = "plotly_selected")
    })

    #Instantiate a reactive value, then update that value dynamically when points are selected.
    selected_range <- shiny::reactiveVal({})

    shiny::observeEvent(plotly::event_data("plotly_selected"),{
      selected_range(plotly::event_data("plotly_selected"))
    })

    #Now render the data table, selecting all points within our boundaries. Would need to update this for lasso selection.,
    output$highlightedTable <- DT::renderDataTable({

      points <- selected_range()$pointNumber + 1

      df <- data %>%
        dplyr::filter(plot_id %in% points) %>%
        #Select the columns you want to see from your data
        dplyr::select(plot_id, text_var, colour_var)

      df_copy <<- df

      DT::datatable(df, filter = "top", options = list(pageLength = 100))
    })


    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        paste("filename","_", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        utils::write.csv(df_copy, file)
      }
    )

  }

  shiny::shinyApp(ui, server)



}
