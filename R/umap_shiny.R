#' Builds a Shiny app that can be used to label embedded texts quickly
#'
#' @param data Data frame or tibble
#' @param text_var Text variable
#' @param colour_var Variable colur should be mapped to
#' @param x_var Variable with x axis co-ordinates
#' @param y_var Variable with y axis co-ordinates
#' @param size The size of each point in the UMAP plot
#' @param umap_height total height of UMAP plot
#' @param type a string instructing what type of trace, "scattergl" can handle more points.
#' @param ... currently unused, may go to select ahead of `dataTableOutput()`
#'
#'
#' @return shiny app
#' @export
#'
umap_shiny <- function(data,text_var, colour_var, size = 2, umap_height = 600, x_var = V1, y_var = V2, type = "scattergl",...){

  text_sym <- rlang::ensym(text_var)
  colour_sym <- rlang::ensym(colour_var)

  data <- dplyr::mutate(data, plot_id = dplyr::row_number(), original_id = row_number())
  data <- dplyr::select(data, plot_id, {{x_var}},{{y_var}}, {{text_var}}, {{colour_var}}, original_id)
  data <- dplyr::rename(data, text_var = 4, colour_var = 5)

  ui <- shiny::fluidPage(

    shiny::downloadButton("downloadData", "Download"),
    shiny::textInput("fileName", "File Name", "mydata"),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(2, shiny::sliderInput( "x1","V1 Greater than", -50, 20, -20)),
      shiny::column(2, shiny::sliderInput("x2","V1 Less than",  -10, 50, 20)),
      shiny::column(2, shiny::sliderInput( "y1","V2 Greater than", -50, 20, -20)),
      shiny::column(2, shiny::sliderInput( "y2","V2 Less than", -10, 50, 20))
    ),
    shiny::fluidRow(
      shiny::column(3, shiny::numericInput("n", "Number of posts per page of table", 25, min = 1, max = 100)),
      shiny::column(6, shiny::selectizeInput("cluster", "Select which clusters to hide", choices = unique(data[,5]), multiple = TRUE))
    ),
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


    reactive_data <- reactive({
      data %>%
        dplyr::filter(V1 > input$x1, V1 < input$x2, V2 > input$y1, V2 < input$y2) %>%
        dplyr::filter(!colour_var %in% input$cluster) %>%
        dplyr::mutate(plot_id = row_number())
    })

    output$umapPlot = plotly::renderPlotly({
      #cluster can be changed
      reactive_data() %>%
        plotly::plot_ly(x = ~V1, y = ~V2, type = type, color = ~colour_var,
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

      df <- reactive_data() %>%
        dplyr::filter(plot_id %in% points) %>%
        #Select the columns you want to see from your data
        dplyr::select(plot_id, text_var, colour_var, original_id)

      df_copy <<- df

      DT::datatable(df, filter = "top", options = list(pageLength = input$n,
                                                       dom = '<"top" pif>'))
    })


    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(input$fileName, ".csv")
      },
      content = function(file) {
        utils::write.csv(df_copy, file)
      }
    )

  }

  shiny::shinyApp(ui, server)
}




