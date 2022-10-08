#' Builds a Shiny app that can be used to label embedded texts quickly
#'
#' @param data Data frame or tibble
#' @param ... Sent to select, allows you to include additional columns`dataTableOutput()`
#' @param text_var Text variable
#' @param colour_var Variable colur should be mapped to
#' @param x_var Variable with x axis co-ordinates
#' @param y_var Variable with y axis co-ordinates
#' @param size The size of each point in the UMAP plot
#' @param umap_height total height of UMAP plot
#' @param type a string instructing what type of trace, "scattergl" can handle more points.
#'
#'
#' @return shiny app
#' @export
#' @examples
#' \dontrun{
#' Example 1:
#' umap_shiny(data, permalink, sentiment, text_var = message, colour_var = cluster)
#'
#' Example 2:
#' umap_shiny(data, text_var = text, colour_var = sentiment)
#'
#' Example 3:
#' umap_shiny(data, text_var = mention_content, colour_var = entity, x_var = x, y_var = y)
#'
#' }
umap_shiny <- function(data,..., text_var = message, colour_var = cluster,  size = 2,
                          umap_height = 600, x_var = V1, y_var = V2, type = "scattergl"){

  #-----
  text_sym <- rlang::ensym(text_var)
  colour_sym <- rlang::ensym(colour_var)

  data <- dplyr::mutate(data, plot_id = dplyr::row_number(), original_id = dplyr::row_number())
  data <- dplyr::select(data, plot_id, {{x_var}},{{y_var}}, {{text_var}}, {{colour_var}}, original_id, ...)
  data <- dplyr::rename(data, text_var = 4, colour_var = 5)

  ui <- shiny::fluidPage(
    # shinythemes::themeSelector(),
    theme = shinythemes::shinytheme(theme = "superhero"),

    shiny::fluidRow(
      shiny::column(2, shiny::textInput("fileName", "File Name", "mydata")),
      shiny::column(1, shiny::div(style = "margin-top: 20px;",shiny::downloadButton("downloadData", "Download", class = "btn btn-success")))
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(2, shiny::sliderInput( "x1","V1 Range", -100, 100, c(-20, 20))),
      shiny::column(2, shiny::sliderInput( "y1","V2 Range", -100, 100, c(-20, 20)))
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(2, shiny::textInput("Regex", "Pattern to filter",  value = NULL)),
      shiny::column(2, shiny::numericInput("n", "Posts shown per page", 10, min = 1, max = 200)),
      shiny::column(1, offset = 4, shiny::div(style = "margin-top: 20px;", shiny::actionButton("delete", "Delete selections", class = 'btn-danger')))
    ),
    shiny::br(),
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


    pattern <- shiny::reactiveVal({})

    shiny::observeEvent(input$Regex, {
      pattern(input$Regex)
    })

    #Get the original IDs saved and save an object for later adding selected points to remove
    remove_range <- shiny::reactiveValues(
      keep_keys = data$original_id,
      remove_keys = NULL
    )

    #Update remove_range's values on delete button press
    shiny::observeEvent(input$delete,{
      req(length(remove_range$keep_keys) > 0)
      remove_range$remove_keys <- selected_range()$key
      remove_range$keep_keys <- remove_range$keep_keys[!remove_range$keep_keys %in% remove_range$remove_keys]

    })

    reactive_data <- shiny::reactive({

      data <- data %>%
        dplyr::filter(V1 > input$x1[[1]], V1 < input$x1[[2]], V2 > input$y1[[1]], V2 < input$y1[[2]]) %>%
        dplyr::filter(!colour_var %in% input$cluster,
                      original_id %in% remove_range$keep_keys) %>%
        dplyr::filter(grepl(pattern(), text_var, ignore.case = TRUE)) %>%
        dplyr::mutate(plot_id = dplyr::row_number())
    })

    output$umapPlot = plotly::renderPlotly({
      #cluster can be changed
      reactive_data() %>%
        plotly::plot_ly(x = ~V1, y = ~V2, type = type, color = ~colour_var,
                        key = ~original_id,
                        #make sure mention_content = text variable of your data
                        text = ~paste("<br> Post:", text_var),
                        hoverinfo = "text", marker = list(size = size), height = umap_height) %>%
        plotly::layout(dragmode = "lasso",
                       legend= list(itemsizing='constant')) %>%
        plotly::event_register(event = "plotly_selected")
    })

    #Instantiate a reactive value, then update that value dynamically when points are selected.
    selected_range <- shiny::reactiveVal({})

    shiny::observeEvent(plotly::event_data("plotly_selected"),{
      selected_range(plotly::event_data("plotly_selected"))
    })

    #Now render the data table, selecting all points within our boundaries. Would need to update this for lasso selection.,
    output$highlightedTable <- DT::renderDataTable({

      #Replacing pointNumber with a key allows for precise showing of points irrespective of variable input type.
      key <- selected_range()$key
      key <- as.numeric(key)

      df <- reactive_data() %>%
        dplyr::filter(original_id %in% key) %>%
        #Select the columns you want to see from your data
        dplyr::select(`ID` = original_id, `Text` = text_var,
                      `Colour Variable` = colour_var,  ...)

      df_copy <<- df
      DT::datatable(df, filter = "top", options = list(pageLength = input$n,
                                                       dom = '<"top" pif>', autoWidth = FALSE),
                    style = "bootstrap", rownames = FALSE,
                    caption = htmltools::tags$caption("Selected Mentions", style="color:white"))
    })


    output$downloadData <- shiny::downloadHandler(
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


