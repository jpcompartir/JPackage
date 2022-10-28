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
#' @param colour_mapping A vector of hexcodes for colour mapping.
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
umap_shiny <- function(data,..., text_var, colour_var,  size = 2,
                          umap_height = 600, x_var = V1, y_var = V2, type = "scattergl", colour_mapping = NULL){

  #----- hide wrangling ----
  text_sym <- rlang::ensym(text_var)
  colour_sym <- rlang::ensym(colour_var)

  data <- dplyr::mutate(data, original_id = dplyr::row_number())
  data <- dplyr::relocate(data, {{x_var}},{{y_var}}, {{text_var}}, {{colour_var}}, original_id)
  #Rename columns to avoid relying on tidy evaluate in server logic
  data <- dplyr::rename(data, text_var = 3, colour_var = 4)

  #---- hide UI ----
  ui <- shiny::fluidPage(
    # shinythemes::themeSelector(),

    theme = shinythemes::shinytheme(theme = "cosmo"),
    shinyWidgets::setSliderColor(color = c("#ff7518", "#ff7518"), sliderId = c(1, 2)),
    shiny::fluidRow(
      shiny::column(2, shiny::textInput("remainingName", "File Name", "data_cleaned")),
      shiny::column(1, shiny::div(style = "margin-top: 20px;",shiny::downloadButton("downloadAll", "Download All Data", class = "btn btn-success")))
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(2, shiny::sliderInput( "x1","V1 Range", step = 5, -100, 100, c(-20, 20))),
      shiny::column(2, shiny::sliderInput( "y1","V2 Range", step = 5, -100, 100, c(-20, 20)))
    ),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(2, shiny::textInput("Regex", "Pattern to filter",  value = NULL)),
      shiny::column(2, shiny::textInput("fileName", "File Name", "mydata")),
      shiny::column(2, shiny::div(style = "margin-top: 20px;",shiny::downloadButton("downloadData", "Download Selections", class = "btn btn-success"))),
      shiny::column(1, offset = 0, shiny::div(style = "margin-top: 20px;", shiny::actionButton("delete", "Delete selections", class = 'btn-danger'))),
      shiny::column(2, offset = 1, shiny::numericInput("n", "Posts shown per page", 10, min = 1, max = 200))
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

  #---- hide server ----
  server <- function(input, output, session){


    pattern <- shiny::reactiveVal({})

    shiny::observeEvent(input$Regex, {
      pattern(input$Regex)
    })

    #Get the original IDs saved and save an object for later adding selected points to remove,
    #use reactiveValues rather than reactiveVal son they are stored.
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

    #Create a reactive data frame from the original
    reactive_data <- shiny::reactive({

      data %>%
        dplyr::filter(V1 > input$x1[[1]], V1 < input$x1[[2]], V2 > input$y1[[1]], V2 < input$y1[[2]]) %>%
        dplyr::filter(!colour_var %in% input$cluster,
                      original_id %in% remove_range$keep_keys) %>%
        dplyr::filter(grepl(pattern(), text_var, ignore.case = TRUE)) %>%
        dplyr::mutate(plot_id = dplyr::row_number())
    })

    output$umapPlot = plotly::renderPlotly({
      #cluster can be changed
      reactive_data() %>%
        plotly::plot_ly(x = ~V1, y = ~V2,
                        type = type,
                        color = ~colour_var,
                        #Allow a vector of colours to be fed in
                        colors = colour_mapping,
                        #Keep track of IDs irrespective of colour variable's data type (otherwise you'll have problems with continuous vs character vs factor)
                        key = ~original_id,
                        #Allows for the post to appear with hover
                        text = ~paste("<br> Post:", text_var),
                        hoverinfo = "text", marker = list(size = size),
                        height = umap_height) %>%
        plotly::layout(dragmode = "lasso", #Instead of box selecting
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

      df_filtered <<- reactive_data() %>%
        dplyr::filter(original_id %in% key)

      df <- df_filtered %>%
        #Select the columns you want to see from your data
        dplyr::select(`ID` = original_id, `Text` = text_var,
                      `Colour Variable` = colour_var,  ...)

      DT::datatable(df, filter = "top", options = list(pageLength = input$n,
                                                       dom = '<"top" pif>', autoWidth = FALSE),
                    style = "bootstrap", rownames = FALSE,
                    caption = htmltools::tags$caption("Selected Mentions", style="color:white"),
                    escape = FALSE)
    })


    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        paste0(input$fileName, ".csv")
      },
      content = function(file) {
        utils::write.csv(df_filtered, file)
      }
    )

    output$downloadAll <- shiny::downloadHandler(
      filename = function() {
        paste0(input$remainingName, ".csv")
      },
      content = function(file) {
        utils::write.csv(reactive_data(), file)
      }
    )

  }
#---- hide app render ----
  shiny::shinyApp(ui, server)
}
