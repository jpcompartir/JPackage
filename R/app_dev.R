#' Shiny Application of a conversation landscape.
#'
#' Multiple tabs - one for cleaning and selecting, others for summary plots.
#'
#' @param data
#' @param ...
#' @param id Your ID column, if there is not one in your data, create one
#' @param text_var The original text variable for display in the data table
#' @param colour_var The variable you wish to map colour to (should be a string or factor)
#' @param cleaned_text_var The cleaned text variable for bigram and token plots
#' @param date_var Your date variable
#' @param sentiment_var Sentiment variable
#' @param point_size Size of the points in the plotly output
#' @param plotly_height height of the plotly output
#' @param x_var Variable which contains your x co-ordinates
#' @param y_var Variable which contains your y co-ordinates
#' @param type Type of the plotly output, unlikely to change from 'scattergl'
#' @param colour_mapping Which colours the colour variable should be mapped to
#'
#' @return A shiny application
#' @export
#'
#' @examples
conversation_landscape <- function(data,..., id,text_var,colour_var, cleaned_text_var, date_var, sentiment_var,
                           size = 2, umap_height = 600, x_var = V1, y_var = V2, type = "scattergl", colour_mapping = NULL){
  plotting_heights <- "450px"
  plotting_widths <- "400px"

  #Store another version of the HelpR function so that we can set the fill with a text input
  .plot_tokens_counter <- function(df, text_var = .data$mention_content, top_n = 20, fill = "#0f50d2"){

    .text_var <- rlang::enquo(text_var)

    df %>%
      tidytext::unnest_tokens(words, rlang::quo_name(.text_var))%>%
      dplyr::count(words, sort = TRUE) %>%
      dplyr::top_n(top_n)%>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(words, n), y = n))+
      ggplot2::geom_col(fill = fill)+
      ggplot2::coord_flip()+
      ggplot2::theme_bw()+
      ggplot2::labs(x = NULL, y =  "Word Count", title = "Bar Chart of Most Frequent Words")+
      ggplot2::theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  }

  #----- hide wrangling ----
  text_sym <- rlang::ensym(text_var)
  colour_sym <- rlang::ensym(colour_var)
  date_sym <- rlang::ensym(date_var)
  sentiment_sym <- rlang::ensym(sentiment_var)
  cleaned_text_sym <- rlang::ensym(cleaned_text_var)
  # id_sym <- rlang::ensym(id_var)

  data <- dplyr::rename(data, id_var = {{id}})
  data <- dplyr::relocate(data, {{x_var}},{{y_var}}, {{text_var}}, {{colour_var}}, id_var)
  #Rename columns to avoid relying on tidy evaluate in server logic
  data <- dplyr::rename(data, text_var = 3, colour_var = 4)

  # hide UI ----
  ui <-
    shiny::navbarPage("Conversation Landscape", theme = shinythemes::shinytheme("cosmo"), position = "fixed-top",
                      shiny::tabPanel("Survey the Landscape",
                                      #---- Tab 1 ----
                                      shiny::br(),
                                      shiny::fluidPage(
                                        # shinythemes::themeSelector(),
                                        theme = shinythemes::shinytheme(theme = "superhero"),
                                        shiny::fluidRow(
                                          shiny::column(2, shiny::textInput("remainingName", "File Name", value = NULL, placeholder = "filename excluding .csv")),
                                          shiny::column(1, shiny::div(style = "margin-top: 25px;",shiny::downloadButton("downloadAll", "Download All Data", class = "btn btn-success")))
                                        ),
                                        shiny::br(),
                                        shiny::fluidRow(
                                          shiny::column(2, shiny::sliderInput( "x1","V1 Range",step = 5,  -100, 100, c(-20, 20))),
                                          shiny::column(2, shiny::sliderInput( "y1","V2 Range",step = 5, -100, 100, c(-20, 20)))
                                        ),
                                        shiny::br(),
                                        shiny::fluidRow(
                                          shiny::column(2, shiny::textInput("fileName", "File Name", value = NULL, placeholder = "filename excluding .csv")),
                                          shiny::column(2, shiny::div(style = "margin-top: 25px;",shiny::downloadButton("downloadData", "Download Selections",class = "btn btn-success" )))
                                        ),
                                        shiny::br(),
                                        shiny::fluidRow(
                                          shiny::column(2, shiny::textInput("Regex", "Pattern to filter",  value = NULL)),
                                          shiny::column(1, shiny::div(style = "margin-top: 25px;", shiny::actionButton("filterPattern", "Filter", class = "btn btn-primary"))),
                                          shiny::column(1, shiny::div(style = "margin-top: 25px;", shiny::actionButton("reset", "Reset", class = "btn btn-light"))),
                                          shiny::column(1, offset = 4, shiny::numericInput("n", "Posts per page", 10, min = 1, max = 200)),
                                          shiny::column(1, shiny::div(style = "margin-top: 25px;", shiny::actionButton("delete", "Delete selections", class = 'btn-danger')))
                                        ),
                                        shiny::br(),
                                        shiny::fluidRow(
                                          shiny::column(7,
                                                        plotly::plotlyOutput("umapPlot")
                                          ),
                                          shiny::column(5,
                                                        DT::dataTableOutput("highlightedTable")
                                          ),
                                        ),
                                      ),),
                      #---- tab 2 ----
                      shiny::br(),
                      shiny::tabPanel("Summarise the Landscape", shiny::fluidPage(theme = shinythemes::shinytheme('superhero')),
                                      shiny::br(),
                                      shiny::p("In this tab you can view, and download if necessary, charts designed to help you understand your selections."),
                                      shiny::p("Below you will find four charts; sentiment distribution, volume over time, tokens counter and a sampled bigram network."),
                                      #---- Sentiment plot ----
                                      shiny::br(),
                                      shiny::titlePanel(title =  "Sentiment Plot"),
                                      shiny::sidebarLayout(
                                        shiny::sidebarPanel(width = 2,
                                                            shiny::sliderInput("sentimentHeight","height",  min = 100, max = 800, value = 400, step = 50),
                                                            shiny::sliderInput("sentimentWidth","width",  min = 100, max = 800, value = 400, step = 50),
                                                            shiny::textInput(inputId = "sentimentTitle", label = "Title",
                                                                             placeholder = "Write title here...", value = ""),
                                                            shiny::textInput(inputId = "sentimentSubtitle", label = "Subtitle",
                                                                             placeholder = "Write subtitle here...", value = ""),
                                                            shiny::textInput(inputId = "sentimentCaption", label = "Caption",
                                                                             placeholder = "Write caption here...", value = ""),
                                                            shiny::textInput(inputId = "sentimentXlabel", label = "X axis title",
                                                                             placeholder = "Write the x axis title here..."),
                                                            shiny::textInput(inputId = "sentimentYlabel", label = "Y axis title",
                                                                             placeholder = "Write the y axis title here"),
                                                            shiny::downloadButton(outputId = "saveSentiment", class = '<button class="btn-download" id="btn-auto-click">
                                                                  <div class="arrow"></div>
                                                                  </button>'),
                                        ),
                                        shiny::mainPanel(
                                          shiny::plotOutput("sentimentPlot",
                                                            height = plotting_heights,
                                                            width  = plotting_widths)
                                        )
                                      ),
                                      shiny::br(),

                                      #Volume Over Time plot ----
                                      shiny::br(),
                                      shiny::titlePanel(title = "Volume Plot"),
                                      shiny::sidebarLayout(
                                        shiny::sidebarPanel(width =  2,
                                                            shiny::sliderInput("volumeHeight","height",  min = 100, max = 800, value = 400, step = 50),
                                                            shiny::sliderInput("volumeWidth","width",  min = 100, max = 800, value = 400, step = 50),
                                                            shiny::textInput("volumeHex", "colour", value ="#107C10"),
                                                            shiny::textInput(inputId = "volumeTitle", label = "Title",
                                                                             placeholder = "Write title here...", value = ""),
                                                            shiny::textInput(inputId = "volumeSubtitle", label = "Subtitle",
                                                                             placeholder = "Write subtitle here...", value = ""),
                                                            shiny::textInput(inputId = "volumeCaption", label = "Caption",
                                                                             placeholder = "Write caption here...", value = ""),
                                                            shiny::textInput(inputId = "volumeXlabel", label = "X axis title",
                                                                             placeholder = "Write the x axis title here..."),
                                                            shiny::textInput(inputId = "volumeYlabel", label = "Y axis title",
                                                                             placeholder = "Write the y axis title here"),
                                                            shiny::downloadButton(outputId = "saveVolume", class = '<button class="btn-download" id="btn-auto-click">
                                                                  <div class="arrow"></div>
                                                                  </button>'),
                                        ),
                                        shiny::mainPanel(shiny::plotOutput("volumePlot",
                                                                           height = plotting_heights,
                                                                           width  = plotting_widths))
                                      ),
                                      shiny::br(),

                                      #Token Plot ----
                                      shiny::br(),
                                      shiny::titlePanel(title = "Token Plot"),
                                      shiny::sidebarLayout(
                                        shiny::sidebarPanel(width = 2,
                                                            shiny::sliderInput("tokenHeight","height",  min = 100, max = 800, value = 400, step = 50),
                                                            shiny::sliderInput("tokenWidth","width",  min = 100, max = 800, value = 400, step = 50),
                                                            shiny::textInput("tokenHex", "colour", value = "#0f50d2"),
                                                            shiny::textInput(inputId = "tokenTitle", label = "Title",
                                                                             placeholder = "Write title here...", value = ""),
                                                            shiny::textInput(inputId = "tokenSubtitle", label = "Subtitle",
                                                                             placeholder = "Write subtitle here...", value = ""),
                                                            shiny::textInput(inputId = "tokenCaption", label = "Caption",
                                                                             placeholder = "Write caption here...", value = ""),
                                                            shiny::textInput(inputId = "tokenXlabel", label = "X axis title",
                                                                             placeholder = "Write the x axis title here..."),
                                                            shiny::textInput(inputId = "tokenYlabel", label = "Y axis title",
                                                                             placeholder = "Write the y axis title here"),
                                                            shiny::downloadButton(outputId = "saveToken", class = '<button class="btn-download" id="btn-auto-click">
                                                                  <div class="arrow"></div>
                                                                  </button>'),
                                        ),
                                        shiny::mainPanel(shiny::plotOutput("tokenPlot",
                                                                           height = plotting_heights,
                                                                           width  = plotting_widths))
                                      ),
                                      shiny::br(),
                                      # Bigram plot ----
                                      shiny::p("Below you'll find a bigram network, this network will help you estimate how clean your selected data is."),
                                      shiny::p("Remember that long and connected chains of words may represent spam or unwanted mentions."),
                                      shiny::p("This bigram network is restricted to a maximum of 5,000 data points for speed and user experience."),
                                      shiny::p("It is therefore not recommended to be saved or exported. If the data looks clean download the selection"),
                                      shiny::p("and create the network in the standard way in R/Rstudio"),
                                      #Need to figure out how to customise helpText before relying on it.
                                      # shiny::helpText("Below you'll find a bigram network, this network will help you estimate how clean your selected data is.",
                                      #                 "Remember that long and connected chains of words may represent spam or unwanted mentions.",
                                      #                 "NOTE: his bigram network is restricted to a maximum of 5,000 data points for speed and user experience.",
                                      #                 "It is therefore not recommended to be saved or exported. If the data looks clean download the selection",
                                      #                 "and create the network in the standard way in R/Rstudio"
                                      #                 , width = 3),

                                      shiny::sidebarPanel(width = 2,
                                                          shiny::sliderInput("bigramHeight","height",  min = 100, max = 1200, value = 600, step = 50),
                                                          shiny::sliderInput("bigramWidth","width",  min = 100, max = 1200, value = 800, step = 50),
                                      ),
                                      shiny::mainPanel(shiny::plotOutput("bigramPlot",
                                                                         height = plotting_heights,
                                                                         width  = plotting_widths))),
                      shiny::br(),


                      #Old UI Commenting out TODO
                      # shiny::fluidRow(
                      #   shiny::column(4,shiny::plotOutput("volumePlot", height = "400px")),
                      #   shiny::column(3,shiny::plotOutput("sentimentPlot", height = "400px")),
                      #   shiny::column(4,shiny::plotOutput("tokenPlot", height = "400px"))
                      # ),
                      # shiny::br(),
                      # shiny::p("Below you'll find a bigram network, this network will help you estimate how clean your selected data is."),
                      # shiny::p("Remember that long and connected chains of words may represent spam or unwanted mentions."),
                      # shiny::fluidRow(
                      #   shiny::column(8, shiny::plotOutput("bigramPlot", height = "600px"))
                      # ),
    )


  #---- Server ----
  server <- function(input, output, session){

    #--- Pattern ----
    pattern <- shiny::reactiveVal(value = "",{})

    shiny::observeEvent(input$filterPattern, {
      pattern(input$Regex)
    })

    #--- Filter + Reset Pattern ----
    shiny::observeEvent(input$reset, {
      pattern(input$Regex)
      updateTextInput(session, "Regex", value = "")
    })

    shiny::observeEvent(input$reset, {
      pattern("")
    })

    #--- Delete IDS ----
    #Get the original IDs saved and save an object for later adding selected points to remove
    remove_range <- shiny::reactiveValues(
      keep_keys = data$id_var,
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
                      id_var %in% remove_range$keep_keys) %>%
        dplyr::filter(grepl(pattern(), text_var, ignore.case = TRUE))
    })

    #--- UMAP Plot ----
    output$umapPlot = plotly::renderPlotly({
      #cluster can be changed
      reactive_data() %>%
        plotly::plot_ly(x = ~V1, y = ~V2,
                        type = type,
                        color = ~colour_var,
                        colors = colour_mapping,
                        key = ~id_var,
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


    #--- Data Table ----
    #Now render the data table, selecting all points within our boundaries. Would need to update this for lasso selection.,
    output$highlightedTable <- DT::renderDataTable({

      #Replacing pointNumber with a key allows for precise showing of points irrespective of variable input type.
      key <- selected_range()$key
      #Changing to universal_message_id changed the need for this I think?
      # key <- as.numeric(key)

      df_filtered <<- reactive_data() %>%
        #This was trying to find {{id_var}} which had been changed
        dplyr::filter(id_var %in% key)

      df <- df_filtered %>%
        #Select the columns you want to see from your data
        dplyr::select(`ID` = original_id, `Text` = text_var,
                      `Colour Variable` = colour_var, ..., !!sentiment_sym)

      DT::datatable(df, filter = "top", options = list(pageLength = input$n,
                                                       dom = '<"top" pifl>', autoWidth = FALSE), #TODO check adding l worked
                    style = "bootstrap", rownames = FALSE,
                    caption = htmltools::tags$caption("Selected Mentions", style="color:white"),
                    escape = FALSE)
    })


    #--- Download Handler Data ----
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

    #--- Reactive plots ----
    shiny::observeEvent(plotly::event_data("plotly_selected"),{
      output$sentimentPlot <- renderPlot({
        df_filtered %>%
          HelpR::plot_sentiment_distribution(sentiment_var = {{sentiment_var}}) +
          HelpR::theme_microsoft_discrete() +
          ggplot2::theme(legend.position = "none") +
          ggplot2::labs(title = paste0(input$sentimentTitle),
                        caption = paste0(input$sentimentCaption),
                        subtitle = paste0(input$sentimentSubtitle),
                        x = paste0(input$sentimentXlabel),
                        y = paste0(input$sentimentYlabel))
      }, res = 100,
      width = function() input$sentimentWidth,
      height = function() input$sentimentHeight)
    })

    shiny::observeEvent(plotly::event_data("plotly_selected"),{
      output$tokenPlot <- renderPlot({
        df_filtered %>%
          .plot_tokens_counter(text_var = {{cleaned_text_var}}, top_n = 25, fill = delayedTokenHex()) +
          ggplot2::labs(title = paste0(input$tokenTitle),
                        caption = paste0(input$tokenCaption),
                        subtitle = paste0(input$tokenSubtitle),
                        x = paste0(input$tokenXlabel),
                        y = paste0(input$tokenYlabel)) +
          ggplot2::scale_fill_manual(values = input$tokenHex)
      }, res = 100,
      width = function() input$tokenWidth,
      height = function() input$tokenHeight)
    })

    shiny::observeEvent(plotly::event_data("plotly_selected"),{
      output$volumePlot <- renderPlot({
        df_filtered %>%
          HelpR::plot_volume_over_time(time =  "week", my_fill = input$volumeHex) +
          ggplot2::labs(title = paste0(input$volumeTitle),
                        caption = paste0(input$volumeCaption),
                        subtitle = paste0(input$volumeSubtitle),
                        x = paste0(input$volumeXlabel),
                        y = paste0(input$volumeYlabel))
      }, res = 100,
      width = function() input$volumeWidth,
      height = function() input$volumeHeight)
    })

    shiny::observeEvent(plotly::event_data("plotly_selected"),{
      output$bigramPlot <- renderPlot({

        if(length(selected_range()) > 1){
          if(!length(selected_range()) >= 5000){
            bigram <- df_filtered %>%
              JPackage::make_bigram_viz(text_var = {{cleaned_text_var}}, clean_text = FALSE, min = 5)
          }else{
            bigram <- df_filtered %>%
              dplyr::sample_n(5000) %>%
              JPackage::make_bigram_viz(text_var = {{cleaned_text_var}}, clean_text = FALSE, min = 5)
          }
        }
        bigram

      }, res = 100,
      width = function() input$bigramWidth,
      height = function() input$bigramHeight)
    })

    output$volumePlot <- renderPlot({
      req(plotly::event_data("plotly_selected"))
      if(length(selected_range()) > 1 ){
        df_filtered %>%
          HelpR::plot_volume_over_time(time =  "week", my_fill = "#107C10")
      }
    },res = 100)

    output$sentimentPlot <- renderPlot({
      req(plotly::event_data("plotly_selected"))
      if(length(selected_range()) > 1 ){
        df_filtered %>%
          HelpR::plot_sentiment_distribution() +
          HelpR::theme_microsoft_discrete() +
          ggplot2::theme(legend.position = "none")
      }

    }, res = 100)

    output$tokenPlot <- renderPlot({
      req(plotly::event_data("plotly_selected"))
      if(length(selected_range())> 1 ){
        df_filtered %>%
          .plot_tokens_counter(text_var = {{cleaned_text_var}}, top_n = 25, fill = delayedTokenHex())
      }
    }, res = 100)

    delayedTokenHex <- shiny::reactive({
      input$tokenHex
    }) %>%
      shiny::debounce(1000)

    output$bigramPlot <- renderPlot({
      req(plotly::event_data("plotly_selected"))
      if(length(selected_range()) > 1){
        if(!length(selected_range()) >= 5000){
          bigram <- df_filtered %>%
            JPackage::make_bigram_viz(text_var = {{cleaned_text_var}}, clean_text = FALSE, min = 5)
        }else{
          bigram <- df_filtered %>%
            dplyr::sample_n(5000) %>%
            JPackage::make_bigram_viz(text_var = {{cleaned_text_var}}, clean_text = FALSE, min = 5)
        }
      }
      bigram


    }, res = 100)

  }
  #---- hide app render ----
  shiny::shinyApp(ui, server)
}
