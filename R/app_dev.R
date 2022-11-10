#' Shiny Application of a conversation landscape.
#'
#' Multiple tabs - one for cleaning and selecting, others for summary plots.
#'
#' @param data Data Frame
#' @param ... Additional Columns to select
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
conversation_landscape <- function(data,..., id,text_var,colour_var, cleaned_text_var, date_var, sentiment_var,
                                   size = 2, x_var = V1, y_var = V2, type = "scattergl", colour_mapping = NULL){

  #Modified version of vol plot ----
  library(htmltools)
  library(tableHTML)
  library(shinyWidgets)
  .plot_volume_over_time <- function(df, date_var , unit = "week",  fill = "#0f50d2"){

    date_sym <- rlang::ensym(date_var)

    df <- df %>% dplyr::mutate(plot_date = lubridate::floor_date(!!date_sym, unit = unit))

    df %>%
      dplyr::count(plot_date) %>%
      ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n)) +
      ggplot2::geom_col(fill = fill) +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%d-%b") +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = element_text(angle = 90))


  }

  #Modified version of token plots
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

  plotting_heights <- "450px"
  plotting_widths <- "400px"

  #----- hide wrangling ----
  text_sym <- rlang::ensym(text_var)
  colour_sym <- rlang::ensym(colour_var)
  date_sym <- rlang::ensym(date_var)
  sentiment_sym <- rlang::ensym(sentiment_var)
  cleaned_text_sym <- rlang::ensym(cleaned_text_var)
  # id_sym <- rlang::ensym(id_var)

  #Get date ranges for volume
  dates <- data %>% select(!!date_sym) %>% summarise(min = min(!!date_sym), max = max(!!date_sym))
  date_min <- as.Date(dates$min)
  date_max <- as.Date(dates$max)

  data <- dplyr::rename(data, id_var = {{id}})
  data <- dplyr::relocate(data, {{x_var}},{{y_var}}, {{text_var}}, {{colour_var}}, id_var)
  #Rename columns to avoid relying on tidy evaluate in server logic
  data <- dplyr::rename(data, text_var = 3, colour_var = 4)

  # hide UI ----
  ui <-
    shiny::navbarPage("Conversation Landscape", theme = shinythemes::shinytheme("cosmo"), position = "fixed-top",
                      tags$style(type="text/css", "body {padding-top: 70px;}"), #Prevents the navbar from eating body of app
                      #colours all 10  sliders orange
                      shinyWidgets::setSliderColor(color = rep("#ff7518", 10), sliderId = c(1:10)),
                      shiny::tabPanel("Survey the Landscape",
                                      #---- Landscape Tab----
                                      shiny::fluidPage(
                                        gotop::use_gotop(),
                                        # shinythemes::themeSelector(),
                                        theme = shinythemes::shinytheme(theme = "cosmo"),
                                        shiny::fluidRow(
                                          shiny::column(2, style = "padding-right: 0px; border: none;",  shiny::textInput("remainingName", "All Data", value = NULL, placeholder = "filename")),
                                          shiny::column(1, style = "padding-left: 10px; padding-right: 20px;", shiny::div(style = "margin-top: 25px;",shiny::downloadButton("downloadAll", "Download", class = "btn btn-warning",  style = "background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;"))),
                                          shiny::column(3, style = "padding-left: 20px; padding-right: 10px;", shinyWidgets::searchInput(
                                            inputId = "filterPattern",
                                            label = "Pattern to search text with",
                                            placeholder = "A placeholder",
                                            btnSearch = shiny::icon("search"),
                                            btnReset = shiny::icon("remove"),
                                            width = "100%",
                                            value = ""
                                          )),
                                          shiny::column(2, shiny::textInput("fileName", "Selected Data", value = NULL, placeholder = "filename excluding .csv")),
                                          shiny::column(2, shiny::div(style = "margin-top: 25px;",shiny::downloadButton("downloadData", "Download",class = "btn btn-warning",  style = "background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;")))
                                        ),
                                        shiny::column(6, style = "width:50%; height: 10000px; position: relative;",
                                                      div(id = "graph",
                                                          shinycssloaders::withSpinner(plotly::plotlyOutput("umapPlot", height = 600)),
                                                          div(id = "button",
                                                              shiny::fluidRow(
                                                                shiny::uiOutput("deleteme"),
                                                                # shiny::actionButton("delete", "Delete selections", class = 'btn-warning', style = "position: absolute; bottom 7px; right: 7px; background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;")
                                                              ),
                                                          ),
                                                          shiny::br(),
                                                          shiny::br(),
                                                          shiny::fluidRow(
                                                            shiny::column(6, div(id = "slider1",
                                                                                 style = "width: 100%;",
                                                                                 shiny::sliderInput("x1","V1 Range",step = 5,  -100, 100, c(-20, 20))),),
                                                            shiny::column(6,
                                                                          div(id = "slider2", style = "width: 100%;",
                                                                              shiny::sliderInput( "y1","V2 Range",step = 5, -100, 100, c(-20, 20)))
                                                            ),
                                                          )
                                                      )
                                        ),
                                        shiny::column(5, shinycssloaders::withSpinner(DT::dataTableOutput("highlightedTable"))),
                                      ),),
                      #---- Distribution Tab ----
                      shiny::tabPanel("Distribution Plots", shiny::fluidPage(theme = shinythemes::shinytheme('cosmo'),
                                                                             gotop::use_gotop()),

                                      shiny::p("In this tab you can view, and download if necessary, charts designed to help you understand your selections."),
                                      shiny::p("Below you will find four charts; sentiment distribution, volume over time, tokens counter and a sampled bigram network."),
                                      #---- Sentiment plot ----
                                      shiny::br(),
                                      shiny::titlePanel(title =  "Sentiment Distribution"),
                                      shiny::sidebarLayout(
                                        shiny::sidebarPanel(width = 2,
                                                            #Should functionise all of this and use map to render the UI elements.
                                                            shiny::sliderInput("sentimentHeight","Height",  min = 100, max = 800, value = 400, step = 50),
                                                            shiny::sliderInput("sentimentWidth","Width",  min = 100, max = 800, value = 400, step = 50),
                                                            shinyWidgets::materialSwitch(
                                                              inputId = "toggleSentimenttitles",
                                                              label = "Customise Titles?",
                                                              status = "primary",
                                                              right = TRUE
                                                            ),
                                                            shiny::uiOutput("sentimentTitles"),
                                                            shiny::downloadButton(outputId = "saveSentiment", class = "btn btn-warning",  style = "background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;"),
                                        ),
                                        shiny::mainPanel(
                                          shinycssloaders::withSpinner(shiny::plotOutput("sentimentPlot",height = plotting_heights, width  = plotting_widths))
                                        )
                                      ),
                                      shiny::hr(),

                                      #Volume Over Time plot ----
                                      shiny::titlePanel(title = "Volume Over Time"),
                                      shiny::br(),
                                      shiny::sidebarLayout(
                                        shiny::sidebarPanel(width =  2,
                                                            shiny::sliderInput("volumeHeight","Height",  min = 100, max = 800, value = 400, step = 50),
                                                            shiny::sliderInput("volumeWidth","Width",  min = 100, max = 800, value = 400, step = 50),
                                                            shiny::dateRangeInput("dateRange", label = "Date Range",start = date_min, end = date_max),

                                                            shiny::selectInput(inputId = "dateBreak", label = "Unit", choices = c("day", "week", "month", "quarter", "year"), selected = "week"),
                                                            shiny::selectInput(inputId = "dateSmooth", label = "Smooth", choices = c("none", "loess", "lm", "glm", "gam"), selected = "none"),
                                                            shiny::uiOutput("smoothControls"),
                                                            shiny::textInput("volumeHex", "colour", value ="#107C10"),
                                                            shinyWidgets::materialSwitch(
                                                              inputId = "toggleVolumetitles",
                                                              label = "Customise Titles?",
                                                              status = "primary",
                                                              right = TRUE
                                                            ),
                                                            shiny::uiOutput("volumeTitles"),
                                                            shiny::downloadButton(outputId = "saveVolume", class = "btn btn-warning",  style = "background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;"),
                                        ),
                                        shiny::mainPanel(
                                          shinycssloaders::withSpinner(shiny::plotOutput("volumePlot", height = plotting_heights,width  = plotting_widths)))
                                      ),
                                      shiny::br(),

                                      #Token Plot ----
                                      shiny::br(),
                                      shiny::titlePanel(title = "Token Distribution"),
                                      shiny::sidebarLayout(
                                        shiny::sidebarPanel(width = 2,
                                                            shiny::sliderInput("tokenHeight","Height",  min = 100, max = 800, value = 400, step = 50),
                                                            shiny::sliderInput("tokenWidth","Width",  min = 100, max = 800, value = 400, step = 50),
                                                            shinyWidgets::materialSwitch(
                                                              inputId = "toggleTokentitles",
                                                              label = "Customise Titles?",
                                                              status = "primary",
                                                              right = TRUE
                                                            ),
                                                            shiny::textInput("tokenHex", "colour", value ="#0f50d2"),
                                                            shiny::uiOutput("tokenTitles"),
                                                            shiny::downloadButton(outputId = "saveToken", class = "btn btn-warning",  style = "background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;"),
                                        ),
                                        shiny::mainPanel(
                                          shinycssloaders::withSpinner(shiny::plotOutput("tokenPlot", height = plotting_heights, width  = plotting_widths)))
                                      ),
                      ),
                      shiny::br(),
                      #---- Bigram Tab ----
                      shiny::tabPanel("Bigram Network", shiny::fluidPage(theme = shinythemes::shinytheme('cosmo'),
                                                                         gotop::use_gotop()),

                                      shiny::br(),
                                      # Bigram plot ----

                                      shiny::fluidRow(
                                        shiny::column(4,
                                                      shiny::p("Below you'll find a bigram network, this network will help you estimate how clean your selected data is. Remember that long and connected chains of words may represent spam or unwanted mentions."),
                                                      shiny::br() ,
                                                      shiny::p("This bigram network is restricted to a maximum of 5,000 data points for speed and user experience. It is therefore not recommended to be saved or exported. If the data looks clean, download the selection and create the network in the standard way in R/Rstudio"),)
                                      ),

                                      shiny::sidebarPanel(width = 2,
                                                          shiny::sliderInput("bigramHeight","Height",  min = 100, max = 1200, value = 600, step = 50),
                                                          shiny::sliderInput("bigramWidth","Width",  min = 100, max = 1200, value = 800, step = 50),
                                      ),
                                      shiny::mainPanel(
                                        shinycssloaders::withSpinner(shiny::plotOutput("bigramPlot",height = plotting_heights,width  = plotting_widths)))
                      ),

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
    #---- reactive data ---
    reactive_data <- shiny::reactive({

      data <- data %>%
        dplyr::filter(V1 > input$x1[[1]], V1 < input$x1[[2]], V2 > input$y1[[1]], V2 < input$y1[[2]]) %>%
        dplyr::filter(!colour_var %in% input$cluster,
                      id_var %in% remove_range$keep_keys) %>%
        dplyr::filter(grepl(input$filterPattern, text_var, ignore.case = TRUE))
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
                        hoverinfo = "text", marker = list(size = size), height = 600) %>%
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
        dplyr::select(`Text` = text_var,
                      `Colour Variable` = colour_var, ..., !!sentiment_sym)

      DT::datatable(df, filter = "top", options = list(pageLength = 25,
                                                       dom = '<"top" ifp> rt<"bottom"lp>', autoWidth = FALSE), #TODO check adding l worked
                    style = "bootstrap", rownames = FALSE,
                    escape = FALSE) #Add escape = False to ensure the HTML clicks work properly
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

    delayedTokenHex <- shiny::reactive({
      input$tokenHex
    }) %>%
      shiny::debounce(2000)

    #--- Reactive plots + Observes ----
    shiny::observeEvent(plotly::event_data("plotly_selected"),{
      output$sentimentPlot <- shiny::renderPlot({
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
    #---- Token plot ----
    shiny::observeEvent(plotly::event_data("plotly_selected"),{
      output$tokenPlot <- shiny::renderPlot({
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
    #---- Volume Plot ----
    shiny::observeEvent(plotly::event_data("plotly_selected"),{
      output$volumePlot <- shiny::renderPlot({

        vol_data <- df_filtered %>%
          dplyr::filter(date >= input$dateRange[[1]], date <= input$dateRange[[2]])

        vol_plot <- vol_data %>%
          .plot_volume_over_time(date = date, unit =  input$dateBreak, fill = input$volumeHex) +
          ggplot2::labs(title = paste0(input$volumeTitle),
                        caption = paste0(input$volumeCaption),
                        subtitle = paste0(input$volumeSubtitle),
                        x = paste0(input$volumeXlabel),
                        y = paste0(input$volumeYlabel))

        if(!input$dateSmooth == "none"){
          if(input$smoothSe == "FALSE"){
            vol_plot <- vol_plot +
              ggplot2::geom_smooth(method = input$dateSmooth, se = FALSE, colour = input$smoothColour)
          }else {
            vol_plot <- vol_plot+
              ggplot2::geom_smooth(method = input$dateSmooth, colour = input$smoothColour)
          }
        }


        return(vol_plot)


      }, res = 100,
      width = function() input$volumeWidth,
      height = function() input$volumeHeight)
    })

    #Render UI plot controls ----


    output$smoothControls <- shiny::renderUI({
      if(input$dateSmooth != "none"){
        shiny::tagList(
          shiny::selectInput("smoothSe", "show standard error?", choices = c("TRUE", "FALSE"), selected = "TRUE"),
          shiny::textInput("smoothColour", "Smooth colour", value ="#000000")
        )
      }

    })

    #Make delete button disappear when nothing selected
    output$deleteme <- shiny::renderUI({
      if(length(selected_range() > 1)){
        shiny::tagList(
          shiny::actionButton("delete", "Delete selections", class = 'btn-warning', style = "position: absolute; bottom 7px; right: 7px; background: #ff4e00; border-radius: 100px; color: #ffffff; border:none;")
        )

      }
    })

    output$volumeTitles <- shiny::renderUI({
      if(input$toggleVolumetitles == "TRUE"){
        shiny::tagList(
          shiny::textInput(inputId = "volumeTitle", label = "Title",
                           placeholder = "Write title here...", value = ""),
          shiny::textInput(inputId = "volumeSubtitle", label = "Subtitle",
                           placeholder = "Write subtitle here...", value = ""),
          shiny::textInput(inputId = "volumeCaption", label = "Caption",
                           placeholder = "Write caption here...", value = ""),
          shiny::textInput(inputId = "volumeXlabel", label = "X axis title",
                           placeholder = "Write the x axis title here..."),
          shiny::textInput(inputId = "volumeYlabel", label = "Y axis title",
                           placeholder = "Write the y axis title here")
        )
      }
    })

    output$sentimentTitles <- shiny::renderUI({
      if(input$toggleSentimenttitles == "TRUE"){
        shiny::tagList(
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
        )

      }
    })
    output$tokenTitles <- shiny::renderUI({
      if(input$toggleTokentitles == "TRUE"){
        shiny::tagList(
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
        )

      }
    })

    #---- Bigram Plot ----
    shiny::observeEvent(plotly::event_data("plotly_selected"),{
      output$bigramPlot <- shiny::renderPlot({
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
    #---- Download boxes for plots ----
    download_box <- function(exportname, plot) {
      shiny::downloadHandler(
        filename = function() {
          paste(exportname, Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          ggplot2::ggsave(file, plot = plot, device = "png", width = 8)
        }
      )
    }
    output$saveVolume <- download_box("volume_plot", volume_reactive())
    output$saveToken <- download_box("token_plot", token_reactive())
    output$saveSentiment <- download_box("sentiment_plot", sentiment_reactive())
  }
  #---- hide app render ----
  shiny::shinyApp(ui, server)
}
