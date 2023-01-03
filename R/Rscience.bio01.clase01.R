
Rscience.bio01.clase01 <- function(){

  # Librerias
  library(shiny)
  library(shinydashboard)
  # library(psych)
  library(colourpicker)

  library(tidyverse)
  # library(shiny)
  # library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(readxl)
  library(openxlsx)
  library("Rscience.base")


registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
if (is.null(data))
   NULL
 else
   list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Bio 01 - Clase 01"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Base", tabName = "base", icon = icon("house")),
      menuItem("Variables", tabName = "variables", icon = icon("house")),
      menuItem("Descriptiva General", tabName = "descriptiva", icon = icon("house")),
      menuItem("Graficos", tabName = "graficos", icon = icon("house"))


    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      tabItem(tabName = "base",
              h2("Base"),
              fileInput(inputId = "File1", label = "File", multiple = FALSE, accept = c(".xlsx")),
              selectInput(inputId = "Sheet1", label = "Select sheet", choices = NULL, selected = NULL),
              fluidRow(dataTableOutput("Data1"))



      ),
      tabItem(tabName = "variables",
              h1("Seleccion de Variables"),
              uiOutput("big_selector01")
      ),
      tabItem(tabName = "descriptiva",
              h1("Descriptiva General"),

              h2("Medidas de Posición"),
              tableOutput("tabla_posicion"),
              h2("Medidas de Dispersión"),
              tableOutput("tabla_dispersion"),
              h2("Medidas Resumen"),
              tableOutput("tabla_resumen")


      ),


      # First tab content
      tabItem(tabName = "graficos",
              h2("Gráficos"),
              "Son todos los gráficos que se pueden realizar para 1 variable cuantitativa.",
              uiOutput("big_selector02"),
              uiOutput("plots2"),

      )

    )
  )
)

server <- function(input, output) {


  observeEvent(input$File1, {

    sheet_names <- readxl::excel_sheets(input$File1$datapath)

    shiny::updateSelectInput(
      inputId = "Sheet1",
      choices = sheet_names,
      selected = sheet_names[[1]]
    )

  }) # %>%
    # bindEvent(input$File1)


  # When the drop down meny is populated, read the selected sheet from the Excel
  # file
  thedata <- reactive({

    req(input$Sheet1)

    # readxl::read_xlsx(input$File1$datapath, sheet = input$Sheet1)
    openxlsx::read.xlsx(input$File1$datapath, sheet = input$Sheet1)
  })

  output$big_selector01 <- renderUI({
    # chooserInput(inputId = "selected_vars",
    #              leftLabel = "Variables",
    #              leftChoices = colnames(thedata()), rightLabel = "Seleccion",
    #              rightChoices = c(),
    #              multiple = T
    #              )

    the_vector <- 1:length(colnames(thedata()))
    names(the_vector) <- colnames(thedata())

    pickerInput(
      inputId = "selected_vars", label = "Selected",
      # choices = split(c("Choice 1" = "Value 1", "Choice 2" = "Value 2"), c("First", "Other")),
      choices = the_vector,
      multiple = TRUE,
      options = list( `live-search` = TRUE),
      choicesOpt = list(
        tokens = c("first choice 1", "other choice 2")
      )
    )
  })



  output$big_selector02 <- renderUI({

    the_vector <- 1:length(colnames(thedata()))
    names(the_vector) <- colnames(thedata())

    pickerInput(
      inputId = "selected_vars_graph", label = "Selected",
      # choices = split(c("Choice 1" = "Value 1", "Choice 2" = "Value 2"), c("First", "Other")),
      choices = the_vector,
      multiple = FALSE,
      options = list( `live-search` = TRUE),
      choicesOpt = list(
        tokens = c("first choice 1", "other choice 2")
      )
    )
  })


  output$Data1 <-
    renderDataTable(
      thedata()
      , extensions = "Buttons"
      , options = list(
        dom = "Bfrtip"
        , buttons = c("copy", "csv", "excel", "pdf", "print")
      )
    )

  ################################################################################

  tabla_n_resumen <- reactive({

    base <- the_data()
    tabla_n_resumen <- n_resumen(base = base)
    tabla_n_resumen

  })

  tabla_posicion <- reactive({

    # cat(input$selected_vars)
    # cat(is.vector(input$selected_vars))
    # cat(is.numeric(input$selected_vars))
    base <- thedata()
    selected_columns = as.numeric(input$selected_vars)
    # selected_columns = c(1,3, 5)
    digits = 2

    # Medidas de posicion
    tabla_posicion <- MedidasPosicion(base = base,
                                      selected_columns = selected_columns,
                                      digits = digits)

    tabla_posicion

  })

  tabla_dispersion <- reactive({

    base <- thedata()
    selected_columns = as.numeric(input$selected_vars)
    # selected_columns = c(1,3, 5)
    digits = 2

    # Medidas de posicion
    tabla_dispersion <- MedidasDispersion(base = base,
                                          selected_columns = selected_columns,
                                          digits = digits)

    tabla_dispersion

  })

  tabla_resumen <- reactive({

    base <- thedata()
    selected_columns = as.numeric(input$selected_vars)
    # selected_columns = c(1,3, 5)
    digits = 2

    # Medidas de posicion
    tabla_resumen <- MedidasResumen(base = base,
                                    selected_columns = selected_columns,
                                    digits = digits)

    tabla_resumen

  })

  sentencias_graficas <- reactive({
    if(is.null(input$selected_vars_graph)) return(NULL)
    base <- thedata()
    # selected_columns = c(1,3, 5)
    selected_columns = as.numeric(input$selected_vars_graph)
    # selected_columns = 1
    digits = 2

    las_sentencias <- Graficos.1C(base = base, selected_columns = selected_columns,
                                  only_plot = FALSE)

    las_sentencias



  })

  output$tabla_posicion <- renderTable({
    tabla_posicion()
  })

  output$tabla_dispersion <- renderTable({
    tabla_dispersion()
  })

  output$tabla_resumen <- renderTable({
    tabla_resumen()
  })


  ##########################################

  # Insert the right number of plot output objects into the web page
  output$plots2 <- renderUI({
    plot_output_list <- lapply(1:length(sentencias_graficas()), function(i) {
      plotname <- paste("plot2", i, sep="")
      div(
        plotOutput(plotname, height = 280, width = 250), br())
    })

    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })

  observe({
    for (i in 1:length(sentencias_graficas())) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("plot2", my_i, sep="")

        output[[plotname]] <- renderPlot({
          base <- thedata()
          selected_columns = c(1)
          digits = 2

          eval(parse(text=sentencias_graficas()[my_i]))
        })
      })
    }
  })

}








shinyApp(ui, server)
}
