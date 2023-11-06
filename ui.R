# Define UI for application that draws a histogram
ui <- dashboardPage(

  #Skin of the website
  skin = "black",
  
  # HEADER ------------------------------------------------------------------
  
  dashboardHeader(
    title = span(img(src = "logo_full.png", height = 35)),
    titleWidth = 210,
    dropdownMenu(
      type = "notifications", 
      headerText = strong("HELP"), 
      icon = icon("info"), 
      badgeStatus = NULL,
      notificationItem(
        text = (steps$text[1]),
        icon = icon("magnifying-glass")
      ),
      notificationItem(
        text = steps$text[2],
        icon = icon("sliders")
      ),
      notificationItem(
        text = steps$text[3],
        icon = icon("list")
      ),
      notificationItem(
        text = tags$b(steps$text[4]),
        icon = icon("refresh")
      ),
      notificationItem(
        text = steps$text[5],
        icon = icon("spinner")
      ),
      notificationItem(
        text = steps$text[6],
        icon = icon("download")
      )
    ),
    tags$li(
      a(
        strong("ABOUT NEMAMOD"),
        height = 40,
        href = "https://github.com/LidiaRipollSanchez/Neuropeptide-Connectome.git",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),
  
  # SIDEBAR -----------------------------------------------------------------
  #Sidebar with controls to select a dataset, a neuron and the right layout
  
  dashboardSidebar(
    width = 200,
    introBox(data.step = 1, data.intro = intro$text[1], #  intro tour
             sidebarMenu(
               menuItem(
                 "NETWORK BY NEURON",
                 tabName = "Network by neuron",
                 icon = icon("share-nodes"),
                 selectInput("node", "Select neuron:", choices = NeuronID$nodeLabel, multiple = TRUE, selected ="I1L"),
                 selectInput("system", "Select Neuropeptide-GPCR pair:", choices = c("All", ""), selected ="All"),
                 sliderTextInput("threshold", "Select diffusion threshold:", choices = c("Short", "Mid", "Long"),  selected = "Short"),
                 div(style="display:inline-block;margin-left: 27%;", 
                     actionButton(inputId = "refresh", label = "Refresh", icon = icon("refresh"), style = "primary")),
                 selectInput("layout", "Select Network layout:", choices = c("Circle", "Force-directed", "Hierarchical"))
               ),
               menuItem(
                 "NETWORK BY GENE",
                 tabName = "Network by gene",
                 icon = icon("dna"),
                 selectInput("ligand", "Select ligand:", choices = Neuropeptides, multiple = FALSE, selected ="NLP_40"),
                 selectInput("receptor", "Select receptor:", choices = c("All", ""), multiple = FALSE, selected ="All"),
                 sliderTextInput("threshold2", "Select diffusion threshold:", choices = c("Short", "Mid", "Long"),  selected = "Short"),
                 div(style="display:inline-block;margin-left: 27%;", 
                     actionButton(inputId = "refresh2", label = "Refresh", icon = icon("refresh"), style = "primary")),
                 selectInput("layout2", "Select Network layout:", choices = c("Circle", "Force-directed", "Hierarchical"))
               )
      )
    )),

  # BODY --------------------------------------------------------------------
  
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "nemamode.css")
    ),
    
    useShinyjs(),
    introjsUI(),
    
    # MAIN BODY ---------------------------------------------------------------
    
    fluidRow(
      column(
        width = 12,
        introBox(
          bsButton("newplot", label = "BY NEURON", 
                   icon = icon("share-nodes"),
                   style = "lidia", type = "action"),
          bsButton("newplot2", label = "BY GENE", 
                   icon = icon("dna"),
                   style = "lidia", type = "action"))
      ),
      fluidRow(
        div(
          id = "network_panel",
          style = "position: relative; backgroundColor: #ecf0f5",
          tabBox(
            width = 12,
            height = "100vh", 
            tabPanel( 
              title = "Network",
              width =12, 
              withSpinner(
                visNetworkOutput("network", height = "85vh"),
                type = 4,
                color = "#80c7f2", 
                size = 0.7 
              )
            ),
            tabPanel(
              title = "Data Table",
              width =12,
              withSpinner(
                DT::dataTableOutput("network1_table"),
                type = 4,
                color = "#80c7f2",
                size = 0.7
              )
            )
          )
        ),
        div(
          id = "network2_panel",
          style = "position: relative; backgroundColor: #ecf0f5",
          tabBox(
            width = 12,
            height = "100vh", 
            tabPanel( 
              title = "Network",
              width =12, 
              withSpinner(
                visNetworkOutput("network2", height = "85vh"),
                type = 4,
                color = "#80c7f2", 
                size = 0.7 
              )
            ),
            tabPanel(
              title = "Data Table",
              width =12,
              withSpinner(
                DT::dataTableOutput("network2_table"),
                type = 4,
                color = "#80c7f2",
                size = 0.7
              )
            )
          )
        )
      ),
      fluidRow(
        div(
          id = "footnote",
          style = "position: relative; backgroundColor: #ecf0f5",
          box(
            width = 12, 
            uiOutput("textfooter")
          )
        )
      )
    )
  )
)
