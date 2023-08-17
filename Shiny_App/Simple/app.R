library(shiny)
library(readxl)
library(utils)
# library(rsconnect)

# runExample("10_download")

#### Data ####
# VXC <- read_excel("MergedModels/MergedModels_VXC.xlsx")
# VXP <- read_excel("MergedModels/MergedModels_VXP.xlsx")
# YCE <- read_excel("MergedModels/MergedModels_VPYC.xlsx")
# YCT <- read_excel("MergedModels/MergedModels_VP_YCT.xlsx")
# YCO2ND <- read_excel("MergedModels/MergedModels_VP_YCO2ND.xlsx")
# YCO2D <- read_excel("MergedModels/MergedModels_VP_YCO2D.xlsx")

# VXC <- read.csv("MergedModels\\MergedModels_VXC.csv")
# VXP <- read.csv(MergedModels_VP_YC,"MergedModels\\MergedModels_VXP.csv")
YCE<- read.csv("MergedModels/MergedModels_VPYC.csv")
YCT <- read.csv("MergedModels/MergedModels_VP_YCT.csv")
YCO2ND <- read.csv("MergedModels/MergedModels_VP_YCO2ND.csv")
YCO2D <- read.csv("MergedModels/MergedModels_VP_YCO2D.csv")

#### User Interface ####

ui <- fluidPage(

  # App title ----
  titlePanel("RothCO2 Data Downloader"),



  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Choose dataset ----
      selectInput("dataset_MOD", "Choose a dataset:",
                  choices = c(

      # "Complete Continuous Models (VXC)",
      # "Complete One-Off Models (VXP)",
      "Carbon Emissions (CE)",
      "Carbon Tails (YCT)",
      "CO2 emissions without delay (GWP100)",
      "CO2 emissions with delay (GWP100)"

                  )),

      selectInput("dataset_CON", "Choose a dataset:",
                  choices = c(

       # "Complete Continuous Models (VXC)",
       # "Complete One-Off Models (VXP)",
       "Spain",
       "Denmark",
       "Norway",
       "Germany"

                  )),
      # Button

      downloadButton("downloadData", "Download")

    )),

    # Main panel for displaying outputs ----
    mainPanel(

      tableOutput("table")

    )

  )


#### Server ####

server <- function(input, output) {

  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset_MOD,

           # "Complete Continuous Models (VXC)" = VXC,
           # "Complete One-Off Models (VXP)" = VXP,
           "Carbon Emissions (CE)" = YCE,
           "Carbon Tails (YCT)" = YCT,
           "CO2 emissions without delay (GWP100)" = YCO2ND,
           "CO2 emissions with delay (GWP100)" = YCO2D
           )
  })

  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )

}

shinyApp(ui, server)
# rsconnect::deployApp('Shiny_App')
