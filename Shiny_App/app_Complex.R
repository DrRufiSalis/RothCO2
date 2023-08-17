library(shiny)
library(readxl)
library(utils)
# library(rsconnect)

# runExample("10_download")

#### Data ####
## Data for Spain
YCE <- read_excel("MergedModels/MergedModels_VPYC.xlsx")
YCT <- read_excel("MergedModels/MergedModels_VP_YCT.xlsx")
YCO2ND <- read_excel("MergedModels/MergedModels_VP_YCO2ND.xlsx")
YCO2D <- read_excel("MergedModels/MergedModels_VP_YCO2D.xlsx")


#### User Interface ####

ui <- fluidPage(

  # App title ----
  titlePanel("RothCO2 Data Downloader"),



  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Choose dataset ----
      selectInput("country", "Select a country:",
                  choices = c(

                    # "Complete Continuous Models (VXC)",
                    # "Complete One-Off Models (VXP)",
                    "Spain",
                    "Denmark",
                    "Norway",
                    "Germany"

                  )),

       selectInput("dataset", "Choose a dataset:",
                   choices = c(

                     # "Complete Continuous Models (VXC)",
                     # "Complete One-Off Models (VXP)",
                     "Carbon Emissions (CE)",
                     "Carbon Tails (YCT)",
                     "CO2 emissions without delay (GWP100)",
                     "CO2 emissions with delay (GWP100)"

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
    if (input$country == "Spain") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_ES,
             "Carbon Tails (YCT)" = YCT_ES,
             "CO2 emissions without delay (GWP100)" = YCO2ND_ES,
             "CO2 emissions with delay (GWP100)" = YCO2D_ES)
    } else if (input$country == "Norway") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_NO,
             "Carbon Tails (YCT)" = YCT_NO,
             "CO2 emissions without delay (GWP100)" = YCO2ND_NO,
             "CO2 emissions with delay (GWP100)" = YCO2D_NO)
    }
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
