library(shiny)
library(readxl)
library(utils)
# library(rsconnect)

# runExample("10_download")

#### Data ####

## Data for UA - Ukraine
YCE_UA <- read_excel("MergedModels_UA/MergedModels_VPYC.xlsx")
YCT_UA <- read_excel("MergedModels_UA/MergedModels_VP_YCT.xlsx")
YCO2ND_UA <- read_excel("MergedModels_UA/MergedModels_VP_YCO2ND.xlsx")
YCO2D_UA <- read_excel("MergedModels_UA/MergedModels_VP_YCO2D.xlsx")

## Data for BE - Belgium
YCE_BE <- read_excel("MergedModels_BE/MergedModels_VPYC.xlsx")
YCT_BE <- read_excel("MergedModels_BE/MergedModels_VP_YCT.xlsx")
YCO2ND_BE <- read_excel("MergedModels_BE/MergedModels_VP_YCO2ND.xlsx")
YCO2D_BE <- read_excel("MergedModels_BE/MergedModels_VP_YCO2D.xlsx")

## Data for CH - Switzerland
YCE_CH <- read_excel("MergedModels_CH/MergedModels_VPYC.xlsx")
YCT_CH <- read_excel("MergedModels_CH/MergedModels_VP_YCT.xlsx")
YCO2ND_CH <- read_excel("MergedModels_CH/MergedModels_VP_YCO2ND.xlsx")
YCO2D_CH <- read_excel("MergedModels_CH/MergedModels_VP_YCO2D.xlsx")

## Data for DE - Germany
YCE_DE <- read_excel("MergedModels_DE/MergedModels_VPYC.xlsx")
YCT_DE <- read_excel("MergedModels_DE/MergedModels_VP_YCT.xlsx")
YCO2ND_DE <- read_excel("MergedModels_DE/MergedModels_VP_YCO2ND.xlsx")
YCO2D_DE <- read_excel("MergedModels_DE/MergedModels_VP_YCO2D.xlsx")

## Data for DK - Denmark
YCE_DK <- read_excel("MergedModels_DK/MergedModels_VPYC.xlsx")
YCT_DK <- read_excel("MergedModels_DK/MergedModels_VP_YCT.xlsx")
YCO2ND_DK <- read_excel("MergedModels_DK/MergedModels_VP_YCO2ND.xlsx")
YCO2D_DK <- read_excel("MergedModels_DK/MergedModels_VP_YCO2D.xlsx")

## Data for ES - Spain
YCE_ES <- read_excel("MergedModels_ES/MergedModels_VPYC.xlsx")
YCT_ES <- read_excel("MergedModels_ES/MergedModels_VP_YCT.xlsx")
YCO2ND_ES <- read_excel("MergedModels_ES/MergedModels_VP_YCO2ND.xlsx")
YCO2D_ES <- read_excel("MergedModels_ES/MergedModels_VP_YCO2D.xlsx")

## Data for FI - Finland
YCE_FI <- read_excel("MergedModels_FI/MergedModels_VPYC.xlsx")
YCT_FI <- read_excel("MergedModels_FI/MergedModels_VP_YCT.xlsx")
YCO2ND_FI <- read_excel("MergedModels_FI/MergedModels_VP_YCO2ND.xlsx")
YCO2D_FI <- read_excel("MergedModels_FI/MergedModels_VP_YCO2D.xlsx")

## Data for FR - France
YCE_FR <- read_excel("MergedModels_FR/MergedModels_VPYC.xlsx")
YCT_FR <- read_excel("MergedModels_FR/MergedModels_VP_YCT.xlsx")
YCO2ND_FR <- read_excel("MergedModels_FR/MergedModels_VP_YCO2ND.xlsx")
YCO2D_FR <- read_excel("MergedModels_FR/MergedModels_VP_YCO2D.xlsx")

## Data for IT - Italy
YCE_IT <- read_excel("MergedModels_IT/MergedModels_VPYC.xlsx")
YCT_IT <- read_excel("MergedModels_IT/MergedModels_VP_YCT.xlsx")
YCO2ND_IT <- read_excel("MergedModels_IT/MergedModels_VP_YCO2ND.xlsx")
YCO2D_IT <- read_excel("MergedModels_IT/MergedModels_VP_YCO2D.xlsx")

## Data for NL - The Netherlands
YCE_NL <- read_excel("MergedModels_NL/MergedModels_VPYC.xlsx")
YCT_NL <- read_excel("MergedModels_NL/MergedModels_VP_YCT.xlsx")
YCO2ND_NL <- read_excel("MergedModels_NL/MergedModels_VP_YCO2ND.xlsx")
YCO2D_NL <- read_excel("MergedModels_NL/MergedModels_VP_YCO2D.xlsx")

## Data for NO - Norway
YCE_NO <- read_excel("MergedModels_NO/MergedModels_VPYC.xlsx")
YCT_NO <- read_excel("MergedModels_NO/MergedModels_VP_YCT.xlsx")
YCO2ND_NO <- read_excel("MergedModels_NO/MergedModels_VP_YCO2ND.xlsx")
YCO2D_NO <- read_excel("MergedModels_NO/MergedModels_VP_YCO2D.xlsx")

## Data for PT - Portugal
YCE_PT <- read_excel("MergedModels_PT/MergedModels_VPYC.xlsx")
YCT_PT <- read_excel("MergedModels_PT/MergedModels_VP_YCT.xlsx")
YCO2ND_PT <- read_excel("MergedModels_PT/MergedModels_VP_YCO2ND.xlsx")
YCO2D_PT <- read_excel("MergedModels_PT/MergedModels_VP_YCO2D.xlsx")

## Data for RS - Russia
YCE_RS <- read_excel("MergedModels_RS/MergedModels_VPYC.xlsx")
YCT_RS <- read_excel("MergedModels_RS/MergedModels_VP_YCT.xlsx")
YCO2ND_RS <- read_excel("MergedModels_RS/MergedModels_VP_YCO2ND.xlsx")
YCO2D_RS <- read_excel("MergedModels_RS/MergedModels_VP_YCO2D.xlsx")

## Data for SE - Sweden
YCE_SE <- read_excel("MergedModels_SE/MergedModels_VPYC.xlsx")
YCT_SE <- read_excel("MergedModels_SE/MergedModels_VP_YCT.xlsx")
YCO2ND_SE <- read_excel("MergedModels_SE/MergedModels_VP_YCO2ND.xlsx")
YCO2D_SE <- read_excel("MergedModels_SE/MergedModels_VP_YCO2D.xlsx")

## Data for UK - United Kingdom
YCE_UK <- read_excel("MergedModels_UK/MergedModels_VPYC.xlsx")
YCT_UK <- read_excel("MergedModels_UK/MergedModels_VP_YCT.xlsx")
YCO2ND_UK <- read_excel("MergedModels_UK/MergedModels_VP_YCO2ND.xlsx")
YCO2D_UK <- read_excel("MergedModels_UK/MergedModels_VP_YCO2D.xlsx")


#### User Interface ####

ui <- fluidPage(

  # App title ----
  titlePanel("RothCO2 Data Downloader - Different Countries"),



  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Choose dataset ----
      selectInput("country", "Select a country:",
                  choices = c("UA - Ukraine",
                              "BE - Belgium",
                              "CH - Switzerland",
                              "DE - Germany",
                              "DK - Denmark",
                              "ES - Spain",
                              "FI - Finland",
                              "FR - France",
                              "IT - Italy",
                              "NL - The Netherlands",
                              "NO - Norway",
                              "PT - Portugal",
                              "RS - Russia",
                              "SE - Sweden",
                              "UK - United Kingdom")
                  ),

       selectInput("dataset", "Choose a dataset:",
                   choices = c(
                     "Carbon Emissions (CE)",
                     "Carbon Tails (YCT)",
                     "CO2 emissions without delay (GWP100)",
                     "CO2 emissions with delay (GWP100)"
                  )),
      # Button

      downloadButton("downloadData", "Download")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      tableOutput("table")

    )
  )
)

#### Server ####

server <- function(input, output) {

  # Reactive value for selected dataset ----
  datasetInput <- reactive({
      #UA - UKRAINE
    if (input$country == "UA - Ukraine") {
    switch(input$dataset,
           "Carbon Emissions (CE)" = YCE_UA,
           "Carbon Tails (YCT)" = YCT_UA,
           "CO2 emissions without delay (GWP100)" = YCO2ND_UA,
           "CO2 emissions with delay (GWP100)" = YCO2D_UA)
      #BE - BELGIUM
    } else if (input$country == "BE - Belgium") {
    switch(input$dataset,
           "Carbon Emissions (CE)" = YCE_BE,
           "Carbon Tails (YCT)" = YCT_BE,
           "CO2 emissions without delay (GWP100)" = YCO2ND_BE,
           "CO2 emissions with delay (GWP100)" = YCO2D_BE)
      #CH - SWITZERLAND
    } else if (input$country == "CH - Switzerland") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_CH,
             "Carbon Tails (YCT)" = YCT_CH,
             "CO2 emissions without delay (GWP100)" = YCO2ND_CH,
             "CO2 emissions with delay (GWP100)" = YCO2D_CH)
      #ES - SPAIN
    } else if (input$country == "ES - Spain") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_ES,
             "Carbon Tails (YCT)" = YCT_ES,
             "CO2 emissions without delay (GWP100)" = YCO2ND_ES,
             "CO2 emissions with delay (GWP100)" = YCO2D_ES)
      #DE - GERMANY
    } else if (input$country == "DE - Germany") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_DE,
             "Carbon Tails (YCT)" = YCT_DE,
             "CO2 emissions without delay (GWP100)" = YCO2ND_DE,
             "CO2 emissions with delay (GWP100)" = YCO2D_DE)
      #DK - DENMARK
    } else if (input$country == "DK - Denmark") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_DK,
             "Carbon Tails (YCT)" = YCT_DK,
             "CO2 emissions without delay (GWP100)" = YCO2ND_DK,
             "CO2 emissions with delay (GWP100)" = YCO2D_DK)
      #FR - FRANCE
    } else if (input$country == "FR - France") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_FR,
             "Carbon Tails (YCT)" = YCT_FR,
             "CO2 emissions without delay (GWP100)" = YCO2ND_FR,
             "CO2 emissions with delay (GWP100)" = YCO2D_FR)
      #IT - ITALY
    } else if (input$country == "IT - Italy") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_IT,
             "Carbon Tails (YCT)" = YCT_IT,
             "CO2 emissions without delay (GWP100)" = YCO2ND_IT,
             "CO2 emissions with delay (GWP100)" = YCO2D_IT)
      #NL - THE NETHERLANDS
    } else if (input$country == "NL - Norway") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_NL,
             "Carbon Tails (YCT)" = YCT_NL,
             "CO2 emissions without delay (GWP100)" = YCO2ND_NL,
             "CO2 emissions with delay (GWP100)" = YCO2D_NL)
      #NO - NORWAY
    } else if (input$country == "NO - Norway") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_NO,
             "Carbon Tails (YCT)" = YCT_NO,
             "CO2 emissions without delay (GWP100)" = YCO2ND_NO,
             "CO2 emissions with delay (GWP100)" = YCO2D_NO)
      #SE - SWEDEN
    } else if (input$country == "SE - Sweden") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_SE,
             "Carbon Tails (YCT)" = YCT_SE,
             "CO2 emissions without delay (GWP100)" = YCO2ND_SE,
             "CO2 emissions with delay (GWP100)" = YCO2D_SE)
      #PT - PORTUGAL
    } else if (input$country == "PT - Portugal") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_PT,
             "Carbon Tails (YCT)" = YCT_PT,
             "CO2 emissions without delay (GWP100)" = YCO2ND_PT,
             "CO2 emissions with delay (GWP100)" = YCO2D_PT)
      #RS - RUSSIA
    } else if (input$country == "RS - Russia") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_RS,
             "Carbon Tails (YCT)" = YCT_RS,
             "CO2 emissions without delay (GWP100)" = YCO2ND_RS,
             "CO2 emissions with delay (GWP100)" = YCO2D_RS)
      #UK - UNITED KINGDOM
    } else if (input$country == "UK - United Kingdom") {
      switch(input$dataset,
             "Carbon Emissions (CE)" = YCE_UK,
             "Carbon Tails (YCT)" = YCT_UK,
             "CO2 emissions without delay (GWP100)" = YCO2ND_UK,
             "CO2 emissions with delay (GWP100)" = YCO2D_UK)

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
# rsconnect::deployApp('Shiny_App/Complex')
