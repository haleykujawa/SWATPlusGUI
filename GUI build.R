#GUI for OWC-SWAT+

library(shiny)
library(readtext)
library(dplyr)
library(splitstackshape)
library(stringr)
library(shinyjs)
library(tictoc)
library(rlang)
library(reshape2)
library(tidyr)
library(rapportools)
library(ggplot2)


# Load scripts to be used ---
source("Change_rot_dist.R")
source("ChangeSWATInputs.R") #changed from improve ditch params
source("TestShiny.R")
source("Reset_scenario.R")
source("RunAllScripts_SWATv60.5.2.R")
source("testGUI.R")
source("ReadHRU_losses.R")
source("ReadChannel_daily2.R")

run_yrs<-c(2009)


# Load needed data ---
rotations <- read.csv("data/LumAreaSummary.csv")
baseline_data_avg<-read.csv("data/baseline_data_avg.csv")

baseline_dir<- paste0(getwd(),"/baseline")
scenario_dir <- paste0(getwd(),"/scenario")


# Define UI ----
ui <- fluidPage(
    titlePanel("OWC-SWAT+"),
    
    sidebarLayout(                   
      sidebarPanel(actionButton("cleardir", "Clear scenario"),
                   br(),br(),br(),
                   
                   
                   
                   #management widgets       
                   numericInput("CSFT", label = "Corn Soy - Full Tillage", value = 21),
                   hr(),
                   fluidRow(column(3, verbatimTextOutput("value"))),
                   p("Baseline rate of Corn Soy - Full Tillage is 21%"),
                   

                   #ditch widget
                   sliderInput("ditch_rate", label = h3("Conservation ditches"), min = 0, 
                               max = 100, value = 10),
                   p("This changes the rate of conservation ditches on streams of order 1-2. Changing to 100% only changes 128 km (80 mi) of stream"),
                   
                   
                   br(),br(),

                   
                   actionButton("simulate", "Apply changes"),
                   
                   actionButton("runswat", "Run OWC-SWAT+")
                   
                   
                   
                   ),
                   
      mainPanel(
        #### INPUTS ##################
        #management scenarios
        textOutput("selected_CSFT_rate"),
        
        #ditches
        textOutput("selected_ditch_rate"),
        
        p(),p(),p(),
        

        
        #### OUTPUTS ##################
        uiOutput("runningmodel"),
        p(),p(),p(),
        actionButton("HRUlossapply", "Nutrient and sediment loss from row crops"),
        tableOutput("HRUloss_reactive"),
        actionButton("ChangeAtOutlet","Changes in streamflow and water quality"),
        plotOutput("ChangeAtOutlet_reactive")
        
        #This is where you can place the output of the script, see https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/ for details
      ) 
    )
  
)

# Define server logic ----
server <- function(input, output, session) {
  
  ###management scenarios###
  #print input management to UI
  output$selected_CSFT_rate <- renderText({paste0("Change Corn Soy - Full Tillage rate to ", input$CSFT ,"%") })

  
  ###ditches###
  #print input ditch rate to UI
  output$selected_ditch_rate <- renderText({paste0("Change conservation ditch rate to ", input$ditch_rate ,"%") })  
  
  #run code to change all inputs
  observeEvent(input$simulate,ChangeSWATInputs(input$ditch_rate,input$CSFT))
  

 
  
  #clear scenario inputs by copying baseline to scenario folder
  observeEvent(input$cleardir,  Reset_scenario(baseline_dir,scenario_dir)) 
# observeEvent(input$cleardir, {shinyjs::reset("side-panel")})
 
 
 #observeEvent(input$runswat,  RunAllScripts_SWATv60.5.2(scenario_dir))
 #output$runningmodel<-renderText({ 
 #  bindEvent(input$runswat , RunAllScripts_SWATv60.5.2(scenario_dir) )
 #  })
 #output$runningmodel<-renderText({RunAllScripts_SWATv60.5.2(scenario_dir)})
 #observeEvent(input$runswat,  RunAllScripts_SWATv60.5.2(scenario_dir))

###################################################
#THIS CODE BELOW WORKS 
# text_reactive <- eventReactive( input$runswat, {
#   testGUI()
# })
 
# text output
# output$runningmodel <- renderText({
#   text_reactive()
# })
###################################################
 
 text_reactive <- eventReactive( input$runswat, {
   showModal(modalDialog("Running SWAT+", footer=NULL)) 
   RunAllScripts_SWATv60.5.2(scenario_dir)
   removeModal()
   testGUI()

 })
 
 # text output
 output$runningmodel <- renderUI({
   strong(text_reactive())
 })
 
 HRUloss_reactive<- eventReactive(input$HRUlossapply,{
                                  ReadHRU_losses(scenario_dir)
 })
 
 output$HRUloss_reactive <- renderTable({
   HRUloss_reactive()

 })
 
ChangeAtOutlet_reactive<- eventReactive(input$ChangeAtOutlet,{
   ReadChannel_daily2(run_yrs,baseline_data_avg,scenario_dir)
 })

output$ChangeAtOutlet_reactive <- renderPlot({
  ChangeAtOutlet_reactive()
  
})

 
}

# Run the app ----
shinyApp(ui = ui, server = server)