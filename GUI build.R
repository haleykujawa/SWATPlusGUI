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
library(stringr)
library(here)


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
                   fluidRow(column(4,numericInput("CSFT", label = "Corn Bean - Full Tillage", value = 21)), 
                            column(4,numericInput("CSFT_B", label = "Vegetated buffer rate", value = 0)),
                            column(4,numericInput("CSFT_GW", label = "Grassed waterway rate", value = 0))),
                   
                   fluidRow(column(4,numericInput("CSNT", label = "Corn Bean - No Till", value = 40)),
                            column(4,numericInput("CSNT_B", label = "Vegetated buffer rate", value = 0)),
                            column(4,numericInput("CSNT_GW", label = "Grassed waterway rate", value = 0))),
                   
                   fluidRow(column(4,numericInput("CSRot", label = "Corn Bean - Rotational No Till", value = 15)),
                            column(4,numericInput("CSRot_B", label = "Vegetated buffer rate", value = 0)),
                            column(4,numericInput("CSRot_GW", label = "Grassed waterway rate", value = 0))),
                   
                   fluidRow(column(4,numericInput("CSRT", label = "Corn Bean - Reduced Till", value = 4)),
                            column(4,numericInput("CSRT_B", label = "Vegetated buffer rate", value = 0)),
                            column(4,numericInput("CSRT_GW", label = "Grassed waterway rate", value = 0))),
                   
                   fluidRow(column(4,numericInput("CSNTcc", label = "Corn Bean - No Till with rye cover crop", value = 10)),
                            column(4,numericInput("CSNTcc_B", label = "Vegetated buffer rate", value = 0)),
                            column(4,numericInput("CSNTcc_GW", label = "Grassed waterway rate", value = 0))),
                   
                   fluidRow(column(4,numericInput("CSWS", label = "Corn Bean Wheat /Double crop bean", value = 9)),
                            column(4,numericInput("CSWS_B", label = "Vegetated buffer rate", value = 0)),
                            column(4,numericInput("CSWS_GW", label = "Grassed waterway rate", value = 0))),
                   
                   fluidRow(column(4,numericInput("CSWcc", label = "Corn Bean Wheat /rye cover crop", value = 1)),
                            column(4,numericInput("CSWcc_B", label = "Vegetated buffer rate", value = 0)),
                            column(4,numericInput("CSWcc_GW", label = "Grassed waterway rate", value = 0))),
        
                   
                   
                   
                   
                            
                            
                   # numericInput("CSNT", label = "Corn Bean - No Till", value = 40),
                   # numericInput("CSRT", label = "Corn Bean - Reduced Till", value = 4),
                   # numericInput("CSRot", label = "Corn Bean - Rotational No Till", value = 15),
                   # numericInput("CSNTcc", label = "Corn Bean - Full No Till with rye cover crop", value = 10),
                   # numericInput("CSWS", label = "Corn Bean Wheat /Double crop bean", value = 9),
                   # numericInput("CSWcc", label = "Corn Bean Wheat /rye cover crop", value = 1),
                   
                   # numericInput("CSFT_B", label = "Corn Bean - Full Tillage - buffer rate", value = 0),
                   # numericInput("CSNT_B", label = "Corn Bean - No Till - buffer rate", value = 0),
                   # numericInput("CSRT_B", label = "Corn Bean - Reduced Till - buffer rate", value = 0),
                   # numericInput("CSRot_B", label = "Corn Bean - Rotational No Till - buffer rate", value = 0),
                   # numericInput("CSNTcc_B", label = "Corn Bean - Full No Till with rye cover crop - buffer rate", value = 0),
                   # numericInput("CSWS_B", label = "Corn Bean Wheat /Double crop bean - buffer rate", value = 0),
                   # numericInput("CSWcc_B", label = "Corn Bean Wheat /rye cover crop - buffer rate", value = 0),
                   
                   # numericInput("CSFT_GW", label = "Corn Bean - Full Tillage - grww rate", value = 0),
                   # numericInput("CSNT_GW", label = "Corn Bean - No Till - grww rate", value = 0),
                   # numericInput("CSRT_GW", label = "Corn Bean - Reduced Till - grww rate", value = 0),
                   # numericInput("CSRot_GW", label = "Corn Bean - Rotational No Till - grww rate", value = 0),
                   # numericInput("CSNTcc_GW", label = "Corn Bean - Full No Till with rye cover crop - grww rate", value = 0),
                   # numericInput("CSWS_GW", label = "Corn Bean Wheat /Double crop bean - grww rate", value = 0),
                   # numericInput("CSWcc_GW", label = "Corn Bean Wheat /rye cover crop - grww rate", value = 0),

                   
                   h5("Baseline rates of management:"),
                   p("Corn Bean - Full Tillage is 21%"), #maybe do /n to remove space between them in UI
                   p("Corn Bean - No Tillage is 40%"),
                   p("Corn Bean - Reduced Tillage is 4%"),
                   p("Corn Bean - Rotational No-Till is 15%"),
                   p("Corn Bean - No-Till with rye cover crop is 10%"),
                   p("Corn Bean Wheat /Double crop bean is 9%"),
                   p("Corn Bean Wheat /rye cover crop is 1%"),
                   

                   #ditch widget
                   fluidRow(column(4,sliderInput("ditch_rate", label = h3("Conservation ditches"), min = 0, 
                               max = 100, value = 10),
                   p("This changes the rate of conservation ditches on streams of order 1-2. Changing to 100% only changes 128 km (80 mi) of stream"))),
                   
                   
                   br(),br(),

                   
                   actionButton("simulate", "Apply changes"),
                   
                   actionButton("runswat", "Run OWC-SWAT+")
                   
                   
                   
                  , width = 6 ),
                   
      mainPanel(
        #### INPUTS ##################
        #Don't know if I need all this printed to the UI if it's already in the left hand panel, consider removing...
        #management scenarios
        textOutput("selected_CSFT_rate"),
        textOutput("selected_CSNT_rate"), 
        
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
      , width = 6) 
    )
  
)

# Define server logic ----
server <- function(input, output, session) {
  
  ###management scenarios###
  #print input management to UI
  output$selected_CSFT_rate <- renderText({paste0("Change Corn Soy - Full Tillage rate to ", input$CSFT ,"%") })
  output$selected_CSNT_rate <- renderText({paste0("Change Corn Soy - No Till rate to ", input$CSNT ,"%") })

  
  ###ditches###
  #print input ditch rate to UI
  output$selected_ditch_rate <- renderText({paste0("Change conservation ditch rate to ", input$ditch_rate ,"%") })  
  
  #run code to change all inputs
  observeEvent(input$simulate,ChangeSWATInputs(input$ditch_rate,input$CSFT,input$CSNT,input$CSRT,input$CSRot,
                                               input$CSNTcc,input$CSWS,input$CSWcc,
                                               
                                               input$CSFT_B,input$CSNT_B,input$CSRT_B,input$CSRot_B,
                                               input$CSNTcc_B,input$CSWS_B,input$CSWcc_B,
                                               
                                               input$CSFT_GW,input$CSNT_GW,input$CSRT_GW,input$CSRot_GW,
                                               input$CSNTcc_GW,input$CSWS_GW,input$CSWcc_GW))
  

 
  
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