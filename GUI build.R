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
                   numericInput("CSFT", label = "Corn Bean - Full Tillage", value = 21),
                   numericInput("CSNT", label = "Corn Bean - No Till", value = 40),
                   numericInput("CSRT", label = "Corn Bean - Reduced Till", value = 4),
                   numericInput("CSRot", label = "Corn Bean - Rotational No Till", value = 15),
                   numericInput("CSNTcc", label = "Corn Bean - Full No Till with rye cover crop", value = 10),
                   numericInput("CSWS", label = "Corn Bean Wheat /Double crop bean", value = 9),
                   numericInput("CSWcc", label = "Corn Bean Wheat /rye cover crop", value = 1),

                   
                   h5("Baseline rates of management:"),
                   p("Corn Bean - Full Tillage is 21%"), #maybe do /n to remove spave netween them in UI
                   p("Corn Bean - No Tillage is 40%"),
                   p("Corn Bean - Reduced Tillage is 4%"),
                   p("Corn Bean - Rotational No-Till is 15%"),
                   p("Corn Bean - No-Till with rye cover crop is 10%"),
                   p("Corn Bean Wheat /Double crop bean is 9%"),
                   p("Corn Bean Wheat /rye cover crop is 1%"),
                   

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
      ) 
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
                                               input$CSNTcc,input$CSWS,input$CSWcc))
  

 
  
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