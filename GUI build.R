#GUI for OWC-SWAT+

library(shiny)
library(readtext)
library(splitstackshape)
library(shinyjs)
library(tictoc)
library(rlang)
library(reshape2)
library(rapportools)
library(stringr)
library(here)
library(ggpmisc) #for testPlot to combine table with plot
library(patchwork)
library(tidyverse) # commenting out all packages contained in the tidyverse
# library(ggplot2)
# library(tidyr)
# library(stringr)
# library(dplyr)

# Load scripts to be used ---
source("Change_rot_dist.R")
source("ChangeSWATInputs.R") #changed from improve ditch params
source("TestShiny.R")
source("Reset_scenario.R")
source("RunAllScripts_SWATv60.5.2.R")
source("testGUI.R")
source("ReadHRU_losses.R")
source("ReadChannel_daily2.R")
source("testPlot.R")
source("ChangeSWATClimate.R")

run_yrs<-c(2009)


# Load needed data ---
rotations <- read.csv("data/LumAreaSummary.csv")
baseline_data_avg<-read.csv("data/baseline_data_avg.csv")

baseline_dir<- paste0(getwd(),"/Baseline")
scenario_dir <- paste0(getwd(),"/Scenario")


# Define UI ----
ui <- fluidPage(
    titlePanel("OWC-SWAT+"),
    tabsetPanel(
      
      tabPanel("Information OWC-SWAT+", br(),br(), p("Information about OWC-SWAT+ here")),
      
      tabPanel("Change inputs",

    sidebarLayout(                   
      sidebarPanel(actionButton("cleardir", "Clear scenario"),
                   br(),
                   
                   
                   tabsetPanel(
                     tabPanel("Management and conservation practices",br(),br(),
                   #management widgets       
                   fluidRow(column(4,numericInput("CSFT", label = "Corn Bean - Full Tillage", value = 21)), 
                            column(4,numericInput("CSFT_B", label = "Vegetated buffer rate", value = 10)),
                            column(4,numericInput("CSFT_GW", label = "Grassed waterway rate", value = 10))),
                   
                   fluidRow(column(4,numericInput("CSNT", label = "Corn Bean - No Till", value = 40)),
                            column(4,numericInput("CSNT_B", label = "Vegetated buffer rate", value = 10)),
                            column(4,numericInput("CSNT_GW", label = "Grassed waterway rate", value = 10))),
                   
                   fluidRow(column(4,numericInput("CSRot", label = "Corn Bean - Rotational No Till", value = 15)),
                            column(4,numericInput("CSRot_B", label = "Vegetated buffer rate", value = 10)),
                            column(4,numericInput("CSRot_GW", label = "Grassed waterway rate", value = 10))),
                   
                   fluidRow(column(4,numericInput("CSRT", label = "Corn Bean - Reduced Till", value = 4)),
                            column(4,numericInput("CSRT_B", label = "Vegetated buffer rate", value = 10)),
                            column(4,numericInput("CSRT_GW", label = "Grassed waterway rate", value = 10))),
                   
                   fluidRow(column(4,numericInput("CSNTcc", label = "Corn Bean - No Till with rye cover crop", value = 10)),
                            column(4,numericInput("CSNTcc_B", label = "Vegetated buffer rate", value = 10)),
                            column(4,numericInput("CSNTcc_GW", label = "Grassed waterway rate", value = 10))),
                   
                   fluidRow(column(4,numericInput("CSWS", label = "Corn Bean Wheat /Double crop bean", value = 9)),
                            column(4,numericInput("CSWS_B", label = "Vegetated buffer rate", value = 10)),
                            column(4,numericInput("CSWS_GW", label = "Grassed waterway rate", value = 10))),
                   
                   fluidRow(column(4,numericInput("CSWcc", label = "Corn Bean Wheat /rye cover crop", value = 1)),
                            column(4,numericInput("CSWcc_B", label = "Vegetated buffer rate", value = 10)),
                            column(4,numericInput("CSWcc_GW", label = "Grassed waterway rate", value = 10))),
        
                   
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
                   p("This changes the rate of conservation ditches on streams of order 1-2. Changing to 100% only changes 128 km (80 mi) of stream"))) ,
                   
                   
                   actionButton("simulate", "Apply changes to management")
                   ),
                   
                   
                   
                   
                   tabPanel("Climate data",  
                            p("put climate inputs here"),  
                   
                    # selectInput("SelectClimateOption", label = h3("Choose climate data to run:"), 
                    # choices = list("Baseline climate (2013-2020)" = "nochange", "Climate models" = "climmod", "Current climate beyond 2020" = "extended"), 
                                        # selected = "nochange"),
                            
                  
                   checkboxGroupInput("SelectClimate", label = h5("Climate data to run:"), 
                                      choices = list("Historical (2013-2020)"="hist","CNRM"="CNRM", "MIROC5"="MIROC5", "IPSL-CM5A-MR"="IPSL-CM5A-MR"),
                                      selected = 0),
                   
                   #  going to remove this option for now and work to add it back in if needed
                   # fileInput("pcpFile", label = h5("If extending climate data beyond 2020, insert pcp here:")),
                   # fileInput("tmpFile", label = h5("If extending climate data beyond 2020, insert tmp here:")),
                   
                   # actionButton("ClimateApply", "Apply changes to climate"),
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   )),
                   
                   br(),br(),

                   
                   # actionButton("simulate", "Apply changes"),
                   
                   actionButton("runswat", "Run OWC-SWAT+")
                   
                   
                   
                  , width = 6 ),
                   
      mainPanel(
        #### INPUTS ##################
        #Don't know if I need all this printed to the UI if it's already in the left hand panel, consider removing...
        #management scenarios
        br(),
        strong("Rates of management on row crop lands:"),
        span(textOutput("total_rate"),style='color:green'),
        textOutput("cc_rate"),
        textOutput("winter_cover_rate"),
        textOutput("FT_rate"),
        textOutput("NT_rate"), 
        textOutput("RT_rate"),
        textOutput("Rot_rate"),

        
        p(),
        
        #ditches
        textOutput("selected_ditch_rate"),
        
        p(),p(),p(),
        
        textOutput("ClimateOut"),
        

        
        #### OUTPUTS ##################
        uiOutput("runningmodel1"),
        p(),p(),p(),
      
        
       


        
        
        #This is where you can place the output of the script, see https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/ for details
       width = 6) ) ),

tabPanel("Visualize outputs",
         br(),
         
         plotOutput("runningmodel2")
)

)

)

# Define server logic ----
server <- function(input, output, session) {
  
  ###management scenarios###
  #error message#
  output$total_rate<-reactive({validate(need((input$CSFT+input$CSNT+input$CSRT+input$CSRot+
           input$CSNTcc+input$CSWS+input$CSWcc) == 100, "Input management rates do not add up to 100% -- Please adjust before using 'Apply changes'"))
    paste0("Rate input ", input$CSFT+input$CSNT+input$CSRT+input$CSRot+input$CSNTcc+input$CSWS+input$CSWcc, "%")})
  
  #print input management to UI
  output$cc_rate <- renderText({paste0("Rye cover crops = ", input$CSNTcc + input$CSWcc ,"%") })
  output$winter_cover_rate <- renderText({paste0("Winter cover = ", input$CSNTcc + input$CSWcc + input$CSWS,"%") })
  output$FT_rate <- renderText({paste0("Full till management = ", input$CSFT ,"%") })
  output$NT_rate <- renderText({paste0("No till management = ", input$CSNT + input$CSNTcc + input$CSWcc + input$CSWS ,"%") })
  output$RT_rate <- renderText({paste0("Reduced till management = ", input$CSRT ,"%") })
  output$Rot_rate <- renderText({paste0("Rotational till management = ", input$CSRot ,"%") })
  
  # add other things from excel spreadsheet, like rates of subsurface placement, total N and total P applied 
  

  
  ###ditches###
  #print input ditch rate to UI
  output$selected_ditch_rate <- renderText({paste0("Conservation ditch rate = ", input$ditch_rate ,"%") })  
  
  #run code to change all inputs
  observeEvent(input$simulate,ChangeSWATInputs(input$ditch_rate,input$CSFT,input$CSNT,input$CSRT,input$CSRot,
                                               input$CSNTcc,input$CSWS,input$CSWcc,
                                               
                                               input$CSFT_B,input$CSNT_B,input$CSRT_B,input$CSRot_B,
                                               input$CSNTcc_B,input$CSWS_B,input$CSWcc_B,
                                               
                                               input$CSFT_GW,input$CSNT_GW,input$CSRT_GW,input$CSRot_GW,
                                               input$CSNTcc_GW,input$CSWS_GW,input$CSWcc_GW))
  

  # ClimateOut<-eventReactive(input$ClimateApply,ChangeSWATClimate(input$SelectClimateOption,input$SelectClimateModels,input$ClimateFile))
  # output$ClimateOut<-renderText({ClimateOut()[[1]]})

 
  
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

  #because of how climate is more straight forward as an input than management, maybe just set up climate when going to run SWAT
  #technically could also do this with management instead of having the button... then whatever management is in the UI when you hit "Run OWC-SWAT+" is the management
  #You have to stick with--benefit to having it outside is user can fiddle with outputs they want, benefits to inside is it's more clean
  #Maybe could have list of inputs that are non-reactive at the top of "visualize outputs" That way there's no confusion what inputs the outputs are reflective of
   
 text_reactive <- eventReactive( input$runswat, {
   showModal(modalDialog("Running SWAT+", footer=NULL)) 
   RunAllScripts_SWATv60.5.2(scenario_dir,input$SelectClimate)
   removeModal()
   testPlot()# testGUI()
 })
 
 # text output
  output$runningmodel1 <- renderUI({
     strong(text_reactive()[[2]])
  })
  
  # plot output
  output$runningmodel2 <- renderPlot({
  text_reactive()[[1]]
  })
 

}
# Run the app ----
shinyApp(ui = ui, server = server)