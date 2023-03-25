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
library(gridExtra) #arranges variable number of plots as "grobs"
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
scenario_dir <- paste0(getwd(),"/Scenarios")


# Define UI ----
ui <- fluidPage(
    titlePanel("OWC-SWAT+"),
    tabsetPanel(
      
      tabPanel("About OWC-SWAT+", br(),br(), p("Information about OWC-SWAT+ here"),br(),br(),
               img(src="owc_map.png",width=1430/2,height=1105/2),br(),p('Figure 1. Map of Old Woman Creek watershed and estuary'),
               br(),br(),br(),br(),
               img(src="old-woman-creek.png",height=503/4,width=800/4),
               img(src="davidson.png",height=117/2,width=432/2),
               img(src="osu.png",height=88/2,width=569/2)),
      
      tabPanel("Change inputs",

    sidebarLayout(                   
      sidebarPanel(#actionButton("cleardir", "Clear scenario"),
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
        
                   
                   # h5("Baseline rates of management:"),
                   # p("Corn Bean - Full Tillage is 21%"), #maybe do /n to remove space between them in UI
                   # p("Corn Bean - No Tillage is 40%"),
                   # p("Corn Bean - Reduced Tillage is 4%"),
                   # p("Corn Bean - Rotational No-Till is 15%"),
                   # p("Corn Bean - No-Till with rye cover crop is 10%"),
                   # p("Corn Bean Wheat /Double crop bean is 9%"),
                   # p("Corn Bean Wheat /rye cover crop is 1%"),
                   

                   #ditch widget
                   fluidRow(column(6,sliderInput("ditch_rate", label = h3("Conservation ditches"), min = 0, 
                               max = 100, value = 10),
                   p("This changes the rate of conservation ditches on streams of order 1-2. Changing to 100% only changes 128 km (80 mi) of stream"))) #,
                   
                   
                   # actionButton("simulate", "Apply changes to management")
                   ),
                   
                   
                   
                   
                   tabPanel("Climate data",  
                            p("put climate inputs here"),  
                   
                    # selectInput("SelectClimateOption", label = h3("Choose climate data to run:"), 
                    # choices = list("Baseline climate (2013-2020)" = "nochange", "Climate models" = "climmod", "Current climate beyond 2020" = "extended"), 
                                        # selected = "nochange"),
                            
                  
                   checkboxGroupInput("SelectClimate", label = h5("Climate data to run:"), 
                                      choices = list("Recent observed climate (2013-2020)"="hist","CNRM"="CNRM", "MIROC5"="MIROC", "IPSL-CM5A-MR"="IPSL","GFDL"="GFDL"),
                                      selected = "hist"),
                   
                   #  going to remove this option for now and work to add it back in if needed
                   # fileInput("pcpFile", label = h5("If extending climate data beyond 2020, insert pcp here:")),
                   # fileInput("tmpFile", label = h5("If extending climate data beyond 2020, insert tmp here:")),
                   
                   # actionButton("ClimateApply", "Apply changes to climate"),
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   )),
                   
                   br(),br(),

                   
                   # actionButton("simulate", "Apply changes"),
                   
                   actionButton("runswat", "Run OWC-SWAT+")
                   
                   
                   
                  , width = 8 ),
                   
      mainPanel(
        #### INPUTS ##################
        #Don't know if I need all this printed to the UI if it's already in the left hand panel, consider removing...
        #management scenarios
        br(),
        strong("Rates of management on row crop lands:"),
        span(textOutput("total_rate"),style='color:green'),
        span(textOutput("buff_grw_rate"),style='color:black'),
        
        textOutput("cc_rate"),
        textOutput("winter_cover_rate"),
        textOutput("FT_rate"),
        textOutput("NT_rate"), 
        textOutput("RT_rate"),
        textOutput("Rot_rate"),
        textOutput("N_incorp"),
        textOutput("P_incorp"),
        textOutput("N_applied"),
        textOutput("P_applied"),

        
        p(),
        
        #ditches
        textOutput("selected_ditch_rate"),
        
        p(),p(),p(),
        
        textOutput("ClimateOut"),
        

        
        #### OUTPUTS ##################
        uiOutput("runningmodel1"),
        p(),p(),p(),
      
        
       


        
        
        #This is where you can place the output of the script, see https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/ for details
       width = 4) ) ),

tabPanel("Visualize outputs",
         # br(),
         
         # imageOutput("runningmodel2"),
         plotOutput("BR_plot"),
         plotOutput("HRU_plot")
         # imageOutput("runningmodel3")
),

tabPanel("Documentation",
         
         # Insert pdf
         tags$iframe(style="height:1000px; width:100%; scrolling=yes",
                     src="OWC_info.pdf")
         
         )

)

)

# Define server logic ----
server <- function(input, output, session) {
  
  ###management scenarios###
  #error message#
  output$total_rate<-reactive({validate(need((input$CSFT+input$CSNT+input$CSRT+input$CSRot+
           input$CSNTcc+input$CSWS+input$CSWcc) == 100, "Input management rates do not add up to 100% -- Adjust before running SWAT+"))
    paste0("Management rate input is ", input$CSFT+input$CSNT+input$CSRT+input$CSRot+input$CSNTcc+input$CSWS+input$CSWcc, "%, ready to run!")})
  
  output$buff_grw_rate<-reactive({validate(need(( (input$CSFT_B + input$CSFT_GW) <= 100) & ((input$CSNT_B + input$CSNT_GW) <= 100) & ((input$CSRT_B + input$CSRT_GW) <= 100) & ((input$CSRot_B + input$CSRot_GW) <= 100) & ((input$CSNTcc_B + input$CSNTcc_GW) <= 100) & ((input$CSWS_B + input$CSWS_GW) <= 100), "Total rate of buffers and grassed waterways on one management scenario cannot be greater than 100% -- Adjust before running SWAT+"))
    paste0("Total input rate of vegetated buffers is ", input$CSFT*input$CSFT_B/100 +input$CSNT*input$CSNT_B/100+ input$CSRT*input$CSRT_B/100+ input$CSRot*input$CSRot_B/100+ input$CSNTcc*input$CSNTcc_B/100 +input$CSWS*input$CSWS_B/100 +input$CSWcc*input$CSWcc_B/100, '%',
           " and total input rate of grassed waterways is ", input$CSFT*input$CSFT_GW/100 +input$CSNT*input$CSNT_GW/100+ input$CSRT*input$CSRT_GW/100+ input$CSRot*input$CSRot_GW/100+ input$CSNTcc*input$CSNTcc_GW/100 +input$CSWS*input$CSWS_GW/100 +input$CSWcc*input$CSWcc_GW/100, '%')})
  
  #print input management to UI
  output$cc_rate <- renderText({paste0("Rye cover crops = ", input$CSNTcc + input$CSWcc ,"%") })
  output$winter_cover_rate <- renderText({paste0("Winter cover (rye + winter wheat) = ", input$CSNTcc + input$CSWcc + input$CSWS,"%") })
  output$FT_rate <- renderText({paste0("Full till management = ", input$CSFT ,"%") })
  output$NT_rate <- renderText({paste0("No till management = ", input$CSNT + input$CSNTcc + input$CSWcc + input$CSWS ,"%") })
  output$RT_rate <- renderText({paste0("Reduced till management = ", input$CSRT ,"%") })
  output$Rot_rate <- renderText({paste0("Rotational till management = ", input$CSRot ,"%") })
  
  # add other things from excel spreadsheet, like rates of subsurface placement, total N and total P applied 
  output$N_incorp<-renderText({paste0("Incorporated N (%) = ",input$CSFT*1+input$CSNT*0.97+input$CSRT*1+input$CSRot*1+input$CSNTcc*0.9+input$CSWS*0.69+input$CSWcc*0.64)})
  output$P_incorp<-renderText({paste0("Incorporated P (%) = ",input$CSFT*1+input$CSNT*0.1+input$CSRT*1+input$CSRot*1+input$CSNTcc*0.1+input$CSWS*0.55+input$CSWcc*0.69)}) 
  output$N_applied<-renderText({paste0("Average annual N applied (lb/acre) = ", round( (223*(input$CSFT/100)/2 +206*(input$CSNT/100)/2 + 223*(input$CSRT/100)/2 + 226*(input$CSRot/100)/2 +222*(input$CSNTcc/100)/2 + 296*(input$CSWS/100)/3 + 311*(input$CSWcc/100)/3) ,0))})
  output$P_applied<-renderText({paste0("Average annual P2O5 applied (lb/acre) = ", round( (118/2*(input$CSFT/100) + 118/2*(input$CSNT/100) + 118/2*(input$CSRT/100) + 116/2*(input$CSRot/100) + 116/2*(input$CSNTcc/100) + 174/3*(input$CSWS/100) + 168/3*(input$CSWcc/100)) ,0))})
  
  
  

  
  ###ditches###
  #print input ditch rate to UI
  output$selected_ditch_rate <- renderText({paste0("Conservation ditch rate = ", input$ditch_rate ,"%") })  
  
  #run code to change all inputs
  # observeEvent(input$simulate,ChangeSWATInputs(input$ditch_rate,input$CSFT,input$CSNT,input$CSRT,input$CSRot,
  #                                              input$CSNTcc,input$CSWS,input$CSWcc,
  #                                              
  #                                              input$CSFT_B,input$CSNT_B,input$CSRT_B,input$CSRot_B,
  #                                              input$CSNTcc_B,input$CSWS_B,input$CSWcc_B,
  #                                              
  #                                              input$CSFT_GW,input$CSNT_GW,input$CSRT_GW,input$CSRot_GW,
  #                                              input$CSNTcc_GW,input$CSWS_GW,input$CSWcc_GW))
  

  # ClimateOut<-eventReactive(input$ClimateApply,ChangeSWATClimate(input$SelectClimateOption,input$SelectClimateModels,input$ClimateFile))
  # output$ClimateOut<-renderText({ClimateOut()[[1]]})

 
  
  #clear scenario inputs by copying baseline to scenario folder
  # observeEvent(input$cleardir,  Reset_scenario(baseline_dir,scenario_dir)) 
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
   
  text_reactive <-eventReactive( input$runswat, {
  
   showModal(modalDialog("Running SWAT+", footer=NULL))
   RunAllScripts_SWATv60.5.2(scenario_dir,input$SelectClimate,input$ditch_rate,input$CSFT,input$CSNT,input$CSRT,input$CSRot,
                             input$CSNTcc,input$CSWS,input$CSWcc,
                             
                             input$CSFT_B,input$CSNT_B,input$CSRT_B,input$CSRot_B,
                             input$CSNTcc_B,input$CSWS_B,input$CSWcc_B,
                             
                             input$CSFT_GW,input$CSNT_GW,input$CSRT_GW,input$CSRot_GW,
                             input$CSNTcc_GW,input$CSWS_GW,input$CSWcc_GW)
   removeModal()
   testPlot(scenario_dir,input$SelectClimate)# testGUI()
   
   
 })
 
 # text output
 output$runningmodel1 <- renderUI({
 strong(text_reactive()[[3]])
 })

 #  # plot output
 #  output$runningmodel2 <- renderPlot({
 #  if(length(input$SelectClimate)==1){
 #                text_reactive()[[1]]}
 #    
 #  if(length(input$SelectClimate)==2){
 #  grid.arrange(text_reactive()[[1]],
 #               text_reactive()[[2]])
 #  }
 #    
 #  if(length(input$SelectClimate)==3){
 #      grid.arrange(text_reactive()[[1]],
 #                   text_reactive()[[2]],
 #                   text_reactive()[[3]])
 #  }
 #    
 #  if(length(input$SelectClimate)==4){
 #      grid.arrange(text_reactive()[[1]],
 #                   text_reactive()[[2]],
 #                   text_reactive()[[3]],
 #                   text_reactive()[[4]])
 #    }
 #  })
  
  output$BR_plot<-renderPlot({text_reactive()[[1]]})
  output$HRU_plot<-renderPlot({text_reactive()[[2]]})
  
  # This code isn't working when using text_reactive()[[]], unsure why
  # # # change to render img
  # output$runningmodel3 <-renderImage({
  #     
  #     # image_file <- paste0('www/','avg_change_BR.png')
  #   image_file <- text_reactive()[[6]]
  # 
  #   return(list(
  #     src = image_file,
  #     filetype = "image/png",
  #     height = 520,
  #     width = 696
  #   ))
  # },
  # deleteFile=T)


  
  
 }
# Run the app ----
shinyApp(ui = ui, server = server)