# GUI for OWC-SWAT+
# Test if I can push changes to the remote repository

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
library(ggpubr)
library(magrittr)

# library(ggplot2)
# library(tidyr)
# library(stringr)
# library(dplyr)

# Load scripts to be used ---
# source("Change_rot_dist.R")
# source("ChangeSWATInputs.R") #changed from improve ditch params
# source("TestShiny.R")
# source("Reset_scenario.R")
source("RunAllScripts_SWATv60.5.2.R")
# source("testGUI.R")
# source("ReadHRU_losses.R")
# source("ReadChannel_daily2.R")
source("testPlot.R")
# source("ChangeSWATClimate.R")
source("ClimateChange.R")

run_yrs<-c(2009)


# Load needed data ---
rotations <- read.csv("data/LumAreaSummary.csv")
baseline_data_avg<-read.csv("data/baseline_data_avg.csv")


scenario_dir <- paste0(getwd(),"/Scenarios")


# Define UI ----
ui <- fluidPage(
    titlePanel("OWC-SWAT+"),
    tabsetPanel(
      
      tabPanel("About OWC-SWAT+", br(),strong("Welcome to the Old Woman Creek Soil and Water Assessment Tool! (OWC-SWAT+)"),
               br(),br(),
               p("This tool was designed to aid conservation efforts in the Old Woman Creek watershed to improve water quality and remove OWC from the 303d list of impaired waters.
                 OWC-SWAT+ summarizes the potential impacts of climate and agricultural land management on changes in water quality."),
               
               img(src="owc_map.png",width=1430/2,height=1105/2),br(),p('Figure 1. Map of Old Woman Creek watershed and estuary'),
               br(),br(),br(),br(),
               img(src="old-woman-creek.png",height=503/4,width=800/4),
               img(src="davidson.png",height=117/2,width=432/2),
               img(src="osu.png",height=88/2,width=569/2),
               img(src='erie_swcd.png')),
      
      tabPanel("Change inputs and run OWC-SWAT+",
               

    sidebarLayout(                   
      sidebarPanel(#actionButton("cleardir", "Clear scenario"),
                   br(),
                   
                tabsetPanel(
                     tabPanel("Management and conservation practices",br(),
                              p('Percent of practice on row-crop lands. All inputs must be between 0-100'),
                              
                              fluidRow(column(6,numericInput("cc", label = "Rye cover crop", value = 10)),
                                       column(6,br(),textOutput("cc_rate_change"))),
                              
                              hr(style="border-color: silver;"),
                              
                              fluidRow(column(6,numericInput("vfs", label = "Vegetative field buffers", value = 35)),
                                       column(6,br(),textOutput("vfs_rate_change"))),
                              
                              hr(style="border-color: silver;"),
                              
                              fluidRow(column(6,numericInput("grww", label = "Grassed water way", value = 21)),
                                       column(6,br(),textOutput("grww_rate_change"))),
                              
                              hr(style="border-color: silver;"),
                              
                              fluidRow(column(6,numericInput("notill", label = "Continuous no-tillage", value = 60)),
                                       column(6,br(),textOutput("notill_rate_change"))),
                              
                              hr(style="border-color: silver;"),
                              
                              fluidRow(column(6,numericInput("subfert", label = "Subsurface placement", value = 21)),
                                       column(6,br(),textOutput("subfert_rate_change"))),
                              
                              hr(style="border-color: silver;"),
                              
                              #ditch widget
                              fluidRow(column(6,numericInput("ditch_rate", label = ("Conservation ditches"), value = 0),
                                              p("This changes the rate of conservation ditches on streams of order 2. Changing to 100% only changes 38 km (24 mi) of stream"))) ,
                              img(src="Stream map.png",width=5846/10,height=4133/10)

                              
                              
                              
                              
                  
                              
                   #              strong('Total rate of all seven management scenarios needs to add up to 100%'),br(),
                   #              strong('Rate of buffers + grassed waterways on each management scenario cannot total greater than 100%'), 
                   #              p('e.g. buffers 50% & grassed waterways 50% = OK, buffers = 100% & grassed waterways = 100% = model will crash'),
                   #              p('To calculate an even rate on a subset of scenarios for grassed waterways (grww) or vegetated buffers, use the following equation:'),
                   #              p('Input rate = desired total rate of buffers or grww on cropland (percent) x (1 / fraction of total cropland to have buffer or grww on (decimal percent)) '),
                   #            br(),
                   # #management widgets       
                   # fluidRow(column(3,numericInput("CSFT", label = "Corn Bean - Full Tillage", value = 21)), 
                   #          column(3,br(),textOutput("CSFT_ratechange")),
                   #          column(3,numericInput("CSFT_B", label = "Vegetated buffer rate", value = 0)),
                   #          column(3,numericInput("CSFT_GW", label = "Grassed waterway rate", value = 0))),
                   # 
                   # hr(style="border-color: silver;"),
                   # 
                   # fluidRow(column(3,numericInput("CSNT", label = "Corn Bean - No Till", value = 40)),
                   #          column(3,br(),textOutput("CSNT_ratechange")),
                   #          column(3,numericInput("CSNT_B", label = "Vegetated buffer rate", value = 44)),
                   #          column(3,numericInput("CSNT_GW", label = "Grassed waterway rate", value = 26))),
                   # 
                   # hr(style="border-color: silver;"),
                   # 
                   # fluidRow(column(3,numericInput("CSRot", label = "Corn Bean - Rotational No Till", value = 15)),
                   #          column(3,br(),textOutput("CSRot_ratechange")),
                   #          column(3,numericInput("CSRot_B", label = "Vegetated buffer rate", value = 44)),
                   #          column(3,numericInput("CSRot_GW", label = "Grassed waterway rate", value = 26))),
                   # 
                   # hr(style="border-color: silver;"),
                   # 
                   # fluidRow(column(3,numericInput("CSRT", label = "Corn Bean - Reduced Till", value = 4)),
                   #          column(3,br(),textOutput("CSRT_ratechange")),
                   #          column(3,numericInput("CSRT_B", label = "Vegetated buffer rate", value = 44)),
                   #          column(3,numericInput("CSRT_GW", label = "Grassed waterway rate", value = 26))),
                   # 
                   # hr(style="border-color: silver;"),
                   # 
                   # fluidRow(column(3,numericInput("CSNTcc", label = "Corn Bean - No Till with rye cover crop", value = 10)),
                   #          column(3,br(),textOutput("CSNTcc_ratechange")),
                   #          column(3,numericInput("CSNTcc_B", label = "Vegetated buffer rate", value = 44)),
                   #          column(3,numericInput("CSNTcc_GW", label = "Grassed waterway rate", value = 26))),
                   # 
                   # hr(style="border-color: silver;"),
                   # 
                   # fluidRow(column(3,numericInput("CSWS", label = "Corn Bean Wheat /Double crop bean", value = 9)),
                   #          column(3,br(),textOutput("CSWS_ratechange")),
                   #          column(3,numericInput("CSWS_B", label = "Vegetated buffer rate", value = 44)),
                   #          column(3,numericInput("CSWS_GW", label = "Grassed waterway rate", value = 26))),
                   # 
                   # hr(style="border-color: silver;"),
                   # 
                   # fluidRow(column(3,numericInput("CSWcc", label = "Corn Bean Wheat /rye cover crop", value = 1)),
                   #          column(3,br(),textOutput("CSWcc_ratechange")),
                   #          column(3,numericInput("CSWcc_B", label = "Vegetated buffer rate", value = 44)),
                   #          column(3,numericInput("CSWcc_GW", label = "Grassed waterway rate", value = 26))),
        
                   
                   # h5("Baseline rates of management:"),
                   # p("Corn Bean - Full Tillage is 21%"), #maybe do /n to remove space between them in UI
                   # p("Corn Bean - No Tillage is 40%"),
                   # p("Corn Bean - Reduced Tillage is 4%"),
                   # p("Corn Bean - Rotational No-Till is 15%"),
                   # p("Corn Bean - No-Till with rye cover crop is 10%"),
                   # p("Corn Bean Wheat /Double crop bean is 9%"),
                   # p("Corn Bean Wheat /rye cover crop is 1%"),
                   


                   
                   
                   # actionButton("simulate", "Apply changes to management")
                   ),
                   
                   
                   
                   
                   tabPanel("Climate change scenario",  
                            p(""),  
                   
                    # selectInput("SelectClimateOption", label = h3("Choose climate data to run:"), 
                    # choices = list("Baseline climate (2013-2020)" = "nochange", "Climate models" = "climmod", "Current climate beyond 2020" = "extended"), 
                                        # selected = "nochange"),
                            
                   h4('Change number of climatic extreme years'),

                   p("You can create a climate scenario based off historical climatic events. You can increase the number of 'extreme' climatic
                     water years to see the response of discharge, nutrient, and sediment loss. You can change the entirety of the 30 year
                     record. Hence, the total number of years input cannot be not be greater than 30."),
                   
                   
                   
                   # nyrs_LOWPCP_HIGHTMP 1991 1999 2010 2012 2016
                   # nyrs_HIGHPCP_AVGTMP 2000 2007 2008 2013 2019
                   # nyrs_AVGPCP_HIGHTMP 1998 2002 2018 
                   
                  fluidRow(
                  
                  column(6,numericInput("LOWPCP_HIGHTMP", label = h5("Years with low precip, high temp:"),
                                      value = "5")),br(),br(),
                  p('  These years include 1991 1999 2010 2012 2016 (5 years)'),
                  column(6, textOutput("LOWPCP_HIGHTMP"))
                   

                  ),
                  
                  fluidRow(
                    
                    column(6,numericInput("HIGHPCP_AVGTMP", label = h5("Years with high precip, average temp:"),
                                          value = "5")),br(),br(),
                    p('  These years include 2000 2007 2008 2013 2019 (5 years)'),
                    column(6, textOutput("HIGHPCP_AVGTMP"))
                    
                    
                  ),
                  
                  fluidRow(
                    
                    column(6,numericInput("AVGPCP_HIGHTMP", label = h5("Years with average precip, high temp:"),
                                          value = "3")),br(),br(),
                    p('  These years include 1998 2002 2018 (3 years)'),
                    column(6, textOutput("AVGPCP_HIGHTMP"))
                    
                    
                  ),  hr(style="border-color: silver;"),
                  
                  h4('Add overall change to precipitation and temperature'),
                
                  p('Add a change to the "future" climate data that is added to the dataset in a linear fashion:
                    e.g., year 1 will change by applied amount divided by total number of years, and the final year will
                    be changed by the total amount'),
                  
                  fluidRow(
                    
                    column(6,numericInput("DELTAT", label = h5("Add linear increase to temperature (C):"),
                                          value = "0")),
                    column(6,numericInput("DELTAP", label = h5("Add linear increase in precipitation (%):"),
                                          value = "0"))
                    
                    
                  ),
          
                   

                   

                   
                  hr(style="border-color: silver;"),
                  
                  # plotOutput("ClimatePlot"),
                  tableOutput("ClimateTable"),

                   
                   #  going to remove this option for now and work to add it back in if needed
                   # fileInput("pcpFile", label = h5("If extending climate data beyond 2020, insert pcp here:")),
                   # fileInput("tmpFile", label = h5("If extending climate data beyond 2020, insert tmp here:")),
                   
                   # actionButton("ClimateApply", "Apply changes to climate"),
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   )),
                   
                   br(),br(),

                   
                   # actionButton("simulate", "Apply changes"),
                   

                   
                   
                   
                   width = 8 ),
                   
      mainPanel(
        #### INPUTS ##################
        #Don't know if I need all this printed to the UI if it's already in the left hand panel, consider removing...
        #management scenarios
        br(),
        
        checkboxGroupInput("SelectClimate", label = strong("Climate data to run:"), 
                           choices = list("Recent observed climate (2013-2020)"="hist","Climate change scenario"="userClimScen"), selected = "hist"),
        
            actionButton("runswat", "Run OWC-SWAT+"),br(),br(),
            strong("Clicking 'Run OWC-SWAT+' can take up to 20 minutes."),
            h5("Recent observed climate = ~ 6 min"),
            h5("Climate change scenario = ~ 12 min"),
            em("Check inputs before running!"),
        h3("Climate change scenario"),
        span(textOutput("climate_rate"),style='color:green'),
        
        h3("Rates of management on row crop lands:"),
        span(textOutput("total_rate"),style='color:green'),
        
        br(),
        strong("Physical conservation practices:"),
        br(),
        span(textOutput("buff_grw_rate"),style='color:black'),
        br(),
        #ditches
        textOutput("selected_ditch_rate"),
        
        br(),
        strong("Winter cover rate:"),
        textOutput("cc_rate"),
        textOutput("winter_cover_rate"),
        
        br(),
        strong("Tillage rates:"),
        textOutput("FT_rate"),
        textOutput("NT_rate"), 
        textOutput("RT_rate"),
        textOutput("Rot_rate"),
        
        br(),
        strong("Fertilizer application:"),
        textOutput("N_incorp"),
        textOutput("P_incorp"),
        textOutput("N_applied"),
        textOutput("P_applied"),

        
        p(),
      
        
        p(),p(),p(),
        
        # textOutput("ClimateOut"),
        

        
        #### OUTPUTS ##################
        uiOutput("runningmodel1"),
        p(),p(),p(),
      
        
       


        
        
        #This is where you can place the output of the script, see https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/ for details
       width = 4) ) ),

tabPanel("Visualize outputs",
         # br(),
         
         tableOutput("area_table"),
         # imageOutput("runningmodel2"),
         h2('Results for a recent climate (2013-2020):'),br(),
         plotOutput("BR_plot"),
         p("Fig 1. Change between the baseline management (representative of years 2013-2020) and changes implemented in management and
           conservation practices tab"),
         plotOutput("HRU_plot"),
         plotOutput("yield_plot"),
         h2('Results for climate and land use change'),
         plotOutput("BR_plot_clim"),
         p("Fig 1. Change between the historical climate and management (representative of years 1990-2019) and changes implemented in management and
           conservation practices + climate change tabs"),
         plotOutput("HRU_plot_clim"),
         plotOutput("yield_plot_clim"),
         p("+Changes in crop yields"),
         p("+Changes in water balance"),
         p("+Estuary P and sediment accumulation")
         # imageOutput("runningmodel3")
),

tabPanel("Documentation",
         
         # Insert pdf
         tags$iframe(style="height:1000px; width:100%; scrolling=yes",
                     src="Instruction manual OWC-SWAT+.pdf")
         
         )

)

)

# Define server logic ----
server <- function(input, output, session) {
  
  #from https://www.r-bloggers.com/2014/04/deploying-desktop-apps-with-r/
  session$onSessionEnded(function() {
    stopApp()
  })
  
  ###management scenarios###
  #error message#
  output$total_rate<-reactive({validate(need((input$CSFT+input$CSNT+input$CSRT+input$CSRot+
           input$CSNTcc+input$CSWS+input$CSWcc) == 100, "Input management rates do not add up to 100% -- Adjust before running SWAT+"))
     paste0("Management rate input is ", input$CSFT+input$CSNT+input$CSRT+input$CSRot+input$CSNTcc+input$CSWS+input$CSWcc, "%, ready to run!")})
  
  # rate change
  # output$CSFT_ratechange<-renderText({paste0("Change from 21% to ", input$CSFT, "% (change ",input$CSFT-21 ,"%)")})
  # output$CSNT_ratechange<-renderText({paste0("Change from 40% to ", input$CSNT, "% (change ",input$CSNT-40 ,"%)")})
  # output$CSRot_ratechange<-renderText({paste0("Change from 15% to ", input$CSRot, "% (change ",input$CSRot-15 ,"%)")})
  # output$CSRT_ratechange<-renderText({paste0("Change from 4% to ", input$CSRT, "% (change ",input$CSRT-4 ,"%)")})
  # output$CSNTcc_ratechange<-renderText({paste0("Change from 10% to ", input$CSNTcc, "% (change ",input$CSNTcc-10 ,"%)")})
  # output$CSWS_ratechange<-renderText({paste0("Change from 9% to ", input$CSWS, "% (change ",input$CSWS-9 ,"%)")})
  # output$CSWcc_ratechange<-renderText({paste0("Change from 1% to ", input$CSWcc, "% (change ",input$CSWcc-1 ,"%)")})
  
  output$cc_rate_change<-renderText({paste0("Change from 10% to ", input$cc, "% (change ",input$cc-10 ,"%)")})
  output$vfs_rate_change<-renderText({paste0("Change from 35% to ", input$vfs, "% (change ",input$vfs-35 ,"%)")})
  output$grww_rate_change<-renderText({paste0("Change from 21% to ", input$grww, "% (change ",input$grww-21 ,"%)")})
  output$notill_rate_change<-renderText({paste0("Change from 60% to ", input$notill, "% (change ",input$notill-60 ,"%)")})
  output$subfert_rate_change<-renderText({paste0("Change from X% to ", input$subfert, "% (change ",input$subfert-0 ,"%)")})
  
  
  
  scenario_buff_rate<-reactive({round(input$CSFT*input$CSFT_B/100 +input$CSNT*input$CSNT_B/100+ input$CSRT*input$CSRT_B/100+ input$CSRot*input$CSRot_B/100+ input$CSNTcc*input$CSNTcc_B/100 +input$CSWS*input$CSWS_B/100 +input$CSWcc*input$CSWcc_B/100)})
  scenario_gww_rate<-reactive({round(input$CSFT*input$CSFT_GW/100 +input$CSNT*input$CSNT_GW/100+ input$CSRT*input$CSRT_GW/100+ input$CSRot*input$CSRot_GW/100+ input$CSNTcc*input$CSNTcc_GW/100 +input$CSWS*input$CSWS_GW/100 +input$CSWcc*input$CSWcc_GW/100)})
  
  output$buff_grw_rate<-reactive({validate(need(( (input$CSFT_B + input$CSFT_GW) <= 100) & ((input$CSNT_B + input$CSNT_GW) <= 100) & ((input$CSRT_B + input$CSRT_GW) <= 100) & ((input$CSRot_B + input$CSRot_GW) <= 100) & ((input$CSNTcc_B + input$CSNTcc_GW) <= 100) & ((input$CSWS_B + input$CSWS_GW) <= 100), "Total rate of buffers and grassed waterways on one management scenario cannot be greater than 100% -- Adjust before running SWAT+"))
    paste0("Total input rate of vegetated buffers is ", scenario_buff_rate(), '%',
           " and total input rate of grassed waterways is ", scenario_gww_rate() , '%. This is an change of ', round(((as.numeric(scenario_buff_rate())/100) - 0.35) * 8080.74), ' cropland acres with vegetated buffers and ',
           round(((as.numeric(scenario_gww_rate())/100) - 0.21) * 8080.74), ' cropland acres with grassed waterways' )})
  
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
  
  # Climate input
  output$LOWPCP_HIGHTMP <- renderText({paste0("Changed low pcp high tmp years by ", input$LOWPCP_HIGHTMP - 5 ," years") }) # - x is based on nyrs in hist dataset
  output$HIGHPCP_AVGTMP <- renderText({paste0("Changed high pcp average tmp years by ", input$HIGHPCP_AVGTMP - 5 ," years") })
  output$AVGPCP_HIGHTMP <- renderText({paste0("Changed average pcp high tmp years by ", input$AVGPCP_HIGHTMP - 3 ," years") })
  
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
  
   showModal(modalDialog(title="Running SWAT+",tagList("Any further changes to the model will not be reflected in results tab"), footer=NULL,easyClose = T))
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
 strong(text_reactive()[[1]])
 })
 
 ##### Climate graphs for user #######
 output$climate_rate<-reactive({validate(need((input$LOWPCP_HIGHTMP+input$HIGHPCP_AVGTMP+input$AVGPCP_HIGHTMP) <= 30, "Number of years altered is greater than the number of years in the climate change data set (30). Decrease input years"))
   paste0("Climate change inputs are ready to run! The overall temperature change is ", round(as.numeric(ClimateDataInput()[[1]]$`Temperature (C)`[1]),2), " C and the 
   overall precipitation change is ", round(as.numeric(ClimateDataInput()[[1]]$`Precipitation (mm)`[1]),2),"%")})
 
 
 
 ClimateDataInput <-reactive({
   
   # recalculate climate here 
   ClimateChange(input$LOWPCP_HIGHTMP,input$HIGHPCP_AVGTMP,input$AVGPCP_HIGHTMP,
                 input$DELTAT, input$DELTAP)
   
 })
 
 # output$ClimatePlot<- renderPlot({
 #   
 #     req(ClimateDataInput()[[1]])
 #   
 #   # ClimateDataInput is returned in a list, have to rebuild data frame
 #   
 #   AnnualClimateData<-data.frame(ClimateDataInput()[[1]]$WY,ClimateDataInput()[[1]]$PCP_mm, 
 #                                 ClimateDataInput()[[1]]$TMP_C,ClimateDataInput()[[1]]$data)
 #   colnames(AnnualClimateData)<-c('WY','PCP_mm','TMP_C','data')
 #   
 #   AnnualClimateData
 #   
 #   # This goes into reactive plot
 #     PCP_ANNUAL_PLOT<-ggplot(AnnualClimateData,aes(y=PCP_mm,x=data))+geom_boxplot()
 # 
 #   
 #   
 # })
 
 output$ClimateTable<- renderTable({
   
   # AnnualClimateData<-data.frame(ClimateDataInput()[[1]]$WY,ClimateDataInput()[[1]]$PCP_mm, 
   #                               ClimateDataInput()[[1]]$TMP_C,ClimateDataInput()[[1]]$data)
   # colnames(AnnualClimateData)<-c('WY','PCP_mm','TMP_C','data')
   # 
   # AnnualClimateData
   
   ClimateDataInput()[[1]]
   
   
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
  
  # Recent climate plots, 2013-2020
  output$BR_plot<-renderPlot({text_reactive()[[2]]})
  output$HRU_plot<-renderPlot({text_reactive()[[3]]})
  output$yield_plot<-renderPlot({text_reactive()[[4]]})
  output$area_table<-renderTable({text_reactive()[[8]]})
  # Climate + land use change plots
  output$BR_plot_clim<-renderPlot({text_reactive()[[5]]})
  output$HRU_plot_clim<-renderPlot({text_reactive()[[6]]})
  output$yield_plot_clim<-renderPlot({text_reactive()[[7]]})
  
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