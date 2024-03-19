# GUI for OWC-SWAT+
# Test if I can push changes to the remote repository

library(shiny)
library(readtext)
library(splitstackshape)
library(shinyjs)
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


source("RunAllScripts_SWATv60.5.2.R")
source("testPlot.R")
source("ClimateChange.R")


scenario_dir <- paste0(getwd(),"/Scenarios")


# Define UI ----
ui <- fluidPage(
    titlePanel("OWC-SWAT+"),
    tabsetPanel(
      
      tabPanel("About OWC-SWAT+", br(),strong("Welcome to the Old Woman Creek Soil and Water Assessment Tool! (OWC-SWAT+)"), br(),br(),
               
               a(href="https://registrationcenter-download.intel.com/akdlm/IRC_NAS/5e411842-8348-40cd-81f6-df193c321611/w_ifort_runtime_p_2024.0.0.49848.exe","***Important!*** Before running the model through this application, you will need to install the following Fortran dependencies"), p("*Download and install this exectutable only once*"),br(),

               p("This tool was designed to aid conservation efforts in the Old Woman Creek (OWC) watershed. Contained within this is a watershed model of Old Woman Creek built with the Soil and Water Assessment Tool framework (SWAT+)."), p("SWAT is a landscape hydrology and pollutant transport model designed by the U.S. Department of Agricultural Research Service (USDA) to simulate the transport of nutrients, sediments, and
pesticides in agricultural landscapes (Arnold et al., 2012; Moriasi et al., 2015)."),
                 p("Old Woman Creek estuary is vulnerable to both climate and land-use changes. The original intent of the national estuary reserve designation was to include the watershed within the conservation area, however this placed an undue burden on farmers to preserve this land (Hanselmann & Vogel, 1978).
                 Therefore, while the final preservation area included the estuary and some of the surrounding uplands, the majority of its watershed remains open to development. Changes in land-use and climate are expected to alter hydrology and nutrient loadings in Lake Erie watersheds (Michalak et al., 2013). Addressing the estuary’s vulnerability to future land-use and climate change 
                 required a process-based watershed model that could incorporate multiple data sources such as
                 climate, land managment, and watershed processes."), br(),
                 p("The OWC-SWAT+ model was designed to focus on addressing nutrient and sediment runoff from row-crop agriculture, as it is the predominant land-use in the OWC watershed (>50%). The tool herein can simulate the effects of increasing agricultural conservation practices (see 'Management and conservation practices' tab) on the landscape as well as the effects of changes in climate and summarize the effects on discharge, phosphorus, and sediment runoff at the watershed and field scale.
                 For more information on the watershed model build (input data, calibration/validation, parameters), you can refence the 'OWC-SWAT+ Model Build tab'. For information on how to use this application, reference the 'OWC-SWAT+ Manual' tab.
                 "),
               
               img(src="owc_map.png",width=1430/2,height=1105/2),br(),p('Figure 1. Map of Old Woman Creek watershed and estuary. Old Woman Creek is located in northern Ohio in the Great Lakes Basin.'),
               br(), hr(style="border-color: silver;"),br(),
               p("This app was written and designed by Haley Kujawa supported by funding from the Margaret A. Davidson fellowship."),
                 a(href="https://github.com/haleykujawa/SWATPlusGUI.git","OWC-SWAT+ GUI gitub"),br(), p("kujawa.21@osu.edu"),br(),
               img(src="old-woman-creek.png",height=503/4,width=800/4),
               img(src="davidson.png",height=117/2,width=432/2),
               img(src="osu.png",height=88/2,width=569/2),
               img(src='erie_swcd.png'),
               
               br(), hr(style="border-color: silver;"),br(),
               
               strong('References'),br(),
               br(), a(href="https://swat.tamu.edu/software/plus/","SWAT+ webpage"),br(),br(),
               p('Arnold, J G, Kiniry, J.R., Srinivasan, R., Williams, J.R., Haney, E.B., 2012. Soil and Water
Assessment Tool (SWAT) User’s Manual, Version 2012. Texas Water Resources
Institute. https://doi.org/10.1007/978-0-387-35973-1_1231'),
               p('Hanselmann, D. P., & Vogel, T. L. (1978). Old woman creek, ohio: The designation of a freshwater
estuarine sanctuary. Coastal Zone Management Journal, 4(3), 329–336.
https://doi.org/10.1080/08920757809361781'),
               p('Michalak, A.M., Anderson, E.J., Beletsky, D., Boland, S., Bosch, N.S., Bridgeman, T.B.,
Chaffin, J.D., Cho, K., Confesor, R., Daloglu, I., Depinto, J. V, Evans, M.A., Fahnenstiel,
G.L., He, L., Ho, J.C., Jenkins, L., Johengen, T.H., Kuo, K.C., Laporte, E., Liu, X.,
McWilliams, M.R., Moore, M.R., Posselt, D.J., Richards, R.P., Scavia, D., Steiner, A.L.,
Verhamme, E., Wright, D.M., Zagorski, M.A., 2013. Record-setting algal bloom in Lake
Erie caused by agricultural and meteorological trends consistent with expected future
conditions. Proc. Natl. Acad. Sci. U. S. A. 110, 6448–6452.
https://doi.org/10.1073/pnas.1216006110'),
               p('Moriasi, D.N., Gitau, M.W., Pai, N., Daggupati, P., 2015. Hydrologic and Water Quality
Models: Performance Measures and Evaluation Criteria. Trans. ASABE 58, 1763–1785.
https://doi.org/10.13031/trans.58.10715'),
                 p('Old Woman Creek National Estuarine Research Reserve Management Plan 2011-2016. (2011).')
               
               
               ),
      
      tabPanel("Change inputs and run OWC-SWAT+",

               

    sidebarLayout(                   
      sidebarPanel(#actionButton("cleardir", "Clear scenario"),
                   br(),
                   
                tabsetPanel(
                     tabPanel("Management and conservation practices",br(),
                              h5('Percent of practice on row-crop lands. All practices are intialized at the baseline rate - representative of years 2013-2020.'),
                              h5('The current Shiny-SWAT+ framework can only alter ', strong('one practice in each of the following categories'), 'per model run:'),
                              
                              hr(style="border-color: silver;"),
                              
                              br(),h5(strong('Category 1:'), 'Edge-of-field BMPs'),br(),
                              
                              fluidRow(column(6,sliderInput("vfs", label = "Vegetative field buffers", min=35,max=100,value = 35)),
                                       column(6,br(),textOutput("vfs_rate_change"))),
                              
                              hr(style="border-color: silver;"),
                              
                              br(),h5(strong('Category 2:'), 'In-field management practices', em('(change one only)')),br(),
                              
                              fluidRow(column(6,sliderInput("cc", label = "Rye cover crop",min=10,max=100, value = 10)),
                                       column(6,br(),textOutput("cc_rate_change"))),
                              
                              # hr(style="border-color: silver;"),
                            
                              fluidRow(column(6,sliderInput("notill", label = "Continuous no-tillage", min=60,max=100,value = 60)),
                                       column(6,br(),textOutput("notill_rate_change"))),
                              
                              # hr(style="border-color: silver;"),
                              
                              fluidRow(column(6,sliderInput("subfert", label = "Subsurface placement",min=0,max=100, value = 0)),
                                       column(6,br(),textOutput("subfert_rate_change"))),
                              
                              span(textOutput("practice_rate"),style='color:green'),br(),
                              
                              # hr(style="border-color: silver;"),
                              
                              hr(style="border-color: silver;"),
                              
                              br(),h5(strong('Category 3:'), 'In-stream BMPs'),br(),
                              
                              #ditch widget--maybe change this to miles of stream as an input?
                              fluidRow(column(6,sliderInput("ditch_rate", label = ("Conservation ditches"),min=0,max=100, value = 0)),
                                              column(6,p("This changes the rate of conservation ditches on streams of order 2. Changing to 100% only changes 38 km (24 mi) of stream"))) ,
                              img(src="Stream map.png",width=5846/10,height=4133/10)

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
                  
                  column(6,numericInput("LOWPCP_HIGHTMP", label = h5("Years with low precipitation, high temperatures:"),
                                      value = "5")),br(),br(),
                  # p('  These years include: 1991, 1999, 2010, 2012, 2016 (5 years)'),
                  column(6, textOutput("LOWPCP_HIGHTMP"))
                   

                  ),
                  
                  fluidRow(
                    
                    column(6,numericInput("HIGHPCP_AVGTMP", label = h5("Years with high precipitation, average temperatures:"),
                                          value = "5")),br(),br(),
                    # p('  These years include: 2000, 2007, 2008, 2013, 2019 (5 years)'),
                    column(6, textOutput("HIGHPCP_AVGTMP"))
                    
                    
                  ),
                  
                  fluidRow(
                    
                    column(6,numericInput("AVGPCP_HIGHTMP", label = h5("Years with average precipitation, high temperatures:"),
                                          value = "3")),br(),br(),
                    # p('  These years include: 1998, 2002, 2018 (3 years)'),
                    column(6, textOutput("AVGPCP_HIGHTMP"))
                    
                    
                  ),  hr(style="border-color: silver;"),
                  
                  h4('Add overall change to precipitation and temperature'),
                
                  p('Add a change to the "future" climate data that is added to the dataset in a linear fashion:
                    e.g., year 1 will change by applied amount divided by total number of years (X/30), and the final year will
                    be changed by the total amount (X).'),
                  
                  fluidRow(
                    
                    column(6,numericInput("DELTAT", label = h5("Add linear increase to temperature (C):"),
                                          value = "0")),
                    column(6,numericInput("DELTAP", label = h5("Add linear increase in precipitation (%):"),
                                          value = "0"))

                    
                    
                  ),
                  
                 img(src="temp_graph3.png",width=808/1.5,height=331/1.5),
                 br(),br(),
                 p('Example of how temperature or precipitation change is applied in a linear change fashion to the climate data, where X is the user input value'),
          
                   

                   

                   
                  hr(style="border-color: silver;"),
                  p('Average annual precipitation and temperature in historical and user generated climate scenario:'),

                  tableOutput("ClimateTable"),
 
                   )),
                   
                   br(),br(),


                   width = 8 ),
                   
      mainPanel(
        #### INPUTS ##################
        #Don't know if I need all this printed to the UI if it's already in the left hand panel, consider removing...
        #management scenarios
        # br(),
        # 
        # # Have to run SWAT+ locally on a windows machine. 
        # #This is because Shiny cannot run a windows exe file. In future, could look into using linux based version of swat
        # p('To run the OWC-SWAT+ model using this app, you will need to download the OWC-SWAT+ model (OWC_SWAT_GUI_FILES.zip) and unzip on your local computer. Note: this model currently only runs on Windows.'),
        # a(href="https://buckeyemailosu-my.sharepoint.com/:f:/g/personal/kujawa_21_osu_edu/ElF6DZ3LvV9OmzzMB6Lt6DwBE0T1rRQNLCigHe4S8oYUbw?e=bCMiv9","OWC-SWAT+ files download"),
        # p('Once those files are downloaded and unzipped, place the path to the files below:'),
        # textAreaInput("local_dir","Copy and paste local directory on computer where downloaded OWC-SWAT+ files are:", value="",width="1000px"),
        # br(),
        
        br(),
        checkboxGroupInput("SelectClimate", label = strong("Climate data to run:"), 
                           choices = list("Recent observed climate (2013-2020)"="hist","Climate change scenario"="userClimScen"), selected = "hist"),
        
            actionButton("runswat", "Run OWC-SWAT+"),br(),br(),
            strong("Clicking 'Run OWC-SWAT+' can take up to 20 minutes."),
            h5("Recent observed climate = ~ 6 min"),
            h5("Climate change scenario = ~ 12 min"),
            em("Once you hit 'Run OWC-SWAT+' you cannot redo the model run. You will have to wait until the model completes the runs or restart your computer. Closing the application will not end the model run. *Check inputs before running!*"),
        br(),
        h3("Climate change scenario"),
        span(textOutput("climate_rate"),style='color:green'),br(),
        
        # span(textOutput("local_dir_input"),style='color:green'),
        

        
        #### OUTPUTS ##################
        uiOutput("runningmodel1"),
        p(),p(),p(),
      
        
       


        
        
        #This is where you can place the output of the script, see https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/ for details
       width = 4) ) ),

tabPanel("Visualize outputs",
         # br(),
         
         tableOutput("area_table"),
         h2('Results for a recent climate (2013-2020):'),br(),
         plotOutput("BR_plot"),
         p("Fig 1. Change between the baseline management (representative of years 2013-2020) and changes implemented in management and
           conservation practices tab"),
         
         fluidRow(column(6,         plotOutput("HRU_per")),
                  column(6,         plotOutput("HRU_abs"))),
         
         fluidRow(column(6,                  p("Fig 2. Change between the baseline management (representative of years 2013-2020) and changes implemented in management and
           conservation practices tab. Only fields with a landscape management change are included on this graph. If no changes are made to land-use, graph will not appear.")),
                  column(6,                  p("Fig 3. Absolute value change between the baseline management (representative of years 2013-2020) and changes implemented in management and
           conservation practices tab. Only fields with a landscape management change are included on this graph"))),
         
         br(),
        
         fluidRow(column(6,         plotOutput("yield_per")),
                  column(6,         plotOutput("yield_abs"))),
         
         fluidRow(column(6,                  p("Fig 4. Yield change (%) between the baseline management (representative of years 2013-2020) and changes implemented in management and
           conservation practices tab. Only fields with a landscape management change are included on this graph. If no changes are made to land-use, graph will not appear.***")),
                  column(6,                  p("Fig 5. Absolute value change (bu/acre) between the baseline management (representative of years 2013-2020) and changes implemented in management and
           conservation practices tab. Only fields with a landscape management change are included on this graph***"))),
         
         br(), 
         
         p("Table 1. Summary of yield changes"),
         tableOutput("yield_table***"),
         
         em('***Note that there were limitations with the calibration of winter wheat yields in the OWC-SWAT+ model. While an average yield of 80 bushels/acre was the target yield
            as advised by the local SWCD, the current SWAT+ configuration could only achieve up to 58 bu/acre on average. This is one of the noted limitations of the OWC-SWAT+
            model that can be improved upon in future studies'),
         
         h2('Results for climate and land use change'),
         
         plotOutput("BR_plot_clim"),
         p("Fig 6. Change between the historical climate and management (representative of years 1990-2019) and changes implemented in management and
           conservation practices + climate change tabs"),
         fluidRow(column(6,         plotOutput("HRU_per_clim")),
                  column(6,         plotOutput("HRU_abs_clim"))),
         
         fluidRow(column(6,                  p("Fig 7. Change between the baseline management (representative of years 1990-2019) and changes implemented in management and
           conservation practices tab. Only fields with a landscape management change are included on this graph")),
                  column(6,                  p("Fig 8. Absolute value change between the baseline management (representative of years 1990-2019) and changes implemented in management and
           conservation practices tab. Only fields with a landscape management change are included on this graph"))),
         
         br(),
         
         fluidRow(column(6,         plotOutput("yield_per_clim")),
                  column(6,         plotOutput("yield_abs_clim"))),
         
         fluidRow(column(6,                  p("Fig 9. Yield change (%) between the baseline management (representative of years 1990-2019) and changes implemented in management and
           conservation practices tab. Only fields with a landscape management change are included on this graph")),
                  column(6,                  p("Fig 10. Absolute value change (bu/acre) between the baseline management (representative of years 1990-2019) and changes implemented in management and
           conservation practices tab. Only fields with a landscape management change are included on this graph"))),
         br(),
         p("Table 2. Summary of yield changes"),
         tableOutput("yield_table_clim"),

),

tabPanel("OWC-SWAT+ Manual",
         
         # Insert pdf
         tags$iframe(style="height:1000px; width:100%; scrolling=yes",
                     src="Instruction manual OWC-SWAT+.pdf")
         
         ),

tabPanel("OWC-SWAT+ Model Build",
         
         br(),strong('The following document is extracted from the dissertation discussing specifics of the OWC-SWAT+ model build and calibration,
                     as well as the simulation of agricultural conservation scenarios:'),br(),br(),
         p('Kujawa, H. A. (2023). Watershed modeling for climate change adaptation in the Laurentian Great Lakes: 
           watershed impact studies and simulation of wetland heterogeneity. Doctoral dissertation, The Ohio State University.'),br(),br(),
         # Insert pdf
         tags$iframe(style="height:1000px; width:100%; scrolling=yes",
                     src="Chapter_2_dissertation.pdf")
         
)

)

)

# Define server logic ----
server <- function(input, output, session) {
  
  #from https://www.r-bloggers.com/2014/04/deploying-desktop-apps-with-r/
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # prevent time out of application
  # https://tickets.dominodatalab.com/hc/en-us/articles/14794822159124-Shiny-App-shows-a-gray-screen-after-sometime-or-timeouts
  keep_alive <- shiny::reactiveTimer(intervalMs = 10000, session = shiny::getDefaultReactiveDomain())
  shiny::observe({keep_alive()})
  
  
  
  ###management scenarios###
  
  output$cc_rate_change<-renderText({paste0("Change from 10% to ", input$cc, "% (change ",input$cc-10 ,"%)")})
  output$vfs_rate_change<-renderText({paste0("Change from 35% to ", input$vfs, "% (change ",input$vfs-35 ,"%)")})
  output$grww_rate_change<-renderText({paste0("Change from 21% to ", input$grww, "% (change ",input$grww-21 ,"%)")})
  output$notill_rate_change<-renderText({paste0("Change from 60% to ", input$notill, "% (change ",input$notill-60 ,"%)")})
  output$subfert_rate_change<-renderText({paste0("Change from 0% to ", input$subfert, "% (change ",input$subfert-0 ,"%)")})
  
  
  # add other things from excel spreadsheet, like rates of subsurface placement, total N and total P applied 
  output$N_incorp<-renderText({paste0("Incorporated N (%) = ",input$CSFT*1+input$CSNT*0.97+input$CSRT*1+input$CSRot*1+input$CSNTcc*0.9+input$CSWS*0.69+input$CSWcc*0.64)})
  output$P_incorp<-renderText({paste0("Incorporated P (%) = ",input$CSFT*1+input$CSNT*0.1+input$CSRT*1+input$CSRot*1+input$CSNTcc*0.1+input$CSWS*0.55+input$CSWcc*0.69)}) 
  # output$N_applied<-renderText({paste0("Average annual N applied (lb/acre) = ", round( (223*(input$CSFT/100)/2 +206*(input$CSNT/100)/2 + 223*(input$CSRT/100)/2 + 226*(input$CSRot/100)/2 +222*(input$CSNTcc/100)/2 + 296*(input$CSWS/100)/3 + 311*(input$CSWcc/100)/3) ,0))})
  # output$P_applied<-renderText({paste0("Average annual P2O5 applied (lb/acre) = ", round( (118/2*(input$CSFT/100) + 118/2*(input$CSNT/100) + 118/2*(input$CSRT/100) + 116/2*(input$CSRot/100) + 116/2*(input$CSNTcc/100) + 174/3*(input$CSWS/100) + 168/3*(input$CSWcc/100)) ,0))})
  
  ###ditches###
  #print input ditch rate to UI
  output$selected_ditch_rate <- renderText({paste0("Conservation ditch rate = ", input$ditch_rate ,"%") })  
  
  # Climate input
  output$LOWPCP_HIGHTMP <- renderText({paste0("Changed low precipitation, high temperature years by ", input$LOWPCP_HIGHTMP - 5 ," years") }) # - x is based on nyrs in hist dataset
  output$HIGHPCP_AVGTMP <- renderText({paste0("Changed high precipitation, average temperature years by ", input$HIGHPCP_AVGTMP - 5 ," years") })
  output$AVGPCP_HIGHTMP <- renderText({paste0("Changed average precipitation, high temperature years by ", input$AVGPCP_HIGHTMP - 3 ," years") })
  

  text_reactive <-eventReactive( input$runswat, {
  
   showModal(modalDialog(title="Running SWAT+. Any further changes to the model will not be reflected in results tab. Once the model is set to run - you must wait for the run to complete to use the app again. Exiting the application will not stop the model run.",tagList(timestamp()), footer=NULL,easyClose = T))
   RunAllScripts_SWATv60.5.2(input$SelectClimate,
                             input$ditch_rate,
                             input$cc,
                             input$subfert,
                             input$notill,
                             input$vfs)
   removeModal()
   testPlot(input$SelectClimate)# testGUI()
   
   
 })
 
 # text output
 output$runningmodel1 <- renderUI({
 strong(text_reactive()[[1]])
 })
 
 ##### Climate graphs for user #######
 output$climate_rate<-reactive({validate(need((input$LOWPCP_HIGHTMP+input$HIGHPCP_AVGTMP+input$AVGPCP_HIGHTMP) <= 30, "Number of years altered is greater than the number of years in the climate change data set (30). Decrease input years"))
   paste0("Climate change inputs are ready to run! The overall temperature change is ", ClimateDataInput()[[1]]$Temperature[3], " and the 
   overall precipitation change is ", ClimateDataInput()[[1]]$Precipitation[3])})
 
 
 # output$local_dir_input<-reactive({validate(need(!is.empty(input$local_dir), "Input directory with SWAT files (Path/to/OWC_SWAT_GUI_FILES) to start app and resolve error above (Error: cannot change working directory)"))
 #   paste0("Local directory is loaded into the app. If above error persists, ensure directory path is correct (e.g., no additional spaces exist in the path name)")})
 
 output$practice_rate<-reactive({validate(need(input$cc==10 & input$notill==60 |  input$cc==10 & input$subfert==0 | input$notill ==60 & input$subfert == 0 , "More than one in-field management was changed. Slide input values to to left-most position to restore inputs. Change at most only one in-field management practice rate."))
   paste0("In field practices ready to run")})
 
 ClimateDataInput <-reactive({
   
   # recalculate climate here 
   ClimateChange(input$LOWPCP_HIGHTMP,input$HIGHPCP_AVGTMP,input$AVGPCP_HIGHTMP,
                 input$DELTAT, input$DELTAP)
   
 })
 
 output$ClimateTable<- renderTable({

   
   ClimateDataInput()[[1]]
   
   
 })

  
  # Recent climate plots, 2013-2020
  output$BR_plot<-renderPlot({text_reactive()[[2]]})
  output$HRU_per<-renderPlot({text_reactive()[[3]]})
  output$HRU_abs<-renderPlot({text_reactive()[[4]]})
  output$yield_per<-renderPlot({text_reactive()[[5]]})
  output$yield_abs<-renderPlot({text_reactive()[[6]]})
  output$yield_table<-renderTable({text_reactive()[[7]]})
  
  # Climate + land use change plots
  output$BR_plot_clim<-renderPlot({text_reactive()[[8]]})
  output$HRU_per_clim<-renderPlot({text_reactive()[[9]]})
  output$HRU_abs_clim<-renderPlot({text_reactive()[[10]]})
  output$yield_per_clim<-renderPlot({text_reactive()[[11]]})
  output$yield_abs_clim<-renderPlot({text_reactive()[[12]]})
  output$yield_table_clim<-renderTable({text_reactive()[[13]]})

  
 }
# Run the app ----
shinyApp(ui = ui, server = server)