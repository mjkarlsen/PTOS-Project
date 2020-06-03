
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(r2d3)
library(plotly)
library(tidyverse)


load(file = "patient_rf.RData")


# library(rsconnect)
# rsconnect::deployApp('E:/Northwestern/12 - Capstone/Project/PTOS-Project/CS_App/app.R')

# ----------------------------------------------------------------------------------------
#  BUILD THE USER INTERFACE FOR SHINY
# ----------------------------------------------------------------------------------------


ui <- dashboardPage(
    

    dashboardHeader(
        title = "Compartment Syndrome Prediction Dashboard",
        titleWidth = 400),
    
    dashboardSidebar(
        helpText("Enter Patient's Information to calculate the probability of suffering from Compartment Syndrome"),
        sliderInput("age_range", 
                    "Select Age Range", 
                    min = 0, max = 105, value = c(25,35)),
        
        pickerInput(inputId = "sex", 
                    label = "Select Patient's Gender",
                    choices = list("MALE", "FEMALE"), 
                    multiple = TRUE, 
                    selected = "MALE",
                    options = list(`actions-box` = TRUE)),
        
        pickerInput(inputId = "race", 
                    label = "Select Patient's Race",
                    choices = list('White', 'Black', 'Asian', 'Other/NFS', "Unknown" ), 
                    multiple = TRUE, 
                    selected = "White", 
                    options = list(`actions-box` = TRUE)), 
        
        pickerInput(inputId = "injury_loc",
                    label = "Injury Location",
                    choices = list('Farm',
                                   'Home',
                                   'Industrial Place and Premises',
                                   'Mine/Quarry',
                                   'Other',
                                   'Public Building',
                                   'Recreational/Sport',
                                   'Residential Institution',
                                   'Street/Highway',
                                   'Unknown',
                                   'Unspecified'), 
                    multiple = TRUE, 
                    selected = 'Street/Highway', 
                    options = list(`actions-box` = TRUE)), 
        
        pickerInput(inputId = "injury_desc",
                    label = "Injury Description",
                    choices = list('Accidental Falls',
                                   'Accidents Caused By Fire And Flames',
                                   'Accidents Due To Natural And Environmental Factors',
                                   'Air And Space Transport Accidents',
                                   'Homicide And Injury Purposely Inflicted By Other Persons',
                                   'Injury Undetermined Whether Accidentally Or Purposely Inflicted',
                                   'Legal Intervention',
                                   'Motor Vehicle Nontraffic Accidents',
                                   'Motor Vehicle Traffic Accidents',
                                   'Other Accidents',
                                   'Other Road Vehicle Accidents',
                                   'Railway Accidents',
                                   'Suicide And Self-Inflicted Injury',
                                   'Unknown',
                                   'Vehicle Accidents Not Elsewhere Classifiable',
                                   'Water Transport Accidents'), 
                    multiple = TRUE, 
                    selected = c('Motor Vehicle Nontraffic Accidents',
                                 'Motor Vehicle Traffic Accidents'), 
                    options = list(`actions-box` = TRUE)), 

        pickerInput(inputId = "forearm_fx_desc", 
                    label ="Forearm Fracture Description",
                    choices = list('Op red-int fix rad/ulna', 
                                   'Open reduc-radius/uln fx', 
                                   'Cl red-int fix rad/ulna', 
                                   'Cl fx reduc-radius/ulna'), 
                    multiple = TRUE, 
                    selected = 'Op red-int fix rad/ulna', 
                    options = list(`actions-box` = TRUE)) , 
         
        
        
        fluidRow( align = "center",
                  br(),
                  h4("Prepared Filters"),
                  shinyjs::useShinyjs(),
                  actionButton("all_pats", "All Patients", width = "175px", 
                               style="color: #fff; background-color: #99CC00; border-color: #2e6da4"), 
                  actionButton("adult_pats", "Adult Patients", width = "175px", 
                               style="color: #fff; background-color: #999999; border-color: #2e6da4"), 
                  actionButton("peds_pats", "Pediatric Patients", width = "175px", 
                               style="color: #fff; background-color: #999999; border-color: #2e6da4"),
                  actionButton("high_risk", "Highest Risk Patients", 
                               width = "175px", 
                               style="color: #fff; background-color: #FF0000; border-color: #2e6da4"), 
                  actionButton("low_risk", "Lowest Risk Patients", width = "175px", 
                               style="color: #fff; background-color: #3399FF; border-color: #2e6da4"), 
                  )

    ),
    dashboardBody(
        tabsetPanel(
            id = "tabs",
            tabPanel(
                title = "Overview",
                value = "page1",
                fluidRow(
                    valueBoxOutput("total_patients"),
                    valueBoxOutput("similar_patients"),
                    valueBoxOutput("cs_percentage")
                ),
                fluidRow(),
                fluidRow(
                    column(
                        width = 4,
                        d3Output("top_injury_loc")
                    ),
                    column(
                        width = 4,
                        d3Output("top_cause_of_injury")
                    ), 
                    column( width = 4, 
                            d3Output("cs_plot")
                    )
                ),
                fluidRow(br(), 
                         htmlOutput("selected_var"))
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # HIGHEST RISK PATIENTS---------------------------------------------------------------
    
    observeEvent(input$high_risk, {
        updateNumericRangeInput(session, "age_range", value = c(48, 68))
        updateTextInput(session, "sex", value = "MALE")
        updateTextInput(session, "race", value = "Asian")
        updateTextInput(session, "injury_loc", value = "Home")
        updateTextInput(session, "injury_desc", value = "Accidental Falls")
        updateTextInput(session, "forearm_fx_desc", value = list('Op red-int fix rad/ulna', 
                                                                 'Open reduc-radius/uln fx', 
                                                                 'Cl red-int fix rad/ulna', 
                                                                 'Cl fx reduc-radius/ulna'))
       
    })
    
    # LOWEST RISK PATIENTS---------------------------------------------------------------    
    observeEvent(input$low_risk, {
        updateNumericRangeInput(session, "age_range", value = c(0, 105))
        updateTextInput(session, "sex", value = "FEMALE")
        updateTextInput(session, "race", value = "Black")
        updateTextInput(session, "injury_loc", value = "Home")
        updateTextInput(session, "injury_desc", value = "Accidental Falls")
        updateTextInput(session, "forearm_fx_desc", value = list('Op red-int fix rad/ulna', 
                                                                 'Open reduc-radius/uln fx', 
                                                                 'Cl red-int fix rad/ulna', 
                                                                 'Cl fx reduc-radius/ulna'))
        
    })
    
    # ALL PATIENTS---------------------------------------------------------------
    observeEvent(input$all_pats, {
        updateNumericRangeInput(session, "age_range", value = c(0, 105))
        updateTextInput(session, "sex", value = list("MALE", "FEMALE"))
        updateTextInput(session, "race", value = list('White', 'Black', 'Asian', 'Other/NFS', "Unknown" ))
        updateTextInput(session, "injury_loc", value = list('Farm',
                                                            'Home',
                                                            'Industrial Place and Premises',
                                                            'Mine/Quarry',
                                                            'Other',
                                                            'Public Building',
                                                            'Recreational/Sport',
                                                            'Residential Institution',
                                                            'Street/Highway',
                                                            'Unknown',
                                                            'Unspecified'))
        updateTextInput(session, "injury_desc", value = list('Accidental Falls',
                                                             'Accidents Caused By Fire And Flames',
                                                             'Accidents Due To Natural And Environmental Factors',
                                                             'Air And Space Transport Accidents',
                                                             'Homicide And Injury Purposely Inflicted By Other Persons',
                                                             'Injury Undetermined Whether Accidentally Or Purposely Inflicted',
                                                             'Legal Intervention',
                                                             'Motor Vehicle Nontraffic Accidents',
                                                             'Motor Vehicle Traffic Accidents',
                                                             'Other Accidents',
                                                             'Other Road Vehicle Accidents',
                                                             'Railway Accidents',
                                                             'Suicide And Self-Inflicted Injury',
                                                             'Unknown',
                                                             'Vehicle Accidents Not Elsewhere Classifiable',
                                                             'Water Transport Accidents'))
        updateTextInput(session, "forearm_fx_desc", value = list('Op red-int fix rad/ulna', 
                                                                 'Open reduc-radius/uln fx', 
                                                                 'Cl red-int fix rad/ulna', 
                                                                 'Cl fx reduc-radius/ulna'))
        
    })
    
    # ADULT PATIENTS---------------------------------------------------------------
    observeEvent(input$adult_pats, {
        updateNumericRangeInput(session, "age_range", value = c(18, 105))
        updateTextInput(session, "sex", value = list("MALE", "FEMALE"))
        updateTextInput(session, "race", value = list('White', 'Black', 'Asian', 'Other/NFS', "Unknown" ))
        updateTextInput(session, "injury_loc", value = list('Farm',
                                                            'Home',
                                                            'Industrial Place and Premises',
                                                            'Mine/Quarry',
                                                            'Other',
                                                            'Public Building',
                                                            'Recreational/Sport',
                                                            'Residential Institution',
                                                            'Street/Highway',
                                                            'Unknown',
                                                            'Unspecified'))
        updateTextInput(session, "injury_desc", value = list('Accidental Falls',
                                                             'Accidents Caused By Fire And Flames',
                                                             'Accidents Due To Natural And Environmental Factors',
                                                             'Air And Space Transport Accidents',
                                                             'Homicide And Injury Purposely Inflicted By Other Persons',
                                                             'Injury Undetermined Whether Accidentally Or Purposely Inflicted',
                                                             'Legal Intervention',
                                                             'Motor Vehicle Nontraffic Accidents',
                                                             'Motor Vehicle Traffic Accidents',
                                                             'Other Accidents',
                                                             'Other Road Vehicle Accidents',
                                                             'Railway Accidents',
                                                             'Suicide And Self-Inflicted Injury',
                                                             'Unknown',
                                                             'Vehicle Accidents Not Elsewhere Classifiable',
                                                             'Water Transport Accidents'))
        updateTextInput(session, "forearm_fx_desc", value = list('Op red-int fix rad/ulna', 
                                                                 'Open reduc-radius/uln fx', 
                                                                 'Cl red-int fix rad/ulna', 
                                                                 'Cl fx reduc-radius/ulna'))
        
    })
    
    # PEDIATRIC PATIENTS---------------------------------------------------------------
    observeEvent(input$peds_pats, {
        updateNumericRangeInput(session, "age_range", value = c(0, 17))
        updateTextInput(session, "sex", value = list("MALE", "FEMALE"))
        updateTextInput(session, "race", value = list('White', 'Black', 'Asian', 'Other/NFS', "Unknown" ))
        updateTextInput(session, "injury_loc", value = list('Farm',
                                                            'Home',
                                                            'Industrial Place and Premises',
                                                            'Mine/Quarry',
                                                            'Other',
                                                            'Public Building',
                                                            'Recreational/Sport',
                                                            'Residential Institution',
                                                            'Street/Highway',
                                                            'Unknown',
                                                            'Unspecified'))
        updateTextInput(session, "injury_desc", value = list('Accidental Falls',
                                                             'Accidents Caused By Fire And Flames',
                                                             'Accidents Due To Natural And Environmental Factors',
                                                             'Air And Space Transport Accidents',
                                                             'Homicide And Injury Purposely Inflicted By Other Persons',
                                                             'Injury Undetermined Whether Accidentally Or Purposely Inflicted',
                                                             'Legal Intervention',
                                                             'Motor Vehicle Nontraffic Accidents',
                                                             'Motor Vehicle Traffic Accidents',
                                                             'Other Accidents',
                                                             'Other Road Vehicle Accidents',
                                                             'Railway Accidents',
                                                             'Suicide And Self-Inflicted Injury',
                                                             'Unknown',
                                                             'Vehicle Accidents Not Elsewhere Classifiable',
                                                             'Water Transport Accidents'))
        updateTextInput(session, "forearm_fx_desc", value = list('Op red-int fix rad/ulna', 
                                                                 'Open reduc-radius/uln fx', 
                                                                 'Cl red-int fix rad/ulna', 
                                                                 'Cl fx reduc-radius/ulna'))
        
    })
    
    
    output$selected_var <- renderText({ 
        
        paste("<b>",
              knitr::combine_words(input$race),
              knitr::combine_words(input$sex),
              "</b>",
              "between the age of ", 
              "<b>",
              input$age_range[1], 
              "to", 
              input$age_range[2],
              "</b>",
              "that had", 
              "<b>", 
              knitr::combine_words(input$forearm_fx_desc),
              "</b>",
              "to correct injuries from",
              "<b>",
              knitr::combine_words(input$injury_desc), 
              '</b>', 
              "will suffer from Compartment Syndrome." )
    })
    
    #--------------------------------------------------
    # UPDATING DATA FOR VISUALIZATIONS
    #--------------------------------------------------

    
    # Total Patient Count ------------------------------------------
    
    total_count <- reactive({
        res <- patient_rf 
        res
    })
    
    output$total_patients <- renderValueBox({
        total_count() %>%
            tally() %>%
            pull() %>%
            as.integer() %>%
            prettyNum(big.mark = ",") %>%
            valueBox(subtitle = "Total Number of Patients in Study")
    })
    
    # Patients Like you (server) ------------------------------------------
    
    patients_like_you <- reactive({
        res <- patient_rf %>%
            filter(sex %in% c(input$sex), 
                   between(age_in_yrs , (input$age_range[1]),  (input$age_range[2])) , 
                   race %in% c(input$race), 
                   forearm_fx_desc %in% c(input$forearm_fx_desc)) 
        res
    })
    
    output$similar_patients <- renderValueBox({
        patients_like_you() %>%
            tally() %>%
            pull() %>%
            as.integer() %>%
            prettyNum(big.mark = ",") %>%
            valueBox(subtitle = "Patients Similar Demographics")
    })
    
    
    # Patients Like you (server) ------------------------------------------
    
    patients_getting_cs <- reactive({
        res <- patient_rf %>%
            filter(sex %in% c(input$sex), 
                   between(age_in_yrs , (input$age_range[1]),  (input$age_range[2])) , 
                   race %in% c(input$race), 
                   forearm_fx_desc %in% c(input$forearm_fx_desc), 
                   fltr_fasciotomy == T) 
        res
    })
    
    output$cs_percentage <- renderValueBox({
        patients_getting_cs() %>%
            tally() %>%
            pull() %>%
            as.integer() %>%
            prettyNum(big.mark = ",") %>%
            valueBox(subtitle = "Patients That Had Compartment Syndrome")
    })
    
    
    
    
    pred_data <- reactive({
       res <-  patient_rf %>% 
            filter(between(age_in_yrs , (input$age_range[1]),  (input$age_range[2])) ,
                   sex %in% c(input$sex),
                   race %in% c(input$race), 
                   place_of_injury %in% c(input$injury_loc), 
                   injury_desc %in% c(input$injury_desc),
                   forearm_fx_desc %in% c(input$forearm_fx_desc),
                   )
       
       res
        
    })


    # BASE TABLE Patient Count ------------------------------------------
    
    base_table <- reactive({
        res <- patient_rf %>% 
            filter(sex %in% c(input$sex), 
                   between(age_in_yrs , (input$age_range[1]),  (input$age_range[2])) ,
                   race %in% c(input$race), 
                   forearm_fx_desc %in% c(input$forearm_fx_desc)) 
        res
    })
        

    
    
    # Top Injury Locations (server) -------------------------------------------
    output$top_injury_loc <- renderD3({
        # The following code runs inside the database
        base_table() %>%
            group_by(place_of_injury) %>%
            tally() %>%
            collect() %>%
            arrange(-n) %>%
            head(10) %>%
            arrange(-n) %>%
            mutate(place_of_injury = str_sub(place_of_injury, 1, 30)) %>%
            rename(
                x = place_of_injury,
                y = n,
                label = place_of_injury
            ) %>%
            r2d3("injury_loc_bar_plot.js")
    })
    
    # Top Injury Locations (server) -------------------------------------------
    output$top_cause_of_injury <- renderD3({
        # The following code runs inside the database
        base_table() %>%
            group_by(injury_desc) %>%
            tally() %>%
            collect() %>%
            arrange(-n) %>%
            head(10) %>%
            arrange(-n) %>%
            mutate(injury_desc = str_sub(injury_desc, 1, 30)) %>%
            rename(
                x = injury_desc,
                y = n,
                label = injury_desc
            ) %>%
            r2d3("injury_types_bar_plot.js")
    })
    
    
    # Compartment Syndrome Graph (server) -------------------------------------------
    output$cs_plot <- renderD3({
        
       pred_data() %>%
            summarise(Compartment_Syndrome = round(median(pred_true, na.rm = TRUE),4)*100, 
                      No_Compartment_Syndrome = 100-Compartment_Syndrome)  %>% 
            pivot_longer(everything()) %>% 
            rename(
                x = name,
                y = value,
                label = name
            ) %>%
            r2d3("cs_bar_plot.js")
    })
   
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
