#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(r2d3)
library(plotly)


# dfs<- list("patient_df", "patient_periods")
# file_path <- 'E:/Northwestern/12 - Capstone/PTOS_Data/'
# 
# 
# for(d in dfs) {
#   if (exists(d) && is.data.frame(get(d))) next(d) 
#   else load(file = paste0(file_path,d, ".RData"))
# }


# Create lists for user selections based off of the 
# PTOS data results


patient_results <- patient_df %>% 
    filter(fltr_procedure == T)

# Bring in randomforest model
readRDS(file = "E:/Northwestern/12 - Capstone/PTOS_Data/rf_model.rds")
load(file = "E:/Northwestern/12 - Capstone/PTOS_Data/full_data.RData")


rf_pred <- 
    predict(rf_model, full_data, type = "prob") %>% 
    bind_cols(full_data %>% select(id, fltr_fasciotomy)) %>% 
    distinct()

rf_model_results <- rf_pred %>% 
    select(id, .pred_FALSE, .pred_TRUE, fltr_fasciotomy) %>% 
    summarize.(pred_false = mean(.pred_FALSE), 
               pred_true = mean(.pred_TRUE), 
               by = c(id, fltr_fasciotomy)) %>% 
    arrange(-pred_true)



patient_rf <- rf_model_results %>% 
    inner_join.(patient_df %>% 
                    select(id, 
                           sex, 
                           race, 
                           forearm_fx_desc, 
                           age_in_yrs, 
                           injury_desc, 
                           place_of_injury
                    ), by = "id")

# ----------------------------------------------------------------------------------------
#  BUILD THE USER INTERFACE FOR SHINY
# ----------------------------------------------------------------------------------------


ui <- dashboardPage(
    

    dashboardHeader(
        title = "Compartment Syndrome Prediction Dashboard",
        titleWidth = 400),
    
    dashboardSidebar(
        sliderInput("age_range", 
                    "Select Age Range", 
                    min = 0, max = 105, value = c(25,35)), 
        
        selectInput(inputId = "sex", 
                    label = "Select Patient's Gender",
                    choices = list("MALE", "FEMALE"), 
                    selected = "MALE"),
        
        selectInput(inputId = "race", 
                    label = "Select Patient's Race",
                    choices = NULL), 
        
        selectInput(inputId = "injury_loc",
                    label = "Injury Location",
                    choices = NULL), 
        
        selectInput(inputId = "injury_desc",
                    label = "Injury Description",
                    choices = NULL), 

        selectInput(inputId = "forearm_fx_desc", 
                    label ="Forearm Fracture Description",
                    choices = NULL) 


    ),
    dashboardBody(
        tabsetPanel(
            id = "tabs",
            tabPanel(
                title = "Main Dashboard",
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
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #--------------------------------------------------
    # UPDATE USER SELECTION OPTIONS 
    #--------------------------------------------------
    
    observe({
        gender_update <- patient_results %>% 
            filter(between(age_in_yrs , input$age_range[1],  input$age_range[2]), 
                   sex == input$sex) %>% 
            select(race) %>% 
            arrange(race) %>% 
            distinct() %>% 
            pull()
        updateSelectInput(session, "race", "Select Patient's Race", choices = gender_update)
        
    })
    
    observe({
        loc_update <- patient_results %>%
            filter(between(age_in_yrs , input$age_range[1],  input$age_range[2]),
                   sex == input$sex,
                   race == input$race) %>%
            select(place_of_injury) %>%
            arrange(place_of_injury) %>%
            distinct() %>%
            pull()
        updateSelectInput(session, "injury_loc", "Injury Location", choices = loc_update)
        
    })
    
    observe({
        injury_update <- patient_results %>%
            filter(between(age_in_yrs , input$age_range[1],  input$age_range[2]),
                   sex == input$sex,
                   race == input$race, 
                   place_of_injury == input$injury_loc) %>%
            select(injury_desc) %>%
            arrange(injury_desc) %>%
            distinct() %>%
            pull()
        updateSelectInput(session, "injury_desc", "Injury Description", choices = injury_update)

    })
    
    observe({
        fx_update <- patient_results %>% 
            filter(between(age_in_yrs , input$age_range[1],  input$age_range[2]), 
                   sex == input$sex, 
                   race == input$race, 
                   place_of_injury == input$injury_loc, 
                   injury_desc == input$injury_desc) %>% 
            select(forearm_fx_desc) %>% 
            arrange(forearm_fx_desc) %>% 
            distinct() %>% 
            pull()
        updateSelectInput(session, "forearm_fx_desc", "Forearm Fracture Description", choices = fx_update)
        
    })
    

    #--------------------------------------------------
    # UPDATING DATA FOR VISUALIZATIONS
    #--------------------------------------------------
    
    pred_data <- reactive({
       res <-  patient_rf %>% 
            filter(between(age_in_yrs , local(input$age_range[1]),  local(input$age_range[2])) ,
                   forearm_fx_desc == local(input$forearm_fx_desc),
                   # place_of_injury == local(input$injury_loc),
                   sex == local(input$sex),
                   race == local(input$race)
                   # injury_desc == local(input$injury_desc),
                   )
       
       res
        
    })


    # BASE TABLE Patient Count ------------------------------------------
    
    base_table <- reactive({
        res <- patient_results %>% 
            filter(sex == local(input$sex), 
                   between(age_in_yrs , local(input$age_range[1]),  local(input$age_range[2])) ,
                   race == local(input$race), 
                   forearm_fx_desc == local(input$forearm_fx_desc)) 
        res
    })
        
    # Total Patient Count ------------------------------------------
    
    total_count <- reactive({
        res <- patient_results 
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
        res <- patient_results %>%
            filter(sex == local(input$sex), 
                   between(age_in_yrs , local(input$age_range[1]),  local(input$age_range[2])) , 
                   race == local(input$race), 
                   forearm_fx_desc == local(input$forearm_fx_desc)) 
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
        res <- patient_results %>%
            filter(sex == local(input$sex), 
                   between(age_in_yrs , local(input$age_range[1]),  local(input$age_range[2])) , 
                   race == local(input$race), 
                   forearm_fx_desc == local(input$forearm_fx_desc), 
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
    
    
    # Top Injury Locations (server) -------------------------------------------
    output$cs_plot <- renderD3({
        
       pred_data() %>%
            summarise(CS = mean(pred_true, na.rm = TRUE), 
                      No_CS = 1-CS)  %>% 
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
