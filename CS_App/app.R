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

create_lists <- function(.df, .col){
    
    .col <- enexpr(.col)
    
    return_list <- .df %>% 
        filter(fltr_procedure == T) %>% 
        select(!!.col) %>%
        arrange(!!.col) %>% 
        distinct() %>% 
        pull()
}

injury_list <- patient_df %>% filter(fltr_procedure == T) %>% create_lists(injury_desc)
injury_loc_list <- patient_df %>% filter(fltr_procedure == T) %>%  create_lists(place_of_injury) 


patient_results <- patient_df %>% 
    filter(fltr_procedure == T)

# Bring in randomforest model
readRDS(file = "E:/Northwestern/12 - Capstone/PTOS_Data/rf_model.rds")


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
        numericInput(inputId = "age",
                     label = "Age of Patient",
                     min = 1,
                     max = 105,
                     value = 30),
        
        selectInput(inputId = "sex", 
                    label = "Gender",
                    choices = list("MALE", 
                                   "FEMALE"), 
                    selected = "MALE"),
        
        selectInput(inputId = "race", 
                    label = "Race",
                    choices = list('White', 
                                   'Black',
                                   'Asian', 
                                   'Other/NFS', 
                                   'Unknown')), 
        
        selectInput(inputId = "forearm_fx_desc", 
                    label ="Forearm Fracture Description",
                    choices = list("Op red-int fix rad/ulna", 
                                   "Cl fx reduc-radius/ulna", 
                                   "Cl red-int fix rad/ulna" ,
                                   "Open reduc-radius/uln fx" )) 
        # selectInput(inputId = "injury_desc",
        #             label = "Injury Description",
        #             choices = injury_list), 
        # 
        # selectInput(inputId = "injury_loc",
        #             label = "Injury Location",
        #             choices = injury_loc_list)

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
    
    output$selected_var <- renderText({
        paste("You have selected the following:", 
              input$sex, ",",
              input$race,  ",",
              input$age
              )
    })
    

    pred_data <- reactive({
       res <-  patient_rf %>% 
            filter(between(age_in_yrs , local(input$age) - 1, local(input$age) + 1) ,
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
                   age_in_yrs == local(input$age),
                   between(age_in_yrs, local(input$age)-1, local(input$age)+1), 
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
                   age_in_yrs == local(input$age), 
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
                   age_in_yrs == local(input$age), 
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
