#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Compartment Syndrome in Pediatric Patients"),
    sidebarLayout(position = "right", 
        sidebarPanel(
            h2("Treatments for Compartment Syndrome"),
            br(),
            br(),
            img(src = "CS_1.png")
        ),
        mainPanel(
            h1("What is Compartment Syndrome?"),
            p("Compartment syndrome (CS) refers to the increased pressure inside the section of the 
              limb that contains muscles, nerves, and blood vessels. This increased pressure results 
              in the reduction of blood supply and tissue necrosis. It usually develops in the lower 
              leg and forearm after traumas like a bone fracture. If not treated in time, it can lead 
              to permanent muscle and nerve damage."),
            br(),
            p("CS typically develops after surgery or trauma, causing excessive fluid in the fascia, 
              causing pressure and pain in the affected limb. The decreased circulation causes the muscle 
              can die, resulting in permanent damage left untreated. Treatment of compartment syndrome is a 
              fasciotomy or removal of external compression, which can relieve the pressure and maintains blood 
              flow to the muscle. "),
            br(),
            h2("Features"),
            p("- Build useful web applications with only a few lines of code—no JavaScript required."),
            p("- Shiny applications are automatically 'live' in the same way that ", 
              strong("spreadsheets"),
              " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
