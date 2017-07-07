dashboardPage(
    dashboardHeader(
        titleWidth = 300,
        title = "Nationals President Race"
    ),
    dashboardSidebar(width = 200,
                     sidebarMenu(
                         menuItem("All Presidents", tabName = "tab1", icon = icon("globe"))),
                     br(),
                     
                     div(align= 'center',actionButton("run", "Run") ),
                     helpText("Click to run a decision tree with new cost matrix and variables", align = 'center'),
                     br(),
                     div(align= 'center',actionButton("reset", "Reset Costs") ),
                     helpText("Click to reset costs to default", align = 'center')
    ),
    dashboardBody(
        useShinyjs(),
        tabItems(
            tabItem(
                tabName = "tab1",
                fluidRow(
                    column(
                        width = 4,
                        selectInput("prob_type",
                                    "Which betting scheme do you want?",
                                    c('All Presidents'='All', 'Teddy vs. Not Teddy'='Ted'),
                                    selected = 'All Presidents'),
                        
                        uiOutput('var_choice')
                    ),
                    column(
                        width = 7,
                        fluidRow(
                            rHandsontableOutput("hot"),
                            checkboxInput('accuracy','Build Decision Tree Based on Accuracy', value = F),
                            br(),
                            valueBoxOutput("profit"),
                            valueBoxOutput("profitgame")
                        ),
                        br(),
                        fluidRow(
                            plotOutput('treeplot'),
                            downloadButton('downloadPlot','Download Decision Tree')
                            
                        )
                    )
                )
            )
        )
    )
)