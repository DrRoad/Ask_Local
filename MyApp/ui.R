shinyUI(pageWithSidebar(
        headerPanel('Ask Local'),
        
        sidebarPanel(
                dateInput('date',
                          label = 'Input date',
                          value = Sys.Date()
                ),
                uiOutput("select_country"),
                uiOutput("select_region"),
                uiOutput("select_municipality"),
                actionButton("update", "Forecast")
        ), #sidebarPanel
        
        mainPanel(
                plotOutput("plot")
        ) #mainPanel
)#pageWithSidebar
)#shinyUI