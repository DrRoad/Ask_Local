shinyUI(pageWithSidebar(
        headerPanel('Ask Local'),
        
        sidebarPanel(
                dateInput('date',
                          label = 'Input date',
                          value = Sys.Date()+1,
                          max =Sys.Date()+360,
                          min =Sys.Date()-360
                ),
                uiOutput("select_country"),
                uiOutput("select_region"),
                uiOutput("select_municipality"),
                checkboxGroupInput('choices', 'What should I do?',
                                   choices=c("Predict temperature"="T",
                                             "Show Historical Trend"="H",
                                             "Predict Precipitation"="P",
                                             "Show pairs plot"="S"),
                                   selected=c("T","H","S")),
                numericInput("num_years", 
                             label = h5("How many years to use?"), 
                             value = 20, min=2, max=50),
                actionButton("update", "Forecast")
        ), #sidebarPanel
        
        mainPanel(
                textOutput("temp"),
                textOutput("rain"),
                textOutput("qual"),
                plotOutput("plot"),
                uiOutput("pairs_button"),
                plotOutput("pairs"),
                tableOutput("table_c")
        ) #mainPanel
)#pageWithSidebar
)#shinyUI