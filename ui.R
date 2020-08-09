shinyUI(fluidPage(

sidebarLayout(
    sidebarPanel(
        sliderInput("user_input_age", "Age:", 0, 120, NULL, 1),
        selectInput("user_input_sex", "Sex:", c("M", "F"), multiple = F),
        selectInput("user_input_condition", "Condition:", c("dementia_alzheim", "neurosis", "mood_affective"), multiple = F),
        actionButton("user_input_submit", "Generate prediction")),
    mainPanel(tabsetPanel(
        tabPanel("Results",
                 tableOutput("results_df")),
        tabPanel("View Data",
                 dataTableOutput("model_dataset"))
    ))
)

))
