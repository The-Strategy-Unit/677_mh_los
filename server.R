shinyServer(function(input, output) {

# Dummy Data Generation ---------------------------------------------------

n <- 2000

set.seed(5115)

mh_los <- tibble(
    id = seq(1, n),
    # age = runif(n, min = 22, max = 90),
    age = ceiling(rnorm(n, 54, 13)),
    sex = sample(c("M", "F"), n, replace = T),
    diag = sample(c("dementia_alzheim", "neurosis", "mood_affective"), n, replace = T)
) %>%
    mutate(diag = ifelse(diag == "neurosis" & id %% 2 == 0, "mood_affective", diag)) %>%
    # count(diag)
    # Set mean los:
    mutate(los = case_when(
        diag == "dementia_alzheim" ~ 100,
        diag == "neurosis" ~ 25,
        diag == "mood_affective" ~ 40
    )) %>% 
    mutate(distrib_dem = rnorm(n, 0, 26)) %>% 
    mutate(distrib_neuro = rnorm(n, 0, 6)) %>% 
    mutate(distrib_mood = rnorm(n, 0, 11)) %>% 
    mutate(los = case_when(
        diag == "dementia_alzheim" ~ los - distrib_dem,
        diag == "neurosis" ~ los - distrib_neuro,
        diag == "mood_affective" ~ los - distrib_mood,
    )) %>% 
    mutate(vec_age = rnorm(n, 0, 3)) %>% 
    mutate(los = ifelse(age >54, los+(age*0.3)+vec_age, los)) %>% 
    mutate(los = ifelse(age < 54, los-((age)*0.3)+vec_age, los)) %>%
    mutate(los = ifelse(sex == "m" & los > 75, 1.1* los, los)) %>% 
    mutate(los = ifelse(los < 1, 1, los)) %>% 
    mutate(los = floor(los)) %>%
    mutate(long_stay = as_factor(ifelse(los > 60, 1, 0))) %>%
    # 60 so that classes are balanced
    identity()

mh_los <- mh_los %>% 
    select(-starts_with("distrib"), - starts_with("vec"))

output$model_dataset <- renderDataTable(mh_los)

# Split data into test and training sets to avoid overfitting -----------------------------------

set.seed(7117)

data_split <- initial_split(mh_los, strata = "long_stay", prop = 0.8)

los_train <- training(data_split)
los_test  <- testing(data_split)

# Model creation --------

mod_lm <- lm(data = los_train,
             formula = los ~ age*sex*diag)

predict_generate <- eventReactive(input$user_input_submit, {
    predict(
        mod_lm,
        newdata = tibble(
            age = input$user_input_age,
            sex = input$user_input_sex,
            diag = input$user_input_condition
        ),
        interval = "prediction"
    ) %>% round(1) %>% as_tibble()
})

# Initiate results df ---------------

## this code is graciously based off of this stack q https://stackoverflow.com/questions/23281841/follow-up-to-add-values-to-a-reactive-table-in-shiny

values <- reactiveValues()

values$df <- tibble(
    age = integer(),
    sex = character(),
    diag = character(),
    `los prediction` = double(),
    `lower interval` = double(),
    `upper interval` = double()
)

newEntry <- observe({
    if (input$user_input_submit > 0) {
        isolate({
            values$df <- values$df %>% add_row(age = input$user_input_age,
                                               sex = input$user_input_sex,
                                               diag = input$user_input_condition,
                                               `los prediction` = predict_generate() %>% pull(fit),
                                               `lower interval` = predict_generate() %>% pull(lwr),
                                               `upper interval` = predict_generate() %>% pull(upr))
        }
        )
    }
})

output$results_df <- renderTable({
    values$df
})

})
