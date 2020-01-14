
library(shiny)
library(tidyverse)
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv') %>%
    filter(!is.na(rank))

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinythemes::shinytheme(theme = "flatly"),

    div(titlePanel("How many of the most common 500 passwords can you guess?"),align = "center"),
    div(HTML("<font size = 5px;> Hint: They're all bad.</font>"), align = "center", style = "padding: 3px 0"),

    div(HTML("Data taken from <a href='https://informationisbeautiful.net/visualizations/top-500-passwords-visualized/?utm_content=buffer994fa&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer'>Information is Beautiful</a> top 500 passwords. App made by <a href = 'https://www.thewayir.com/'> Rebecca Janis </a> for <a href = 'https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-14/readme.md'> TidyTuesday </a>"),align = "center"),
    br(),
    # HTML("<font size=12px;> Password guess: </font>"),
    div(shiny::textInput(inputId = "guess", label = "", placeholder = "Password guess"),align = "center"),
    div(shiny::htmlOutput("hint_text"), align = "center"),
    # br(),
    div(shiny::actionButton(inputId = "submit", label = "Guess", style="color: white; background-color: #67999a; border-color: #67999a"), align = "center", style = "padding:3px 0"),
    div(actionButton("hint_button", "Hint"), align = "center", style = "padding:3px 0"),
    div(shiny::actionButton("show", "Show all 500 passwords"), align = "center", style = "padding:3px 0"),
    br(),
    div(htmlOutput("result", inline = F), align = "center"),
    div(htmlOutput("score"),align = "center"),
    div(htmlOutput("correct_guess_list"), align = "center"),
    br(),
    div(DT::DTOutput("table"), align = "center")
)


server <- function(input, output) {
    correct_guesses <- reactiveValues(correct = c())
    guess <- eventReactive(input$submit, {
        stringr::str_to_lower(input$guess)
    })

    guess_result <- eventReactive(input$submit, {
        if (guess() %in% passwords$password & !guess() %in% correct_guesses$correct) {
            HTML("<font color='green'; size = 16px> Yes!</font>")
        } else if (!guess() %in% passwords$password) {
            HTML("<font color='red'; size = 16px> Nope, try again.</font>")
        } else if (guess() %in% passwords$password & guess() %in% correct_guesses$correct) {
            HTML("<font color='red'; size = 16px> Password already guessed.</font>")
        }
    }, ignoreNULL = T, ignoreInit = T)

    output$result <- renderUI({
        guess_result()
    })

    hint <- eventReactive(input$hint_button, {
        glue::glue("Passwords fall in the following categories: {paste(unique(passwords$category), collapse = ', ')}")
    })
    output$hint_text <- renderUI({
        hint()
    })

    counter <- reactiveValues(scorevalue = 0)
    observeEvent(input$submit, {
        if (guess() %in% passwords$password & !guess() %in% correct_guesses$correct) {
            counter$scorevalue <- counter$scorevalue + 1
        }
    })

    output$score <- renderUI({
        HTML(glue::glue("<font size = 14px> Score: {counter$scorevalue} </font>"))
    })

    observeEvent(input$submit, {
        if (guess() %in% passwords$password & !guess() %in% correct_guesses$correct) {
            correct_guesses$correct <- c(correct_guesses$correct, guess())
        }
    })

    output$correct_guess_list <- renderUI({
        glue::glue("Correct guesses: {paste(correct_guesses$correct, collapse = ', ')}")
        })

    table_button <- eventReactive(input$show, {
        select(passwords, rank, password, category)
    }, ignoreNULL = T, ignoreInit = T)

    output$table <- DT::renderDataTable({
        DT::datatable(table_button(),
        rownames= FALSE)
    })
}


shinyApp(ui = ui, server = server)
