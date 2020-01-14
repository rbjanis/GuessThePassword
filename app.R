
library(shiny)
library(tidyverse)
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv') %>%
    filter(!is.na(rank))

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinythemes::shinytheme(theme = "flatly"),

    div(titlePanel("How many of the most common 500 passwords can you guess?"),align = "center"),

    div(HTML("Data taken from <a href='https://informationisbeautiful.net/visualizations/top-500-passwords-visualized/?utm_content=buffer994fa&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer'>Information is Beautiful</a> top 500 passwords. App made for <a href = 'https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-01-14/readme.md'> TidyTuesday </a>"),align = "center"),
    br(),
    # HTML("<font size=12px;> Password guess: </font>"),
    div(shiny::textInput(inputId = "guess", label = "", placeholder = "Password guess"),align = "center"),
    div(shiny::actionButton(inputId = "submit", label = "Guess", style="color: white; background-color: #67999a; border-color: #67999a"), align = "center"),
    div(shiny::actionButton("show", "Show all 500 passwords"), align = "center"),
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
        if (input$guess %in% passwords$password & !input$guess %in% correct_guesses$correct) {
            HTML("<font color='green'; size = 16px> Yes!</font>")
        } else if (!input$guess %in% passwords$password) {
            HTML("<font color='red'; size = 16px> Nope, try again.</font>")
        } else if (input$guess %in% passwords$password & input$guess %in% correct_guesses$correct) {
            HTML("<font color='red'; size = 16px> Password already guessed.</font>")
        }
    }, ignoreNULL = T, ignoreInit = T)

    output$result <- renderUI({
        guess()
    })

    counter <- reactiveValues(scorevalue = 0)
    observeEvent(input$submit, {
        if (input$guess %in% passwords$password & !input$guess %in% correct_guesses$correct) {
            counter$scorevalue <- counter$scorevalue + 1
        }
    })

    output$score <- renderUI({
        HTML(glue::glue("<font size = 14px> Score: {counter$scorevalue} </font>"))
    })

    observeEvent(input$submit, {
        if (input$guess %in% passwords$password & !input$guess %in% correct_guesses$correct) {
            correct_guesses$correct <- c(correct_guesses$correct, input$guess)
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
