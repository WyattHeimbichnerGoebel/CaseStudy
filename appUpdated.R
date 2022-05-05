

library(shiny)
library(vroom)
library(tidyverse)


injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")

prod_codes <- setNames(products$prod_code, products$title)

count_top <- function(df, var, n) {
    df %>%
        mutate({{ var }} := fct_lump(fct_infreq({{ var }}), {{n}},
                                     other_level="Sum of Other Catergories")) %>%
        group_by({{ var }}) %>%
        summarise(n = as.integer(sum(weight)))
}

ui <- fluidPage(
    h1("Introduction"),
    p("This app allows you to explore data collected by the Consumer Product Safety Commission in 2017 as part of the National Electronic Injury Surveillance System (NEISS). NEISS was a long-term study that recorded all accidents in a representative sample of hospitals in the United States from 2013 to 2017. More information about the data set can be found here:"),
    a(href="github.com/hadley/neiss", "https://github.com/hadley/neiss"),
    hr(),
    p("Please select a product code from the list below. This filters the data set so that only information about injuries associated with product you select will be shown."),
    fluidRow(
        column(8,
               selectInput("code", "Product",
                           choices = setNames(products$prod_code, products$title),
                           width = "100%"
               )
        )
    ),
    hr(),
    h2("Summary"),
    p("Below are several tables that summarize the injuries associated with the product you selected based on the type of the injury, the body part on which the injury occured, and the location where the injury happened. You can select how many rows will be displayed in the tables using the slider below."),
    fluidRow(
        column(4, sliderInput("nRows", "Number of Rows", min=2,max=20,step=1, value=5))
    ),
    fluidRow(
        column(4, tableOutput("diag")),
        column(4, tableOutput("body_part")),
        column(4, tableOutput("location"))
    ),
    hr(),
    h2("Plot"),
    p("Below is a plot that displays the amount of injuries associated with the product you chose by the age and sex of the people who were injured. You can choose to display the data as a rate (number of injuries per 10,000 people) or a count (estimated total number of injuries)."),
    fluidRow(
        column(4, selectInput("y", "Y axis", c("rate", "count")))
    ),
    fluidRow(
        column(12, plotOutput("age_sex"))
    ),
    hr(),
    h2("Narrative"),
    p("Each injury is accompanied by a short narrative that describes how the injury occured. Click the button below to display a random narrative:"),
    fluidRow(
        column(2, actionButton("story", "Random Narrative")),
        column(8, textOutput("randNarrative"))
    ),
    p("Or you can go through all the narratives in order:"),
    fluidRow(
        column(2, actionButton("previousNarrative", "Previous Narrative")),
        column(8, textOutput("narrative")),
        column(2, actionButton("nextNarrative", "Next Narrative"))
    )
)


server <- function(input, output, session) {
    selected <- reactive(injuries %>% filter(prod_code == input$code))
    rows <- reactive(input$nRows-1)
    
    output$diag <- renderTable({
        diagnosis <- count_top(selected(), diag, rows())
        colnames(diagnosis) <- c("Diagnosis", "Number")
        diagnosis
        },width="100%")
    output$body_part <- renderTable({
        body <- count_top(selected(), body_part, rows())
        colnames(body) <- c("Body Part", "Number")
        body
        },width="100%")
    output$location <- renderTable({
        place <- count_top(selected(), location, rows())
        colnames(place) <- c("Location", "Number")
        place
        },width="100%")
    
    summary <- reactive({
        selected() %>%
            count(age, sex, wt = weight) %>%
            left_join(population, by = c("age", "sex")) %>%
            mutate(rate = n / population * 1e4)
    })
    
    output$age_sex <- renderPlot({
        if (input$y == "count") {
            summary() %>%
                ggplot(aes(age, n, colour = sex)) +
                geom_line() +
                labs(x = "Age", y = "Estimated number of injuries", color = "Sex")+
                scale_color_manual(labels = c("Female", "Male"), values=c("red", "blue"))+
                theme_classic()
        } else {
            summary() %>%
                ggplot(aes(age, rate, colour = sex)) +
                geom_line(na.rm = TRUE) +
                labs(x = "Age", y = "Injuries per 10,000 people", color = "Sex")+
                scale_color_manual(labels = c("Female", "Male"), values=c("red", "blue"))+
                theme_classic()
        }
    }, res = 96)
    
    narrative_sample <- eventReactive(
        list(input$story, selected()),
        selected() %>% pull(narrative) %>% sample(1)
    )
    output$randNarrative <- renderText(narrative_sample())
    
    narratives <- reactive(selected() %>% pull(narrative))
    res <- reactiveValues(narrativeIndex = 1)
    
    observeEvent(input$nextNarrative,{
        if(res$narrativeIndex > length(narratives())) res$narrativeIndex <- 1
        else res$narrativeIndex <- res$narrativeIndex + 1
    })
    
    observeEvent(input$previousNarrative,{
        if(res$narrativeIndex < 2) res$narrativeIndex <- length(narratives())
        else res$narrativeIndex <- res$narrativeIndex - 1
    })
    
    output$narrative <- renderText({
        narratives()[res$narrativeIndex]
    })
}

shinyApp(ui = ui, server = server)
