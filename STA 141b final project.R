#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(tidyverse)
library(httr)
library(PetfindeR)
library(DT)
library(plotly)

pf <- Petfinder(
  key = Sys.getenv("PETFINDER_KEY"),
  secret = Sys.getenv("PETFINDER_SECRET")
)

# Getting CA animals
CA_animals <- pf$animals(location = "CA", page = NULL)

CA_a.p1 <- CA_animals$page1
CA_a.p2 <- CA_animals$page2
CA_a.p3 <- CA_animals$page3
CA_a.p4 <- CA_animals$page4
CA_a.p5 <- CA_animals$page5
CA_a.p6 <- CA_animals$page6
CA_a.p7 <- CA_animals$page7
CA_a.p8 <- CA_animals$page8
CA_a.p9 <- CA_animals$page9
CA_a.p10 <- CA_animals$page10
CA_a.p11 <- CA_animals$page11
CA_a.p12 <- CA_animals$page12
CA_a.p13 <- CA_animals$page13
CA_a.p14 <- CA_animals$page14
CA_a.p15 <- CA_animals$page15

CA_Animals <- as.data.frame(rbind(CA_a.p1, CA_a.p2, CA_a.p3, CA_a.p4, CA_a.p5, CA_a.p6, CA_a.p7, CA_a.p8, CA_a.p9, CA_a.p10, CA_a.p11, CA_a.p12, CA_a.p13, CA_a.p14, CA_a.p15))
CA_Animals <- as.data.frame.data.frame(CA_Animals)
CA_Animals$url <- paste0("<a href='", CA_Animals$url, "'>", "Links on Petfinder", "</a>")
CA_Animals1 <- CA_Animals %>%
  select(-"coat":-"tags") %>%
  select(-"description":-"distance") %>%
  select(-"breeds.secondary":-"_links.organization.href")

# Breeds
dog_breeds <- CA_Animals1 %>%
  select(type, breeds.primary) %>%
  filter(type == "Dog") %>%
  select(breeds.primary) %>%
  distinct()

cat_breeds <- CA_Animals1 %>%
  select(type, breeds.primary) %>%
  filter(type == "Cat") %>%
  select(breeds.primary) %>%
  distinct()

rabbit_breeds <- CA_Animals1 %>%
  select(type, breeds.primary) %>%
  filter(type == "Rabbit") %>%
  select(breeds.primary) %>%
  distinct()

sf_breeds <- CA_Animals1 %>%
  select(type, breeds.primary) %>%
  filter(type == "Small & Furry") %>%
  select(breeds.primary) %>%
  distinct()

# Size
Dog_size <- CA_Animals1 %>%
  select(type, size) %>%
  filter(type == "Dog") %>%
  select(size) %>%
  distinct()

Cat_size <- CA_Animals1 %>%
  select(type, size) %>%
  filter(type == "Cat") %>%
  select(size) %>%
  distinct()

Rabbit_size <- CA_Animals1 %>%
  select(type, size) %>%
  filter(type == "Rabbit") %>%
  select(size) %>%
  distinct()

Small_furry_size <- CA_Animals1 %>%
  select(type, size) %>%
  filter(type == "Small & Furry") %>%
  select(size) %>%
  distinct()

# Age
Dog_age <- CA_Animals1 %>%
  select(type, age) %>%
  filter(type == "Dog") %>%
  select(age) %>%
  distinct()

Cat_age <- CA_Animals1 %>%
  select(type, age) %>%
  filter(type == "Cat") %>%
  select(age) %>%
  distinct()

Rabbit_age <- CA_Animals1 %>%
  select(type, age) %>%
  filter(type == "Rabbit") %>%
  select(age) %>%
  distinct()

Small_furry_age <- CA_Animals1 %>%
  select(type, age) %>%
  filter(type == "Small & Furry") %>%
  select(age) %>%
  distinct()

# Gender
dog_gender <- CA_Animals1 %>%
  select(type, gender) %>%
  filter(type == "Dog") %>%
  select(gender) %>%
  distinct()
cat_gender <- CA_Animals1 %>%
  select(type, gender) %>%
  filter(type == "Cat") %>%
  select(gender) %>%
  distinct()
rabbit_gender <- CA_Animals1 %>%
  select(type, gender) %>%
  filter(type == "Rabbit") %>%
  select(gender) %>%
  distinct()
sf_gender <- CA_Animals1 %>%
  select(type, gender) %>%
  filter(type == "Small & Furry") %>%
  select(gender) %>%
  distinct()

# Animal type
animal_types <- CA_Animals1 %>%
  filter(type != "Bird") %>%
  distinct(type)

ui <- navbarPage(
  "Find Your Favourite Pet Today in California",
  tabPanel(
    "DataTable",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "type",
          label = "Animal Type:",
          choices = animal_types
        ),

        conditionalPanel(
          condition = "input.type == 'Dog'",
          selectInput(
            inputId = "dog_breed",
            label = "Breed:",
            choices = c("All", dog_breeds),
            multiple = F
          ),
          selectInput(
            inputId = "dog_size",
            label = "Size:",
            choices = c("All", Dog_size),
            multiple = F
          ),
          selectInput(
            inputId = "dog_age",
            label = "Age:",
            choices = c("All", Dog_age),
            multiple = F
          ),
          selectInput(
            inputId = "dog_gender",
            label = "Gender:",
            choices = c("All", dog_gender),
            selected = "",
            multiple = F
          )
        ),

        conditionalPanel(
          condition = "input.type == 'Cat'",
          selectInput(
            inputId = "cat_breed",
            label = "Breed:",
            choices = c("All", cat_breeds),
            multiple = F
          ),
          selectInput(
            inputId = "cat_size",
            label = "Size:",
            choices = c("All", Cat_size),
            multiple = F
          ),
          selectInput(
            inputId = "cat_age",
            label = "Age:",
            choices = c("All", Cat_age),
            multiple = F
          ),
          selectInput(
            inputId = "cat_gender",
            label = "Gender:",
            choices = c("All", cat_gender),
            selected = "",
            multiple = F
          )
        ),

        conditionalPanel(
          condition = "input.type == 'Rabbit'",
          selectInput(
            inputId = "rabbit_breed",
            label = "Breed:",
            choices = c("All", rabbit_breeds),
            multiple = F
          ),
          selectInput(
            inputId = "rabbit_size",
            label = "Size:",
            choices = c("All", Rabbit_size),
            multiple = F
          ),
          selectInput(
            inputId = "rabbit_age",
            label = "Age:",
            choices = c("All", Rabbit_age),
            multiple = F
          ),
          selectInput(
            inputId = "rabbit_gender",
            label = "Gender:",
            choices = c("All", rabbit_gender),
            selected = "",
            multiple = F
          )
        ),


        conditionalPanel(
          condition = "input.type == 'Small & Furry'",
          selectInput(
            inputId = "sf_breed",
            label = "Breed:",
            choices = c("All", sf_breeds),
            multiple = F
          ),
          selectInput(
            inputId = "sf_size",
            label = "Size:",
            choices = c("All", Small_furry_size),
            multiple = F
          ),
          selectInput(
            inputId = "sf_age",
            label = "Age:",
            choices = c("All", Small_furry_age),
            multiple = F
          ),
          selectInput(
            inputId = "sf_gender",
            label = "Gender:",
            choices = c("All", sf_gender),
            selected = "",
            multiple = F
          )
        ),
      ),
      mainPanel("Pets Found", dataTableOutput("list"))
    )
  ),

  tabPanel(
    "Bar Chart",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "type1",
          label = "Animal Type:",
          choices = animal_types
        )
      ),
      mainPanel("Bar Chart", plotlyOutput("plot1"))
    )
  )
)


require(tidyverse)
require(plotly)
server <- function(input, output) {
  table1 <- reactive({
    type0 <- input$type
    breed0 <- input$breed
    breed_d <- input$dog_breed
    breed_c <- input$cat_breed
    breed_r <- input$rabbit_breed
    breed_sf <- input$sf_breed
    size0 <- input$size
    size_d <- input$dog_size
    size_c <- input$cat_size
    size_r <- input$rabbit_size
    size_sf <- input$sf_size
    age0 <- input$age
    age_d <- input$dog_age
    age_c <- input$cat_age
    age_r <- input$rabbit_age
    age_sf <- input$sf_age
    gender0 <- input$gender
    gender_d <- input$dog_gender
    gender_c <- input$cat_gender
    gender_r <- input$rabbit_gender
    gender_sf <- input$sf_gender
    allCA <- CA_Animals1

    if (type0 == "Dog") {
      allCA <- allCA %>%
        filter(type == "Dog") %>%
        filter(breeds.primary == breed_d | breed_d == "All") %>%
        filter(size == size_d | size_d == "All") %>%
        filter(age == age_d | age_d == "All") %>%
        filter(gender == gender_d | gender_d == "All")
    } else if (type0 == "Cat") {
      allCA <- allCA %>%
        filter(type == "Cat") %>%
        filter(breeds.primary == breed_c | breed_c == "All") %>%
        filter(size == size_c | size_c == "All") %>%
        filter(age == age_c | age_c == "All") %>%
        filter(gender == gender_c | gender_c == "All")
    } else if (type0 == "Rabbit") {
      allCA <- allCA %>%
        filter(type == "Rabbit") %>%
        filter(breeds.primary == breed_r | breed_r == "All") %>%
        filter(size == size_r | size_r == "All") %>%
        filter(age == age_r | age_r == "All") %>%
        filter(gender == gender_r | gender_r == "All")
    } else if (type0 == "Small & Furry") {
      allCA <- allCA %>%
        filter(type == "Small & Furry") %>%
        filter(breeds.primary == breed_sf | breed_sf == "All") %>%
        filter(size == size_sf | size_sf == "All") %>%
        filter(age == age_sf | age_sf == "All") %>%
        filter(gender == gender_sf | gender_sf == "All")
    }
    allCA
  })

  output$list <- renderDataTable(
    {
      table1()
    },
    escape = FALSE
  )
  output$plot1 <- renderPlotly({
    fig <- CA_Animals1
    type1 <- input$type1
    fig <- fig %>%
      filter(type == type1) %>%
      count(age, size)
    fig <- fig %>%
      plot_ly(x = ~age, y = ~n, type = "bar", color = ~size) %>%
      layout(xaxis = list(title = "Age"), yaxis = list(title = "Count"), title = "Number of Animals to Adopt Given Age and Size")
    fig
  })
}

shinyApp(ui = ui, server = server)
