## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Recommend by Genre", tabName = "ByGenre"),
    menuItem("Recommend by Rating", tabName = "ByRating")
  )
)

body <- dashboardBody(includeCSS("css/books.css"),
              tabItems(
                tabItem(tabName = "ByGenre",
                        h2("Recommend by Genre"),
                        fluidRow(
                          useShinyjs(),
                          box(width = 12, title = "Step 1: Select your favorite genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                              selectInput("selectGenre", "Genres",
                                          c("Action", "Adventure", "Animation", 
                                            "Children's", "Comedy", "Crime",
                                            "Documentary", "Drama", "Fantasy",
                                            "Film-Noir", "Horror", "Musical", 
                                            "Mystery", "Romance", "Sci-Fi", 
                                            "Thriller", "War", "Western"),
                                          selected = NULL),
                              tableOutput("data")
                          )
                        ),
                        fluidRow(
                          useShinyjs(),
                          box(width = 12, title = "Step 2: Discover Movies you might like", status = "info", solidHeader = TRUE, collapsible = TRUE,
                            br(),
                            withBusyIndicatorUI(
                              actionButton("btnByGenre", "Click here to get your recommendations", class = "btn-warning")
                            ),
                            br(),
                            tableOutput("resultsByGenre")
                          )
                        )
                        
                ),
                tabItem(tabName = "ByRating",
                        h2("Recommend by Rating"),
                        fluidRow(
                          box(width = 12, title = "Step 1: Rate as many Movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                              div(class = "rateitems",
                                  uiOutput('ratings')
                              )
                          )
                        ),
                        fluidRow(
                          useShinyjs(),
                          box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Step 2: Discover Movies you might like",
                            br(),
                            withBusyIndicatorUI(
                              actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                            ),
                            br(),
                            tableOutput("results")
                          )
                        )
                )
              )
)

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          sidebar,
          body
    )
) 