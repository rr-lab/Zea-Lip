
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
                  
  # Application title
  navbarPage("Lipidomics",
    tabPanel("Load data", id="tab1", icon = icon("upload"),
      fluidRow(
        column(4,
               helpText("Load the dataset you want to analyze. The Developmental Stages dataset is loaded by default")
        ),
        column(7, 
               
               h4("Overview of the dataset"),
               DT::dataTableOutput('distribution_data')
        )
      )
    ),
    tabPanel("Plot data", id="tab2", icon = icon('sliders'),
      fluidRow(
        column(3, 
               h4("Plot"),
               helpText("Choose the genotypes, variables, lipid classes and colours to map"),
               selectInput("genotypes_to_plot", label="Genotypes to plot", choices = c("Load datafile"), 
                           selected = NULL, multiple = TRUE, width="100%"),
               selectInput("to_plot", label = "Variable to plot", choices = c("")),
               selectInput("to_plot_2", label = "Value to plot", choices = c("")),
               selectInput("to_plot_3", label = "Colours to plot", choices = c(""))
        ),
        column(8,
               
               plotOutput("my_plot")
        )
      )
    )
             
  )
))
