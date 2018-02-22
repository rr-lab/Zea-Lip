
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
    tabPanel("Plot individual data", id="tab2", icon = icon('sliders'),
      fluidRow(
        column(3, 
               h4("Plot"),
               helpText("Choose the genotypes, variables, lipid classes and colours to map"),
               selectInput("to_plot", label = "Grouping variable", choices = c("")),
               selectInput("to_plot_3", label = "Coloring variable", choices = c("")),
               tags$hr(),
               checkboxInput("isboxplot", label="Want boxplot instead?"),
               tags$hr(),
               checkboxInput("more_filters", label="More filters?", value = F),
               conditionalPanel(
                 condition = "input.more_filters == true",
                 selectInput("genotypes_to_plot", label="Filter genotypes", choices = c("Load datafile"), 
                             selected = NULL, multiple = TRUE, width="100%"),
                 selectInput("zone_to_plot", label="Filter leaf zones", choices = c("Load datafile"), 
                             selected = NULL, multiple = TRUE, width="100%")
               )
        ),
        column(8,
               selectInput("to_plot_2", label = "Variable to plot", choices = c("")),
               plotOutput("my_plot")
        )
      )
    ),
    tabPanel("Principal component analysis", id="tab2", icon = icon('bullseye'),
       fluidRow(
         column(3, 
                helpText("Principal component analysis of the whole dataset"),
                selectInput("genotypes_to_plot_2", label="Genotypes to plot", choices = c("Load datafile"), 
                            selected = NULL, multiple = TRUE, width="100%"),
                selectInput("to_plot_4", label = "Colours to plot", choices = c("")),
                selectInput("variable_to_pca", label="Variables NOT TO include in PCA", choices = c("Load datafile"), 
                            selected = NULL, multiple = TRUE, width="100%"),
                tags$hr()         
        ),
         column(7,
                h4("Plot the principal component analysis"),
                tags$hr(),
                fluidRow(
                  column(6, 
                        selectInput("to_plot_pca_x", label = "PC to plot on X axis", choices = c(1:10))
                  ),
                  column(6, 
                        selectInput("to_plot_pca_y", label = "PC to plot on Y axis", choices = c(1:10), selected = 2)
                  )
                ),
                plotOutput("pca_plot", height = 600)
         )
       )
    )
    
             
  )
))
