
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
           
  tags$head(tags$style(
    type="text/css",
    "#maize img {max-width: 100%; width: 100%; height: auto}"
  )),
  # Vizualize image ----------
  
  # Application title
  navbarPage("Zea Lip, an atlas of glycerolipid content in maize",

     tabPanel("View plants", id="tab1", icon = icon("leaf"),
              # fluidRow(
              #   column(4,
                       helpText("Choose the genotype and developmental stage to vizualise"),
                       selectInput("img_stage", label = "Day After Planting", choices = c(28, 40, 50, 58)),
                       tags$hr(),
                # ),
                # column(7, 
                       h4("Representative plants for each genotype"),
                       imageOutput("maize")
              #   )
              # )
     ),
     
     
 # Load data ----------     
     
  tabPanel("View data", id="tab1", icon = icon("eye"),
      # fluidRow(
      #   column(4,
      #          helpText("This is the Developmental Stages dataset.")
      #   ),
      #   column(7, 
           tabsetPanel(
             tabPanel("Greenhouse data",
               tags$hr(),
               downloadButton('download_dataset', 'Download full table'),
               tags$hr(),
               DT::dataTableOutput('distribution_data')
             ),
             tabPanel("Field data",
                tags$hr(),
                downloadButton('download_dataset_field', 'Download full table'),
                tags$hr(),
                DT::dataTableOutput('field_data')
             )
           )
      #   )
      # )
    ),
  
  
  # Boxplots ----------
  
    tabPanel("Plot individual data", id="tab2", icon = icon('sliders'),
     tabsetPanel(
       tabPanel("Greenhouse data",
        tags$hr(),
        fluidRow(
          column(3, 
                 helpText("Choose the genotypes, variables, lipid classes and colours to map"),
                 checkboxInput("plot_sum", label = "Plot summed classes instead", value=F),
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
      tabPanel("Field data",
        tags$hr(),               
        fluidRow(
          column(3, 
                 checkboxInput("plot_sum_field", label = "Plot summed classes instead", value=F),
                 checkboxInput("add_greenhouse", label = "Add greenhouse data", value=F),
                 tags$hr(),
                 checkboxInput("isboxplot_field", label="Want boxplot instead?"),
                 tags$hr(),
                 checkboxInput("more_filters_field", label="More filters?", value = F),
                 conditionalPanel(
                   condition = "input.more_filters_field == true",
                   selectInput("genotypes_to_plot_field", label="Filter genotypes", choices = c("Load datafile"), 
                               selected = NULL, multiple = TRUE, width="100%")
                 )
          ),
          column(8,
                 selectInput("to_plot_2_field", label = "Variable to plot", choices = c("")),
                 plotOutput("my_plot_field")
          )
        )
      )
       )
    ),
  
  
  # Heatmaps ----------
  
  tabPanel("Correlation", id="tab2", icon = icon('bolt'),
           fluidRow(
             column(7, 
                    selectInput("corr_to_plot", label = "Variable to plot", choices = c("r-squares", "Spearman", "Pearson")),
                    plotOutput("correlation_heatmap"),#, click = "heatmap_click")             
                    checkboxInput("correlation_individual", "Load individual data instead", value=F),
                    checkboxInput("process_indiv_corr", "re-Process individual data", value=F)
             ),
             column(5,
                    fluidRow(
                      column(6,selectInput("variable_corr_1", label="X Variable", choices = c("Load datafile"))), 
                      column(6,selectInput("variable_corr_2", label="Y Variable", choices = c("Load datafile"))) 
                    ),
                    
                    plotOutput("correlation_plot"),#, click = "heatmap_click")     
                    tags$hr(),
                    h4("Global correlation parameters"),
                    textOutput("corr_text"),
                    tags$hr(),
                    fluidRow(
                      column(6,selectInput("to_plot_reg_3", label = "Group by", choices = c(""))),
                      column(6,checkboxInput("correlation_color", "Correlation by group", value=F))
                    )
             )
           )
  ),
  
  
  
  # PCA ----------
    tabPanel("Principal component analysis", id="tab2", icon = icon('bullseye'),
       fluidRow(
         column(3, 
                helpText("Principal component analysis of the whole dataset"),
                selectInput("genotypes_to_plot_2", label="Genotypes to plot", choices = c("Load datafile"), 
                            selected = NULL, multiple = TRUE, width="100%"),
                selectInput("to_plot_4", label = "Colours to plot", choices = c("")),
                selectInput("variable_to_pca", label="Variables NOT TO include in PCA", choices = c("Load datafile"), 
                            selected = NULL, multiple = TRUE, width="100%"),
                checkboxInput("pca_aggregated", "Use summed classes instead", value=F),
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
