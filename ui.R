
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
           
  tags$head(tags$style(
    type="text/css",
    "#maize img {max-width: 100%; width: 100%; height: auto}"
  )),
  
  tags$head(tags$style(
    type="text/css",
    "#leaf img {max-width: 100%; width: 100%; height: auto}"
  )),
  # Vizualize image ----------
  
  # Application title
  navbarPage("Zea Lip, an atlas of glycerolipid content in maize",

     tabPanel("View plants", id="tab1", icon = icon("leaf"),
              tabsetPanel(
                tabPanel("Whole plant",
                       helpText("Choose the genotype and developmental stage to vizualise"),
                       selectInput("img_stage", label = "Day After Planting", choices = c(28, 40, 50, 58)),
                       tags$hr(),

                      h4("Representative plants for each genotype"),
                      imageOutput("maize")
                ),
                tabPanel("Leaf",
                      h4("Leaf sampling positions"),
                      imageOutput("leaf")
                )
              )
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
           
     tabsetPanel(
       tabPanel("Greenhouse data",
            tags$hr(),
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
       tabPanel("Field data",
                tags$hr(),
                fluidRow(
                  column(7, 
                         selectInput("corr_to_plot_field", label = "Variable to plot", choices = c("r-squares", "Spearman", "Pearson")),
                         plotOutput("correlation_heatmap_field"),#, click = "heatmap_click")             
                         checkboxInput("correlation_individual_field", "Load individual data instead", value=F),
                         checkboxInput("process_indiv_corr_field", "re-Process individual data", value=F)
                  ),
                  column(5,
                         fluidRow(
                           column(6,selectInput("variable_corr_1_field", label="X Variable", choices = c("Load datafile"))), 
                           column(6,selectInput("variable_corr_2_field", label="Y Variable", choices = c("Load datafile"))) 
                         ),
                         
                         plotOutput("correlation_plot_field"),#, click = "heatmap_click")     
                         tags$hr(),
                         h4("Global correlation parameters"),
                         textOutput("corr_text_field"),
                         tags$hr(),
                         fluidRow(
                           column(6,selectInput("to_plot_reg_3_field", label = "Group by", choices = c("genotype"))),
                           column(6,checkboxInput("correlation_color_field", "Correlation by group", value=F))
                         )
                  )
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
    ),

 tabPanel("About", id="tab1", icon = icon("plus-circle"),
          fluidRow(
            column(3),
            column(6,
                   h4("What is the Zea Lip? "),
                   helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, 
                            sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. 
                            Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris 
                            nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in "),
                   actionButton(inputId='ab2', label="View source code", icon = icon("flask"), onclick ="window.open('https://github.com/rr-lab/zea_lip', '_blank')"),                                              
                   tags$hr(),
                   h4("How to contribute"),
                   helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit, 
                            sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."),
                   tags$hr(),
                   h4("How to cite us"),
                   tags$strong("Zea Lip: An atlas of glycerolipid species across development in maize B73, CML312 inbreds and Palomero Toluqueño landrace."),
                   helpText("Karla Juárez-Núñez, Guillaume Lobet,  Rubén Rellán-Álvarez"),
                   #actionButton(inputId='ab1', label="View paper", icon = icon("flask"), onclick ="window.open('http://link.springer.com/10.1007/s11104-015-2673-4', '_blank')"),                                              
                   tags$hr(),
                   h4("Licence"),
                   helpText("Zea Lip is free to use and released under a GPL licence. It means that redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v2 and provided that the following conditions are met:
                            
                            1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
                            
                            2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
                            
                            3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission."),                    
                   tags$hr()                   
                   )
                   )
          )
    
             
  )
))
