
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
             tabPanel("Data",
               tags$hr(),
               downloadButton('download_dataset', 'Download full table'),
               tags$hr(),
               DT::dataTableOutput('distribution_data')
             )
             ## FIELD DATA REMOVED
             # tabPanel("Field data",
             #    tags$hr(),
             #    downloadButton('download_dataset_field', 'Download full table'),
             #    tags$hr(),
             #    DT::dataTableOutput('field_data')
             # )
           )
      #   )
      # )
    ),
  
  
  # Boxplots ----------
  
    tabPanel("Plot individual data", id="tab2", icon = icon('sliders'),
     tabsetPanel(
       tabPanel("Data",
        tags$hr(),
        fluidRow(
          column(3, 
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
                 checkboxInput("plot_sum", label = "Plot summed classes instead", value=F),
                 selectInput("to_plot_2", label = "Variable to plot", choices = c("")),
                 plotOutput("my_plot")
          )
        )
      )
      
      # tabPanel("Field data",
      #   tags$hr(),               
      #   fluidRow(
      #     column(3, 
      #            #checkboxInput("add_greenhouse", label = "Add greenhouse data", value=F),
      #            #tags$hr(),
      #            checkboxInput("isboxplot_field", label="Want boxplot instead?"),
      #            tags$hr(),
      #            checkboxInput("more_filters_field", label="More filters?", value = F),
      #            conditionalPanel(
      #              condition = "input.more_filters_field == true",
      #              selectInput("genotypes_to_plot_field", label="Filter genotypes", choices = c("Load datafile"), 
      #                          selected = NULL, multiple = TRUE, width="100%")
      #            )
      #     ),
      #     column(8,
      #            checkboxInput("plot_sum_field", label = "Plot summed classes instead", value=F),
      #            selectInput("to_plot_2_field", label = "Variable to plot", choices = c("")),
      #            plotOutput("my_plot_field")
      #     )
      #   )
      # )
       )
    ),
  
  
  # Heatmaps ----------
  
  tabPanel("Correlation", id="tab2", icon = icon('bolt'),
           
     tabsetPanel(
       tabPanel("Data",
            tags$hr(),
           fluidRow(
             column(7,
                    selectInput("corr_to_plot", label = "Variable to plot", choices = c("r-squares", "Spearman", "Pearson")),
                    plotOutput("correlation_heatmap"),
                    checkboxInput("correlation_individual", "Load individual data instead", value=F),
                    tags$hr(),
                    h4("Correlation Values Table"),
                    helpText("Search and sort to find specific correlations"),
                    DT::dataTableOutput("correlation_table")
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
       )
       # tabPanel("Field data",
       #          tags$hr(),
       #          fluidRow(
       #            column(7, 
       #                   selectInput("corr_to_plot_field", label = "Variable to plot", choices = c("r-squares", "Spearman", "Pearson")),
       #                   plotOutput("correlation_heatmap_field"),#, click = "heatmap_click")             
       #                   checkboxInput("correlation_individual_field", "Load individual data instead", value=F)#,
       #                   # checkboxInput("process_indiv_corr_field", "re-Process individual data", value=F)
       #            ),
       #            column(5,
       #                   fluidRow(
       #                     column(6,selectInput("variable_corr_1_field", label="X Variable", choices = c("Load datafile"))), 
       #                     column(6,selectInput("variable_corr_2_field", label="Y Variable", choices = c("Load datafile"))) 
       #                   ),
       #                   
       #                   plotOutput("correlation_plot_field"),#, click = "heatmap_click")     
       #                   tags$hr(),
       #                   h4("Global correlation parameters"),
       #                   textOutput("corr_text_field"),
       #                   tags$hr(),
       #                   fluidRow(
       #                     column(6,selectInput("to_plot_reg_3_field", label = "Group by", choices = c("genotype"))),
       #                     column(6,checkboxInput("correlation_color_field", "Correlation by group", value=F))
       #                   )
       #            )
       #          )
       # )
     )
  ),
  
  
  
  # PCA ----------
    tabPanel("Principal component analysis", id="tab2", icon = icon('bullseye'),
             
       tabsetPanel(
         tabPanel("Data",
                  tags$hr(),
                   fluidRow(
                     column(3, 
                            helpText("Principal component analysis of the whole dataset"),
                            selectInput("genotypes_to_plot_2", label="Genotypes to plot", choices = c("Load datafile"), 
                                        selected = NULL, multiple = TRUE, width="100%"),
                            selectInput("to_plot_4", label = "Colours to plot", choices = c("")),
                            selectInput("variable_to_pca", label="Variables NOT TO include in PCA", choices = c("Load datafile"), 
                                        selected = NULL, multiple = TRUE, width="100%"),
                            checkboxInput("pca_aggregated", "Use summed classes instead", value=F),
                            tags$hr()  ,
                            numericInput("n_loadings", "Number of components:", 10, min = 1, max = 100),
                            plotOutput("loadings_plot")
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
         # tabPanel("Field data",
         #          tags$hr(),
         #          fluidRow(
         #            column(3, 
         #                   helpText("Principal component analysis of the field dataset"),
         #                   selectInput("genotypes_to_plot_2_field", label="Genotypes to plot", choices = c("Load datafile"), 
         #                               selected = NULL, multiple = TRUE, width="100%"),
         #                   selectInput("to_plot_4_field", label = "Colours to plot", choices = c("")),
         #                   selectInput("variable_to_pca_field", label="Variables NOT TO include in PCA", choices = c("Load datafile"), 
         #                               selected = NULL, multiple = TRUE, width="100%"),
         #                   checkboxInput("pca_aggregated_field", "Use summed classes instead", value=F),
         #                   tags$hr(),
         #                   numericInput("n_loadings_field", "Number of components:", 10, min = 1, max = 100),
         #                   plotOutput("loadings_plot_field")
         #            ),
         #            column(7,
         #                   h4("Plot the principal component analysis"),
         #                   tags$hr(),
         #                   fluidRow(
         #                     column(6, 
         #                            selectInput("to_plot_pca_x_field", label = "PC to plot on X axis", choices = c(1:10))
         #                     ),
         #                     column(6, 
         #                            selectInput("to_plot_pca_y_field", label = "PC to plot on Y axis", choices = c(1:10), selected = 2)
         #                     )
         #                   ),
         #                   plotOutput("pca_plot_field", height = 600)
         #            )
         #          )
         # )
       )
    ),

  # Gene expression ----------
  tabPanel("Gene expression", id="tab3", icon = icon("dna"),
     tabsetPanel(
       tabPanel("Gene explorer",
          tags$hr(),
          fluidRow(
            column(3,
                   helpText("Search a curated glycerolipid gene by ID or enzyme name. Expression comes from the
                            Stelpflug et al. 2016 B73 leaf series and the NAM founders (Hufford et al. 2021),
                            sampled at the base, middle and tip of the 10th leaf at V11."),
                   selectizeInput("gx_gene", label = "Gene", choices = NULL, width = "100%"),
                   htmlOutput("gx_gene_info"),
                   tags$hr(),
                   radioButtons("gx_lipid_axis", label = "Show linked Zea Lip lipids by",
                                choices = c("Leaf zone" = "zone", "Leaf position" = "leaf"))
            ),
            column(9,
                   fluidRow(
                     column(6, plotOutput("gx_stelpflug_plot", height = 450)),
                     column(6, plotOutput("gx_nam_plot", height = 450))
                   ),
                   tags$hr(),
                   h4("Linked Zea Lip lipids"),
                   helpText("Lipid classes linked a priori to this gene's pathway modules."),
                   plotOutput("gx_lipid_plot", height = 450),
                   tags$hr(),
                   h4("Expression changes on the matched contrasts"),
                   DT::dataTableOutput("gx_contrast_table")
            )
          )
       ),
       tabPanel("Pathways vs lipids",
          tags$hr(),
          helpText("Colour is signed -log10 p (red up, blue down). Labels give log2 fold change where p < 0.05; * marks FDR < 0.05.
                   Pathway modules are tested against all expressed genes with limma camera."),
          h4("Zea Lip lipid classes"),
          plotOutput("gx_lipid_heatmap", height = 520),
          tags$hr(),
          h4("Pathway modules in the expression atlases"),
          plotOutput("gx_module_heatmap", height = 620),
          tags$hr(),
          h4("Pathway to lipid concordance"),
          helpText("Each row is one a priori pathway to lipid link in one matched comparison. A link is concordant when the
                   lipid moved in the direction its pathway's expression predicts."),
          DT::dataTableOutput("gx_concordance_table"),
          tags$hr(),
          fluidRow(column(4, selectInput("gx_module", label = "Genes in pathway module", choices = NULL, width = "100%"))),
          DT::dataTableOutput("gx_module_genes")
       ),
       tabPanel("Lipid to genes",
          tags$hr(),
          fluidRow(
            column(3,
                   selectInput("gx_lipid", label = "Lipid class or ratio", choices = NULL),
                   helpText("Candidate genes belong to a pathway linked to the lipid, change at FDR < 0.05 with
                            log2 CPM > 1, and move in the direction that matches a nominally significant lipid change
                            on the same axis.")
            ),
            column(9,
                   h4("Lipid changes in Zea Lip"),
                   DT::dataTableOutput("gx_lipid_table"),
                   tags$hr(),
                   h4("Candidate genes"),
                   DT::dataTableOutput("gx_candidate_table")
            )
          )
       ),
       tabPanel("HPC1 and leaf phase",
          tags$hr(),
          fluidRow(
            column(6, plotOutput("gx_hpc1_plot", height = 450)),
            column(6, plotOutput("gx_phase_plot", height = 450))
          ),
          tags$hr(),
          h4("PC/LPC in adult (leaves 8-12) vs juvenile (leaves 3-4) leaves"),
          DT::dataTableOutput("gx_phase_table")
       ),
       tabPanel("About this analysis",
          fluidRow(
            column(3),
            column(6,
                   h4("What this tab shows"),
                   helpText("Zea Lip lipid changes are compared with expression of 597 curated glycerolipid genes
                            (High-PC1 paper, Supplementary File 2) along three axes that both datasets sample: leaf zone
                            (tip vs base), leaf position (per leaf up the plant) and genotype (CML312 vs B73, compared with
                            tropical vs temperate NAM founders). The atlases are separate experiments, mostly B73, so this
                            compares directions of change. It is not a sample-level correlation between genes and lipids."),
                   h4("Data"),
                   helpText("Stelpflug et al. 2016 B73 atlas (Expression Atlas E-MTAB-4342) and Hufford et al. 2021 NAM founders
                            (E-MTAB-8633, B73 from E-MTAB-8628), re-analysed from per-run counts with voom/limma. NAM reads are
                            mapped to the B73 v5 reference; v4 gene IDs were matched with the MaizeGDB crosswalk."),
                   h4("Caveats"),
                   helpText("Growth conditions, sampling times and plant ages differ between the datasets. Palomero Toluqueno has
                            no counterpart in either atlas. HPC1 cannot be measured in the NAM data because its split v5 gene
                            model receives no unambiguous reads. Leaf numbers follow the metadata leaf_number column; sample
                            D13B14A is treated as leaf 4."),
                   tags$hr()
            )
          )
       )
     )
  ),

 tabPanel("About", id="tab1", icon = icon("plus-circle"),
          fluidRow(
            column(3),
            column(6,
                   h4("What is Zea Lip?"),
                   helpText("Zea Lip is an atlas of glycerolipid profiles across leaf development in maize.
                            It provides lipid composition data from three maize genotypes (B73, CML312, and Palomero Toluqueno)
                            across multiple developmental stages and leaf zones, allowing users to explore and compare
                            lipid levels across tissues and genotypes. For more information, visit:"),
                   tags$a(href="https://github.com/rr-lab/Zea-Lip", "https://github.com/rr-lab/Zea-Lip", target="_blank"),
                   tags$hr(),
                   h4("How to cite us"),
                   tags$strong("Zea Lip: An atlas of glycerolipid profiles across leaf development in maize"),
                   helpText("Allison C. Barnes*, Karla Juarez Nunez*, Guillaume Lobet, Nirwan Tandukar, Eric Jadidzadeh, Asher Pasha, Nicholas J. Provart, James B. Holland, Ruben Rellan-Alvarez"),
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
