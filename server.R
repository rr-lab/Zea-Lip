
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(
  function(input, output, clientData, session) {  
    
    # global variable that can be re-call across functions
    rs <- reactiveValues(lipids = NULL)
    
    
    # Load and prepare the dataset
    observe({
      
      data <-  read_csv("www/data_dev_stages.csv")
      
      data <- data %>%
        mutate(dev_group = ifelse(dev_stage > 3 & dev_stage < 7, "4-6",
                                  ifelse(dev_stage > 7 & dev_stage < 10, "7-9", ">9")))
      
      # sums of different classes of lipids 
      
      data <- data %>%
        mutate(LPCs = rowSums(data[,57:62])) %>%
        mutate(PCs = rowSums(data[,76:101])) %>%
        mutate(PEs = rowSums(data[,102:112])) %>%
        mutate(PGs = rowSums(data[,113:120])) %>%
        mutate(SQDGs = rowSums(data[,129:135])) %>%
        mutate(TGs = rowSums(data[,136:187])) %>%
        mutate(DGs = rowSums(data[,25:52])) %>%
        mutate(MGDGs = rowSums(data[,63:74])) 
      
      # ratios
      
      data <- data %>%
        mutate(PCLPC = PCs/LPCs) %>%
        mutate(PELPC = PEs/LPCs) %>%
        mutate(PEPC = PEs/PCs)
      
      data <- data[, c(1:16, 25:199)]      
      
      data <- melt(data, id=c("sample_id", "genotype", "block_number", "replica_number", "dev_stage", "dev_group", "leaf_number", "leaf_zone", "dag", "tissue_designation", "developmental_stage_dap"))
      
      rs$lipids <- data
    })
    

    ############################################################
    ### UI commands
    ############################################################
    # Update the user interface item based on the data
    
    
    observe({
      if(is.null(rs$lipids)){return()}
      vars <- unique(rs$lipids$genotype)
      ct_options <- list()
      sel <- input$genotypes_to_plot
      if(length(sel) == 0) sel = vars
      for(ct in vars) ct_options[[ct]] <- ct
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "genotypes_to_plot", choices = ct_options, selected=sel) 
    }) 
    
    observe({
      if(is.null(rs$lipids)){return()}
      vars <- unique(rs$lipids$variable)
      ct_options <- list()
      sel <- input$to_plot_2
      for(ct in vars) ct_options[[ct]] <- ct
      if(length(sel) == 0 | sel == "") sel = ct_options[1]
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "to_plot_2", choices = ct_options, selected=sel) 
    }) 
    
    
    
    
    observe({
      if(is.null(rs$lipids)){return()}
      vars <- colnames(rs$lipids)[c(2:10)]
      ct_options <- list()
      sel <- input$to_plot
      for(ct in vars) ct_options[[ct]] <- ct
      if(length(sel) == 0 | sel == "") sel = ct_options[1]
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "to_plot", choices = ct_options, selected=sel) 
    }) 
    
    observe({
      if(is.null(rs$lipids)){return()}
      vars <- colnames(rs$lipids)[c(2:10)]
      ct_options <- list()
      sel <- input$to_plot_3
      for(ct in vars) ct_options[[ct]] <- ct
      if(length(sel) == 0 | sel == "") sel = ct_options[1]
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "to_plot_3", choices = ct_options, selected=sel) 
    })     
    
    ############################################################
    ### PLOTS
    ############################################################
    
    # Time plot
    
    output$my_plot <- renderPlot({

      temp <- rs$lipids
      temp <- temp[temp$genotype %in% input$genotypes_to_plot,]
      temp <- temp[temp$variable == input$to_plot_2,]
      temp$value <- as.numeric(temp$value)
      
      print(str(temp))
      
      pl <- ggplot(temp, aes_string(x=input$to_plot, y= "value", color = input$to_plot_3)) + 
        geom_jitter(width = 0.25) + 
        theme_classic()
      pl
      
    }) 
    
    
    
    
    ############################################################
    ### TABLE
    ############################################################  
    
    output$distribution_data <- DT::renderDataTable({
      if(is.null(rs$lipids)){return()}
      DT::datatable(rs$lipids, options = list(scrollX = TRUE, pageLength = 5))
    })
    
    
    })
