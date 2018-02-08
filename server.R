
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
      vars <- unique(rs$lipids$leaf_zone)
      ct_options <- list()
      sel <- input$zone_to_plot
      if(length(sel) == 0) sel = vars
      for(ct in vars) ct_options[[ct]] <- ct
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "zone_to_plot", choices = ct_options, selected=sel) 
    }) 
    
    observe({
      if(is.null(rs$lipids)){return()}
      vars <- unique(rs$lipids$genotype)
      ct_options <- list()
      sel <- input$genotypes_to_plot_2
      if(length(sel) == 0) sel = vars
      for(ct in vars) ct_options[[ct]] <- ct
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "genotypes_to_plot_2", choices = ct_options, selected=sel) 
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
    
    observe({
      if(is.null(rs$lipids)){return()}
      vars <- colnames(rs$lipids)[c(2:10)]
      ct_options <- list()
      sel <- input$to_plot_4
      for(ct in vars) ct_options[[ct]] <- ct
      if(length(sel) == 0 | sel == "") sel = ct_options[1]
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "to_plot_4", choices = ct_options, selected=sel) 
    })  
    
    observe({
      if(is.null(rs$lipids)){return()}
      vars <- unique(rs$lipids$variable)
      ct_options <- list()
      sel <- input$variable_to_pca
      for(ct in vars) ct_options[[ct]] <- ct
      if(is.null(sel)) sel = ct_options
      if(length(sel) == 0 | sel == "") sel = ct_options
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "variable_to_pca", choices = ct_options, selected=sel) 
    })     
    
    ############################################################
    ### PLOTS
    ############################################################
    
    # Time plot
    
  
    
    output$my_plot <- renderPlot({

      temp <- rs$lipids %>%
        filter(genotype %in% input$genotypes_to_plot)%>%
        filter(leaf_zone %in% input$zone_to_plot)%>%
        filter(variable == input$to_plot_2)
      # temp <- data

      temp$value <- as.numeric(temp$value)
      temp$var <- temp[[input$to_plot]]
      temp$var2 <- factor(temp[[input$to_plot_3]])
      
      if(!input$isboxplot){
        pl <- ggplot(temp, aes(x=var, y= value, color = var2)) +
          geom_jitter(width = 0.25) + 
          theme_classic()
      }else{
        pl <- ggplot(temp, aes(x=var, y= value, color = var2)) + 
          geom_boxplot(width = 0.25) + 
          theme_classic()
        
        # ggplot(temp, aes(x=genotype, y= value, color=factor(dev_stage))) + 
        #   geom_boxplot() + 
        #   theme_classic()
      }
      pl
      
    }) 
      
    
    # PCA PLOT
    output$pca_plot <- renderPlot({
      if(is.null(rs$lipids)){return()}
      
      temp <- rs$lipids[rs$lipids$genotype %in% input$genotypes_to_plot_2,]
      samples <- temp$sample_id
      genotypes <- temp$genotype
      vars <- input$variable_to_pca
      cats <- colnames(temp)
      for( i in c(1:2)) cats <- cats[-length(cats)]
      temp <- filter(temp, variable == vars)
      temp <- dcast(temp, cats ~ variable)
      vars <- vars[vars %in%colnames(temp)]
      temp <- temp[,vars]
      
      # remove <- NULL
      # for(i in c(1:ncol(temp))){
      #   if(sd(temp[,i]) == 0) remove <- c(remove,i)
      # }
      
      # if(!is.null(remove)) temp <- temp[,-remove]
      
      str(temp)
      
      pca <- prcomp(temp, retx = T, scale=T)  # Make the PCA
      pca.results <- cbind(plant=plants, genotype=genotypes, data.frame(pca$x)[,])
      
      vars <- apply(pca$x, 2, var)  
      props <- round((vars / sum(vars) * 100), 1)
      xl <- paste0("\nPrincipal Component 1 (",props[1],"%)")
      yl <-paste0("Principal Component 2 (",props[2],"%)\n")
      
      pl1 <- ggplot(data = pca.results) + 
        geom_point(aes(PC1, PC2, colour=genotype)) +
        stat_ellipse(aes(PC1, PC2, colour=genotype), level = 0.9, size=1) + 
        theme_bw() + 
        xlab(xl) + 
        ylab(yl)
      
      z2 <- data.frame(var_names = rownames(pca$rotation), pca$rotation[, 1:2])
      z2$var_names <- gsub("_", " ", z2$var_names)
      
      pl2 <- ggplot(data=z2, aes(0, 0, xend=PC1, yend=PC2)) + 
        geom_segment(col="grey", size=1.2, arrow = arrow(length = unit(0.5,"cm")), alpha=0.9) +
        geom_text_repel(data=z2, aes(PC1, PC2, label=var_names), col="black", size=9) +
        geom_point(aes(x=0, y=0), colour="grey") +
        #scale_y_continuous(limits = c(-1, 0.3)) +
        theme_classic() +
        xlab(xl) + ylab(yl)
      
      pl <- grid.arrange(pl1, pl2, ncol=1)
      
    })
    
    
    ############################################################
    ### TABLE
    ############################################################  
    
    output$distribution_data <- DT::renderDataTable({
      if(is.null(rs$lipids)){return()}
      DT::datatable(rs$lipids, options = list(scrollX = TRUE, pageLength = 5))
    })
    
    
    })
