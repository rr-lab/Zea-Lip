
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
      imgs <-  read_csv("www/illustrations.csv")
      
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
      rs$imgs <- imgs
    })
    

    ############################################################
    ### UI commands
    ############################################################
    # Update the user interface item based on the data

    observe({
      if(is.null(rs$imgs)){return()}
      vars <- unique(rs$imgs$genotype)
      ct_options <- list()
      sel <- input$img_genotype
      if(nchar(sel) == 0) sel = vars[1]
      for(ct in vars) ct_options[[ct]] <- ct
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "img_genotype", choices = ct_options, selected=sel) 
    }) 
    
    observe({
      if(is.null(rs$imgs)){return()}
      if(nchar(input$img_genotype) == 0){return()}
      temp <- rs$imgs[rs$imgs$genotype == input$img_genotype,]
      vars <- as.character(unique(temp$stage))
      ct_options <- list()
      sel <- input$img_stage
      if(nchar(sel) == 0) sel = vars[1]
      for(ct in vars) ct_options[[ct]] <- ct
      updateSelectInput(session, "img_stage", choices = ct_options, selected=sel) 
    }) 
        
    
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
      # if(is.null(sel)) sel = ct_options
      # if(length(sel) == 0 | sel == "") sel = ct_options
      
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
        pl <- ggplot(temp, aes(x=factor(var), y= value, color = factor(var2))) + 
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
      
      temp <- data
      
      temp <- rs$lipids[rs$lipids$genotype %in% input$genotypes_to_plot_2,]

      # get the categorial variables
      inds <- c(1:(ncol(temp)-1))
      all_cats <- temp[,inds]
      cats <- colnames(all_cats)
      
      # Get the measurments
      vars <- input$variable_to_pca
      # vars <- unique(temp$variable)
      print(vars)

      # change from lon to wide format
      if(is.null(vars)){
        vars <- c("")
      }
      temp <- temp %>%
        filter(variable != vars) %>%
        spread(variable, value)
      
      
      # temp <- dcast(temp, cats ~ variable)
      # vars <- vars[vars %in% colnames(temp)]
      # temp <- temp[, -vars]
      
      # remove <- NULL
      # for(i in c(1:ncol(temp))){
      #   if(sd(temp[,i]) == 0) remove <- c(remove,i)
      # }
      
      # if(!is.null(remove)) temp <- temp[,-remove]
      
      pca <- prcomp(temp[,-inds], retx = T, scale=T)  # Make the PCA
      pca.results <- cbind(all_cats, data.frame(pca$x)[,])
      
      print(pca$rotation)
      
      vars <- apply(pca$x, 2, var)  
      props <- round((vars / sum(vars) * 100), 1)
      xl <- paste0("\nPrincipal Component ",input$to_plot_pca_x," (",props[as.numeric(input$to_plot_pca_x)],"%)")
      yl <-paste0("Principal Component ",input$to_plot_pca_y," (",props[as.numeric(input$to_plot_pca_y)],"%)\n")
      
      print(paste0("PC",input$to_plot_pca_y))
      
      data[[input$to_plot_4]] <- factor(data[[input$to_plot_4]])
      
      ggplot(data = pca.results) + 
        geom_point(aes_string(paste0("PC",input$to_plot_pca_x), paste0("PC",input$to_plot_pca_y), colour=input$to_plot_4)) +
        stat_ellipse(aes_string(paste0("PC",input$to_plot_pca_x), paste0("PC",input$to_plot_pca_y), colour=input$to_plot_4), level = 0.9, size=1) + 
        theme_bw() + 
        xlab(xl) + 
        ylab(yl) + 
        coord_fixed()
      
    })
    
    
    ############################################################
    ### TABLE
    ############################################################  
    
    output$distribution_data <- DT::renderDataTable({
      if(is.null(rs$lipids)){return()}
      DT::datatable(rs$lipids, options = list(scrollX = TRUE, pageLength = 15))
    })
    
    
    ############################################################
    ### IMAGE
    ############################################################      
    
    # image2 sends pre-rendered images
    output$maize <- renderImage({
      
      if(nchar(input$img_stage) == 0) {return()}
      print(input$img_genotype)
      temp <- rs$imgs[rs$imgs$genotype == input$img_genotype & rs$imgs$stage == input$img_stage,]
      
        return(list(
          src = paste0("www/imgs/",temp$name[1]),
          contentType = "image/jpg",
          alt = "Face"
        ))
      
    }, deleteFile = FALSE)
    
    
    })
