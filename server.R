
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(
  function(input, output, clientData, session) {  
    
    # Global variable 
    rs <- reactiveValues(lipids = NULL)
    
    
    ## Process data -------
    observe({
    
      if(1==2){    
        data <-  read_csv("www/data_dev_stages.csv")
        field <-  read_csv("www/data_field_long.csv")
        expressions <- read_csv("www/tissue_exp_long_leaf.csv") %>% 
          mutate(dev_stage = as.numeric(gsub("[^0-9]", "", dev_stage)) ) %>% 
          # filter(sec_tissue == "Pooled") %>% 
          filter(dev_stage %in% unique(data$dev_stage))
        
        imgs <-  read_csv("www/illustrations.csv")
        
        data <- data %>%
          mutate(dev_group = ifelse(dev_stage > 3 & dev_stage < 7, "4-6",
                                    ifelse(dev_stage > 7 & dev_stage < 10, "7-9", ">9")))
        
       
        
        # sums of different classes of lipids 
        
        data_sum <- data %>%
          mutate(LPCs = rowSums(data[,57:62])) %>%
          mutate(PCs = rowSums(data[,76:101])) %>%
          mutate(PEs = rowSums(data[,102:112])) %>%
          mutate(PGs = rowSums(data[,113:120])) %>%
          mutate(SQDGs = rowSums(data[,129:135])) %>%
          mutate(TGs = rowSums(data[,136:187])) %>%
          mutate(DGs = rowSums(data[,25:52])) %>%
          mutate(MGDGs = rowSums(data[,63:74])) 
        
        # ratios
        
        data_sum <- data_sum %>%
          mutate(PCLPC = PCs/LPCs) %>%
          mutate(PELPC = PEs/LPCs) %>%
          mutate(PEPC = PEs/PCs)
        
        data_sum <- data_sum[, c(1:10, 188:199)]      
        
        data_sum <- melt(data_sum, id=c("sample_id", "genotype", "block_number", "replica_number", 
                                "dev_stage", "dev_group", "leaf_number", "leaf_zone", "dag", 
                                "tissue_designation", "developmental_stage_dap"))
        
        data <- melt(data, id=c("sample_id", "genotype", "block_number", "replica_number", 
                                        "dev_stage", "dev_group", "leaf_number", "leaf_zone", "dag", 
                                        "tissue_designation", "developmental_stage_dap"))
        
        
        p.list <- unique(data_sum$variable)
        l1 <- length(p.list)
        
        
        field <- read_csv("www/field_data_short.csv")
        
        # sums of different classes of lipids 
        
        field_sum <- field %>%
          mutate(LPCs = rowSums(field[,34:38])) %>%
          mutate(PCs = rowSums(field[,52:74])) %>%
          mutate(PEs = rowSums(field[,75:79])) %>%
          mutate(PGs = rowSums(field[,80:85])) %>%
          mutate(SQDGs = rowSums(field[,92:98])) %>%
          mutate(TGs = rowSums(field[,99:116])) %>%
          mutate(DGs = rowSums(field[,11:30])) %>%
          mutate(MGDGs = rowSums(field[,39:51])) %>%
          mutate(PCLPC = PCs/LPCs) %>%
          mutate(PELPC = PEs/LPCs) %>%
          mutate(PEPC = PEs/PCs)
        
        field_sum <- field_sum[, c(1:2, 123:133)]
        
        field_sum <- field_sum %>%
          gather(variable, value, -ID, -genotype) 
        
        field <- field %>%
          gather(variable, value, -ID, -genotype) 
        
        
        corrs_data_sum <- getCorrelations(data_sum)
        corrs_data <- getCorrelations(data)
        corrs_field_sum <- getCorrelations(field_sum)
        corrs_field <- getCorrelations(field)
        
        save(data, data_sum, field, field_sum, 
             corrs_data_sum,
             corrs_data,
             corrs_field_sum,
             corrs_field,
             imgs, file = "www/all_lidip_data.RData")
        
        
        # fit.results <<- matrix(NA, ncol = l1, nrow = l1) 
        # colnames(fit.results) <- p.list 
        # rownames(fit.results) <- p.list
        # 
        # pearson.results <<- matrix(NA, ncol = l1, nrow = l1) 
        # colnames(pearson.results) <- p.list 
        # rownames(pearson.results) <- p.list
        # 
        # spearman.results <<- matrix(NA, ncol = l1, nrow = l1) 
        # colnames(spearman.results) <- p.list 
        # rownames(spearman.results) <- p.list
        # 
        # fit.table <- NULL
        # 
        # i <- 1
        # k <- 1
        # for(p in p.list){
        #   j <- 1
        #   for(p1 in p.list){
        #     if(p != p1 & is.na(fit.results[j,i])){
        #       
        #       # MANOVA analysis to compare the plants
        #       x <- data_sum$value[data_sum$variable == p]
        #       y <- data_sum$value[data_sum$variable == p1]
        #       
        #       fit.results[j,i] <- summary(lm(y ~ x))$r.squared
        #       fit.results[i,j] <- fit.results[j,i]
        #       
        #       pearson.results[j,i] <- rcorr(x, y, type = "pearson")[[1]][1,2]
        #       pearson.results[i,j] <- pearson.results[j,i]
        #       
        #       spearman.results[j,i] <- rcorr(x, y, type = "spearman")[[1]][1,2]
        #       spearman.results[i,j] <- spearman.results[j,i]
        #       
        #       
        #       fit.table <- rbind(fit.table, data.table(line1 = p, line2=p1, 
        #                                                r2 = fit.results[j,i], 
        #                                                r.pvalue = summary(lm(y ~ x))$coefficients[2, 4],
        #                                                pearson = pearson.results[j,i],
        #                                                pearson.pvalue = rcorr(x, y, type = "pearson")[[3]][1,2],
        #                                                spearman = spearman.results[j,i],
        #                                                spearman.pvalue = rcorr(x, y, type = "spearman")[[3]][1,2]))
        #       fit.table <- rbind(fit.table, data.table(line1 = p1, line2=p, 
        #                                                r2 = fit.results[j,i], 
        #                                                r.pvalue = summary(lm(y ~ x))$coefficients[2, 4],
        #                                                pearson = pearson.results[j,i],
        #                                                pearson.pvalue = rcorr(x, y, type = "pearson")[[3]][1,2],
        #                                                spearman = spearman.results[j,i],
        #                                                spearman.pvalue = rcorr(x, y, type = "spearman")[[3]][1,2]))
        #     }
        #     j <- j+1
        #   }
        #   i <- i+1
        # }    
        # for(i in c(1:length(p.list))){
        #   fit.results[i,i] <- 1
        #   pearson.results[i,i] <- 1
        #   spearman.results[i,i] <- 1
        # }
        # 
        # if(input$process_indiv_corr){
        #   
        #   p.list <- unique(data$variable)
        #   l1 <- length(p.list)
        # 
        #   fit.results.indiv <<- matrix(NA, ncol = l1, nrow = l1)
        #   colnames(fit.results.indiv) <- p.list
        #   rownames(fit.results.indiv) <- p.list
        # 
        #   pearson.results.indiv <<- matrix(NA, ncol = l1, nrow = l1)
        #   colnames(pearson.results.indiv) <- p.list
        #   rownames(pearson.results.indiv) <- p.list
        # 
        #   spearman.results.indiv <<- matrix(NA, ncol = l1, nrow = l1)
        #   colnames(spearman.results.indiv) <- p.list
        #   rownames(spearman.results.indiv) <- p.list
        # 
        #   fit.table.indiv <- NULL
        # 
        #   i <- 1
        #   k <- 1
        #   for(p in p.list){
        #     j <- 1
        #     for(p1 in p.list){
        #       if(p != p1 & is.na(fit.results.indiv[j,i])){
        # 
        #         # MANOVA analysis to compare the plants
        #         x <- data$value[data$variable == p]
        #         y <- data$value[data$variable == p1]
        # 
        #         fit.results.indiv[j,i] <- summary(lm(y ~ x))$r.squared
        #         fit.results.indiv[i,j] <- fit.results.indiv[j,i]
        # 
        #         pearson.results.indiv[j,i] <- rcorr(x, y, type = "pearson")[[1]][1,2]
        #         pearson.results.indiv[i,j] <- pearson.results.indiv[j,i]
        # 
        #         spearman.results.indiv[j,i] <- rcorr(x, y, type = "spearman")[[1]][1,2]
        #         spearman.results.indiv[i,j] <- spearman.results.indiv[j,i]
        # 
        # 
        #         fit.table.indiv <- rbind(fit.table.indiv, data.table(line1 = p, line2=p1,
        #                                                  r2 = fit.results.indiv[j,i],
        #                                                  r.pvalue = summary(lm(y ~ x))$coefficients[2, 4],
        #                                                  pearson = pearson.results.indiv[j,i],
        #                                                  pearson.pvalue = rcorr(x, y, type = "pearson")[[3]][1,2],
        #                                                  spearman = spearman.results.indiv[j,i],
        #                                                  spearman.pvalue = rcorr(x, y, type = "spearman")[[3]][1,2]))
        #       }
        #       j <- j+1
        #     }
        #     i <- i+1
        #   }
        #   
        #   save(spearman.results.indiv, fit.results.indiv, fit.table.indiv, pearson.results.indiv, file="www/indiv_corr.RData")
        # }
        # 
        # load(file = "www/indiv_corr.RData")
        # 

    }
      
      load("www/all_lidip_data.RData")
      
      rs$lipids <- data
      rs$lipids_sum <- data_sum
      
      rs$field <- field
      rs$field_sum <- field_sum
      
      rs$fit.table <- corrs_data_sum$fit.table
      rs$pearson.results <- corrs_data_sum$pearson.results
      rs$spearman.results <- corrs_data_sum$spearman.results
      rs$fit.results <- corrs_data_sum$fit.results
      
      rs$fit.table.indiv <- corrs_data$fit.table.indiv
      rs$pearson.results.indiv <- corrs_data$pearson.results.indiv
      rs$spearman.results.indiv <- corrs_data$spearman.results.indiv
      rs$fit.results.indiv <- corrs_data$fit.results.indiv
      
      rs$fit.table.field <- corrs_field_sum$fit.table
      rs$pearson.results.field <- corrs_field_sum$pearson.results
      rs$spearman.results.field <- corrs_field_sum$spearman.results
      rs$fit.results.field <- corrs_field_sum$fit.results
      
      rs$fit.table.indiv.field <- corrs_field$fit.table.indiv
      rs$pearson.results.indiv.field <- corrs_field$pearson.results.indiv
      rs$spearman.results.indiv.field <- corrs_field$spearman.results.indiv
      rs$fit.results.indiv.field <- corrs_field$fit.results.indiv
      
      
      rs$imgs <- imgs
    })
    

    ### UI commands ---- 
    
    observe({
      if(is.null(rs$lipids_sum)){return()}
      if(input$correlation_individual) vars <- unique(rs$lipids$variable)
        else vars <- unique(rs$lipids_sum$variable)
      ct_options <- list()
      sel <- input$variable_corr_1
      if(nchar(sel) == 0) sel = vars[1]
      for(ct in vars) ct_options[[ct]] <- ct
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "variable_corr_1", choices = ct_options, selected=sel) 
    }) 
    
    observe({
      if(is.null(rs$lipids_sum)){return()}
      if(input$correlation_individual) vars <- unique(rs$lipids$variable)
        else vars <- unique(rs$lipids_sum$variable)
      ct_options <- list()
      sel <- input$variable_corr_2
      if(nchar(sel) == 0) sel = vars[2]
      for(ct in vars) ct_options[[ct]] <- ct
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "variable_corr_2", choices = ct_options, selected=sel) 
    }) 
    
    
    
    observe({
      if(is.null(rs$field_sum)){return()}
      if(input$correlation_individual_field) vars <- unique(rs$field$variable)
      else vars <- unique(rs$field_sum$variable)
      ct_options <- list()
      sel <- input$variable_corr_1_field
      if(nchar(sel) == 0) sel = vars[1]
      for(ct in vars) ct_options[[ct]] <- ct
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "variable_corr_1_field", choices = ct_options, selected=sel) 
    }) 
    
    observe({
      if(is.null(rs$field_sum)){return()}
      if(input$correlation_individual_field) vars <- unique(rs$field$variable)
      else vars <- unique(rs$field_sum$variable)
      ct_options <- list()
      sel <- input$variable_corr_2_field
      if(nchar(sel) == 0) sel = vars[2]
      for(ct in vars) ct_options[[ct]] <- ct
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "variable_corr_2_field", choices = ct_options, selected=sel) 
    }) 
    
    
    # observe({
    #   if(is.null(rs$imgs)){return()}
    #   vars <- unique(rs$imgs$genotype)
    #   ct_options <- list()
    #   sel <- input$img_genotype
    #   if(nchar(sel) == 0) sel = vars[1]
    #   for(ct in vars) ct_options[[ct]] <- ct
    #   # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
    #   updateSelectInput(session, "img_genotype", choices = ct_options, selected=sel) 
    # }) 
    # 
    # observe({
    #   if(is.null(rs$imgs)){return()}
    #   if(nchar(input$img_genotype) == 0){return()}
    #   temp <- rs$imgs[rs$imgs$genotype == input$img_genotype,]
    #   vars <- as.character(unique(temp$stage))
    #   ct_options <- list()
    #   sel <- input$img_stage
    #   if(nchar(sel) == 0) sel = vars[1]
    #   for(ct in vars) ct_options[[ct]] <- ct
    #   updateSelectInput(session, "img_stage", choices = ct_options, selected=sel) 
    # }) 
        
    
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
      if(is.null(rs$field)){return()}
      vars <- unique(rs$field$genotype)
      ct_options <- list()
      sel <- input$genotypes_to_plot_field
      if(length(sel) == 0) sel = vars
      for(ct in vars) ct_options[[ct]] <- ct
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "genotypes_to_plot_field", choices = ct_options, selected=sel) 
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
      if(input$plot_sum) vars <- unique(rs$lipids_sum$variable)
      else vars <- unique(rs$lipids$variable)
      ct_options <- list()
      sel <- input$to_plot_2
      for(ct in vars) ct_options[[ct]] <- ct
      if(length(sel) == 0 | sel == "") sel = ct_options[1]
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "to_plot_2", choices = ct_options, selected=sel) 
    }) 
    
    observe({
      if(is.null(rs$lipids)){return()}
      if(input$plot_sum_field) vars <- unique(rs$field_sum$variable)
      else vars <- unique(rs$field$variable)
      ct_options <- list()
      sel <- input$to_plot_2_field
      for(ct in vars) ct_options[[ct]] <- ct
      if(length(sel) == 0 | sel == "") sel = ct_options[1]
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "to_plot_2_field", choices = ct_options, selected=sel) 
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
      sel <- input$to_plot_reg_3
      for(ct in vars) ct_options[[ct]] <- ct
      if(length(sel) == 0 | sel == "") sel = ct_options[1]
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "to_plot_reg_3", choices = ct_options, selected=sel) 
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
      if(input$pca_aggregated){
        vars <- unique(rs$lipids_sum$variable)
      }else{
        vars <- unique(rs$lipids$variable)
      }
      ct_options <- list()
      sel <- input$variable_to_pca
      for(ct in vars) ct_options[[ct]] <- ct
      # if(is.null(sel)) sel = ct_options
      # if(length(sel) == 0 | sel == "") sel = ct_options
      
      # cts  <- c("tot_root_length","n_laterals","tot_lat_length")
      updateSelectInput(session, "variable_to_pca", choices = ct_options, selected=sel) 
    })     
    
    
    ### PLOTS -----

    ## > Boxplot plot ----
    
    
    output$my_plot <- renderPlot({

      if(is.null(rs$lipids_sum)){return()}
      
      if(input$plot_sum) temp <- rs$lipids_sum
      else  temp <- rs$lipids
      
      temp  <- temp %>%
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
          stat_summary(fun.y = "mean", fun.ymin = "mean", fun.ymax = "mean", size = 0.25,
                       geom = "crossbar")
          
      }else{
        pl <- ggplot(temp, aes(x=factor(var), y= value, color = factor(var2))) + 
          geom_boxplot(width = 0.25)
      }
      
      pl <- pl + 
        ylab(input$to_plot_2) + 
        xlab(input$to_plot)
      
      pl
      
    }) 
    
    
    output$my_plot_field <- renderPlot({
      
      if(is.null(rs$field_sum)){return()}
      
      if(input$plot_sum_field) temp <- rs$field_sum
      else  temp <- rs$field

      print(input$genotypes_to_plot_field)
      temp  <- temp %>%
        filter(genotype %in% input$genotypes_to_plot_field)%>%
        filter(variable == input$to_plot_2_field)
      # temp <- data
      
      temp$value <- as.numeric(temp$value)

      print(str(temp))
      
      if(!input$isboxplot_field){
        print("HELLO")
        pl <- ggplot(temp, aes(x=genotype, y= value, color = factor(genotype))) +
          geom_jitter(width = 0.25) +
          stat_summary(fun.y = "mean", fun.ymin = "mean", fun.ymax = "mean", size = 0.25,
                       geom = "crossbar")
        
      }else{
        pl <- ggplot(temp, aes(x=genotype, y= value, color =  factor(genotype))) + 
          geom_boxplot(width = 0.25)
      }
      
      pl <- pl + 
        ylab(input$to_plot_2_field) + 
        xlab("Genotype")
      pl
      
    })     
    
    
      
    
    ## > Heatmap -----
    
    output$correlation_heatmap <- renderPlot({
      if(is.null(rs$fit.results)){return()}
    
      if(input$correlation_individual){
        if(input$corr_to_plot == "r-squares") print(heatmap(rs$fit.results.indiv))
        else if(input$corr_to_plot == "Spearman") print(heatmap(rs$spearman.results.indiv))
        else  print(heatmap(rs$pearson.results.indiv))
      }else{
        if(input$corr_to_plot == "r-squares") print(heatmap(rs$fit.results))
        else if(input$corr_to_plot == "Spearman") print(heatmap(rs$spearman.results))
        else  print(heatmap(rs$pearson.results))
      }
    })  
    
    
    ## > Heatmap -----
    
    output$correlation_heatmap_field <- renderPlot({
      if(is.null(rs$fit.results.field)){return()}
      
      if(input$correlation_individual_field){
        if(input$corr_to_plot_field == "r-squares") print(heatmap(rs$fit.results.indiv.field))
        else if(input$corr_to_plot_field == "Spearman") print(heatmap(rs$spearman.results.indiv.field))
        else  print(heatmap(rs$pearson.results.indiv.field))
      }else{
        if(input$corr_to_plot_field == "r-squares") print(heatmap(rs$fit.results.field))
        else if(input$corr_to_plot_field == "Spearman") print(heatmap(rs$spearman.results.field))
        else  print(heatmap(rs$pearson.results.field))
      }
    })  
    
    
    ## > Correlation -----
    
    output$correlation_plot_field <- renderPlot({
      if(is.null(rs$field_sum)){return()}
  
      
      if(input$correlation_individual_field){
        temp  <-  rs$field 
      }else{
        temp  <-  rs$field_sum 
      }
      temp  <-  temp %>%
        filter(variable == input$variable_corr_1_field | variable == input$variable_corr_2_field)%>%
        spread(variable, value) 
      temp$x = temp[[input$variable_corr_1_field]]
      temp$y = temp[[input$variable_corr_2_field]]
      temp$group = factor(temp[[input$to_plot_reg_3_field]])
      
      if(input$correlation_color_field){
        ggplot(temp, aes(x, y, colour=group)) + 
          geom_point() + 
          geom_smooth(method="lm", se=F, lty=2)
      }else{
        ggplot(temp, aes(x, y)) + 
          geom_point() + 
          geom_smooth(method="lm", se=F, lty=2)
      }
    })   
    

    
    
    ## > Correlation -----
    
    output$correlation_plot <- renderPlot({
      if(is.null(rs$lipids_sum)){return()}
      
      
      if(input$correlation_individual){
        temp  <-  rs$lipids 
      }else{
        temp  <-  rs$lipids_sum 
      }
      temp  <-  temp %>%
        filter(variable == input$variable_corr_1 | variable == input$variable_corr_2)%>%
        spread(variable, value) 
      temp$x = temp[[input$variable_corr_1]]
      temp$y = temp[[input$variable_corr_2]]
      temp$group = factor(temp[[input$to_plot_reg_3]])
      # temp <- data.frame(x,y)
      
      # fit <- lm(y~x)
      
      if(input$correlation_color){
        ggplot(temp, aes(x, y, colour=group)) + 
          geom_point() + 
          geom_smooth(method="lm", se=F, lty=2)
      }else{
        ggplot(temp, aes(x, y)) + 
          geom_point() + 
          geom_smooth(method="lm", se=F, lty=2)
      }
    })   
    
    
    
    ## > PCA plot -----
    output$pca_plot <- renderPlot({
      if(is.null(rs$lipids)){return()}
      
      # temp <- data
      if(input$pca_aggregated){
        temp <- rs$lipids_sum[rs$lipids_sum$genotype %in% input$genotypes_to_plot_2,]
      }else{
        temp <- rs$lipids[rs$lipids$genotype %in% input$genotypes_to_plot_2,]
      }
      # get the categorial variables
      inds <- c(1:(ncol(temp)-1))
      all_cats <- temp[,inds]
      cats <- colnames(all_cats)
      
      # Get the measurments
      vars <- input$variable_to_pca
      # vars <- unique(temp$variable)
      

      # change from lon to wide format
      if(is.null(vars)){
        vars <- c("")
      }
      temp <- temp %>%
        filter(!(variable %in% vars)) %>%
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
      
      #data[[input$to_plot_4]] <- factor(data[[input$to_plot_4]])
      pca.results$group <- factor(pca.results[[input$to_plot_4]])
      
      ggplot(data = pca.results) + 
        geom_point(aes_string(paste0("PC",input$to_plot_pca_x), paste0("PC",input$to_plot_pca_y), colour="group")) +
        stat_ellipse(aes_string(paste0("PC",input$to_plot_pca_x), paste0("PC",input$to_plot_pca_y), colour="group"), level = 0.9, size=1) + 
        xlab(xl) + 
        ylab(yl) + 
        coord_fixed()
      
    })
    
    

    
    
    ### TABLE -----
    
    output$distribution_data <- DT::renderDataTable({
      if(is.null(rs$lipids)){return()}
      temp <- rbind(rs$lipids, rs$lipids_sum)
      DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 15))
    })
    
    output$field_data <- DT::renderDataTable({
      if(is.null(rs$field)){return()}
      temp <- rbind(rs$field, rs$field_sum)
      DT::datatable(temp, options = list(scrollX = TRUE, pageLength = 15))
    })
    
  
    
    
      
    ### IMAGES ----- 
    
    # image2 sends pre-rendered images
    output$maize <- renderImage({
      
      if(nchar(input$img_stage) == 0) {return()}
      
        return(list(
          src = paste0("www/imgs/",input$img_stage,"_dap.png"),
          contentType = "image/jpg",
          alt = "Face"
        ))
      
    }, deleteFile = FALSE)
    
    
    # image2 sends pre-rendered images
    output$leaf <- renderImage({
      
      return(list(
        src = paste0("www/imgs/leaf_zones.png"),
        contentType = "image/jpg",
        alt = "Face"
      ))
      
    }, deleteFile = FALSE)
    
    
    
    ### TEXT --------
    
    
    output$corr_text <- renderText({
      if(is.null(rs$fit.table)){return()}
      temp <- rs$fit.table %>%
        filter(line1 == input$variable_corr_1 & line2 == input$variable_corr_2)
      
      txt  = paste0("r-squared = ",round(temp$r2,3),"  ||  ","Pearson coefficient = ",round(temp$pearson,3),
                    "  ||  ","Spearman coefficient = ",round(temp$spearman,3))
      return(HTML(txt))
      
    })
    
    
    output$corr_text_field <- renderText({
      if(is.null(rs$fit.table.field)){return()}
      temp <- rs$fit.table.field %>%
        filter(line1 == input$variable_corr_1_field & line2 == input$variable_corr_2_field)
      
      txt  = paste0("r-squared = ",round(temp$r2,3),"  ||  ","Pearson coefficient = ",round(temp$pearson,3),
                    "  ||  ","Spearman coefficient = ",round(temp$spearman,3))
      return(HTML(txt))
      
    })
    
    
    ### DOWNLOAD --------
    output$download_dataset <- downloadHandler(
      filename = function() {"zea_lipid_dataset.csv"},
      content = function(file) {
        temp <- rbind(rs$lipids, rs$lipids_sum)
        write.csv(rs$temp, file)
      }
    )
    
    output$download_dataset_field <- downloadHandler(
      filename = function() {"zea_lipid_field_dataset.csv"},
      content = function(file) {
        temp <- rbind(rs$field, rs$field_sum)
        write.csv(temp, file)
      }
    )
    
    
    })
