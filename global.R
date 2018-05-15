# Copyright © 2017, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2017 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
#   
# 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
# 
# Disclaimer
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# You should have received the GNU GENERAL PUBLIC LICENSE v3 with this file in license.txt but can also be found at http://www.gnu.org/licenses/gpl-3.0.en.html
# 
# NOTE: The GPL.v3 license requires that all derivative work is distributed under the same license. That means that if you use this source code in any other program, you can only distribute that program with the full source code included and licensed under a GPL license.

library(tidyverse)
library(gridExtra)
library(Hmisc)
library(DT)
library(shinyBS)
library(data.table)
#library(plotly)
library(gridExtra)
library(ggrepel)
library(cowplot)

cscale <- c("#ffffcc", "#a1dab4","#41b6c4", "#2c7fb8", "#253494")
cscale1 <- c("#b30000","#e34a33","#fc8d59","#fdcc8a")
cscale2 <- c("#f1eef6","#d7b5d8","#df65b0","#dd1c77","#980043")
cscale2 <- c("#a21d4d","lightgrey","#2a3e98")
cscale3 <- c("#a1dab4","#41b6c4", "#2c7fb8", "#253494")



mydata <- mtcars[, c(1,3,4,5,6,7)]
cormat <- round(cor(mydata),2)



heatmap <- function(corr.matrix){
  #corr.matrix <- fit.results
  dat <- melt(corr.matrix) 

  plot1 <- ggplot(dat, aes(Var1, Var2)) + 
    geom_tile(aes(fill = value)) + 
    theme_classic() + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size=12),
      axis.text = element_text(size=12),
      legend.text = element_text(size=10),
      legend.title = element_text(size=10))+
      #legend.position="none") +
    # scale_fill_distiller(palette="Spectral")+
    xlab("") + ylab("") + 
    coord_fixed() + 
    scale_fill_gradientn(colours=cscale, name="r-squared values")  
  
  plot1
}


getCorrelations <- function(data){
  
  variables <- unique(data$variable)
  l1 = length(variables)
  
  fit.results <<- matrix(NA, ncol = l1, nrow = l1) 
  colnames(fit.results) <- variables 
  rownames(fit.results) <- variables
  
  pearson.results <<- matrix(NA, ncol = l1, nrow = l1) 
  colnames(pearson.results) <- variables 
  rownames(pearson.results) <- variables
  
  spearman.results <<- matrix(NA, ncol = l1, nrow = l1) 
  colnames(spearman.results) <- variables 
  rownames(spearman.results) <- variables
  
  fit.table <- NULL
  
  i <- 1
  k <- 1
  for(p in variables){
    j <- 1
    for(p1 in variables){
      if(p != p1 & is.na(fit.results[j,i])){
        
        # MANOVA analysis to compare the plants
        x <- data$value[data$variable == p]
        y <- data$value[data$variable == p1]
        
        fit.results[j,i] <- summary(lm(y ~ x))$r.squared
        fit.results[i,j] <- fit.results[j,i]
        
        pearson.results[j,i] <- rcorr(x, y, type = "pearson")[[1]][1,2]
        pearson.results[i,j] <- pearson.results[j,i]
        
        spearman.results[j,i] <- rcorr(x, y, type = "spearman")[[1]][1,2]
        spearman.results[i,j] <- spearman.results[j,i]
        
        
        fit.table <- rbind(fit.table, data.table(line1 = p, line2=p1, 
                                                 r2 = fit.results[j,i], 
                                                 r.pvalue = summary(lm(y ~ x))$coefficients[2, 4],
                                                 pearson = pearson.results[j,i],
                                                 pearson.pvalue = rcorr(x, y, type = "pearson")[[3]][1,2],
                                                 spearman = spearman.results[j,i],
                                                 spearman.pvalue = rcorr(x, y, type = "spearman")[[3]][1,2]))
        
        fit.table <- rbind(fit.table, data.table(line1 = p1, line2=p, 
                                                 r2 = fit.results[j,i], 
                                                 r.pvalue = summary(lm(y ~ x))$coefficients[2, 4],
                                                 pearson = pearson.results[j,i],
                                                 pearson.pvalue = rcorr(x, y, type = "pearson")[[3]][1,2],
                                                 spearman = spearman.results[j,i],
                                                 spearman.pvalue = rcorr(x, y, type = "spearman")[[3]][1,2]))
      }
      j <- j+1
    }
    i <- i+1
  }    
  for(i in c(1:length(variables))){
    fit.results[i,i] <- 1
    pearson.results[i,i] <- 1
    spearman.results[i,i] <- 1
  }
  
  return(list(fit.table = fit.table, 
              fit.results = fit.results, 
              pearson.results = pearson.results, 
              spearman.results = spearman.results))
  
}


