setwd("D:\\A-DATA\\GoogleDriveYork\\THESIS\\scripts")

library(plyr)
library(extrafont)
# pdfFonts()
fontType <- "serif"
pdfWidth <- 3.54
pdfHeight <- 5
# pdfWidth <- 7.07
# pdfHeight <- 10

generate_graph <- function(type, dimension, case, filename, target_file, max_y) {
  
  raw <- read.table(filename, sep = ",", header = TRUE)
  
  events <- raw$levc + raw$revc
  elements <- raw$lelc + raw$relc
  data_ecbp <- NULL
  data_emfc <- NULL
  data_e <- NULL
  label_y <- NULL
  data <- NULL
  legend_labels <- NULL
  legend_colors <- NULL
  legend_bg_colors <- NULL
  legend_columns <- NULL
  legend_pch <- NULL
  
  # GENERAL --------------------------------------------------------
  if (type == "general") {
    if (dimension == "time") {
      data_ecbp <- data.frame((events) / 1000, raw$cct / 1000000000)
      data_ecbp$group <- 21
      data_ecbp$color <- "white"
      colnames(data_ecbp) <- c("events", "value", "group", "color")
      
      data_emfc <- data.frame((events) / 1000, raw$sct / 1000000000)
      data_emfc$group <- 21
      data_emfc$color <- "black"
      colnames(data_emfc) <- c("events", "value", "group", "color")
      
      data_emfs <- data.frame((events) / 1000, raw$ect / 1000000000)
      data_emfs$group <- 21
      data_emfs$color <- "grey"
      colnames(data_emfs) <- c("events", "value", "group", "color")
      
      label_y <- "Execution Time (seconds)"
      
    } else if (dimension == "memory") {
      data_ecbp <- data.frame((events) / 1000, raw$ccm / 1000000000)
      data_ecbp$group <- 21
      data_ecbp$color <- "white"
      colnames(data_ecbp) <- c("events", "value", "group", "color")
      
      data_emfc <- data.frame((events) / 1000, raw$scm / 1000000000)
      data_emfc$group <- 21
      data_emfc$color <- "black"
      colnames(data_emfc) <- c("events", "value", "group", "color")
      
      data_emfs <- data.frame((events) / 1000, raw$ecm / 1000000000)
      data_emfs$group <- 21
      data_emfs$color <- "grey"
      colnames(data_emfs) <- c("events", "value", "group", "color")
      
      label_y <- "Memory Footprint (GBs)"
      
    } else if (dimension == "count") {
      data_ecbp <- data.frame((events) / 1000, raw$cxc / 1000)
      data_ecbp$group <- 21
      data_ecbp$color <- "white"
      colnames(data_ecbp) <- c("events", "value", "group", "color")
      
      data_emfc <- data.frame((events) / 1000, raw$sxc / 1000)
      data_emfc$group <- 21
      data_emfc$color <- "black"
      colnames(data_emfc) <- c("events", "value", "group", "color")
      
      data_emfs <- data.frame((events) / 1000, raw$exc / 1000)
      data_emfs$group <- 21
      data_emfs$color <- "grey"
      colnames(data_emfs) <- c("events", "value", "group", "color")
      
      label_y <- "Conflict Count (x1K)"
      
    }
    
    data <- rbind(data_ecbp, data_emfc, data_emfs)
    legend_labels <- c("EMF CBP","EMF Compare", "EMF Store")
    legend_colors <- c("black", "black" ,"black")
    legend_bg_colors <- c("white", "black" ,"gray")
    legend_columns <- 3
    legend_pch <- c(21, 21, 21)
    
  } 
  
  # DETAIL ------------------------------------------------------------
  else if (type == "detail") {
    if (case == "ecbp"){
      if (dimension == "time") {
        
        data_clt <- data.frame((events) / 1000, raw$clt / 1000000000)
        data_clt$group <- 24
        data_clt$color <- "white"
        colnames(data_clt) <- c("events", "value", "group", "color")
        
        data_ctt <- data.frame((events) / 1000, raw$ctt / 1000000000)
        data_ctt$group <- 22
        data_ctt$color <- "black"
        colnames(data_ctt) <- c("events", "value", "group", "color")
        
        data_cxt <- data.frame((events) / 1000, raw$cxt / 1000000000)
        data_cxt$group <- 23
        data_cxt$color <- "gray"
        colnames(data_cxt) <- c("events", "value", "group", "color")
  
        data <- rbind(data_clt, data_ctt, data_cxt)
        label_y <- "Execution Time (seconds)"
        legend_labels <- c("Event Load", "Tree", "Conflict")
        legend_colors <- c("black", "black", "black")
        legend_bg_colors <- c("white", "black", "gray")
        legend_columns <- 3
        legend_pch <- c(24, 22, 23)
        
      } else if (dimension == "memory") {
        
        data_clt <- data.frame((events) / 1000, raw$clm / 1000000000)
        data_clt$group <- 24
        data_clt$color <- "white"
        colnames(data_clt) <- c("events", "value", "group", "color")
        
        data_ctt <- data.frame((events) / 1000, raw$ctm / 1000000000)
        data_ctt$group <- 22
        data_ctt$color <- "black"
        colnames(data_ctt) <- c("events", "value", "group", "color")
        
        data_cxt <- data.frame((events) / 1000, raw$cxm / 1000000000)
        data_cxt$group <- 23
        data_cxt$color <- "gray"
        colnames(data_cxt) <- c("events", "value", "group", "color")
        
        data <- rbind(data_clt, data_ctt, data_cxt)
        label_y <- "Memory Footprint (GBs)"
        legend_labels <- c("Event Load", "Tree", "Conflict")
        legend_colors <- c("black", "black", "black")
        legend_bg_colors <- c("white", "black", "gray")
        legend_columns <- 3
        legend_pch <- c(24, 22, 23)
        
      }
    } else if (case == "emfc"){
      if (dimension == "time") {
        
        data_smt <- data.frame((events) / 1000, raw$smt / 1000000000)
        data_smt$group <- 24
        data_smt$color <- "white"
        colnames(data_smt) <- c("events", "value", "group", "color")
        
        data_sdt <- data.frame((events) / 1000, raw$sdt / 1000000000)
        data_sdt$group <- 22
        data_sdt$color <- "black"
        colnames(data_sdt) <- c("events", "value", "group", "color")
        
        data_sxt <- data.frame((events) / 1000, raw$sxt / 1000000000)
        data_sxt$group <- 23
        data_sxt$color <- "gray"
        colnames(data_sxt) <- c("events", "value", "group", "color")
        
        data <- rbind(data_smt, data_sdt, data_sxt)
        label_y <- "Execution Time (seconds)"
        legend_labels <- c("Matching", "Diffing", "Conflict")
        legend_colors <- c("black", "black", "black")
        legend_bg_colors <- c("white", "black", "gray")
        legend_columns <- 3
        legend_pch <- c(24, 22, 23)
        
      } else if (dimension == "memory") {
        
        data_smm <- data.frame((events) / 1000, raw$smm / 1000000000)
        data_smm$group <- 24
        data_smm$color <- "white"
        colnames(data_smm) <- c("events", "value", "group", "color")
        
        data_sdm <- data.frame((events) / 1000, raw$sdm / 1000000000)
        data_sdm$group <- 22
        data_sdm$color <- "black"
        colnames(data_sdm) <- c("events", "value", "group", "color")
        
        data_sxm <- data.frame((events) / 1000, raw$sxm / 1000000000)
        data_sxm$group <- 23
        data_sxm$color <- "gray"
        colnames(data_sxm) <- c("events", "value", "group", "color")
        
        data <- rbind(data_smm, data_sdm, data_sxm)
        label_y <- "Memory Footprint (GBs)"
        legend_labels <- c("Matching", "Diffing", "Conflict")
        legend_colors <- c("black", "black", "black")
        legend_bg_colors <- c("white", "black", "gray")
        legend_columns <- 3
        legend_pch <- c(24, 22, 23)
        
      }
    } else if (case == "emfs"){
      if (dimension == "time") {
        
        data_ept <- data.frame((events) / 1000, raw$ept / 1000000000)
        data_ept$group <- 24
        data_ept$color <- "white"
        colnames(data_ept) <- c("events", "value", "group", "color")
        
        data_ext <- data.frame((events) / 1000, raw$ext / 1000000000)
        data_ext$group <- 23
        data_ext$color <- "gray"
        colnames(data_ext) <- c("events", "value", "group", "color")
        
        data <- rbind(data_ept, data_ext)
        label_y <- "Execution Time (seconds)"
        legend_labels <- c("Loading & Mapping", "Conflict")
        legend_colors <- c("black", "black")
        legend_bg_colors <- c("white", "gray")
        legend_columns <- 2
        legend_pch <- c(24, 23)
        
      } else if (dimension == "memory") {
        
        data_epm <- data.frame((events) / 1000, raw$epm / 1000000000)
        data_epm$group <- 24
        data_epm$color <- "white"
        colnames(data_epm) <- c("events", "value", "group", "color")
        
        data_exm <- data.frame((events) / 1000, raw$ext / 1000000000)
        data_exm$group <- 23
        data_exm$color <- "gray"
        colnames(data_exm) <- c("events", "value", "group", "color")
        
        data <- rbind(data_epm, data_exm)
        label_y <- "Memory Footprint (GBs)"
        legend_labels <- c("Loading & Mapping", "Conflict")
        legend_colors <- c("black", "black")
        legend_bg_colors <- c("white", "gray")
        legend_columns <- 2
        legend_pch <- c(24, 23)
        
      }
    }
    
  } 
  
  
  # AFFECTED ELEMENTS AND SIZE OF BOTH MODELS -------------------------
  else if (type == "count") {
    data_aoc <- data.frame((events) / 1000, raw$aoc / 1000)
    data_aoc$group <- 22
    data_aoc$color <- "white"
    colnames(data_aoc) <- c("events", "value", "group", "color")
    
    data_elc <- data.frame((events) / 1000, elements / 1000)
    data_elc$group <- 24
    data_elc$color <- "gray"
    colnames(data_elc) <- c("events", "value", "group", "color")
    
    data <- rbind(data_aoc, data_elc)
    label_y <- "Count (x1K)"
    legend_labels <- c("Affected Elements", "Total Elements")
    legend_colors <- c("black", "black")
    legend_bg_colors <- c("white", "gray")
    legend_columns <- 2
    legend_pch <- c(22, 24)
  }
  
  
  
  pdf(file = target_file, height <- pdfHeight, width <- pdfWidth)
  par(family = fontType, mar = c(3, 3, 1.5, 0.5))
  plt <- plot(data$events, data$value, pch=data$group, col="black", bg=data$color, ylim=c(0,max_y))
  grid (NULL,NULL, lty = 2, col = "lightgray")
  par(xpd=TRUE)
  legend(
    "top",
    # 50, 6,5 
    legend_labels,  col=legend_colors,
        pt.bg=legend_bg_colors,
        # fill="white",
        border="white", 
        pch=legend_pch, 
        bg="white",
        ncol=legend_columns,
        inset = -0.13,
        bty="n"
    )
  title(ylab=label_y, line=2, cex.lab=1)
  title(xlab="Number of Events (x1K)", line=2, cex.lab=1)
  dev.off()
  
}
#add only
generate_graph("general", "time", "all", "..\\data\\add-multiconflict.csv", "images/add-conflict-time-events.pdf", 45)
generate_graph("general", "memory","all", "..\\data\\add-multiconflict.csv", "images/add-conflict-memory-events.pdf", 20)
generate_graph("general", "count","all", "..\\data\\add-multiconflict.csv", "images/add-conflict-count-events.pdf", 20)

#change only
generate_graph("general", "time", "all", "..\\data\\change-multiconflict.csv", "images/change-conflict-time-events.pdf", 45)
generate_graph("general", "memory", "all", "..\\data\\change-multiconflict.csv", "images/change-conflict-memory-events.pdf", 20)
generate_graph("general", "count", "all", "..\\data\\change-multiconflict.csv", "images/change-conflict-count-events.pdf", 140)

#delete only
generate_graph("general", "time", "all", "..\\data\\delete-multiconflict.csv", "images/delete-conflict-time-events.pdf", 73)
generate_graph("general", "memory", "all", "..\\data\\delete-multiconflict.csv", "images/delete-conflict-memory-events.pdf", 3.5)
generate_graph("general", "count", "all", "..\\data\\delete-multiconflict.csv", "images/delete-conflict-count-events.pdf", 80)

#move only
generate_graph("general", "time", "all", "..\\data\\move-multiconflict.csv", "images/move-conflict-time-events.pdf", 45)
generate_graph("general", "memory", "all", "..\\data\\move-multiconflict.csv", "images/move-conflict-memory-events.pdf", 20)
generate_graph("general", "count", "all", "..\\data\\move-multiconflict.csv", "images/move-conflict-count-events.pdf", 120)

#mix general
generate_graph("general", "time", "all", "..\\data\\mix-multiconflict.csv", "images/conflict-time-events.pdf", 45)
generate_graph("general", "memory", "all", "..\\data\\mix-multiconflict.csv", "images/conflict-memory-events.pdf", 20)
generate_graph("general", "count", "all", "..\\data\\mix-multiconflict.csv", "images/conflict-count-events.pdf", 120)
generate_graph("count", "count", "all", "..\\data\\mix-multiconflict.csv", "images/conflict-size-events.pdf", 1200)

#mix detail
generate_graph("detail", "memory", "ecbp", "..\\data\\mix-multiconflict.csv", "images/ecbp-conflict-memory-events.pdf", 3.5)
generate_graph("detail", "time", "ecbp", "..\\data\\mix-multiconflict.csv", "images/ecbp-conflict-time-events.pdf", 5.5)

generate_graph("detail", "memory", "emfc", "..\\data\\mix-multiconflict.csv", "images/emfc-conflict-memory-events.pdf", 8.2)
generate_graph("detail", "time", "emfc", "..\\data\\mix-multiconflict.csv", "images/emfc-conflict-time-events.pdf", 19)

generate_graph("detail", "memory", "emfs", "..\\data\\mix-multiconflict.csv", "images/emfs-conflict-memory-events.pdf", 450)
generate_graph("detail", "time", "emfs", "..\\data\\mix-multiconflict.csv", "images/emfs-conflict-time-events.pdf", 460)





