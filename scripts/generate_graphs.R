setwd("D:\\A-DATA\\GoogleDriveYork\\THESIS\\scripts")

library(plyr)
library(extrafont)
# pdfFonts()
fontType <- "serif"
pdfWidth <- 3.54
pdfHeight <- 5
# pdfWidth <- 7.07
# pdfHeight <- 10

raw <- read.table("..\\data\\add-ungrouped.csv", sep = ",", header = TRUE)

data_ecbp <- data.frame((raw$levc + raw$revc) / 1000, raw$cct / 1000000000)
data_ecbp$group <- 21
data_ecbp$color <- "white"
colnames(data_ecbp) <- c("events", "time", "group", "color")

data_emfc <- data.frame((raw$levc + raw$revc) / 1000, raw$sct / 1000000000)
data_emfc$group <- 21
data_emfc$color <- "black"
colnames(data_emfc) <- c("events", "time", "group", "color")

data_emfs <- data.frame((raw$levc + raw$revc) / 1000, raw$ect / 1000000000)
data_emfs$group <- 21
data_emfs$color <- "grey"
colnames(data_emfs) <- c("events", "time", "group", "color")
data <- rbind(data_ecbp, data_emfc, data_emfs)

pdf(file = "images/add-conflict-time-events.pdf", height <- pdfHeight, width <- pdfWidth)
par(family = fontType, mar = c(3, 3, 1.4, 0.05))
plt <- plot(data$events, data$time, pch=data$group, col="black", bg=data$color, ylim=c(0,55))
grid (NULL,NULL, lty = 1, col = "lightgray")
par(xpd=TRUE)
legend(50, 65, c("EMF CBP","EMF Compare", "EMF Store"),  col=c("black", "black" ,"black"),
       pt.bg=c("white", "black" ,"gray"),
       border=NA, pch=c(21, 21, 21), 
       ncol=3, bty="n")
title(ylab="Execution Time (seconds)", line=2, cex.lab=1)
title(xlab="Number of Events (x1K)", line=2, cex.lab=1)
dev.off()
