#Librerias
library(vegan)


#Load data
data <- readRDS("data/evaluation_table_all.rds")
# Semi
semi <- subset(data, data$lfmc=="semi")
#Variation partitioning
res <- varpart(as.numeric(semi$MAE), as.factor(semi$lai_source), as.factor(semi$meteo_source), 
               as.factor(semi$taw))
res$part
#Plot
plot(res,
     Xnames = c("LAI", "Meteo", "TAW"), # name the partitions
     bg = c("#FDE333FF", "#00AD8DFF", "#491562FF"), 
     digits = 2, # only show 2 digits
     id.size = 1.5,
     cex = 1.25)
# Full 1
full1 <- subset(data, data$lfmc=="full1")
#Variation partitioning
res <- varpart(as.numeric(full1$MAE), as.factor(full1$lai_source), as.factor(full1$meteo_source), 
               as.factor(full1$taw))
#Plot
plot(res,
     Xnames = c("LAI", "Meteo", "TAW"), # name the partitions
     bg = c("#FDE333FF", "#00AD8DFF", "#491562FF"), 
     digits = 2, # only show 2 digits
     id.size = 1.5,
     cex = 1.25)
# Full 2
full2 <- subset(data, data$lfmc=="full2")
#Variation partitioning
res <- varpart(as.numeric(full2$MAE), as.factor(full2$lai_source), as.factor(full2$meteo_source), 
               as.factor(full2$taw))
#Plot
plot(res,
     Xnames = c("LAI", "Meteo", "TAW"), # name the partitions
     bg = c("#FDE333FF", "#00AD8DFF", "#491562FF"), 
     digits = 2, # only show 2 digits
     id.size = 1.5,
     cex = 1.25)
# Full 3
full3 <- subset(data, data$lfmc=="full3")
#Variation partitioning
res <- varpart(as.numeric(full3$MAE), as.factor(full3$lai_source), as.factor(full3$meteo_source), 
               as.factor(full3$taw))
#Plot
plot(res,
     Xnames = c("LAI", "Meteo", "TAW"), # name the partitions
     bg = c("#FDE333FF", "#00AD8DFF", "#491562FF"), 
     digits = 2, # only show 2 digits
     id.size = 1.5,
     cex = 1.25)

# Export and process out of R

