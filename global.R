#-------------------------
# Global files to populate map 
#-------------------------
plots <- read.csv("./data/Plots.csv")
plots$plot_num <- substr(plots$Plot_Name, 6, 8)

bboxes <- read.csv("./data/boundboxes.csv")

meanLat <- (bboxes[bboxes$ParkCode == "MIDN", c("LatS")] + 
              bboxes[bboxes$ParkCode == "MIDN", c("LatN")])/2

meanLong <- (bboxes[bboxes$ParkCode == "MIDN", c("LongE")] + 
               bboxes[bboxes$ParkCode == "MIDN", c("LongW")])/2

