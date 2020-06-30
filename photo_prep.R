#----------------------------------
# Script pulls in forest photopoints, resizes, adds border and title, and saves them as jpegs
#----------------------------------
library(tidyverse)
library(magick)
library(stringi)

plots <- read.csv("./data/Plots.csv")
names(plots)

path <- c("D:/MIDN/ForestVegetation/Photopoints")

photo_name <- list.files(path, pattern = 'JPG$', full.names = F)
drops <- c("ID", "RN", "UC")

photo_name2 <- photo_name[!grepl("ID|RN|UC", photo_name)] #drop ID photos

full_names <- paste(path, photo_name2, sep = "/")

photo_name_df <- data.frame(photo_name2) %>% 
                 mutate(plot_name = substr(photo_name2, 1, 8),
                        scene = case_when(grepl("UR", photo_name2) ~ "UR", 
                                          grepl("BR", photo_name2) ~ "BR",
                                          grepl("BL", photo_name2) ~ "BL",
                                          grepl("UL", photo_name2) ~ "UL")) %>% 
                 arrange(plot_name, photo_name2) %>% 
                 group_by(plot_name, scene) %>% 
                 summarise(photo_name = first(photo_name2))

photo_name_wide <- photo_name_df %>% spread(scene, photo_name) %>% select(-`<NA>`)

plots2 <- merge(plots, photo_name_wide, by.x = "plot_name2", by.y = "plot_name", all.x = TRUE)

write.csv(plots2, "./data/Plots.csv", row.names = FALSE)

# Function to name each photo based on its name
view_namer<-function(pname){
         ifelse(grepl("UR", pname), paste0(" Upper Right "),
         ifelse(grepl("BR", pname), paste0(" Bottom Right "),
         ifelse(grepl("BL", pname), paste0(" Bottom Left "),
         ifelse(grepl("UL", pname), paste0(" Upper Left "),
                paste("unknown"))
         )))
}

process_image <- function(import_name, export_name){
  title <- view_namer(pname = export_name)
  
  img <- image_read(import_name)
  img2 <- image_border(img, 'black','5x5')
  img3 <- image_scale(img2, 'X500')
  img4 <- image_annotate(img3, 
                         paste(title), 
                         size = 16, 
                         color = 'black',
                         boxcolor = 'white', 
                         location = "+10+10",
                         degrees = 0)
  image_write(img4, format='jpeg', paste0("./www/", export_name))
}

process_image(import_name=full_names[40], export_name=photo_name2[40])

map2(full_names[1:500], photo_name2[1:500], ~process_image(.x,.y))
map2(full_names[501:1000], photo_name2[501:1000], ~process_image(.x,.y))
map2(full_names[1001:1555], photo_name2[1001:1555], ~process_image(.x,.y))
