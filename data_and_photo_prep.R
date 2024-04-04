#----------------------------------
# Script pulls in forest photopoints from server, resizes, adds border and title, and saves them as jpegs
#----------------------------------
# Note that because this is pulling 1,500+ images from the Z drive, this can be slow. If you have all
# of the most recent cycle of photopoints locally on your machine, it can go a lot faster.

#----- Libraries -----
library(tidyverse)
library(magick)
library(stringi)
library(forestMIDN)

importData()

#----- Params updated every year -----
MIDN1 <- c("FRSP", "PETE", "RICH")
MIDN2 <- c("APCO", "BOWA", "GETT", "HOFU", "VAFO")
NCBN <- c("GEWA", "THST")
# SAHI, ASIS, COLO

# First and last year of most recent 4-year cycle
years_MIDN1 <- c(2021, 2023)
years_MIDN2 <- c(2019, 2023)
years_NCBN <- c(2021, 2023)
years_COLO <- c(2019, 2023)
years_ASIS <- c(2019, 2023)
years_SAHI <- 2023

#----- Compile data for the popups -----
plots1 <- rbind(joinLocEvent(park = MIDN1, from = years_MIDN1[1], to = years_MIDN1[2], output = 'verbose'),
                joinLocEvent(park = MIDN2, from = years_MIDN2[1], to = years_MIDN2[2], output = 'verbose'),
                joinLocEvent(park = NCBN, from = years_NCBN[1], to = years_NCBN[2], output = 'verbose'),
                joinLocEvent(park = "COLO", from = years_COLO[1], to = years_COLO[2], output = 'verbose'),
                joinLocEvent(park = "ASIS", from = years_ASIS[1], to = years_ASIS[2], output = 'verbose'),
                joinLocEvent(park = "SAHI", from = years_SAHI, to = years_SAHI, output = 'verbose')) |> 
  mutate(Physio = paste0(PhysiographySummary, "- ", PhysiographyLabel)) |> 
  select(Plot_Name, Unit_Code = ParkUnit, Panel = PanelCode, Physio,
         Directions, Location_Notes = PlotNotes, SampleYear, Lat, Long)

trees <- rbind(joinTreeData(park = MIDN1, from = years_MIDN1[1], to = years_MIDN1[2], status = 'active'),
               joinTreeData(park = MIDN2, from = years_MIDN2[1], to = years_MIDN2[2], status = 'active'),
               joinTreeData(park = NCBN, from = years_NCBN[1], to = years_NCBN[2], status = 'active'),
               joinTreeData(park = "COLO", from = years_COLO[1], to = years_COLO[2], status = 'active'),
               joinTreeData(park = "ASIS", from = years_ASIS[1], to = years_ASIS[2], status = 'active'),
               joinTreeData(park = "SAHI", from = years_SAHI, to = years_SAHI, status = 'active'))

live = c("AB", "AF", "AL", "AM", "AS", "RB", "RF", "RL", "RS")
dead = c("DB", "DL", "DM", "DS")

trees_sum <- trees |> 
  mutate(status = case_when(TreeStatusCode %in% live ~ "live",
                            TreeStatusCode %in% dead ~ "dead",
                            TRUE ~ "X")) |> 
  filter(!status %in% "X") |>
  group_by(Plot_Name, SampleYear, status) |> 
  summarize(Num_Stems = sum(num_stems), 
            .groups = 'drop') |> 
  pivot_wider(names_from = status, values_from = Num_Stems, values_fill = 0) |> 
  select(Plot_Name, Num_Live_Trees = live, Num_Dead_Trees = dead)


inv_shrubs <- rbind(joinMicroShrubData(park = MIDN1, from = years_MIDN1[1], to = years_MIDN1[2], speciesType = 'invasive'),
                    joinMicroShrubData(park = MIDN2, from = years_MIDN2[1], to = years_MIDN2[2], speciesType = 'invasive'),
                    joinMicroShrubData(park = NCBN, from = years_NCBN[1], to = years_NCBN[2], speciesType = 'invasive'),
                    joinMicroShrubData(park = "COLO", from = years_COLO[1], to = years_COLO[2], speciesType = 'invasive'),
                    joinMicroShrubData(park = "ASIS", from = years_ASIS[1], to = years_ASIS[2], speciesType = 'invasive'),
                    joinMicroShrubData(park = "SAHI", from = years_SAHI, to = years_SAHI, speciesType = 'invasive')) |> 
  group_by(Plot_Name) |> summarize(Inv_Shrub_Cov = round(sum(shrub_avg_cov), 1))

regen1 <- rbind(joinRegenData(park = MIDN1, from = years_MIDN1[1], to = years_MIDN1[2]),
                joinRegenData(park = MIDN2, from = years_MIDN2[1], to = years_MIDN2[2]),
                joinRegenData(park = NCBN, from = years_NCBN[1], to = years_NCBN[2]),
                joinRegenData(park = "COLO", from = years_COLO[1], to = years_COLO[2]),
                joinRegenData(park = "ASIS", from = years_ASIS[1], to = years_ASIS[2]),
                joinRegenData(park = "SAHI", from = years_SAHI, to = years_SAHI))

regen <- regen1 |> group_by(Plot_Name) |> 
  summarize(Num_Seedlings = sum(seed_den)*12, #convert from sq.m to # on plot
            Num_Saplings = sum(sap_den)*(3*(3^2*pi))) #convert from sq.m to # on plot
                                                   
numspp1 <- rbind(sumSpeciesList(park = MIDN1, from = years_MIDN1[1], to = years_MIDN1[2]),
                 sumSpeciesList(park = MIDN2, from = years_MIDN2[1], to = years_MIDN2[2]),
                 sumSpeciesList(park = NCBN, from = years_NCBN[1], to = years_NCBN[2]),
                 sumSpeciesList(park = "COLO", from = years_COLO[1], to = years_COLO[2]),
                 sumSpeciesList(park = "ASIS", from = years_ASIS[1], to = years_ASIS[2]),
                 sumSpeciesList(park = "SAHI", from = years_SAHI, to = years_SAHI)) 

numspp <- numspp1 |> group_by(Plot_Name) |> summarize(Num_Quad_Species = sum(quad_avg_cov > 0),
                                                      Num_Add_Species = sum(addspp_present > 0))

dfs <- list(plots1, trees_sum, inv_shrubs, regen, numspp)
comb <- reduce(dfs, left_join, by = "Plot_Name")
names(comb)
comb[,10:ncol(comb)][is.na(comb[,10:ncol(comb)])] <- 0

#----- Join sample data with photo info -----

#----- Uncomment next 3 lines to remove previous set of photos -----
# old_photos <- list.files("./www", pattern = "JPG$", full.names = T)
# old_photos <- old_photos[!grepl("AH_small", old_photos)]
# file.remove(old_photos)
table(plots1$Unit_Code, plots1$Panel) # correct number of plots
# example: "Y:\Monitoring\ForestVegetation\2023\03_Data\Photos\PhotoPoints\APCO"

path <- c('Y:/Monitoring/ForestVegetation/')
          
park_year <- data.frame(rbind(
  expand.grid(park = MIDN1, years = years_MIDN1[1]:years_MIDN1[2]),
  expand.grid(park = MIDN2, years = years_MIDN2[1]:years_MIDN2[2]),
  expand.grid(park = NCBN, years = years_NCBN[1]:years_NCBN[2]),
  expand.grid(park = "COLO", years = years_COLO[1]:years_COLO[2]),
  expand.grid(park = "ASIS", years = years_ASIS[1]:years_ASIS[2]),
  expand.grid(park = "SAHI", years = years_SAHI)))

table(park_year$park, park_year$year) # returns years parks weren't sampled, but they won't have any photopoints, so okay.

full_names <- map2_dfr(park_year$year, park_year$park, function(y, p){
  data.frame(full_name = list.files(paste0(path, y, "/03_Data/Photos/PhotoPoints/", p),
                                    pattern = 'JPG$', full.names = T))
})

full_names[1:10,]
head(full_names)
name_df1 <- data.frame(full_name = full_names$full_name, 
                       photo_name = substr(full_names$full_name, 
                                           nchar(full_names$full_name) - 23, 
                                           nchar(full_names$full_name)))

name_df <- name_df1[!grepl("ID|RN|UC|QAQC", name_df1$photo_name),]

photo_name_df <- #data.frame(photo_name)  |>  
  name_df |> 
  mutate(plot_name = substr(photo_name, 1, 8),
         scene = case_when(grepl("UR", photo_name) ~ "UR", 
                           grepl("BR", photo_name) ~ "BR",
                           grepl("BL", photo_name) ~ "BL",
                           grepl("UL", photo_name) ~ "UL"))  |>  
  arrange(plot_name, photo_name) |>  
  group_by(plot_name, scene) |>  
  summarise(photo_name = first(photo_name),
            .groups = 'drop')

head(photo_name_df)

photo_name_wide <- photo_name_df %>% spread(scene, photo_name) %>% select(-`<NA>`)
photo_name_wide$Plot_Name <- sub("_", "-", photo_name_wide$plot_name)

plots <- left_join(comb, photo_name_wide[,-1], by = "Plot_Name")

write.csv(plots, "./data/Plots.csv", row.names = FALSE)

# Check for duplicate plot records (ie QAQC photopoints missing _QAQC in the file name)
dups <- plots$Plot_Name[duplicated(plots$Plot_Name)]

if(length(dups) > 0){
  warning(paste0("There are ", length(unique(dups)), " duplicates in the plots data frame. Check plots: ", 
                 paste0(unique(dups), collapse = ",")))}

# Function to rename each photo based on its file name
view_namer <- function(pname){
  ifelse(grepl("UR", pname), paste0(" Upper Right "),
         ifelse(grepl("BR", pname), paste0(" Bottom Right "),
                ifelse(grepl("BL", pname), paste0(" Bottom Left "),
                       ifelse(grepl("UL", pname), paste0(" Upper Left "),
                              paste("unknown"))
                )))
}

# Function to resize and label each photo
process_image <- function(import_name, export_name){
  title <- view_namer(pname = export_name)
  img <- image_read(import_name)
  img2 <- image_border(img, 'black','6x6')
  img3 <- image_scale(img2, 'X600')
  img4 <- image_annotate(img3, 
                         paste(title), 
                         size = 16, 
                         color = 'black',
                         boxcolor = 'white', 
                         location = "+10+10",
                         degrees = 0)
  image_write(img4, format='jpeg', paste0("./www/", export_name))
}

# test on 1 photo
# process_image(import_name = full_names[40], export_name = photo_name[40])

# Run through all photos. Can break into even smaller chunks of bogs down computer too much
num_photos <- nrow(name_df) #1570
head(name_df)

map2(name_df$full_name[1:500], name_df$photo_name[1:500], ~process_image(.x,.y), .progress = T)
map2(name_df$full_name[501:1000], name_df$photo_name[501:1000], ~process_image(.x,.y), .progress = T)
map2(name_df$full_name[1001:num_photos], name_df$photo_name[1001:num_photos], ~process_image(.x,.y), .progress = T)
