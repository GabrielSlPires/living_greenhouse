library(dplyr)
#library(googledrive)
#
#I need to ask for google drive file
#drive_download("database.csv", overwrite = T)
#id <- "1215nETQNv0kb_x-Eplk2qRTf-NE0Y6o5" # google file ID
#pneumatron<-read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download",id),sep=",",header = F,as.is = T)

#Open pneumatron data
# I prefer fread() because it is a bit faster than read.csv(). But further we could use a better function
pneumatron <- data.table::fread(input = "data/2021_12_06.csv",
                                header = FALSE)

# organize and create dataframes from Pneumatron data 

#I didn't understand why you made it
#pneumatron$error <-  substring(pneumatron$V1,1,1)
#pneumatron$error <- as.numeric(as.character(pneumatron$error))
#pneumatron <- na.omit(pneumatron)

#I think it's easier to read the script when we break lines. but that's not a rule
#This processes takes more than 15s
raw <- reshape2::colsplit(string = pneumatron$V1,
                          pattern = ",",
                          names = c("id",
                                    "ms",
                                    "temp1",
                                    "pressure",
                                    "humid1",
                                    "temp2",
                                    "atm_pres2",
                                    "humid2",
                                    "seq",
                                    "measure",
                                    "log_line",
                                    "voltage",
                                    "unknown" #We have 3 columns here, are they trash? We could delete them in raspberry
                                    )
                          ) %>%
  data.frame(datetime = lubridate::ymd_hms(pneumatron$V2)) %>% 
  na.omit() %>% 
  #I've added the time calculations here
  dplyr::mutate(time_min = lubridate::time_length(datetime - min(datetime),
                                                  unit = "min"),
                step_min = round(time_min, 0),
                step_min15 = plyr::round_any(step_min, 15))
write.csv(raw, "data/pneumatron_fixed.csv", row.names = FALSE)




