library(dplyr)
library(googledrive)

id <- "1XsTjMYXl7pNVC2qwAON51wtppDdNztZk" # google file ID
download_link <- sprintf("https://docs.google.com/uc?id=%s&export=download", id)
#Open pneumatron data
pneumatron <- data.table::fread(input = download_link,
                                header = FALSE)

# organize and create dataframes from Pneumatron data 

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
dir.create("data/", showWarnings = FALSE)
#write.csv(raw, "data/pneumatron_fixed.csv", row.names = FALSE)
message("Databe updated!")


