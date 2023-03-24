###########################
# trade matrix for 3d flow
###########################

# trade matrix per CP range
CSVr <- c("tradeVH.csv", "tradeH.csv", "tradeM.csv", "tradeL.csv", "tradeTotal.csv")
CSVw <- c("VH/", "H/", "M/", "L/", "Total/")
path <- "../sfp/matrix/"

# get full country list
agr.Total <- read.csv(CSVr[5], stringsAsFactors = F) %>% 
  rename(exp.iso3 = "exp_iso3", imp.iso3 = "imp_iso3", SPAM.short.name = "SPAM_short_name", crop.area = "crop_area")
exp <- sort(unique(agr.Total$exp.iso3))
imp <- sort(unique(agr.Total$imp.iso3))            
iso3 <- sort(unique(c(exp, imp))) 

for (range in 1:5) {
  agr.df <- read.csv(CSVr[range], stringsAsFactors = F) %>% 
    rename(exp.iso3 = "exp_iso3", imp.iso3 = "imp_iso3", SPAM.short.name = "SPAM_short_name", crop.area = "crop_area")
  imp.iso3 <- vector()
  for (i in 1:length(iso3)) {
    imp.i <- rep(iso3[i], length(iso3))
    imp.iso3 <- c(imp.iso3, imp.i)
  }
  exp.iso3 <- rep(iso3, length(iso3))
  
  # all commodities
  com.name <- sort(unique(agr.df$SPAM.short.name))
  SPAM.short.name <- vector()
  for (i in 1:length(com.name)) {
    com.i <- rep(com.name[i], length(exp.iso3))
    SPAM.short.name <- c(SPAM.short.name, com.i)
  }
  imp.iso3 <- rep(imp.iso3, length(com.name))
  exp.iso3 <- rep(exp.iso3, length(com.name))
  square.df <- data.frame(imp.iso3, exp.iso3, SPAM.short.name)
  
  # matrix
  agr.m <- left_join(square.df, agr.df)
  for (i in 1:length(com.name)) {
    com.m <- agr.m %>% 
      filter(SPAM.short.name == com.name[i], as.character(imp.iso3) != as.character(exp.iso3)) %>% 
      dplyr::select(-SPAM.short.name) %>% 
      spread(imp.iso3, crop.area) %>% 
      mutate_all(~replace(., is.na(.), 0))
    outname <- paste0(path, CSVw[range], com.name[i], ".csv")
    write.csv(com.m, file = outname, row.names = F)
    print(outname)
  }
}