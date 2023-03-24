############
# land use
############

# 1. PRODUCTION

# mapspam data
mapspam.ATA <- read.csv("spam2010V2r0_global_A_TA.csv", quote = "") %>% 
  filter(!is.na(x)) %>% 
  filter(!is.na(y)) %>% 
  mutate(iso3 = substr(iso3, start = 2, stop = 4)) %>% 
  mutate(iso3 = ifelse(name_adm1 == '"Taiwan"', "TWN", iso3),
         iso3 = ifelse(iso3 == "ind", "CHN", iso3),
         iso3 = replace(iso3, iso3 == "SDN", "ZZY")) %>%
  dplyr::select(1, 4:6, 10:51)
cp.cell5m <- read.csv("cp_cell5m.csv", stringsAsFactors = F)

spam.ATA <- left_join(mapspam.ATA, cp.cell5m) %>% 
  select(-cell5m, -rankmap_c_rmPA_r2) %>% 
  mutate(
    acof_a = acof_a*0.01, bana_a = bana_a*0.01, barl_a = barl_a*0.01,
    bean_a = bean_a*0.01, cass_a = cass_a*0.01, chic_a = chic_a*0.01,
    cnut_a = cnut_a*0.01, coco_a = coco_a*0.01, cott_a = cott_a*0.01,
    cowp_a = cowp_a*0.01, grou_a = grou_a*0.01, lent_a = lent_a*0.01,
    maiz_a = maiz_a*0.01, ocer_a = ocer_a*0.01, ofib_a = ofib_a*0.01,
    oilp_a = oilp_a*0.01, ooil_a = ooil_a*0.01, opul_a = opul_a*0.01,
    orts_a = orts_a*0.01, pige_a = pige_a*0.01, plnt_a = plnt_a*0.01,
    pmil_a = pmil_a*0.01, pota_a = pota_a*0.01, rape_a = rape_a*0.01,
    rcof_a = rcof_a*0.01, rest_a = rest_a*0.01, rice_a = rice_a*0.01,
    sesa_a = sesa_a*0.01, smil_a = smil_a*0.01, sorg_a = sorg_a*0.01,
    soyb_a = soyb_a*0.01, sugb_a = sugb_a*0.01, sugc_a = sugc_a*0.01,
    sunf_a = sunf_a*0.01, swpo_a = swpo_a*0.01, teas_a = teas_a*0.01,
    temf_a = temf_a*0.01, toba_a = toba_a*0.01, trof_a = trof_a*0.01,
    vege_a = vege_a*0.01, whea_a = whea_a*0.01, yams_a = yams_a*0.01
  ) %>% 
  mutate(crop_area = acof_a+bana_a+barl_a+bean_a+cass_a+chic_a+
           cnut_a+coco_a+cott_a+cowp_a+grou_a+lent_a+
           maiz_a+ocer_a+ofib_a+oilp_a+ooil_a+opul_a+
           orts_a+pige_a+plnt_a+pmil_a+pota_a+rape_a+
           rcof_a+rest_a+rice_a+sesa_a+smil_a+sorg_a+
           soyb_a+sugb_a+sugc_a+sunf_a+swpo_a+teas_a+
           temf_a+toba_a+trof_a+vege_a+whea_a+yams_a) %>% 
  filter(!is.na(cp_pixel)) %>% 
  group_by(iso3, cp_pixel) %>% 
  summarise_all(list(sum = sum)) %>%
  as.data.frame()

# livestock data
stock10area.cp.norm <- read.csv("stock10area.cp.norm.csv", stringsAsFactors = F) %>% 
  select(1:2, 6:11) %>% 
  mutate(stock_area = area.cattle+area.sheep+area.goat+area.chick+area.duck+area.pig)

# combine crop and livestock
agr.cp.prod <- spam.ATA %>% 
  full_join(stock10area.cp.norm) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(agr.area = crop_area_sum + stock_area) %>% 
  rename(cattle = "area.cattle", sheep = "area.sheep", goat = "area.goat",
         henc = "area.chick", duck = "area.duck", pigs = "area.pig") %>% 
  select(-crop_area_sum, -stock_area) %>% 
  as.data.frame()

write.csv(agr.cp.prod, "agr.cp.prod.csv", row.names = F)

#2. CONSUMPTION

# CROP
codedf <- read.csv("code.cropcalorie.csv", stringsAsFactors = F)
cou <- read.csv("countrygroup.csv", stringsAsFactors = F) %>% 
  select(Country.Code, ISO3.Code) %>%
  rename(exp = "Country.Code") %>%
  rename(iso3 = "ISO3.Code") %>%
  mutate(iso3 = as.character(iso3)) %>%
  distinct(exp, iso3)

FA.pre <- read.csv("Production_Crops_Livestock_E_All_Data_(Normalized).csv") %>%
  filter(Year == 2010) %>%
  filter(Element == "Production") %>%
  select(Area.Code, Item.Code, Value) %>%
  rename(exp = "Area.Code") %>%
  rename(FAOCODE = "Item.Code") %>%
  left_join(codedf) %>%
  filter(!is.na(SPAM.short.name)) %>%
  mutate(Value = Value/ratio) %>% 
  group_by(exp, SPAM.short.name) %>%
  summarise(Value = sum(Value, na.rm = T)) %>%
  left_join(cou) %>%
  filter(!is.na(iso3)) %>%
  as.data.frame()

write.csv(FA.pre, "FA.pre.kcal.csv", row.names = F)

# crop.percent including feed for stock (using for locating land use per irrigation systems)
trade10df.percent <- read.csv("dt.crop.feed.percent.csv", stringsAsFactors = F) %>% 
  left_join(cou %>% rename(iso3.exp = "iso3")) %>% 
  left_join(cou %>% rename(iso3.imp = "iso3", imp = "exp")) %>% 
  rename(per.imp = "percent") %>% 
  select(exp, imp, SPAM.short.name, per.imp) %>% 
  as.data.frame()

write.csv(trade10df.percent, "trade10df.percent.pro.csv", row.names = F)  

# crop.percent excluding feed
trade10df.crop.percent <- read.csv("dt.crop.feed.percent.csv", stringsAsFactors = F) %>% 
  rename(exp.iso3 = "iso3.exp", imp.iso3 = "iso3.imp") %>% 
  mutate(percent1 = ifelse(as.character(exp.iso3) == as.character(imp.iso3),
                           0, percent)) %>% 
  group_by(exp.iso3, SPAM.short.name) %>% 
  mutate(percent.e = sum(percent1)) %>% 
  as.data.frame() %>% 
  mutate(percent.e = ifelse(as.character(exp.iso3) == as.character(imp.iso3),
                            percent, percent.e)) %>% 
  mutate(crop.fix = crop.percent/percent.e) %>% 
  mutate(crop.fix = ifelse(is.na(crop.fix), 0, crop.fix)) %>% 
  select(exp.iso3, imp.iso3, SPAM.short.name, crop.fix) %>% 
  as.data.frame()

write.csv(trade10df.crop.percent, "trade10df.crop.percent.csv", row.names = F)

# SPAM data per irrigation systems
spamPTA <- read.csv("spam2010V2r0_global_P_TA.csv", quote = "") %>% 
  filter(!is.na(x)) %>% 
  filter(!is.na(y)) %>% 
  mutate(iso3 = substr(iso3, start = 2, stop = 4)) %>% 
  mutate(iso3 = ifelse(name_adm1 == '"Taiwan"', "TWN", iso3),
         iso3 = ifelse(iso3 == "ind", "CHN", iso3)) %>% 
  dplyr::select(1, 10:51) %>% 
  gather(key = crop, value = prodTA, -iso3) %>% 
  mutate(SPAM.short.name = substr(as.character(crop), start = 1, stop = 4)) %>% 
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "pmil_a", "mill")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "smil_a", "mill")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "acof_a", "coff")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "rcof_a", "coff")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugb_a", "suga")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugc_a", "suga")) %>%
  group_by(iso3, SPAM.short.name) %>% 
  summarise(MA = sum(prodTA, na.rm = T)) %>%
  as.data.frame() %>% 
  mutate(iso3 = replace(iso3, iso3 == "SDN", "ZZY")) %>%
  left_join(cou) %>% 
  select(exp, SPAM.short.name, MA)

spamPTI <- read.csv("spam2010V2r0_global_P_TI.csv", quote = "") %>% 
  filter(!is.na(x)) %>% 
  filter(!is.na(y)) %>% 
  mutate(iso3 = substr(iso3, start = 2, stop = 4)) %>% 
  mutate(iso3 = ifelse(name_adm1 == '"Taiwan"', "TWN", iso3),
         iso3 = ifelse(iso3 == "ind", "CHN", iso3)) %>% 
  dplyr::select(1, 10:51) %>% 
  gather(key = crop, value = prod, -iso3) %>% 
  mutate(SPAM.short.name = substr(as.character(crop), start = 1, stop = 4)) %>% 
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "pmil_i", "mill")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "smil_i", "mill")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "acof_i", "coff")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "rcof_i", "coff")) %>% 
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugb_i", "suga")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugc_i", "suga")) %>%
  group_by(iso3, SPAM.short.name) %>% 
  summarise(MHI = sum(prod, na.rm = T)) %>%
  as.data.frame() %>% 
  mutate(iso3 = replace(iso3, iso3 == "SDN", "ZZY")) %>%
  left_join(cou) %>% 
  select(exp, SPAM.short.name, MHI)

spamPTH <- read.csv("spam2010V2r0_global_P_TH.csv", quote = "") %>% 
  filter(!is.na(x)) %>% 
  filter(!is.na(y)) %>% 
  mutate(iso3 = substr(iso3, start = 2, stop = 4)) %>% 
  mutate(iso3 = ifelse(name_adm1 == '"Taiwan"', "TWN", iso3),
         iso3 = ifelse(iso3 == "ind", "CHN", iso3)) %>% 
  dplyr::select(1, 10:51) %>% 
  gather(key = crop, value = prod, -iso3) %>% 
  mutate(SPAM.short.name = substr(as.character(crop), start = 1, stop = 4)) %>% 
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "pmil_h", "mill")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "smil_h", "mill")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "acof_h", "coff")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "rcof_h", "coff")) %>% 
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugb_h", "suga")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugc_h", "suga")) %>%
  group_by(iso3, SPAM.short.name) %>% 
  summarise(MHI = sum(prod, na.rm = T)) %>%
  as.data.frame() %>% 
  mutate(iso3 = replace(iso3, iso3 == "SDN", "ZZY")) %>%
  left_join(cou) %>% 
  select(exp, SPAM.short.name, MHI)    

spamPTITH <- bind_rows(spamPTI, spamPTH) %>% 
  group_by(exp, SPAM.short.name) %>% 
  summarise(MHI = sum(MHI, na.rm = T)) %>% 
  left_join(cou) %>% 
  select(exp, SPAM.short.name, MHI) %>% 
  as.data.frame()

spamdf <- spamPTA %>% 
  full_join(spamPTITH) %>% 
  mutate(MLI = MA - MHI) %>% 
  mutate(MLI = ifelse(MLI < 0, 0, MLI))

write.csv(spamdf, "spamdf.csv", row.names = F)

# calculate export and domestic ratio from SPAM data per irrigation systems
spam.fao.imp <- read.csv("spamdf.csv", stringsAsFactors = F) %>% 
  full_join(
    read.csv("trade10df.percent.pro.csv", stringsAsFactors = F) %>% 
      filter(exp == imp) %>% 
      left_join(
        read.csv("FA.pre.kcal.csv", stringsAsFactors = F) %>% 
          select(exp, SPAM.short.name, Value)
      ) %>% 
      mutate(q.imp = per.imp*Value) %>% 
      group_by(exp, SPAM.short.name) %>% 
      summarise(FD = sum(q.imp, na.rm = T)) %>% 
      as.data.frame()
  ) %>% 
  full_join(
    read.csv("trade10df.percent.pro.csv", stringsAsFactors = F) %>% 
      filter(exp != imp) %>% 
      left_join(
        read.csv("FA.pre.kcal.csv", stringsAsFactors = F) %>% 
          select(exp, SPAM.short.name, Value)
      ) %>% 
      mutate(q.imp = per.imp*Value) %>% 
      group_by(exp, SPAM.short.name) %>% 
      summarise(FE = sum(q.imp, na.rm = T)) %>% 
      as.data.frame()
  ) %>%
  full_join(
    read.csv("FA.pre.kcal.csv", stringsAsFactors = F) %>% 
      select(exp, SPAM.short.name, Value) %>% 
      rename(FA = "Value") %>% 
      as.data.frame()
  ) %>% 
  filter(!is.na(MA)) %>% 
  filter(MA != 0) %>% 
  filter(!is.na(FA)) %>% 
  filter(FA != 0) %>% 
  mutate(FE = ifelse(is.na(FE), 0, FE)) %>% 
  mutate(ME = MA*FE/FA) %>% 
  mutate(MD = MA*FD/FA) %>% 
  mutate(ME.hi = ifelse(MHI < ME, 1, ME/MHI)) %>% 
  mutate(ME.hi = ifelse(is.nan(ME.hi), 1, ME.hi)) %>% 
  mutate(ME.li = ifelse(MHI < ME, (ME - MHI)/MLI, 0)) %>%   
  mutate(ME.li = ifelse(is.infinite(ME.li), 1, ME.li)) %>% 
  mutate(MD.hi = ifelse(MHI < ME, 0, (MHI - ME)/MHI)) %>% 
  mutate(MD.hi = ifelse(is.nan(MD.hi), 0, MD.hi)) %>% 
  mutate(MD.li = ifelse(MHI < ME, MD/MLI, 1)) %>% 
  mutate(MD.li = ifelse(is.nan(MD.li), 0, MD.li)) %>% 
  select(exp, SPAM.short.name, ME.hi, ME.li, MD.hi, MD.li)

fwrite(spam.fao.imp, "spam.fao.imp.pro.csv")

# connect MAPSPAM to FAO
mapspam.fao.imp <- read.csv("spam2010V2r0_global_A_TA.csv", quote = "") %>% 
  filter(!is.na(x)) %>% 
  filter(!is.na(y)) %>% 
  mutate(iso3 = substr(iso3, start = 2, stop = 4)) %>% 
  mutate(iso3 = ifelse(name_adm1 == '"Taiwan"', "TWN", iso3),
         iso3 = ifelse(iso3 == "ind", "CHN", iso3),
         iso3 = replace(iso3, iso3 == "SDN", "ZZY")) %>%
  dplyr::select(1, 4:6, 10:51) %>% 
  gather(key = crop, value = ATA, -c(iso3, cell5m, x, y)) %>%
  filter(ATA != 0) %>% 
  mutate(SPAM.short.name = substr(as.character(crop), start = 1, stop = 4)) %>% 
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "pmil_a", "mill")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "smil_a", "mill")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "acof_a", "coff")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "rcof_a", "coff")) %>% 
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugb_a", "suga")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugc_a", "suga")) %>%
  select(-crop) %>% 
  full_join(
    read.csv("spam2010V2r0_global_A_TI.csv", quote = "") %>% 
      filter(!is.na(x)) %>% 
      filter(!is.na(y)) %>% 
      mutate(iso3 = substr(iso3, start = 2, stop = 4)) %>% 
      mutate(iso3 = ifelse(name_adm1 == '"Taiwan"', "TWN", iso3),
             iso3 = ifelse(iso3 == "ind", "CHN", iso3),
             iso3 = replace(iso3, iso3 == "SDN", "ZZY")) %>%
      dplyr::select(1, 4:6, 10:51) %>% 
      gather(key = crop, value = ATI, -c(iso3, cell5m, x, y)) %>%
      filter(ATI != 0) %>% 
      mutate(SPAM.short.name = substr(as.character(crop), start = 1, stop = 4)) %>% 
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "pmil_i", "mill")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "smil_i", "mill")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "acof_i", "coff")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "rcof_i", "coff")) %>% 
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugb_i", "suga")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugc_i", "suga")) %>%
      select(-crop)
  ) %>% 
  full_join(
    read.csv("spam2010V2r0_global_A_TH.csv", quote = "") %>% 
      filter(!is.na(x)) %>% 
      filter(!is.na(y)) %>% 
      mutate(iso3 = substr(iso3, start = 2, stop = 4)) %>% 
      mutate(iso3 = ifelse(name_adm1 == '"Taiwan"', "TWN", iso3),
             iso3 = ifelse(iso3 == "ind", "CHN", iso3),
             iso3 = replace(iso3, iso3 == "SDN", "ZZY")) %>%
      dplyr::select(1, 4:6, 10:51) %>% 
      gather(key = crop, value = ATH, -c(iso3, cell5m, x, y)) %>%
      filter(ATH != 0) %>% 
      mutate(SPAM.short.name = substr(as.character(crop), start = 1, stop = 4)) %>% 
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "pmil_h", "mill")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "smil_h", "mill")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "acof_h", "coff")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "rcof_h", "coff")) %>% 
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugb_h", "suga")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugc_h", "suga")) %>%
      select(-crop)
  ) %>% 
  mutate(ATI = replace_na(ATI, 0), ATH = replace_na(ATH, 0)) %>% 
  mutate(AHI = ATI + ATH, ALI = ATA - AHI) %>% 
  mutate(AHI = ifelse(AHI < 0, 0, AHI*0.01), ALI = ifelse(ALI < 0, 0, ALI*0.01)) %>% 
  select(-c(ATI, ATH)) %>% 
  left_join(cou) %>%
  full_join(
    read.csv("spam.fao.imp.pro.csv", stringsAsFactors = F) %>%
      mutate(SPAM.short.name = as.character(SPAM.short.name))
  ) %>% 
  mutate(AE = ME.hi*AHI + ME.li*ALI, AD = MD.hi*AHI + MD.li*ALI) %>% 
  select(iso3, cell5m, x, y, SPAM.short.name, exp, AE, AD) %>% 
  rename(exp.iso3 = "iso3") %>% 
  filter(!is.na(AE)) %>%
  left_join(
    read.csv("trade10df.percent.pro.csv", stringsAsFactors = F) %>% 
      select(exp, imp, SPAM.short.name, per.imp) %>% 
      filter(per.imp != 0) %>% 
      left_join(
        cou %>% rename(imp = "exp")
      ) %>% 
      rename(imp.iso3 = "iso3") %>% 
      rename(percent = "per.imp") %>% 
      select(-imp) %>% 
      mutate(SPAM.short.name = as.character(SPAM.short.name))
  ) %>% 
  select(-exp) %>% 
  filter(!SPAM.short.name %in% c("cowp", "pige", "yams"))

# crop footprint
mapspam.imp.rmPA <- mapspam.fao.imp %>% 
  mutate(cell5m = as.numeric(as.character(cell5m))) %>% 
  left_join(read.csv("mapspam.cp.csv", stringsAsFactors = F)) %>% 
  dplyr::select(-cell5m, -x, -y) %>% 
  left_join(read.csv("trade10df.crop.percent.csv", stringsAsFactors = F) %>%
              filter(crop.fix > 0)) %>% 
  mutate(crop.area = ifelse(as.character(exp.iso3) == as.character(imp.iso3),
                            crop.fix*AD, crop.fix*AE)) %>% 
  group_by(exp.iso3, cp_pixel, imp.iso3, SPAM.short.name) %>% 
  summarise(crop.area = sum(crop.area, na.rm = T)) %>% 
  as.data.frame()

fwrite(mapspam.imp.rmPA, "mapspam.imp.rmPA.pro.csv")

# CROP FOR FEED
dt.feed.sum <- read.csv("spam2010V2r0_global_A_TA.csv", quote = "") %>% 
  filter(!is.na(x)) %>% 
  filter(!is.na(y)) %>% 
  mutate(iso3 = substr(iso3, start = 2, stop = 4)) %>% 
  mutate(iso3 = ifelse(name_adm1 == '"Taiwan"', "TWN", iso3),
         iso3 = ifelse(iso3 == "ind", "CHN", iso3),
         iso3 = replace(iso3, iso3 == "SDN", "ZZY")) %>%
  dplyr::select(1, 4:6, 10:51) %>% 
  gather(key = crop, value = ATA, -c(iso3, cell5m, x, y)) %>%
  filter(ATA != 0) %>% 
  mutate(SPAM.short.name = substr(as.character(crop), start = 1, stop = 4)) %>% 
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "pmil_a", "mill")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "smil_a", "mill")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "acof_a", "coff")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "rcof_a", "coff")) %>% 
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugb_a", "suga")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugc_a", "suga")) %>%
  dplyr::select(-crop) %>% 
  full_join(
    read.csv("spam2010V2r0_global_A_TI.csv", quote = "") %>% 
      filter(!is.na(x)) %>% 
      filter(!is.na(y)) %>% 
      mutate(iso3 = substr(iso3, start = 2, stop = 4)) %>% 
      mutate(iso3 = ifelse(name_adm1 == '"Taiwan"', "TWN", iso3),
             iso3 = ifelse(iso3 == "ind", "CHN", iso3),
             iso3 = replace(iso3, iso3 == "SDN", "ZZY")) %>%
      dplyr::select(1, 4:6, 10:51) %>% 
      gather(key = crop, value = ATI, -c(iso3, cell5m, x, y)) %>%
      filter(ATI != 0) %>% 
      mutate(SPAM.short.name = substr(as.character(crop), start = 1, stop = 4)) %>% 
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "pmil_i", "mill")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "smil_i", "mill")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "acof_i", "coff")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "rcof_i", "coff")) %>% 
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugb_i", "suga")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugc_i", "suga")) %>%
      dplyr::select(-crop)
  ) %>% 
  full_join(
    read.csv("spam2010V2r0_global_A_TH.csv", quote = "") %>% 
      filter(!is.na(x)) %>% 
      filter(!is.na(y)) %>% 
      mutate(iso3 = substr(iso3, start = 2, stop = 4)) %>% 
      mutate(iso3 = ifelse(name_adm1 == '"Taiwan"', "TWN", iso3),
             iso3 = ifelse(iso3 == "ind", "CHN", iso3),
             iso3 = replace(iso3, iso3 == "SDN", "ZZY")) %>%
      dplyr::select(1, 4:6, 10:51) %>% 
      gather(key = crop, value = ATH, -c(iso3, cell5m, x, y)) %>%
      filter(ATH != 0) %>% 
      mutate(SPAM.short.name = substr(as.character(crop), start = 1, stop = 4)) %>% 
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "pmil_h", "mill")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "smil_h", "mill")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "acof_h", "coff")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "rcof_h", "coff")) %>% 
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugb_h", "suga")) %>%
      mutate(SPAM.short.name = replace(SPAM.short.name, crop == "sugc_h", "suga")) %>%
      dplyr::select(-crop)
  ) %>% 
  mutate(ATI = replace_na(ATI, 0), ATH = replace_na(ATH, 0)) %>% 
  mutate(AHI = ATI + ATH, ALI = ATA - AHI) %>% 
  mutate(AHI = ifelse(AHI < 0, 0, AHI*0.01), ALI = ifelse(ALI < 0, 0, ALI*0.01)) %>% 
  dplyr::select(-c(ATI, ATH)) %>% 
  left_join(cou) %>%
  full_join(
    read.csv("spam.fao.imp.pro.csv", stringsAsFactors = F) %>%
      mutate(SPAM.short.name = as.character(SPAM.short.name))
  ) %>% 
  mutate(AE = ME.hi*AHI + ME.li*ALI, AD = MD.hi*AHI + MD.li*ALI) %>% 
  mutate(AT = AD + AE) %>% 
  dplyr::select(iso3, cell5m, x, y, SPAM.short.name, AT, AD, AE) %>% 
  filter(!is.na(AT)) %>% 
  rename(exp.iso3 = "iso3") %>% 
  left_join(
    read.csv("trade10df.crop.percent.csv", stringsAsFactors = F) %>% 
      filter(as.character(exp.iso3) == as.character(imp.iso3)) %>% 
      dplyr::select(exp.iso3, SPAM.short.name, crop.fix) %>% 
      rename(d.percent = "crop.fix") %>% 
      full_join(
        read.csv("trade10df.crop.percent.csv", stringsAsFactors = F) %>%
          filter(as.character(exp.iso3) != as.character(imp.iso3)) %>%
          group_by(exp.iso3, SPAM.short.name) %>% 
          summarise(e.percent = sum(crop.fix, na.rm = T)) %>% 
          as.data.frame())
  ) %>% 
  mutate(feed.area = AT - (d.percent*AD + e.percent*AE)) %>% 
  mutate(cell5m = as.numeric(as.character(cell5m))) %>%
  left_join(read.csv("mapspam.cp.csv", stringsAsFactors = F)) %>% 
  group_by(exp.iso3, cp_pixel, SPAM.short.name) %>% 
  summarise(feed.area = sum(feed.area, na.rm = T)) %>% 
  as.data.frame()

fwrite(dt.feed.sum, "dt.feed.sum.csv")

mapspam.feed.livestock <- read.csv("dt.feed.sum.csv", stringsAsFactors = F) %>% 
  left_join(
    read.csv("dt.Rh.cropforfeed.csv", stringsAsFactors = F) %>% 
      rename(exp.iso3 = "iso3.exp", imp.iso3 = "iso3.imp") %>% 
      filter(value > 0)
  ) %>% 
  mutate(feed.area = value*feed.area) %>% 
  dplyr::select(-value) %>% 
  as.data.frame()

dt.Rh.livestock <- copy(setDT(mapspam.feed.livestock))[
  , .(feed.area.livestock = sum(feed.area, na.rm = T)), by = .(exp.iso3, cp_pixel, imp.iso3, livestock)][
    feed.area.livestock > 0][
      !is.na(cp_pixel)][
        , setnames(.SD, "livestock", "SPAM.short.name")][
          , merge(.SD, fread("code.livestock.species.csv", check.names = T)[, species := NULL],
                  all.x = TRUE, by = "SPAM.short.name", allow.cartesian=TRUE)][
                    , SPAM.short.name := NULL][
                      , .(crop.area = sum(feed.area.livestock, na.rm = T)), by = .(exp.iso3, cp_pixel, imp.iso3, short.name)][
                        , setnames(.SD, "short.name", "SPAM.short.name")]

fwrite(dt.Rh.livestock, "mapspam.livestock.rmPA.pro.csv")

# LIVESTOCK
livestock.trade10df.percent <- read.csv("dt.livestock.csv", stringsAsFactors = F) %>% 
  left_join(cou %>% rename(iso3.exp = "iso3")) %>% 
  left_join(cou %>% rename(iso3.imp = "iso3", imp = "exp")) %>% 
  rename(h.per.imp = "percent") %>% 
  dplyr::select(exp, imp, SPAM.short.name, h.per.imp) %>% 
  as.data.frame()

fwrite(livestock.trade10df.percent, "stock.trade10df.percent.pro.csv")  

stock.fao.imp <- read.csv("stock10area.cp.norm.csv", stringsAsFactors = F) %>% 
  dplyr::select(1:2, 6:11) %>% 
  rename(catm = "area.cattle", shem = "area.sheep", goam = "area.goat",
         chim = "area.chick", ducm = "area.duck", pigm = "area.pig") %>% 
  mutate(catp = catm, shep = shem, goap = goam, chie = chim) %>% 
  gather(SPAM.short.name, stock.area, c(3:12)) %>%
  left_join(cou) %>% 
  rename(exp.iso3 = "iso3") %>% 
  left_join(
    read.csv("stock.trade10df.percent.pro.csv", stringsAsFactors = F) %>% 
      dplyr::select(exp, imp, SPAM.short.name, h.per.imp) %>% 
      left_join(
        cou %>% rename(imp = "exp", imp.iso3 = "iso3")
      )
  ) %>% 
  left_join(
    read.csv("stock.area.ratio.csv", stringsAsFactors = F) %>% 
      dplyr::select(exp, SPAM.short.name, area.ratio.imp)
  ) %>% 
  mutate(stock.area.species = stock.area*area.ratio.imp*h.per.imp) %>% 
  dplyr::select(exp.iso3, cp_pixel, imp.iso3, SPAM.short.name, stock.area.species) %>% 
  as.data.frame()

fwrite(stock.fao.imp, "stock.fao.imp.pro.csv")

stock.imp.rmPA <- read.csv("stock.fao.imp.pro.csv", stringsAsFactors = F) %>% 
  filter(stock.area.species > 0) %>% 
  left_join(
    read.csv("code.livestock.species.csv", stringsAsFactors = F) %>% 
      dplyr::select(-species)
  ) %>% 
  dplyr::select(-SPAM.short.name) %>% 
  group_by(exp.iso3, cp_pixel, imp.iso3, short.name) %>% 
  summarise(stock.area = sum(stock.area.species, na.rm = T)) %>%
  as.data.frame() %>% 
  rename(SPAM.short.name = "short.name", crop.area = "stock.area")

fwrite(stock.imp.rmPA, "stock.imp.rmPA.csv")  

# LIVESTOCK+FEED
livestock.plus.feed.imp.rmPA <- rbind(
  fread("stock.imp.rmPA.csv", check.names = T),
  fread("mapspam.livestock.rmPA.pro.csv", check.names = T)
)[, .(crop.area = sum(crop.area, na.rm = T)), by = .(exp.iso3, cp_pixel, imp.iso3, SPAM.short.name)][
  crop.area > 0]

fwrite(livestock.plus.feed.imp.rmPA, "livestock.plus.feed.imp.rmPA.pro.csv")

# CONNECT CROP TO LIVESTOCK
agr.imp.rmPA <- rbind(
  fread("mapspam.imp.rmPA.pro.csv", check.names = T),
  fread("livestock.plus.feed.imp.rmPA.pro.csv", check.names = T)
)[crop.area > 0][!is.na(cp_pixel)]

fwrite(agr.imp.rmPA, "agr.imp.rmPA.pro.csv")