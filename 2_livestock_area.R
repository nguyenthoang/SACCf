######################################
# normalize stock area based on MODIS
######################################

# load data
MCD12C1 <- raster("MCD12C1.tif")
cprasc <- raster("rankmap_c_rmPA_r2.tif")
modis.p.area <- read.csv("modis_area_centroid.csv", stringsAsFactors = F)
cp.area <- read.csv("cp_area.csv", stringsAsFactors = F)
stock10area <- read.csv("stock_2010_area.csv", stringsAsFactors = F)
mapspam.ATA <- read.csv("spam2010V2r0_global_A_TA.csv", quote = "") %>%
  filter(!is.na(x)) %>%
  filter(!is.na(y)) %>%
  mutate(spam = rowSums(.[names(.)[10:51]], na.rm = T) * 0.01) %>%
  select(cell5m, x, y, spam)

# cp_value
rascpolygon <- rasterToPolygons(cprasc)
rascpolygon$cp_pixel <- 1:nrow(rascpolygon)
rascpolygon_sf <- st_as_sf(rascpolygon)
st_crs(rascpolygon_sf) <- crs(MCD12C1)
modis.p.sf <- st_as_sf(modis.p.area, coords = c("x", "y"), crs = crs(MCD12C1))
modis.p.cp <- st_join(modis.p.sf, rascpolygon_sf)
modis.cp <- geo_to_cols(modis.p.cp, geometry)
st_geometry(modis.cp) <- NULL

# suitable modis area
modis.cp.area <- modis.cp %>%
  filter(!is.na(cp_pixel)) %>%
  filter(MCD12C1 %in% c(6:10, 14, 16)) %>%
  rename(cp = "rankmap_c_rmPA_r2") %>%
  group_by(cp_pixel, cp) %>%
  summarise(modis.area = sum(modis.area, na.rm = T)) %>%
  as.data.frame()

# combine cp and modis area
cp.cent.area <- cp.area %>%
  rename(xnew = "x", ynew = "y") %>%
  left_join(modis.cp.area %>% select(-cp)) %>%
  mutate(
    modis.area = replace_na(modis.area, 0),
    unavailable = cp.area - modis.area
  )

# spam total area
mapspam.ATA.sf <- st_as_sf(mapspam.ATA,
  coords = c("x", "y"), crs = crs(MCD12C1)
)
mapspam.ATA.cp <- st_join(mapspam.ATA.sf, rascpolygon_sf)
st_geometry(mapspam.ATA.cp) <- NULL
mapspam.cp <- mapspam.ATA.cp %>%
  rename(cp = "rankmap_c_rmPA_r2") %>%
  filter(!is.na(cp_pixel)) %>%
  group_by(cp_pixel, cp) %>%
  summarise(spam = sum(spam, na.rm = T)) %>%
  as.data.frame()

# max area for stock
stock.max.area <- cp.cent.area %>%
  left_join(mapspam.cp %>% select(-cp)) %>%
  mutate(
    spam = replace_na(spam, 0),
    max.stock = cp.area - (unavailable + spam)
  )

# normalize stock area using max stock area
stock10area.norm <- stock10area %>%
  filter(!is.na(cp_pixel)) %>%
  select(-cp) %>%
  left_join(
    stock.max.area %>%
      select(2:5, 9)
  ) %>%
  mutate(
    max.stock = ifelse(max.stock < 0, 0, max.stock),
    stock = area.cattle_sum + area.sheep_sum +
      area.goat_sum + area.chick_sum + area.duck_sum + area.pig_sum,
    availabe = max.stock - stock
  ) %>%
  mutate(
    area.cattle = ifelse(max.stock == 0, 0,
      ifelse(availabe < 0, max.stock * area.cattle_sum / stock, area.cattle_sum)
    ),
    area.sheep = ifelse(max.stock == 0, 0,
      ifelse(availabe < 0, max.stock * area.sheep_sum / stock, area.sheep_sum)
    ),
    area.goat = ifelse(max.stock == 0, 0,
      ifelse(availabe < 0, max.stock * area.goat_sum / stock, area.goat_sum)
    ),
    area.chick = ifelse(max.stock == 0, 0,
      ifelse(availabe < 0, max.stock * area.chick_sum / stock, area.chick_sum)
    ),
    area.duck = ifelse(max.stock == 0, 0,
      ifelse(availabe < 0, max.stock * area.duck_sum / stock, area.duck_sum)
    ),
    area.pig = ifelse(max.stock == 0, 0,
      ifelse(availabe < 0, max.stock * area.pig_sum / stock, area.pig_sum)
    )
  ) %>%
  select(
    iso3, cp_pixel, cp.area, xnew, ynew, area.cattle, area.sheep,
    area.goat, area.chick, area.duck, area.pig
  )

# normalize stock area for more than two countries per pixel
rep <- as.data.frame(table(stock10area.norm$cp_pixel)) %>%
  filter(Freq > 1)
stock10area.rep <- stock10area.norm %>%
  filter(cp_pixel %in% rep$Var1) %>%
  select(
    cp_pixel, area.cattle, area.sheep, area.goat,
    area.chick, area.duck, area.pig
  ) %>%
  group_by(cp_pixel) %>%
  summarise_all(list(sum = sum)) %>%
  as.data.frame() %>%
  mutate(sum = area.cattle_sum + area.sheep_sum + area.goat_sum +
    area.chick_sum + area.duck_sum + area.pig_sum) %>%
  select(cp_pixel, sum) %>%
  filter(sum > 0) %>%
  left_join(
    stock.max.area %>% select(cp_pixel, max.stock) %>%
      mutate(max.stock = ifelse(max.stock < 0, 0, max.stock))
  ) %>%
  mutate(norm = ifelse(max.stock == 0, 0, sum / max.stock)) %>%
  filter(norm > 1) %>%
  select(cp_pixel, norm)
stock10area.cp.norm <- stock10area.norm %>%
  left_join(stock10area.rep) %>%
  mutate(
    norm = replace_na(norm, 1),
    area.cattle = area.cattle / norm,
    area.sheep = area.sheep / norm,
    area.goat = area.goat / norm,
    area.chick = area.chick / norm,
    area.duck = area.duck / norm,
    area.pig = area.pig / norm
  )

write.csv(stock10area.cp.norm, "stock10area.cp.norm.csv", row.names = F)