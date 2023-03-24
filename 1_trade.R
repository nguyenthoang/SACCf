###############################
# calculate trade from FAOSTAT
###############################

library(raster)
library(sf)
library(tidyverse)
library(data.table)
setwd("sfp/input")

# 1. CROP

cou <- fread("countrygroup.csv", check.names = T)[
  , .(Country.Code, ISO3.Code)][
    , setnames(.SD, c("Country.Code", "ISO3.Code"), c("exp", "iso3"))][
      , unique(.SD)][iso3 != ""]

prod <- fread("Production_Crops_Livestock_E_All_Data_(Normalized).csv",
              check.names = T)[
                Element == "Production"][
                  , .(Area.Code, Item.Code, Value)][
                    , setnames(.SD, c("Area.Code", "Item.Code"), c("exp", "FAOCODE"))]

trad <- fread("Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv",
              check.names = T)[
                Element == "Export Quantity"][
                  , .(Reporter.Country.Code, Partner.Country.Code, Item.Code, Value)][
                    , setnames(.SD, c("Item.Code", "Reporter.Country.Code", "Partner.Country.Code", "Value"), 
                               c("FAOCODE", "exp", "imp", "value.exp"))][
                                 , merge(.SD, 
                                         fread("Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv",
                                               check.names = T)[
                                                 Element == "Import Quantity"][
                                                   , .(Reporter.Country.Code, Partner.Country.Code, Item.Code, Value)][
                                                     , setnames(.SD, c("Item.Code", "Reporter.Country.Code", "Partner.Country.Code", "Value"), 
                                                                c("FAOCODE", "imp", "exp", "value.imp"))], 
                                         all = TRUE)][
                                           , value := ifelse(is.na(value.imp), value.exp, value.imp)]

# check country across domains
exp.prod <- copy(prod)[, merge(.SD, cou, all.x = TRUE, by = "exp")][
  , setnames(.SD, "iso3", "iso3.prod")][
    !is.na(iso3.prod)][
      , unique(.SD, by = c("exp", "iso3.prod"))][
        , .(exp, iso3.prod)]

exp.trad <- copy(trad)[, merge(.SD, cou, all.x = TRUE, by = "exp")][
  , setnames(.SD, "iso3", "iso3.exp")][
    !is.na(iso3.exp)][
      , unique(.SD, by = c("exp", "iso3.exp"))][
        , .(exp, iso3.exp)]

imp.trad <- copy(trad)[, merge(.SD, copy(cou)[, setnames(.SD, "exp", "imp")], all.x = TRUE, by = "imp")][
  , .(imp, iso3)][
    !is.na(iso3)][
      , unique(.SD, by = c("imp", "iso3"))][
        , setnames(.SD, c("imp", "iso3"), c("exp", "iso3.imp"))]

cou.list <- copy(exp.prod)[, merge(.SD, exp.trad, all = TRUE, by = "exp")][
  , merge(.SD, imp.trad, all = TRUE, by = "exp")]

fwrite(cou.list, "cou.list.csv")

# ratio.yield
ratio.yield <- fread("Production_Crops_Livestock_E_All_Data_(Normalized).csv",
                     check.names = T)[
                       Element == "Yield"][
                         , .(Area.Code, Item.Code, Value, Unit)][
                           , setnames(.SD, c("Area.Code", "Item.Code"), c("exp", "FAOCODE"))][
                             , merge(.SD, fread("code.cropcalorie.csv", check.names = T), all = TRUE)][
                               !is.na(SPAM.short.name)][
                                 , by = .(exp, SPAM.short.name), maxV := max(Value)][
                                   , by = .(exp, SPAM.short.name), ratio.pre := Value/maxV][
                                     , ratio.yield := fifelse(ratio.pre > 0, ratio.pre, 1, 1)][
                                       , c("maxV", "ratio.pre") := NULL]

fwrite(ratio.yield, "ratio.yield.csv")

# DT of production and trade
dt.p <- copy(prod)[Value > 0][
  , merge(.SD, fread("code.cropcalorie.csv", check.names = T), all.x = TRUE)][
    !is.na(SPAM.short.name)][
      , Value := Value/ratio][
        , merge(.SD, fread("faocode.subgroup.csv", check.names = T), all.x = TRUE, by = c("FAOCODE", "SPAM.short.name"))][
          , merge(.SD, exp.prod, all = TRUE, by = "exp")][!is.na(iso3.prod)][
            , .(Value = sum(Value, na.rm = T)), by = .(iso3.prod, subgroup)]

fwrite(dt.p, "dt.p.crop.csv")

dt.z <- copy(trad)[
  , merge(.SD, fread("code.cropprotein.trad.csv", check.names = T), all.x = TRUE, by = "FAOCODE")][
    !is.na(SPAM.short.name)][
      , value := value/ratio][
        , merge(.SD, fread("faocode.subgroup.csv", check.names = T), all.x = TRUE, by = c("FAOCODE", "SPAM.short.name"))][
          , merge(.SD, exp.prod, all.x = TRUE, by = "exp")][
            , merge(.SD, exp.prod[, setnames(.SD, c("exp", "iso3.prod"), c("imp", "iso3.imp"))], all.x = TRUE, by = "imp")][
              , iso3.imp := ifelse(imp == 252, "ZZZ", iso3.imp)][
                , .(value = sum(value, na.rm = T)), by = .(iso3.prod, iso3.imp, subgroup)]

fwrite(dt.z, "dt.z.crop.csv")

# crop consumption according to country of origin
subgroup <- sort(unique(c(dt.p$subgroup, dt.z$subgroup)))
iso3 <- sort(unique(c(dt.p$iso3.prod, dt.z$iso3.prod, dt.z$iso3.imp)))
dt.Rh <- data.table()

for (v in 1:length(subgroup)) {
  p <- data.table(iso3.prod = iso3, subgroup = rep(subgroup[v], length(iso3)))[
    , merge(.SD, dt.p, all.x = TRUE, by = c("iso3.prod", "subgroup"))][
      , Value := ifelse(is.na(Value), 0.01, Value)][, Value]
  comm <- rep(subgroup[v], length(iso3)*length(iso3))
  imp <- vector()
  for (j in 1:length(iso3)) {
    imp.j <- rep(iso3[j], length(iso3))
    imp <- c(imp, imp.j)
  }
  exp <- rep(iso3, length(iso3))
  square.dt <- data.table(iso3.imp = imp, iso3.prod = exp, subgroup = comm)[
    , merge(.SD, dt.z, all.x = TRUE, by = c("iso3.imp", "iso3.prod", "subgroup"))][
      , value := ifelse(is.na(value), 0, value)][
        , dcast(.SD, ... ~ iso3.prod, value.var = "value")][, subgroup := NULL]
  z <- as.matrix(square.dt, rownames = T)
  i <- rep(1, length(iso3))
  p.hat <- diag(p)
  x <- p + z %*% matrix(i)
  A <- z %*% solve(diag(as.vector(x)))
  I <- diag(dim(A)[1])
  c <- (x - t(z) %*% matrix(i))/x
  R <- solve(I - A) %*% p.hat
  Rh <- diag(as.vector(c)) %*% R
  dimnames(Rh) <- list(iso3.imp = iso3, iso3.exp = iso3)
  dt.add <- setDT(as.data.frame(as.table(Rh)))[
    , setnames(.SD, "Freq", "value")][, subgroup := subgroup[v]]
  dt.Rh <- rbind(dt.Rh, dt.add)
}

fwrite(dt.Rh, "dt.Rh.crop.csv")

# consumption percent considering domestic/export ration from FBS
subgroup.ratio <- fread("faocode.subgroup.csv", check.names = T)[
  , merge(.SD, fread("ratio.yield.csv", check.names = T), all.x = TRUE, by = c("FAOCODE", "SPAM.short.name"))][
    , .(ratio.fix = mean(ratio.yield, na.rm = T)), by = .(exp, SPAM.short.name, subgroup)][
      , merge(.SD, exp.prod, all.x = TRUE, by = "exp")][
        !is.na(iso3.prod)][
          , setnames(.SD, "iso3.prod", "iso3.exp")][, exp := NULL]

fbs.percent <- fread("FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv", check.names = T)[
  , setnames(.SD, c("Area.Code", "Item.Code"), c("exp", "FAOCODE"))][
    , .(exp, FAOCODE, Item, Element, Value)][
      , dcast(.SD, ... ~ Element, value.var = "Value")][
        , lapply(.SD, function(x){ifelse(is.na(x), 0, x)})][
          , merge(.SD, fread("code.fbs.calorie.csv", check.names = T)[
            , .(FAOCODE, SPAM.short.name, ratio)], all.x = TRUE, by = "FAOCODE", allow.cartesian=TRUE)][
              , c("domestic", "export", "import", "prod", "stock") := 
                .(`Domestic supply quantity`/ratio, 
                  `Export Quantity`/ratio,
                  `Import Quantity`/ratio,
                  Production/ratio,
                  `Stock Variation`/ratio)][
                    , domestic := ifelse(domestic < 0, 0, domestic)][
                      , .(exp, SPAM.short.name, domestic, export, import, prod, stock)][
                        , by = .(exp, SPAM.short.name), lapply(.SD, sum)][
                          !is.na(SPAM.short.name)][, c("d.percent", "e.percent") := 
                                                     .(domestic/(domestic + export), export/(domestic + export))][
                                                       , lapply(.SD, function(x){ifelse(is.na(x), 0, x)})][
                                                         , .(exp, SPAM.short.name, d.percent, e.percent)]
fwrite(fbs.percent, "fbs10.percent.csv")

dt.crop <- copy(dt.Rh)[, value := ifelse(value > 0.01, value, 0)][
  , merge(.SD, fread("faocode.subgroup.csv", check.names = T)[
    , FAOCODE := NULL][
      , unique(.SD, by = c("subgroup", "SPAM.short.name"))], all.x = TRUE)][
        , merge(.SD, subgroup.ratio, all.x = TRUE, by = c("subgroup", "SPAM.short.name", "iso3.exp"))][
          , ratio.yield := ifelse(subgroup == SPAM.short.name, 1, ratio.fix)][
            , ratio.yield := ifelse(is.na(ratio.yield), 1, ratio.yield)][
              , value := value/ratio.yield][
                , .(value = sum(value, na.rm = T)), by = .(iso3.imp, iso3.exp, SPAM.short.name)][
                  , percent1 := value/sum(value), by = .(iso3.exp, SPAM.short.name)][
                    , percent1 := ifelse(is.na(percent1), 0, percent1)][
                      , merge(.SD, dt.p[, setnames(.SD, c("iso3.prod", "subgroup", "Value"), c("iso3.exp", "SPAM.short.name", "prod"))], 
                              all.x = TRUE, by = c("iso3.exp", "SPAM.short.name"))][
                                , prod := ifelse(is.na(prod), 0, prod)][
                                  , merge(.SD, fread("fbs10.percent.csv", check.names = T)[
                                    , merge(.SD, exp.prod, all.x = TRUE, by = "exp")][
                                      !is.na(iso3.prod)][
                                        , setnames(.SD, "iso3.prod", "iso3.exp")][
                                          , exp := NULL], 
                                    all.x = TRUE, by = c("iso3.exp", "SPAM.short.name"))][
                                      , d.percent := ifelse(is.na(d.percent), 0, d.percent)][
                                        , e.percent := ifelse(is.na(e.percent), 1, e.percent)][
                                          , percent2 := ifelse(iso3.exp == iso3.imp & prod > 0 & percent1 == 0, d.percent, percent1)][
                                            , sum.percent2 := sum(percent2), by = .(iso3.exp, SPAM.short.name)][
                                              , percent := ifelse(sum.percent2 < 1.0000001, percent1, 
                                                                  ifelse(iso3.exp == iso3.imp, d.percent, percent1*e.percent))][
                                                                    , .(iso3.imp, iso3.exp, SPAM.short.name, value, percent)]
fwrite(dt.crop, "dt.crop.csv")

# 2. LIVESTOCK
prod <- fread("Production_Crops_Livestock_E_All_Data_(Normalized).csv",
              check.names = T)[
                Element == "Production"][
                  , .(Area.Code, Item.Code, Value)][
                    , setnames(.SD, c("Area.Code", "Item.Code"), c("exp", "FAOCODE"))]

trad <- fread("Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv",
              check.names = T)[
                Element == "Export Quantity"][
                  , .(Reporter.Country.Code, Partner.Country.Code, Item.Code, Value)][
                    , setnames(.SD, c("Item.Code", "Reporter.Country.Code", "Partner.Country.Code", "Value"), 
                               c("FAOCODE", "exp", "imp", "value.exp"))][
                                 , merge(.SD, 
                                         fread("Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv",
                                               check.names = T)[
                                                 Element == "Import Quantity"][
                                                   , .(Reporter.Country.Code, Partner.Country.Code, Item.Code, Value)][
                                                     , setnames(.SD, c("Item.Code", "Reporter.Country.Code", "Partner.Country.Code", "Value"), 
                                                                c("FAOCODE", "imp", "exp", "value.imp"))], 
                                         all = TRUE)][
                                           , value := ifelse(is.na(value.imp), value.exp, value.imp)]

cou <- fread("countrygroup.csv", check.names = T)[
  , .(Country.Code, ISO3.Code)][
    , setnames(.SD, c("Country.Code", "ISO3.Code"), c("exp", "iso3"))][
      , unique(.SD)][iso3 != ""]

exp.prod <- copy(prod)[, merge(.SD, cou, all.x = TRUE, by = "exp")][
  , setnames(.SD, "iso3", "iso3.prod")][
    !is.na(iso3.prod)][
      , unique(.SD, by = c("exp", "iso3.prod"))][
        , .(exp, iso3.prod)]

#DT of production and trade
dt.p <- copy(prod)[Value > 0][
  , merge(.SD, fread("code.stockprotein.level2.csv", check.names = T), all.x = TRUE)][
    !is.na(SPAM.short.name)][
      , Value := Value/ratio][
        , merge(.SD, exp.prod, all = TRUE, by = "exp")][
          !is.na(iso3.prod)][
            !is.na(SPAM.short.name)][
              , .(Value = sum(Value, na.rm = T)), by = .(iso3.prod, SPAM.short.name)][
                , setnames(.SD, "SPAM.short.name", "subgroup")]

fwrite(dt.p, "dt.p.livestock.csv")

dt.z <- copy(trad)[
  , merge(.SD, fread("code.stockprotein.level3.csv", check.names = T), all.x = TRUE, by = "FAOCODE")][
    !is.na(SPAM.short.name)][
      , value := value/ratio][
        , merge(.SD, exp.prod, all.x = TRUE, by = "exp")][
          , merge(.SD, exp.prod[, setnames(.SD, c("exp", "iso3.prod"), c("imp", "iso3.imp"))], all.x = TRUE, by = "imp")][
            , iso3.prod := ifelse(exp == 252, "ZZZ", iso3.prod)][
              , iso3.imp := ifelse(imp == 252, "ZZZ", iso3.imp)][
                , .(value = sum(value, na.rm = T)), by = .(iso3.prod, iso3.imp, SPAM.short.name)][
                  , setnames(.SD, "SPAM.short.name", "subgroup")]

fwrite(dt.z, "dt.z.livestock.csv")

#livestock consumption according to country of origin
subgroup <- sort(unique(c(dt.p$subgroup, dt.z$subgroup)))
iso3 <- sort(unique(c(dt.p$iso3.prod, dt.z$iso3.prod, dt.z$iso3.imp)))
dt.Rh <- data.table()

for (v in 1:length(subgroup)) {
  p <- data.table(iso3.prod = iso3, subgroup = rep(subgroup[v], length(iso3)))[
    , merge(.SD, dt.p, all.x = TRUE, by = c("iso3.prod", "subgroup"))][
      , Value := ifelse(is.na(Value), 0.001, Value)][, Value]
  comm <- rep(subgroup[v], length(iso3)*length(iso3))
  imp <- vector()
  for (j in 1:length(iso3)) {
    imp.j <- rep(iso3[j], length(iso3))
    imp <- c(imp, imp.j)
  }
  exp <- rep(iso3, length(iso3))
  square.dt <- data.table(iso3.imp = imp, iso3.prod = exp, subgroup = comm)[
    , merge(.SD, dt.z, all.x = TRUE, by = c("iso3.imp", "iso3.prod", "subgroup"))][
      , value := ifelse(is.na(value), 0, value)][
        , dcast(.SD, ... ~ iso3.prod, value.var = "value")][, subgroup := NULL]
  z <- as.matrix(square.dt, rownames = T)
  i <- rep(1, length(iso3))
  p.hat <- diag(p)
  x <- p + z %*% matrix(i)
  A <- z %*% solve(diag(as.vector(x)))
  I <- diag(dim(A)[1])
  c <- (x - t(z) %*% matrix(i))/x
  R <- solve(I - A) %*% p.hat
  Rh <- diag(as.vector(c)) %*% R
  dimnames(Rh) <- list(iso3.imp = iso3, iso3.exp = iso3)
  dt.add <- setDT(as.data.frame(as.table(Rh)))[
    , setnames(.SD, "Freq", "value")][, subgroup := subgroup[v]]
  dt.Rh <- rbind(dt.Rh, dt.add)
}

fwrite(dt.Rh, "dt.Rh.livestock.csv")

#consumption percent considering domestic/export ration from FBS
dt.livestock <- copy(dt.Rh)[, value := ifelse(value > 0, value, 0)][
  , percent1 := value/sum(value), by = .(iso3.exp, subgroup)][
    , merge(.SD, dt.p[, setnames(.SD, c("iso3.prod", "Value"), c("iso3.exp", "prod"))], 
            all.x = TRUE, by = c("iso3.exp", "subgroup"))][
              , prod := ifelse(is.na(prod), 0, prod)][
                , merge(.SD, fread("fbs10.percent.csv", check.names = T)[
                  , merge(.SD, exp.prod, all.x = TRUE, by = "exp")][
                    !is.na(iso3.prod)][
                      , setnames(.SD, c("iso3.prod", "SPAM.short.name"), c("iso3.exp", "subgroup"))][
                        , exp := NULL], 
                  all.x = TRUE, by = c("iso3.exp", "subgroup"))][
                    , d.percent := ifelse(is.na(d.percent), 0, d.percent)][
                      , e.percent := ifelse(is.na(e.percent), 1, e.percent)][
                        , percent2 := ifelse(iso3.exp == iso3.imp & prod > 0 & percent1 == 0, d.percent, percent1)][
                          , sum.percent2 := sum(percent2), by = .(iso3.exp, subgroup)][
                            , percent := ifelse(sum.percent2 < 1.0000001, percent1, 
                                                ifelse(iso3.exp == iso3.imp, d.percent, percent1*e.percent))][
                                                  , setnames(.SD, "subgroup", "SPAM.short.name")][
                                                    , .(iso3.imp, iso3.exp, SPAM.short.name, value, percent)]

fwrite(dt.livestock, "dt.livestock.csv")  

# 3. FEED
#collect code from FBS and CB
pre.code.fbs <- fread("FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv", check.names = T)[
  Element == "Feed"][
    Value > 0][
      , .(Item.Code, Item)][
        , unique(.SD, by = c("Item.Code", "Item"))][
          order(Item.Code)][
            , setnames(.SD, "Item.Code", "FAOCODE")][
              , merge(.SD, fread("code.fbs.calorie.csv", check.names = T)[
                , .(FAOCODE, Item, SPAM.short.name)],
                all.x = TRUE, by = c("FAOCODE", "Item"))]

fwrite(pre.code.fbs, "pre.code.fbs.csv")

pre.code.cb <- fread("CommodityBalances_(non-food)_E_All_Data_(Normalized).csv", check.names = T)[
  Element == "Feed"][
    Value > 0][
      , .(Item.Code, Item)][
        , unique(.SD, by = c("Item.Code", "Item"))][
          order(Item.Code)][
            , setnames(.SD, "Item.Code", "FAOCODE")][
              , merge(.SD, fread("code.fbs.calorie.csv", check.names = T)[
                , .(FAOCODE, Item, SPAM.short.name)],
                all.x = TRUE, by = c("FAOCODE", "Item"))]

fwrite(pre.code.cb, "pre.code.cb.csv")

#percent of feed from domestic supply quantity
cou <- fread("countrygroup.csv", check.names = T)[
  , .(Country.Code, ISO3.Code)][
    , setnames(.SD, c("Country.Code", "ISO3.Code"), c("exp", "iso3"))][
      , unique(.SD)][iso3 != ""]

code.fbs.domestic <- fread("code.fbs.domestic.csv", check.names = T)
code.fbs.feed <- fread("code.fbs.feed.csv", check.names = T)
code.cb.feed <- fread("code.cb.feed.csv", check.names = T)

fbs.domestic <- fread("FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv", check.names = T)[
  Element == "Domestic supply quantity"][
    Item.Code %chin% code.fbs.domestic$FAOCODE][
      , .(Area.Code, Item.Code, Value)][
        , setnames(.SD, c("Area.Code", "Item.Code"), c("exp", "FAOCODE"))][
          , merge(.SD, code.fbs.domestic, all.x = TRUE, by = "FAOCODE", allow.cartesian=TRUE)][
            , Value := Value*1000/ratio][, merge(.SD, cou, all.x = TRUE, by = "exp")][
              !is.na(iso3)][
                , .(Value = sum(Value, na.rm = T)), by = .(iso3, SPAM.short.name, subgroup)]

fbs.feed <- fread("FoodBalanceSheetsHistoric_E_All_Data_(Normalized).csv", check.names = T)[
  Element == "Feed"][
    Item.Code %chin% code.fbs.feed$FAOCODE][
      , .(Area.Code, Item.Code, Value)][
        , setnames(.SD, c("Area.Code", "Item.Code"), c("exp", "FAOCODE"))][
          , merge(.SD, code.fbs.feed, all.x = TRUE, by = "FAOCODE", allow.cartesian=TRUE)][
            , Value := Value*1000/ratio][, merge(.SD, cou, all.x = TRUE, by = "exp")][
              !is.na(iso3)][
                , .(Value = sum(Value, na.rm = T)), by = .(iso3, SPAM.short.name, subgroup)]

cb.feed <- fread("CommodityBalances_(non-food)_E_All_Data_(Normalized).csv", check.names = T)[
  Element == "Feed"][
    Item.Code %chin% code.cb.feed$FAOCODE][
      , .(Area.Code, Item.Code, Value)][
        , setnames(.SD, c("Area.Code", "Item.Code"), c("exp", "FAOCODE"))][
          , merge(.SD, code.cb.feed, all.x = TRUE, by = "FAOCODE", allow.cartesian=TRUE)][
            , Value := Value/ratio][, merge(.SD, cou, all.x = TRUE, by = "exp")][
              !is.na(iso3)][
                , .(Value = sum(Value, na.rm = T)), by = .(iso3, SPAM.short.name, subgroup)]

feed <- rbind(fbs.feed, cb.feed)[
  , .(Value = sum(Value, na.rm = T)), by = .(iso3, SPAM.short.name, subgroup)][
    Value > 0][
      , merge(.SD, fbs.domestic[, setnames(.SD, "Value", "domestic")], all.x = TRUE)][
        , percent := fifelse(domestic <= 0, 0.9, Value/domestic, 0)][
          , percent := ifelse(percent > 1, 1, percent)]

fwrite(feed, "feed.percent.csv")

#integrate with crop matrix subgroup level
#separate crop for feed from total crop
subgroup.ratio <- fread("faocode.subgroup.csv", check.names = T)[
  , merge(.SD, fread("ratio.yield.csv", check.names = T), all.x = TRUE, by = c("FAOCODE", "SPAM.short.name"))][
    , .(ratio.fix = mean(ratio.yield, na.rm = T)), by = .(exp, SPAM.short.name, subgroup)][
      , merge(.SD, cou, all.x = TRUE, by = "exp")][
        !is.na(iso3)][
          , setnames(.SD, "iso3", "iso3.exp")][, exp := NULL]

feed.crop <- fread("dt.Rh.crop.csv", check.names = T)[
  , value := ifelse(value > 0.01, value, 0)][
    , merge(.SD, fread("faocode.subgroup.csv", check.names = T)[
      , FAOCODE := NULL][
        , unique(.SD, by = c("subgroup", "SPAM.short.name"))], all.x = TRUE)][
          , merge(.SD, subgroup.ratio, all.x = TRUE, by = c("subgroup", "SPAM.short.name", "iso3.exp"))][
            , ratio.yield := ifelse(subgroup == SPAM.short.name, 1, ratio.fix)][
              , ratio.yield := ifelse(is.na(ratio.yield), 1, ratio.yield)][
                , value := value/ratio.yield][
                  , merge(.SD, fread("feed.percent.csv", check.names = T)[
                    , .(iso3, subgroup, percent)][
                      , setnames(.SD, "iso3", "iso3.imp")], all.x = TRUE, by = c("iso3.imp", "subgroup"))][
                        , percent := ifelse(is.na(percent), 0, percent)][
                          , value.feed := percent*value][
                            , .(value.feed = sum(value.feed, na.rm = T)), by = .(iso3.imp, iso3.exp, SPAM.short.name)]

dt.crop.feed.percent <- fread("dt.crop.csv", check.names = T)[
  , merge(.SD, feed.crop, all.x = TRUE, by = c("iso3.imp", "iso3.exp", "SPAM.short.name"))][
    , feed.percent := value.feed*percent/value][
      , feed.percent := ifelse(is.na(feed.percent), 0, feed.percent)][
        , crop.percent := percent - feed.percent][
          , crop.percent := ifelse(crop.percent < 0, 0, crop.percent)]

fwrite(dt.crop.feed.percent, "dt.crop.feed.percent.csv")

#disaggregate crop for feed into livestock types (for production)
intake <- fread("gleam.csv", check.names = T)
in.percent <- fread("stock.pri.csv", check.names = T)[
  , .(pri.value = sum(pri.value, na.rm = T)), by = .(exp, SPAM.short.name)][
    , merge(.SD, intake, all.x = TRUE)][
      , merge(.SD, cou, all.x = TRUE, by = "exp")][
        !is.na(iso3)][
          , intake := intake*pri.value][
            , in.percent := intake/sum(intake), by = "iso3"][
              , dcast(.SD, iso3 ~ SPAM.short.name, value.var = "in.percent")][
                , lapply(.SD, function(x){ifelse(is.na(x),0,x)})]

col <- c("catm", "catp", "pigm", "goam", "goap", "shem", "shep", "chim", "chie", "ducm")

dt.crop.livestock.percent <- fread("dt.crop.feed.percent.csv", check.names = T)[
  , .(iso3.imp, iso3.exp, SPAM.short.name, feed.percent)][
    , merge(.SD, in.percent[, setnames(.SD, "iso3", "iso3.imp")], all.x = TRUE, by = "iso3.imp")][
      , (col) := lapply(.SD, "*", feed.percent), .SDcols = col][
        , lapply(.SD, function(x){ifelse(is.na(x),0,x)})]

fwrite(dt.crop.livestock.percent, "dt.crop.livestock.percent.csv")

#crop consumption through livestock feed according to country of origin
dt.crop.livestock.percent <- fread("dt.crop.livestock.percent.csv", check.names = T)
dt.livestock <- fread("dt.livestock.csv", check.names = T)
cou.list <- fread("cou.list.csv", check.names = T)
iso3 <- unique(c(cou.list$iso3.prod, cou.list$iso3.prod, cou.list$iso3.imp))
iso3 <- sort(iso3[iso3 != ""])
crop <- sort(unique(dt.crop.livestock.percent$SPAM.short.name))
dt.Rh <- data.table()

for (v in 1:length(crop)) {
  comm <- rep(crop[v], length(iso3)*length(iso3))
  imp <- vector()
  for (j in 1:length(iso3)) {
    imp.j <- rep(iso3[j], length(iso3))
    imp <- c(imp, imp.j)
  }
  exp <- rep(iso3, length(iso3))
  for (i in 1:length(col)) {
    livestock <- rep(col[i], length(iso3)*length(iso3))
    square.dt <- data.table(iso3.imp = imp, iso3.exp = exp, SPAM.short.name = livestock)[
      , merge(.SD, dt.livestock, all.x = TRUE, by = c("iso3.imp", "iso3.exp", "SPAM.short.name"))][
        , c("SPAM.short.name", "value") := NULL][
          , dcast(.SD, ... ~ iso3.exp, value.var = "percent")]
    l <- as.matrix(square.dt, rownames = T)
    cols <- c(col[col != col[i]], "feed.percent")
    square.dt <- data.table(iso3.imp = imp, iso3.exp = exp, SPAM.short.name = comm)[
      , merge(.SD, copy(dt.crop.livestock.percent)[, (cols) := NULL], 
              all.x = TRUE, by = c("iso3.imp", "iso3.exp", "SPAM.short.name"))][
                , "SPAM.short.name" := NULL][
                  , dcast(.SD, ... ~ iso3.exp, value.var = col[i])]
    f <- as.matrix(square.dt, rownames = T)
    Rh <- l %*% f
    dimnames(Rh) <- list(iso3.imp = iso3, iso3.exp = iso3)
    dt.add <- setDT(as.data.frame(as.table(Rh)))[
      , setnames(.SD, "Freq", "value")][
        , c("SPAM.short.name", "livestock") := .(crop[v], col[i])]
    dt.Rh <- rbind(dt.Rh, dt.add)
    print(paste(crop[v], "for", col[i], "DONE"))
  }
}

fwrite(dt.Rh, "dt.Rh.cropforfeed.csv")