##########
# Ranking
##########

# main agricultural species per country per cp_pixel
agr.imp.rmPA <- fread("agr.imp.rmPA.pro.csv", check.names = T)
main.species <- agr.imp.rmPA %>% 
  filter(crop.area > 0) %>% 
  group_by(exp.iso3, cp_pixel, imp.iso3) %>% 
  slice(which.max(crop.area)) %>% 
  rename(main.crop = "SPAM.short.name") %>% 
  as.data.frame()

write.csv(main.species, "main.species.rmPA.pro.csv", row.names = F)

# total agricultural area per country per cp_pixel
total.species <- agr.imp.rmPA %>% 
  filter(crop.area > 0) %>% 
  group_by(exp.iso3, cp_pixel, imp.iso3) %>%
  summarise(total.crop.area = sum(crop.area, na.rm = T)) %>% 
  as.data.frame()

write.csv(total.species, "total.species.rmPA.kcal.csv", row.names = F)

# rank agricultural species area per country per cp_pixel
rank.species <- total.species %>% 
  group_by(exp.iso3, cp_pixel) %>%
  mutate(imp.rank = rank(-total.crop.area, ties.method= "min")) %>% 
  as.data.frame()

write.csv(rank.species, "rank.species.rmPA.kcal.csv", row.names = F)

# consumption rank
rank.species.df <- read.csv("cp.cent.rmPA.csv", stringsAsFactors = F) %>% 
  left_join(
    rank.species %>% 
      filter(imp.rank == 1) %>% 
      rename(top.cons = "imp.iso3") %>% 
      rename(top.agr.area = "total.crop.area") %>% 
      select(exp.iso3, cp_pixel, top.cons, top.agr.area) %>% 
      left_join(
        main.species %>% 
          rename(top.cons = "imp.iso3") %>%
          rename(top.main.spec = "main.crop") %>%
          select(exp.iso3, cp_pixel, top.cons, top.main.spec)
      )
  ) %>% 
  left_join(
    rank.species %>% 
      filter(imp.rank == 2) %>% 
      rename(second.cons = "imp.iso3") %>% 
      rename(second.agr.area = "total.crop.area") %>% 
      select(exp.iso3, cp_pixel, second.cons, second.agr.area) %>% 
      left_join(
        main.species %>% 
          rename(second.cons = "imp.iso3") %>%
          rename(second.main.spec = "main.crop") %>%
          select(exp.iso3, cp_pixel, second.cons, second.main.spec)
      )
  ) %>% 
  left_join(
    rank.species %>% 
      filter(imp.rank == 3) %>% 
      rename(third.cons = "imp.iso3") %>% 
      rename(third.agr.area = "total.crop.area") %>% 
      select(exp.iso3, cp_pixel, third.cons, third.agr.area) %>% 
      left_join(
        main.species %>% 
          rename(third.cons = "imp.iso3") %>%
          rename(third.main.spec = "main.crop") %>%
          select(exp.iso3, cp_pixel, third.cons, third.main.spec)
      )
  ) %>% 
  left_join(
    rank.species %>% 
      filter(imp.rank == 4) %>% 
      rename(four.cons = "imp.iso3") %>% 
      rename(four.agr.area = "total.crop.area") %>% 
      select(exp.iso3, cp_pixel, four.cons, four.agr.area) %>% 
      left_join(
        main.species %>% 
          rename(four.cons = "imp.iso3") %>%
          rename(four.main.spec = "main.crop") %>%
          select(exp.iso3, cp_pixel, four.cons, four.main.spec)
      )
  ) %>% 
  left_join(
    rank.species %>% 
      filter(imp.rank == 5) %>% 
      rename(five.cons = "imp.iso3") %>% 
      rename(five.agr.area = "total.crop.area") %>% 
      select(exp.iso3, cp_pixel, five.cons, five.agr.area) %>% 
      left_join(
        main.species %>% 
          rename(five.cons = "imp.iso3") %>%
          rename(five.main.spec = "main.crop") %>%
          select(exp.iso3, cp_pixel, five.cons, five.main.spec)
      )
  ) %>% 
  left_join(
    rank.species %>% 
      filter(imp.rank == 6) %>% 
      rename(six.cons = "imp.iso3") %>% 
      rename(six.agr.area = "total.crop.area") %>% 
      select(exp.iso3, cp_pixel, six.cons, six.agr.area) %>% 
      left_join(
        main.species %>% 
          rename(six.cons = "imp.iso3") %>%
          rename(six.main.spec = "main.crop") %>%
          select(exp.iso3, cp_pixel, six.cons, six.main.spec)
      )
  ) %>% 
  left_join(
    rank.species %>% 
      filter(imp.rank == 7) %>% 
      rename(seven.cons = "imp.iso3") %>% 
      rename(seven.agr.area = "total.crop.area") %>% 
      select(exp.iso3, cp_pixel, seven.cons, seven.agr.area) %>% 
      left_join(
        main.species %>% 
          rename(seven.cons = "imp.iso3") %>%
          rename(seven.main.spec = "main.crop") %>%
          select(exp.iso3, cp_pixel, seven.cons, seven.main.spec)
      )
  ) %>% 
  left_join(
    rank.species %>% 
      filter(imp.rank == 8) %>% 
      rename(eight.cons = "imp.iso3") %>% 
      rename(eight.agr.area = "total.crop.area") %>% 
      select(exp.iso3, cp_pixel, eight.cons, eight.agr.area) %>% 
      left_join(
        main.species %>% 
          rename(eight.cons = "imp.iso3") %>%
          rename(eight.main.spec = "main.crop") %>%
          select(exp.iso3, cp_pixel, eight.cons, eight.main.spec)
      )
  ) %>% 
  left_join(
    rank.species %>% 
      filter(imp.rank == 9) %>% 
      rename(nine.cons = "imp.iso3") %>% 
      rename(nine.agr.area = "total.crop.area") %>% 
      select(exp.iso3, cp_pixel, nine.cons, nine.agr.area) %>% 
      left_join(
        main.species %>% 
          rename(nine.cons = "imp.iso3") %>%
          rename(nine.main.spec = "main.crop") %>%
          select(exp.iso3, cp_pixel, nine.cons, nine.main.spec)
      )
  ) %>% 
  left_join(
    rank.species %>% 
      filter(imp.rank == 10) %>% 
      rename(ten.cons = "imp.iso3") %>% 
      rename(ten.agr.area = "total.crop.area") %>% 
      select(exp.iso3, cp_pixel, ten.cons, ten.agr.area) %>% 
      left_join(
        main.species %>% 
          rename(ten.cons = "imp.iso3") %>%
          rename(ten.main.spec = "main.crop") %>%
          select(exp.iso3, cp_pixel, ten.cons, ten.main.spec)
      )
  ) %>% 
  filter(!is.na(top.cons)) %>% 
  mutate(top.agr.area.pct = top.agr.area/land) %>% 
  mutate(second.agr.area.pct = second.agr.area/land) %>%
  mutate(third.agr.area.pct = third.agr.area/land) %>% 
  mutate(four.agr.area.pct = four.agr.area/land) %>% 
  mutate(five.agr.area.pct = five.agr.area/land) %>% 
  mutate(six.agr.area.pct = six.agr.area/land) %>% 
  mutate(seven.agr.area.pct = seven.agr.area/land) %>% 
  mutate(eight.agr.area.pct = eight.agr.area/land) %>% 
  mutate(nine.agr.area.pct = nine.agr.area/land) %>% 
  mutate(ten.agr.area.pct = ten.agr.area/land) %>% 
  select(-top.agr.area, -second.agr.area, -third.agr.area, -cp.area, -land,
         -four.agr.area, -five.agr.area, -six.agr.area, -seven.agr.area, 
         -eight.agr.area, -nine.agr.area, -ten.agr.area)

agr.df.cons <- rank.species.df %>% 
  select(exp.iso3, cp_pixel, index2010, index2070f2, index2070f8, top.cons, top.main.spec, top.agr.area.pct,
         second.cons, second.main.spec, second.agr.area.pct, third.cons, third.main.spec, third.agr.area.pct,
         four.cons, four.main.spec, four.agr.area.pct, five.cons, five.main.spec, five.agr.area.pct,
         six.cons, six.main.spec, six.agr.area.pct, seven.cons, seven.main.spec, seven.agr.area.pct,
         eight.cons, eight.main.spec, eight.agr.area.pct, nine.cons, nine.main.spec, nine.agr.area.pct,
         ten.cons, ten.main.spec, ten.agr.area.pct, xnew, ynew) %>% 
  as.data.frame()

write.csv(agr.df.cons, "consumption.pro.csv", row.names = F)

# production rank
agr.cp.prod <- fread("agr.cp.prod.csv", check.names = T)

agr.prod.rank <- agr.cp.prod %>% 
  select(1:50) %>% 
  gather(species, area, 3:50) %>% 
  filter(area > 0) %>% 
  mutate(SPAM.short.name = substr(as.character(species), start = 1, stop = 4)) %>% 
  mutate(SPAM.short.name = replace(SPAM.short.name, species == "pmil_a_sum", "mill")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, species == "smil_a_sum", "mill")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, species == "acof_a_sum", "coff")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, species == "rcof_a_sum", "coff")) %>% 
  mutate(SPAM.short.name = replace(SPAM.short.name, species == "sugb_a_sum", "suga")) %>%
  mutate(SPAM.short.name = replace(SPAM.short.name, species == "sugc_a_sum", "suga")) %>%
  select(-species) %>%
  group_by(iso3, cp_pixel) %>%
  mutate(species.rank = rank(-area, ties.method= "min")) %>% 
  select(-area) %>%
  as.data.frame()

write.csv(agr.prod.rank, "agr.prod.rank.csv", row.names = F)

# top 5 production
cp.cent.rmPA <- read.csv("cp.cent.rmPA.csv", stringsAsFactors = F) %>% 
  select(-cp.area) %>% 
  rename(cp.area = "land")

agr.prod.df <- agr.cp.prod %>% 
  select(1:2, 51) %>%
  left_join(cp.cent.rmPA) %>% 
  mutate(agr.area.pct = agr.area/cp.area) %>% 
  select(iso3, cp_pixel, agr.area.pct, xnew, ynew) %>% 
  rename(prod = "iso3") %>% 
  filter(!is.na(cp_pixel)) %>% 
  left_join(
    agr.prod.rank %>% 
      filter(species.rank == 1) %>% 
      rename(prod = "iso3") %>% 
      rename(species1 = "SPAM.short.name") %>% 
      select(prod, cp_pixel, species1)
  ) %>% 
  left_join(
    agr.prod.rank %>% 
      filter(species.rank == 2) %>% 
      rename(prod = "iso3") %>% 
      rename(species2 = "SPAM.short.name") %>% 
      select(prod, cp_pixel, species2)
  ) %>%
  left_join(
    agr.prod.rank %>% 
      filter(species.rank == 3) %>% 
      rename(prod = "iso3") %>% 
      rename(species3 = "SPAM.short.name") %>% 
      select(prod, cp_pixel, species3)
  ) %>%
  left_join(
    agr.prod.rank %>% 
      filter(species.rank == 4) %>% 
      rename(prod = "iso3") %>% 
      rename(species4 = "SPAM.short.name") %>% 
      select(prod, cp_pixel, species4)
  ) %>%
  left_join(
    agr.prod.rank %>% 
      filter(species.rank == 5) %>% 
      rename(prod = "iso3") %>% 
      rename(species5 = "SPAM.short.name") %>% 
      select(prod, cp_pixel, species5)
  )

write.csv(agr.prod.df, "production.csv", row.names = F)

# combine production and consumption
full.df <- left_join(read.csv("production.csv", stringsAsFactors = F),
                     read.csv("consumption.pro.csv", stringsAsFactors = F) %>% 
                       rename(prod = "exp.iso3") %>% 
                       mutate(prod = as.character(prod))
)

full.df <- full.df[c(1:2,11:13,3,6:10,14:43,4:5)]

write.csv(full.df, "consumption_production.pro.csv", row.names = F)
