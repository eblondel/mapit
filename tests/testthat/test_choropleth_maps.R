# test_choropleth_maps.R
#=======================
require(testthat)
context("choropleth_maps")

test_that("create map - choropleth 1",{
  
  df <- as.data.frame(readr::read_csv(system.file("extdata/examples/sofia_2022_fish_consumption.csv", package = "mapit")))
  
  pdf("test_map_choropleth_1_highres1.pdf", width = 13, height = 7)
  par(mar = rep(0.01,4))
  create_map(sfby = "countries", stats = df, by = "M49", variable = "VALUE", digits = 0,
                    classtype = "fixed", pal = RColorBrewer::brewer.pal(5, "Blues"),
                    breaks = c(0,5,10,25,50,max(df$VALUE)), 
                    boundCol = "grey", 
                    legendtitle = "AVERAGE AQUATIC FOODS CONSUMPTION PER CAPITA\n(IN KG/YEAR)",
                    legendunit = ""
  )
  dev.off()
  
  pdf("test_map_choropleth_1_lowres1.pdf", width = 13, height = 7)
  par(mar = rep(0.01,4))
  create_map(sfby = "countries_lowres", stats = df, by = "M49", variable = "VALUE", digits = 0,
                    classtype = "fixed", pal = RColorBrewer::brewer.pal(5, "Blues"),
                    breaks = c(0,5,10,25,50,max(df$VALUE)),
                    boundCol = "grey", 
                    legendtitle = "AVERAGE AQUATIC FOODS CONSUMPTION PER CAPITA\n(IN KG/YEAR)",
                    legendunit = ""
  )
  dev.off()
  
})

test_that("create map - choropleth 2",{
  
  df <- as.data.frame(readr::read_csv(system.file("extdata/examples/sofia_2022_fish_protein_supply.csv", package = "mapit")))
  df_bis <- df[df$FISH_PROT2 == "Yes",]
  
  pdf("test_map_choropleth_2.pdf", width = 15, height = 10)
  mapit::create_map(sfby = "countries", stat = df, by = "M49", variable = "FISH_PROT1", digits = 0,
                     classtype = "fixed", pal = RColorBrewer::brewer.pal(5, "Blues"),
                     breaks = c(0,2,4,6,10,max(df$FISH_PROT1)),
                     boundCol = "grey", 
                     legendtitle = "AQUATIC FOOD PROTEINS\n(GRAMS PER CAPITA PER DAY)",
                     legendunit = "g"
  )
  newvar <- mapit::spatialize_dataset(sfby = "countries", sfby.code = "M49", stats = df_bis, by = "M49", variable = "FISH_PROT2", maptype = "symbols")
  plot(sf::st_centroid(newvar), border="transparent", pch = 19, cex = 1.1, col = "#F18804", add = TRUE)
  mapit::create_legend(-16500000, -4000000 - (6*680000), pch = 19, cex = .8, col = "#F18804", legend = "> 20% contribution of aquatic foods to animal protein intake", box.col = "transparent", border = "transparent", xjust = 0, text.col = "black", y.intersp=1.5)
  
  dev.off()
})

test_that("create map - choropleth - FAO global production",{
  
  df <- as.data.frame(readr::read_csv(system.file("extdata/examples/fao_global_production.csv", package = "mapit")))
  
  #jenks
  pdf("test_map_choropleth_fao_global_production_jenks.pdf", width = 15, height = 10)
  mapit::create_map(sfby = "countries", stat = df, by = "country_code", variable = "prod", digits = 2,
                    classtype = "jenks", classnumber = 5, pal = RColorBrewer::brewer.pal(5, "Blues"),
                    boundCol = "grey", 
                    legendtitle = "GLOBAL PRODUCTION",
                    legendunit = ""
  )
  dev.off()
  
  #fixed
  pdf("test_map_choropleth_fao_global_production_jenks.pdf", width = 15, height = 10)
  mapit::create_map(sfby = "countries", stat = df, by = "country_code", variable = "prod", digits = 2,
                    classtype = "fixed", pal = RColorBrewer::brewer.pal(5, "Blues"),
                    breaks = c(0,3,10,30,70,max(df$prod)),
                    boundCol = "grey", 
                    legendtitle = "GLOBAL PRODUCTION",
                    legendunit = ""
  )
  dev.off()
})

test_that("create map - choropleth - BBOX",{
  
  df <- as.data.frame(readr::read_csv(system.file("extdata/examples/sofia_2022_fish_consumption.csv", package = "mapit")))
  
  pdf("test_map_choropleth_1_highres1_bbox.pdf", width = 10, height = 15)
  par(mar = rep(0.01,4))
  
  bbox = as.list(sf::st_bbox(sf::st_transform(fdi4R::fao_areas_eck4[fdi4R::fao_areas_eck4$F_AREA == 47,],4326)))
  create_map(sfby = "countries", bbox=bbox, stats = df, by = "M49", variable = "VALUE", digits = 0,
             classtype = "fixed", pal = RColorBrewer::brewer.pal(5, "Blues"),
             breaks = c(0,5,10,25,50,max(df$VALUE)), 
             boundCol = "grey", 
             legendtitle = "AVERAGE AQUATIC FOODS CONSUMPTION PER CAPITA\n(IN KG/YEAR)",
             legendunit = ""
  )
  dev.off()
  
})