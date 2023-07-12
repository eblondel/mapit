# test_choropleth_maps.R
#=======================
require(testthat)
context("graduated_symbol_maps")

test_that("create map - graduated linear symbols",{
  
  df <- as.data.frame(readr::read_csv(system.file("extdata/examples/sofia_2022_fish_consumption.csv", package = "mapit")))

  pdf("test_map_graduated_symbols_linear.pdf", width = 15, height = 10)
  mapit::create_map(sfby = "countries", stat = df, by = "M49", variable = "VALUE", digits = 0,
                    classtype = "fixed", maptype = "graduated_linear_symbols",
                    breaks = c(0,5,10,20,30,50,max(df$VALUE)),
                    boundCol = "grey",
                    legendtitle = "AVERAGE AQUATIC FOODS CONSUMPTION PER CAPITA\n(IN KG/YEAR)",
                    legendunit = "", legend_nesting = TRUE
  )
  dev.off()
  
})

test_that("create map - graduated mean symbols",{
  
  df <- as.data.frame(readr::read_csv(system.file("extdata/examples/sofia_2022_fish_consumption.csv", package = "mapit")))
  
  pdf("test_map_graduated_symbols_mean.pdf", width = 15, height = 10)
  mapit::create_map(sfby = "countries", stat = df, by = "M49", variable = "VALUE", digits = 0,
                    classtype = "fixed", maptype = "graduated_mean_symbols",
                    breaks = c(0,5,10,20,30,50,max(df$VALUE)),
                    boundCol = "grey",
                    legendtitle = "AVERAGE AQUATIC FOODS CONSUMPTION PER CAPITA\n(IN KG/YEAR)",
                    legendunit = "", legend_nesting = TRUE
  )
  dev.off()
  
})