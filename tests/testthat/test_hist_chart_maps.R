# test_choropleth_maps.R
#=======================
require(testthat)
context("pie_chart_maps")

test_that("create map - with histogram",{
  
  df <- as.data.frame(readr::read_csv(system.file("extdata/examples/sofia_2022_fish_consumption.csv", package = "mapit")))
  
  pdf("test_map_graduated_symbols_mean_with_hist.pdf", width = 15, height = 10)
  mapit::create_map(sfby = "countries", stat = df, by = "M49", variable = "VALUE", digits = 0,
                    classtype = "fixed", maptype = "graduated_mean_symbols",
                    plot.handler = function(x){
                      hist(rnorm(100), breaks = 20, col = "blue", main = NULL, axes = FALSE, xlab = "", ylab = "", border = "blue")
                    },
                    breaks = c(0,5,10,20,30,50,max(df$VALUE)),
                    boundCol = "grey", 
                    legendtitle = "BREAKDOWN BY COUNTRY",
                    legendunit = ""
  )
  dev.off()
  
})