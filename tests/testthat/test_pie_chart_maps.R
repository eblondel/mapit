# test_choropleth_maps.R
#=======================
require(testthat)
context("pie_chart_maps")

test_that("create map - with pie charts",{
  
  df <- as.data.frame(readr::read_csv(system.file("extdata/examples/sofia_2022_fish_consumption.csv", package = "mapit")))
  
  pdf("test_map_graduated_symbols_mean_with_piechart.pdf", width = 15, height = 10)
  mapit::create_map(sfby = "countries", stat = df, by = "M49", variable = "VALUE", digits = 0,
                    classtype = "fixed", maptype = "graduated_mean_symbols",
                    plot.handler = function(x){
                      pie(rep(1,5),col = rainbow(5), radius = 1, border = NA, labels = "")
                    },
                    breaks = c(0,5,10,20,30,50,max(df$VALUE)),
                    boundCol = "grey", 
                    legendtitle = "BREAKDOWN BY COUNTRY",
                    legendunit = ""
  )
  dev.off()
  
})

test_that("create map - with pie charts / real example",{
  
  df <- as.data.frame(readr::read_csv(system.file("extdata/examples/sofia_2022_capture_by_fao_area.csv", package = "mapit")))
  df[is.na(df)] <- 0
  class(df$f_code) <- "character"
  
  pdf("test_map_graduated_symbols_mean_with_piechart_fao_areas.pdf", width = 15, height = 10)
  mapit::create_map(sfby = "fao_areas", stat = df, by = "f_code", variable = "catch", digits = 2,
                    classtype = "jenks", classnumber = 4, maptype = "graduated_mean_symbols",
                    plot.handler = function(x){
                      df.x <- as.data.frame(x)
                      df.x <- df.x[,c("demersal", "tuna", "other_pelagic", "other_aquatic")]
                      pie.data = unlist(as.vector(df.x))
                      names(pie.data) <- NULL
                      pie.data <- data.frame(value = pie.data, col = c("#a67a69", "#2c5dd8", "#8cc8ea", "#ece44e"))
                      pie.data <- pie.data[pie.data$value > 0,]
                      pie(pie.data$value, col = pie.data$col, radius = 1, border = NA, labels = "")
                    },
                    boundCol = "grey", 
                    legendtitle = "CATCH (MILLION TONNES)",
                    legendunit = ""
  )
  dev.off()
  
})


for(i in 1:nrow(df)){
  x = df[i,]
  x[is.na(x)] <- 0
  pie.data = unlist(as.vector(x[,2:5]))
  names(pie.data) <- NULL
  pie(pie.data,col = c("#a67a69", "#2c5dd8", "#8cc8ea", "#ece44e"), radius = 1, border = NA, labels = "")
}
