# test_ggplot_chart_maps.R
#=======================
require(testthat)
context("ggplot_chart_maps")

test_that("create map - with ggplot",{

  layers = mapit::get_baselayers()
  
  data = data.frame(
    code = c("SDG_CSA", "SDG_ENA", "SDG_ESEA", "SDG_LAC", "SDG_NAWA", "SDG_OCE", "SDG_SSA"),
    value = 1:7
  )
  
  
  # load library
  library(ggplot2)
  
  # Create test data.
  ggdata <- data.frame(
    category=c("A", "B", "C"),
    count=c(10, 60, 30)
  )
  
  # Compute percentages
  ggdata$fraction = ggdata$count / sum(ggdata$count)
  
  # Compute the cumulative percentages (top of each rectangle)
  ggdata$ymax = cumsum(ggdata$fraction)
  
  # Compute the bottom of each rectangle
  ggdata$ymin = c(0, head(ggdata$ymax, n=-1))
  
  
  pdf("test_map_ggplot.pdf", width = 15, height = 10)
  create_map(sfby = "un_sdg_regions", stat = data, by = "code", variable = "value", digits = 0,
                    classtype = "jenks", classnumber = 3, maptype = "graduated_mean_symbols",
                    plot.type = "ggplot",
                    plot.handler = function(x){
                      p = ggplot(ggdata, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
                        geom_rect() +
                        coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
                        xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
                      return(p)
                    },
                    boundCol = "grey", 
                    legendtitle = "test",
                    legendunit = "",
                    legend_nesting = TRUE
  )
  dev.off()
  
})