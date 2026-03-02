require(fdisfdata)

#preload spatial datasets in Eckert IV projection to avoid having to do it at runtime

#un_continent_lowres
un_continent_lowres_eck4 = fdisfdata::un_continent_lowres |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(un_continent_lowres_eck4, overwrite = TRUE)
#un_continent_nopole_lowres
un_continent_nopole_lowres_eck4 = fdisfdata::un_continent_nopole_lowres |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(un_continent_nopole_lowres_eck4, overwrite = TRUE)
#un_water_bodies
un_water_bodies_eck4 = fdisfdata::un_water_bodies |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(un_water_bodies_eck4, overwrite = TRUE)
#un_countries
un_countries_eck4 = fdisfdata::un_countries |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(un_countries_eck4, overwrite = TRUE)
#un_countries_lowres
un_countries_lowres_eck4 = fdisfdata::un_countries_lowres |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(un_countries_lowres_eck4, overwrite = TRUE)
#un_boundaries
un_boundaries_eck4 = fdisfdata::un_boundaries |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(un_boundaries_eck4, overwrite = TRUE)
#un_boundaries_lowres
un_boundaries_lowres_eck4 = fdisfdata::un_boundaries_lowres |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(un_boundaries_lowres_eck4, overwrite = TRUE)
#fao_areas
fao_areas_eck4 = fdisfdata::fao_areas |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(fao_areas_eck4, overwrite = TRUE)
#fao_areas_lowres
fao_areas_lowres_eck4 = fdisfdata::fao_areas_lowres |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(fao_areas_lowres_eck4, overwrite = TRUE)
#fao_major_areas_lowres
fao_major_areas_lowres_eck4 = fdisfdata::fao_major_areas_lowres |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(fao_major_areas_lowres_eck4, overwrite = TRUE)
#fao_areas_lines
fao_areas_lines_eck4 = fdisfdata::fao_areas_lines |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(fao_areas_lines_eck4, overwrite = TRUE)
#fao_areas_inland
fao_areas_inland_eck4 = fdisfdata::fao_areas_inland |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(fao_areas_inland_eck4, overwrite = TRUE)
#un_sdg_regions
un_sdg_regions_eck4 = fdisfdata::un_sdg_regions |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(un_sdg_regions_eck4, overwrite = TRUE)
#un_sdg_regions_lowres
un_sdg_regions_lowres_eck4 = fdisfdata::un_sdg_regions_lowres |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(un_sdg_regions_lowres_eck4, overwrite = TRUE)
#un_sdg_regions
un_sdg_regions_placemarks_eck4 = fdisfdata::un_sdg_regions_placemarks |> sf::st_transform(crs = "+proj=eck4") |> sf::st_make_valid()
usethis::use_data(un_sdg_regions_placemarks_eck4, overwrite = TRUE)