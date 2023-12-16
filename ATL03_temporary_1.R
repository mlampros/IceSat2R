
require(rvest)

atl03_exists = rvest::read_html(x = "https://openaltimetry.earthdatacloud.nasa.gov/data/icesat2/") |> 
  rvest::html_nodes(xpath='//*[@id="select_products_div"]') |>
  rvest::html_text() |> 
  strsplit(split = "\n") |> 
  unlist() |> 
  trimws(which = "both") |> 
  stringr::str_detect(pattern = "ATL03") |>
  sum()


if (atl03_exists == 0) stop("The ATL03 product does not exist in the openaltimetry.earthdatacloud.nasa.gov/data/icesat2/ website!")