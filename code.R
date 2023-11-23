library(tidyverse)
library(rvest)

# collect links -----------------------------------------------------------
c("gaba-tea", "puer/view-all", "ulun/view-all", "green_tea/view-all",
  "krasnyj_chaj/view-all", "zheltyj_chaj", "belyj_chaj", "chernyj_chaj") |> 
  map(function(i){
    read_html(str_c("https://moychay.ru/catalog/", i))  |> 
      html_nodes("a.o_p_name") |> 
      html_attr("href") |> 
      tibble(type = i,
             link = _)
  }) |> 
  list_rbind() |> 
  mutate(type = str_remove(type, "/view-all"),
         link = str_c("https://moychay.ru", link))->
  all_links

# extract data from webpages ----------------------------------------------
map(seq_along(all_links$link), .progress = TRUE, function(i){
  source <- read_html(all_links$link[i])
  
  # flavors
  
  source |> 
    html_node("div.flavors") ->
    flavors
  
  flavors |> 
    html_nodes("div.flavor-name") |> 
    html_text() |> 
    str_squish() ->
    flavor_names
  
  flavors |> 
    html_nodes("div.flavor-ratings-sprite > span") |> 
    html_attr("style") |> 
    str_remove("width:") |> 
    str_remove("%") ->
    flavor_ratings
  
  flavor <- str_c(flavor_names, ": ", flavor_ratings, collapse = "; ")
  
  # rating
  
  source |> 
    html_nodes("div.star-ratings-sprite > span") |> 
    html_attr("style") |> 
    str_remove("width:") |> 
    str_remove("%") |> 
    str_squish() |> 
    unique() ->
    rating 
  
  # price 
  
  source |> 
    html_node("span#variant_price") |> 
    html_text() |> 
    str_squish() |> 
    as.double() ->
    price
  
  # preparation
  
  source |> 
    html_node("div.preparation-container") |> 
    html_text() |> 
    str_squish() ->
    preparation
  
  # description
  
  source |> 
    html_node("article.item-description div.description") |> 
    html_text() |> 
    str_squish() ->
    description
  
  # title
  
  source |> 
    html_node("h1") |> 
    html_text()  |> 
    str_squish() ->
    title
  
  tibble(title,
         type = all_links$type[i],
         flavor,
         rating,
         price,
         preparation,
         description,
         url = all_links$link[i])
}) |> 
  list_rbind()  |> 
  mutate_all(function(x){ifelse(x == "", NA, x)}) |> 
  na.omit() |> 
  mutate(type = case_when(type == "belyj_chaj" ~ "white",
                          type == "chernyj_chaj" ~ "black",
                          type == "gaba-tea" ~ "gaba",
                          type == "green_tea" ~ "green",
                          type == "krasnyj_chaj" ~ "red",
                          type == "zheltyj_chaj" ~ "yellow",
                          TRUE ~ type),
         preparation_time = str_extract(preparation, "\\d{1,} с"),
         preparation_time = str_remove(preparation_time, " с"),
         preparation_time = as.double(preparation_time),
         preparation_temperature = str_extract(preparation, "\\d{1,}°C"),
         preparation_temperature = str_remove(preparation_temperature, "°C"),
         preparation_temperature = as.double(preparation_temperature),
         preparation_amount = str_extract(preparation, "\\d{1,} г"),
         preparation_amount = str_remove(preparation_amount, " г"),
         preparation_amount = as.double(preparation_amount),
         flavor = str_split(flavor, "; ")) |> 
  select(-preparation) |> 
  unnest_longer(flavor) |> 
  separate(flavor, into = c("flavor_type", "percentage"), sep = ": ") |> 
  pivot_wider(names_from = flavor_type, values_from = percentage) |> 
  write_csv("tea_dataset.csv", na = "")
