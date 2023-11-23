library(tidyverse)
library(doc2vec)

df <- read_csv("tea_dataset.csv")

df |>
  mutate(doc_id = 1:n()) |>
  rename(text = description) |>
  select(doc_id, text) |> 
  paragraph2vec(threads = 15) ->
  model

library(uwot)
as.matrix(model, which = "docs") |>
  umap(metric = "cosine", init = "pca") |> 
  as_tibble() |> 
  bind_cols(df) ->
  df_merged

df_merged |> 
  ggplot(aes(V1, V2, color = type))+
  geom_point()


library(ggiraph)
df_merged |> 
  ggplot(aes(V1, V2, color = type, tooltip = description))+
  geom_point_interactive() ->
  p1

girafe(ggobj = p1)
