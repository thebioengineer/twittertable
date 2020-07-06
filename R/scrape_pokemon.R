## libraries
library(tidyverse)
library(rvest)
library(colortable)
library(jsonlite)


## gotta list'em all


poke_list_html <- read_html("https://www.ign.com/wikis/pokemon-go/List_of_Pokemon_(Pokedex)")
pokemon_cards <- poke_list_html %>% 
  html_nodes("table") %>%  ## go to table element
  `[[`(1) %>% 
  html_nodes("td") %>% 
  lapply(function(x){
    
    href <- x %>% html_nodes("a") %>% html_attr("href")
    pokemon_num_name <- html_text(x)
    tibble(
      pokemon = pokemon_num_name, 
      href = href
    )
  }) %>% 
  bind_rows() %>% 
  mutate(
    pokemon = gsub("(\\d)\\s","\\1___", trimws(pokemon)),
  ) %>% 
  separate(
    pokemon,
    c("pokemon_number","pokemon"),
    sep = "___"
  ) %>% 
  filter(
    grepl("#\\d\\d\\d",pokemon_number) 
  ) %>% 
  split(.$pokemon_number)


pokemon_images <- poke_list_html %>% 
  html_nodes("table") %>%  ## go to table element
  html_nodes("td") %>% 
  as.character() %>% 
  lapply(function(x){
    
    x <- read_html(x)
    
    href <- x %>% html_nodes("a") %>% html_attr("href")
    image <- x %>% html_nodes("img") %>% html_attr("src")
    
    if(length(href) == 0){
      href <- NA_character_
    }
    
    if(length(image) == 0){
      image <- NA_character_
    }
    
    data.frame(
      href = href,
      image = image,
      stringsAsFactors = FALSE
    )
  }) %>% 
  do.call('rbind',.) %>% 
  mutate(
    href = case_when(
      grepl("33[.]jpg",image) ~ "/wikis/pokemon-go/Nidorino",
      grepl("34[.]jpg",image) ~ "/wikis/pokemon-go/Nidoking",
      grepl("71[.]jpg",image) ~ "/wikis/pokemon-go/Victreebel",
      grepl("92[.]jpg",image) ~ "/wikis/pokemon-go/Gastly",
      grepl("151[.]jpg",image) ~ "/wikis/pokemon-go/Mew",
      TRUE ~ href
    )
  ) %>% 
  filter(!is.na(image))
  


## gotta catch'em all

pokedex <- function(pokemon){
  
  url <- paste0("https://www.ign.com",pokemon$href)
  
  poke_ref <- read_html(url) %>% html_nodes("table")
  
  info <-
    poke_ref[[1]] %>% html_nodes("td") %>% html_text %>% `[`(-1) %>% matrix(nrow = 2, byrow = TRUE)
  
  type <- gsub("\\s*","",info[2,1])
  evolve_level <- NA
  if(ncol(info) == 2){
    evolve_level <- as.numeric(str_extract(info[2,2],"\\d+"))
  }
  
  data.frame(
    pokemon,
    type = type,
    evolve_level = evolve_level
  )
}

poke_details <- pokemon_cards %>% 
  map( ~ pokedex(.x))

c_vec <- c(Grass = "green",
           Fire  = "red",
           Water = "mediumblue",
           Bug   = "lightgreen",
           Normal = "gray",
           Poison = "purple",
           Electric = "yellow",
           Ground = "brown",
           Fairy = "amethyst",
           Fighting = "americanrose",
           Psychic = "mediumpurple4",
           Rock = "saddlebrown",
           Ghost = "lavender",
           Ice = "lightblue",
           Dragon = "indianred",
           Flying = "airforceblue",
           Steel = "grey30")

poke_table <- poke_details %>% 
  bind_rows() %>% 
  left_join(
    pokemon_images, by = "href"
  ) %>% 
  separate(type,
           c("Type 1", "Type 2"),
           sep = "/", fill = "right"
  ) %>% 
  mutate(
  `Type 1` = color_vctr(
    `Type 1`,
    text_color = c_vec[`Type 1`]
  ),
  `Type 2` = color_vctr(
    `Type 2`,
    text_color = c_vec[`Type 2`]
  )) %>% 
  select( -href, -evolve_level)


poke_output <- poke_table %>%
  filter(!is.na(image)) %>% 
  mutate(
    Photo = paste0("<img src='",gsub("&quality=20&dpr=0.05","",image),"' height= '50px'/>")
  ) %>%
  mutate(
    Type_1_html = format(`Type 1`, method = "html"),
    Type_2_html = format(`Type 2`, method = "html"),
    Type = ifelse(
      is.na(`Type 2`),
      Type_1_html,
      paste0(Type_1_html,"/",Type_2_html))
  ) %>%
  select(
    Number = pokemon_number,
    Pokemon = pokemon,
    Photo,
    Type
  )

list(
  data = poke_output,
  options = list(
    html = list(
      Number  = FALSE,
      Pokemon = FALSE,
      Photo   = TRUE,
      Type    = TRUE
      ),
    rows = list(
      min = 5,
      max = 20
    )
  )) %>%
  write_json(path = "pokemon.json",auto_unbox = TRUE)
  

