#----------------------------------------#
#  Criando a lista do Oscar Awards 2023  #
#----------------------------------------#

# WebScraping dos filmes indicados para o Oscar 2023
library(httr)
library(xml2)
library(tidyverse)
library(purrr)

# Site oficial do Oscar, com a lista de indicados (Nome dos filmes em inglês)
link <-  "https://www.oscars.org/oscars/ceremonies/2023"

# Lendo a página html
pagina <- read_html(link)

# As categorias estão divididas dentro desta classe
# Note que cada elemento da lista é uma categoria. Note também que a primeira 
# string de cada elemento é o nome da categoria
indicados <- xml_find_all(pagina, ".//div[@class='view-grouping']") 

# O site põe os vencedores na primeira posição
cria_base <- function(xml_list){
  
  base <- tibble()
  
  for (i in xml_list){
    # Categorias
    categoria <- i %>%  
      xml_find_all(".//h2") %>%
      xml_text() 
    # Condição de parada (caso tenhamos um elemento na lista que esteja vazia)
    if(is_empty(categoria)){
      break
    }
    
    # Indicad@s
    indicado <- i %>%  
      xml_find_all(".//h4") %>%
      xml_text()
    
    # Complementos
    complemento <- i %>%  
      xml_find_all(".//span[@class='field-content']") %>%
      xml_text() %>% 
      str_remove_all('\n')   
    complemento <- complemento[complemento != ""]
    
    # Ganhador@s 
    ganhador <- c(TRUE, rep(FALSE, each = length(indicado) - 1))
    
    base <- bind_rows(
      base,
      tibble(categoria, indicado, complemento, ganhador)
    )
    
  }
  
  return(base)
  
}

base_oscar = cria_base(indicados)
  

# Indicado é o filme?
indicado_filme <- function(x){ 
  case_when(
    x == 'Actor in a Leading Role' ~ 'Nao',
    x == 'Actor in a Supporting Role' ~ 'Nao',
    x == 'Actress in a Leading Role' ~ 'Nao',
    x == 'Actress in a Supporting Role' ~ 'Nao',
    x == 'Directing' ~ 'Nao',
    .default = 'Sim'
  )
}

base_oscar <- base_oscar %>% 
  mutate(filme = if_else(indicado_filme(categoria) == 'Sim', indicado, complemento),
         apendice = if_else(indicado_filme(categoria) == 'Sim', complemento, indicado) ) %>% 
  select(categoria, indicado, filme, apendice, ganhador)

#---------------------------------#
# Adicionando a sinopse do filme  #
#---------------------------------#

#busca_imdb <- function(filme){
  
filme <- 'The Whale'
filme <- str_replace_all(filme, ' ', '%20')
busca <- paste0("https://www.imdb.com/find/?q=", filme, "&ref_=nv_sr_sm")
resultados <- read_html(busca)
link_filme <- paste0("https://www.imdb.com", xml_find_first(resultados, ".//a[@class='ipc-metadata-list-summary-item__t']") %>% xml_attr('href'))
pagina_filme <- read_html(link_filme)

a <- xml_find_all(pagina_filme, ".//section[@class = 'ipc-page-section ipc-page-section--base celwidget']")
for (i in 1:12){
  a[i] %>% xml_attr('data-testid') %>% print()  
}

