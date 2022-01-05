library(tidyverse)

arq_antigo <- "PL-1917-2015.pdf" 

antigo <- arq_antigo %>%
  pdftools::pdf_text() %>%
  read_lines() %>%
  enframe(name = NULL) %>%
  mutate(numero_row = row_number()) %>%
  slice(2:n()) %>%
  mutate(value = str_trim(value, side = "both") %>% stringi::stri_trans_general("Latin-ASCII") %>% str_to_upper()) %>%
  mutate(numeros_pagina = as.numeric(value)) %>%
  # Remove linhas vazias e numeros de pagina
  filter(value != "", is.na(numeros_pagina)) %>%
  select(-numeros_pagina) %>%
  mutate(artigo = str_extract(value, "^ART. \\d*") %>% str_remove("ART. ") %>% as.numeric(),
         paragrafo = str_extract(value, "^§ \\d*") %>% str_remove("§ ") %>% as.numeric(),
         paragrafo = ifelse(is.na(paragrafo),
                            str_extract(value, "^PARAGRAFO UNICO"),
                            paragrafo),
         inciso = str_extract(value, ".*?-") %>% str_remove(" -") %>% as.roman() %>% as.numeric()) %>%
  # Nao pode repetir artigos (talvez nao, por causa de alteracoes em outras leis)
  group_by(artigo) %>% mutate(artigo = ifelse(numero_row == min(numero_row),
                                               artigo,
                                               NA)) %>%
  ungroup() %>%
  # Filtra o que estiver acima do art. 1
  mutate(temp = artigo) %>% fill(temp, .direction = "down") %>%
  filter(!is.na(temp)) %>% select(-temp) %>%
  fill(artigo, paragrafo, inciso, .direction = "down") %>%
  mutate(paragrafo = replace_na(paragrafo, 0),
         inciso = replace_na(inciso, 0)) %>%
  arrange(numero_row) %>%
  group_by(artigo, paragrafo, inciso) %>%
  summarise(value = paste(value, collapse = " "),
            artigo = last(artigo),
            paragrafo = last(paragrafo),
            inciso = last(inciso)) %>%
  ungroup()