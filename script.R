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
  mutate(capitulo = ifelse(str_detect(value, "^CAPITULO"),
                            value,
                            NA),
         artigo = str_extract(value, "^ART. \\d*"),
         paragrafo = str_extract(value, "^§ \\d*"),
         paragrafo = ifelse(is.na(paragrafo),
                            str_extract(value, "^PARAGRAFO UNICO"),
                            paragrafo),
         inciso = str_extract(value, ".*?-") %>% str_remove(" -") %>% as.roman()) %>%
  fill(capitulo, artigo, paragrafo, inciso, .direction = "down") %>%
  filter(!is.na(capitulo))