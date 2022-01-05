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
  # Remove linhas vazias, numeros de pagina e o texto da imagem da camara dos deputados
  # ISSO PODE MUDAR CASO TENHA ALGO DIFERENTE, DO TIPO IMAGEM DO SENADO
  filter(value != "", is.na(numeros_pagina), value != "CAMARA DOS DEPUTADOS") %>%
  select(-numeros_pagina) %>%
  # PREMISSA: O maximo de granularidade em um projeto de lei eh ate alineas
  mutate(artigo = str_extract(value, "^ART. \\d*") %>% str_remove("ART. ") %>% as.numeric(),
         paragrafo = str_extract(value, "^§ \\d*") %>% str_remove("§ ") %>% as.numeric(),
         paragrafo = ifelse(is.na(paragrafo),
                            str_extract(value, "^PARAGRAFO UNICO"),
                            paragrafo),
         inciso = str_extract(value, ".*?-") %>% str_remove(" -") %>% as.roman() %>% as.numeric(),
         alinea = str_extract(value, "^\\w\\)") %>% str_remove("\\)")) %>%
  # Nao pode repetir artigos (talvez nao, por causa de alteracoes em outras leis)
  group_by(artigo) %>% 
  mutate(artigo = ifelse(numero_row == min(numero_row),
                         artigo,
                         NA)) %>%
  ungroup() %>%
  # Filtra o que estiver acima do art. 1
  mutate(temp = artigo) %>% fill(temp, .direction = "down") %>%
  filter(!is.na(temp)) %>% select(-temp) %>%
  arrange(numero_row) %>%
  fill(artigo, .direction = "down") %>%
  mutate(paragrafo = ifelse(artigo != lag(artigo),
                            0,
                            paragrafo)) %>%
  fill(paragrafo, .direction = "down") %>%
  mutate(paragrafo = replace_na(paragrafo, 0)) %>%
  mutate(inciso = ifelse(paragrafo != lag(paragrafo) | artigo != lag(artigo),
                         0,
                         inciso)) %>%
  fill(inciso, .direction = "down") %>%
  mutate(inciso = replace_na(inciso, 0)) %>%
  mutate(alinea = ifelse(paragrafo != lag(paragrafo) | artigo != lag(artigo) | inciso != lag(inciso),
                         0,
                         alinea)) %>%
  arrange(numero_row) %>%
  fill(alinea, .direction = "downup") %>%
  group_by(artigo, paragrafo, inciso, alinea) %>%
  summarise(value = paste(value, collapse = " "),
            artigo = last(artigo),
            paragrafo = last(paragrafo),
            inciso = last(inciso),
            alinea = last(alinea)) %>%
  ungroup()