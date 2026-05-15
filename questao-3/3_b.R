# ============================================
# b) MORTALIDADE INFANTIL E PERINATAL
# Curitiba - PR
# ============================================

library(tidyverse)
library(microdatasus)
library(lubridate)

# ============================================
# ÓBITOS FETAIS
# ============================================

lista_fetal <- lapply(c(2022, 2023, 2024), function(a){
  
  fetch_datasus(
    year_start = a,
    year_end = a,
    uf = "PR",
    information_system = "SIM-DOFET"
  )
  
})

sim_fetal <- bind_rows(lista_fetal)

sim_fetal <- process_sim(sim_fetal)

# FILTRAR CURITIBA
sim_fetal <- sim_fetal %>%
  
  filter(
    munResNome == "Curitiba"
  )

# média anual
media_obitos_fetais <- sim_fetal %>%
  
  mutate(
    ano = year(ymd(DTOBITO))
  ) %>%
  
  group_by(ano) %>%
  
  summarise(
    obitos_fetais = n(),
    .groups = "drop"
  ) %>%
  
  summarise(
    media = mean(obitos_fetais)
  ) %>%
  
  pull(media)

# ============================================
# 1. BAIXAR ÓBITOS SIM
# ============================================

anos <- c(2022, 2023, 2024)

lista_sim <- lapply(anos, function(a){
  
  fetch_datasus(
    year_start = a,
    year_end = a,
    uf = "PR",
    information_system = "SIM-DO"
  )
  
})

sim_pr <- bind_rows(lista_sim)

sim_pr <- process_sim(sim_pr)

# FILTRAR CURITIBA
sim_pr <- sim_pr %>%
  
  filter(
    munResNome == "Curitiba"
  )

# ============================================
# 2. PREPARAR VARIÁVEIS
# ============================================

sim_pr <- sim_pr %>%
  
  mutate(
    
    DTOBITO = ymd(DTOBITO),
    ano = year(DTOBITO),
    
    idade_num = as.numeric(substr(IDADE, 2, 4)),
    tipo_idade = substr(IDADE, 1, 1),
    
    # idade em dias
    idade_dias = case_when(
      
      # horas
      tipo_idade == "1" ~ 0,
      
      # dias
      tipo_idade == "2" ~ idade_num,
      
      # meses
      tipo_idade == "3" ~ idade_num * 30,
      
      # anos
      tipo_idade == "4" ~ idade_num * 365,
      
      TRUE ~ NA_real_
    )
    
  )

# ============================================
# 3. CLASSIFICAÇÃO DOS ÓBITOS INFANTIS
# ============================================

obitos_infantis <- sim_pr %>%
  
  filter(
    idade_dias < 365
  ) %>%
  
  mutate(
    
    componente = case_when(
      
      idade_dias <= 6 ~ "Neonatal precoce",
      
      idade_dias >= 7 & idade_dias <= 27 ~ "Neonatal tardia",
      
      idade_dias >= 28 & idade_dias < 365 ~ "Posneonatal",
      
      TRUE ~ NA_character_
    )
    
  )

# ============================================
# 4. NÚMERO MÉDIO DE ÓBITOS (2022-2024)
# ============================================

obitos_resumo <- obitos_infantis %>%
  
  group_by(ano, componente) %>%
  
  summarise(
    obitos = n(),
    .groups = "drop"
  )

media_obitos <- obitos_resumo %>%
  
  group_by(componente) %>%
  
  summarise(
    media_obitos = mean(obitos),
    .groups = "drop"
  )

# Extrair valores
obitos_neonatal_precoce <- media_obitos %>%
  filter(componente == "Neonatal precoce") %>%
  pull(media_obitos)

obitos_neonatal_tardia <- media_obitos %>%
  filter(componente == "Neonatal tardia") %>%
  pull(media_obitos)

obitos_posneonatal <- media_obitos %>%
  filter(componente == "Posneonatal") %>%
  pull(media_obitos)

obitos_neonatal <-
  obitos_neonatal_precoce +
  obitos_neonatal_tardia

obitos_infantis_total <-
  obitos_neonatal +
  obitos_posneonatal

# ============================================
# 5. NASCIDOS VIVOS - SINASC 2023
# ============================================

sinasc_2023 <- fetch_datasus(
  year_start = 2023,
  year_end = 2023,
  uf = "PR",
  information_system = "SINASC"
)

sinasc_2023 <- process_sinasc(sinasc_2023)

# FILTRAR CURITIBA
sinasc_2023 <- sinasc_2023 %>%
  
  filter(
    munResNome == "Curitiba"
  )

nascidos_vivos <- nrow(sinasc_2023)


# ============================================
# TMI POR SEXO
# ============================================

# ----------------------------
# ÓBITOS INFANTIS POR SEXO
# ----------------------------

obitos_sexo <- obitos_infantis %>%
  
  filter(!is.na(SEXO)) %>%
  
  group_by(ano, SEXO) %>%
  
  summarise(
    obitos_infantis = n(),
    .groups = "drop"
  )

# média de óbitos 2022-2024
media_obitos_sexo <- obitos_sexo %>%
  
  group_by(SEXO) %>%
  
  summarise(
    media_obitos = mean(obitos_infantis),
    .groups = "drop"
  )

# ----------------------------
# NASCIDOS VIVOS POR SEXO
# ----------------------------

nascidos_sexo <- sinasc_2023 %>%
  
  filter(!is.na(SEXO)) %>%
  
  group_by(SEXO) %>%
  
  summarise(
    nascidos_vivos = n(),
    .groups = "drop"
  )

# ----------------------------
# CÁLCULO DA TMI
# ----------------------------

tmi_sexo <- media_obitos_sexo %>%
  
  left_join(
    nascidos_sexo,
    by = "SEXO"
  ) %>%
  
  mutate(
    TMI = round(
      (media_obitos / nascidos_vivos) * 1000,
      2
    )
  )


# ============================================
# 6. TAXAS DE MORTALIDADE
# ============================================

tmi <- (
  obitos_infantis_total /
    nascidos_vivos
) * 1000

tm_neonatal <- (
  obitos_neonatal /
    nascidos_vivos
) * 1000

tm_neonatal_precoce <- (
  obitos_neonatal_precoce /
    nascidos_vivos
) * 1000

tm_neonatal_tardia <- (
  obitos_neonatal_tardia /
    nascidos_vivos
) * 1000

tm_posneonatal <- (
  obitos_posneonatal /
    nascidos_vivos
) * 1000

# ============================================
# 7. ÓBITOS FETAIS
# ============================================

obitos_fetais_ano <- sim_fetal %>%
  
  mutate(
    ano = year(ymd(DTOBITO))
  ) %>%
  
  group_by(ano) %>%
  
  summarise(
    obitos_fetais = n(),
    .groups = "drop"
  )

media_obitos_fetais <-
  mean(obitos_fetais_ano$obitos_fetais)

# ============================================
# 8. TAXA DE MORTALIDADE PERINATAL
# ============================================

# Óbitos fetais 2023
obitos_fetais_2023 <- sim_fetal %>%
  
  mutate(
    ano = year(ymd(DTOBITO))
  ) %>%
  
  filter(ano == 2023) %>%
  
  summarise(n = n()) %>%
  
  pull(n)

# Óbitos neonatais precoces 2023
obitos_neonatal_precoce_2023 <- obitos_infantis %>%
  
  filter(
    ano == 2023,
    componente == "Neonatal precoce"
  ) %>%
  
  summarise(n = n()) %>%
  
  pull(n)

# Taxa de mortalidade perinatal
tx_mort_perinatal <- (
  (obitos_fetais_2023 + obitos_neonatal_precoce_2023)
  /
    (nascidos_vivos + obitos_fetais_2023)
) * 1000

# ============================================
# 9. RESULTADOS
# ============================================

tmi
tm_neonatal
tm_neonatal_precoce
tm_neonatal_tardia
tm_posneonatal



# ============================================
# PACOTES
# ============================================

library(tidyverse)
library(gt)

# ============================================
# TABELA RESUMO DAS TAXAS
# ============================================

tabela_taxas <- tibble(
  
  Indicador = c(
    "Mortalidade infantil",
    "Mortalidade neonatal",
    "Mortalidade neonatal precoce",
    "Mortalidade neonatal tardia",
    "Mortalidade pós-neonatal",
    "Mortalidade perinatal"
  ),
  
  Numerador = c(
    round(obitos_infantis_total, 1),
    round(obitos_neonatal, 1),
    round(obitos_neonatal_precoce, 1),
    round(obitos_neonatal_tardia, 1),
    round(obitos_posneonatal, 1),
    
    # Perinatal
    round(
      obitos_fetais_2023 +
        obitos_neonatal_precoce_2023,
      1
    )
  ),
  
  Denominador = c(
    nascidos_vivos,
    nascidos_vivos,
    nascidos_vivos,
    nascidos_vivos,
    nascidos_vivos,
    
    # Perinatal
    nascidos_vivos +
      obitos_fetais_2023
  ),
  
  Taxa = c(
    tmi,
    tm_neonatal,
    tm_neonatal_precoce,
    tm_neonatal_tardia,
    tm_posneonatal,
    tx_mort_perinatal
  )
  
) %>%
  
  mutate(
    
    Taxa = round(Taxa, 2)
    
  )


# ============================================
# FORMATAR TABELA
# ============================================

tabela_gt <- tabela_taxas %>%
  
  gt() %>%
  
  tab_header(
    
    title = md(
      "**Taxas de Mortalidade Infantil e Perinatal**"
    ),
    
    subtitle =
      "Curitiba - PR | Numerador: média de óbitos (2022-2024) | Denominador: nascidos vivos de 2023"
    
  ) %>%
  
  cols_label(
    
    Indicador = "Indicador",
    Numerador = "Numerador",
    Denominador = "Denominador",
    Taxa = "Taxa por 1.000 NV"
    
  ) %>%
  
  fmt_number(
    
    columns = c(
      Numerador,
      Denominador
    ),
    
    decimals = 1,
    
    sep_mark = ".",
    
    dec_mark = ","
    
  ) %>%
  
  fmt_number(
    
    columns = Taxa,
    
    decimals = 2,
    
    dec_mark = ","
    
  ) %>%
  
  cols_align(
    
    align = "center",
    
    columns = everything()
    
  ) %>%
  
  tab_style(
    
    style = list(
      
      cell_fill(
        color = "#D9EAD3"
      ),
      
      cell_text(
        weight = "bold"
      )
      
    ),
    
    locations = cells_column_labels(
      everything()
    )
    
  ) %>%
  
  tab_options(
    
    table.font.size = px(11),
    
    data_row.padding = px(3)
    
  )


# ============================================
# VISUALIZAR
# ============================================

tabela_gt


# ============================================
# SALVAR PNG
# ============================================

gtsave(
  
  data = tabela_gt,
  
  filename = "tabela_taxas_mortalidade.png",
  
  zoom = 2.5
  
)

