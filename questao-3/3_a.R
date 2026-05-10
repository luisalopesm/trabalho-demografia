# ============================================
# ANÁLISE DE MORTALIDADE - PARANÁ
# SIM + População projetada IBGE
# ============================================

# Pacotes
library(tidyverse)
library(microdatasus)
library(lubridate)
library(ggplot2)
library(scales)

# ============================================
# 1. BAIXAR DADOS DO SIM
# ============================================

anos <- c(2010, 2019, 2021, 2022, 2024)

lista_sim <- lapply(anos, function(a){
  
  fetch_datasus(
    year_start = a,
    year_end = a,
    uf = "PR",
    information_system = "SIM-DO"
  )
  
})

# Juntar
sim_pr <- bind_rows(lista_sim)

# Processar
sim_pr <- process_sim(sim_pr)


# ============================================
# 2. PREPARAR VARIÁVEIS
# ============================================

sim_pr <- sim_pr %>%
  
  mutate(
    
    DTOBITO = ymd(DTOBITO),
    ano = year(DTOBITO),
    
    # Sexo
    sexo = case_when(
      SEXO == "M" ~ "Masculino",
      SEXO == "F" ~ "Feminino",
      TRUE ~ NA_character_
    ),
    
    # Converter padrão DATASUS da idade
    idade_num = as.numeric(substr(IDADE, 2, 4)),
    
    tipo_idade = substr(IDADE, 1, 1),
    
    # Converter para anos
    idade_anos = case_when(
      
      # anos
      tipo_idade == "4" ~ idade_num,
      
      # meses -> anos
      tipo_idade == "3" ~ idade_num / 12,
      
      # dias
      tipo_idade == "2" ~ 0,
      
      # horas
      tipo_idade == "1" ~ 0,
      
      TRUE ~ NA_real_
    ),
    
    # Faixas etárias
    faixa_etaria = case_when(
      
      idade_anos < 1 ~ "<1",
      
      idade_anos >= 1  & idade_anos <= 4  ~ "1-4",
      
      idade_anos >= 5  & idade_anos <= 9  ~ "5-9",
      
      idade_anos >= 10 & idade_anos <= 14 ~ "10-14",
      
      idade_anos >= 15 & idade_anos <= 19 ~ "15-19",
      
      idade_anos >= 20 & idade_anos <= 24 ~ "20-24",
      
      idade_anos >= 25 & idade_anos <= 29 ~ "25-29",
      
      idade_anos >= 30 & idade_anos <= 34 ~ "30-34",
      
      idade_anos >= 35 & idade_anos <= 39 ~ "35-39",
      
      idade_anos >= 40 & idade_anos <= 44 ~ "40-44",
      
      idade_anos >= 45 & idade_anos <= 49 ~ "45-49",
      
      idade_anos >= 50 & idade_anos <= 54 ~ "50-54",
      
      idade_anos >= 55 & idade_anos <= 59 ~ "55-59",
      
      idade_anos >= 60 & idade_anos <= 64 ~ "60-64",
      
      idade_anos >= 65 & idade_anos <= 69 ~ "65-69",
      
      idade_anos >= 70 & idade_anos <= 74 ~ "70-74",
      
      idade_anos >= 75 & idade_anos <= 79 ~ "75-79",
      
      idade_anos >= 80 ~ "80+",
      
      TRUE ~ NA_character_
    )
    
  ) %>%
  
  filter(
    ano %in% c(2010, 2019, 2021, 2022, 2024)
  )


# ============================================
# 3. ÓBITOS TOTAIS
# ============================================

obitos_total <- sim_pr %>%
  
  group_by(ano) %>%
  
  summarise(
    obitos = n(),
    .groups = "drop"
  )

# ============================================
# 4. ÓBITOS POR SEXO E IDADE
# ============================================

obitos_especificos <- sim_pr %>%
  group_by(ano, SEXO, faixa_etaria) %>%
  summarise(
    obitos = n(),
    .groups = "drop"
  )

# ============================================
# 5. IMPORTAR POPULAÇÃO PROJETADA
# ============================================
# Arquivo deve conter:
# ano | sexo | faixa_etaria | populacao
# ============================================

library(readxl)

pop_pr <- read_xlsx("projecoes_2024_tab1_idade_simples.xlsx", skip = 5, col_names = TRUE)

pop_pr_tidy <- pop_pr %>%
  
  # Renomear variáveis
  rename(
    faixa_etaria = IDADE,
    sexo = SEXO
  ) %>%
  
  filter(SIGLA == "PR") %>% 
  
  # Transformar anos em linhas
  pivot_longer(
    cols = `2000`:`2070`,
    names_to = "ano",
    values_to = "populacao"
  ) %>%
  
  # Ajustar tipos
  mutate(
    
    ano = as.numeric(ano),
    
    populacao = as.numeric(populacao),
    
    sexo = case_when(
      sexo == "Homens" ~ "Masculino",
      sexo == "Mulheres" ~ "Feminino",
      TRUE ~ sexo
    )
    
  ) %>%
  
  # Selecionar apenas anos desejados
  filter(
    ano %in% c(2010, 2019, 2021, 2022, 2024)
  ) %>%
  
  # Remover totais se existirem
  filter(
    !faixa_etaria %in% c("Total", "Todas as idades")
  )


# ============================================
# 6. TAXA BRUTA DE MORTALIDADE
# ============================================


pop_total <- pop_pr_tidy %>%
  group_by(ano) %>%
  summarise(
    populacao = sum(populacao),
    .groups = "drop"
  )

tbm <- obitos_total %>%
  
  left_join(pop_total, by = "ano") %>%
  
  mutate(
    TBM = (obitos / populacao) * 1000
  )


# ============================================
# 7. TAXAS ESPECÍFICAS DE MORTALIDADE (nMx)
# ============================================

pop_pr_tidy <- pop_pr_tidy %>%
  
  mutate(
    
    idade = as.numeric(faixa_etaria),
      
      faixa_etaria = case_when(
        
        idade < 1 ~ "<1",
        
        idade >= 1  & idade <= 4  ~ "1-4",
        
        idade >= 5  & idade <= 9  ~ "5-9",
        
        idade >= 10 & idade <= 14 ~ "10-14",
        
        idade >= 15 & idade <= 19 ~ "15-19",
        
        idade >= 20 & idade <= 24 ~ "20-24",
        
        idade >= 25 & idade <= 29 ~ "25-29",
        
        idade >= 30 & idade <= 34 ~ "30-34",
        
        idade >= 35 & idade <= 39 ~ "35-39",
        
        idade >= 40 & idade <= 44 ~ "40-44",
        
        idade >= 45 & idade <= 49 ~ "45-49",
        
        idade >= 50 & idade <= 54 ~ "50-54",
        
        idade >= 55 & idade <= 59 ~ "55-59",
        
        idade >= 60 & idade <= 64 ~ "60-64",
        
        idade >= 65 & idade <= 69 ~ "65-69",
        
        idade >= 70 & idade <= 74 ~ "70-74",
        
        idade >= 75 & idade <= 79 ~ "75-79",
        
        idade >= 80 ~ "80+",
        
        TRUE ~ NA_character_
      )
    
  ) %>%
  
  group_by(
    ano,
    sexo,
    faixa_etaria
  ) %>%
  
  summarise(
    populacao = sum(populacao, na.rm = TRUE),
    .groups = "drop"
  )

nmx <- obitos_especificos %>%
  
  left_join(
    pop_pr_tidy,
    by = c(
      "ano",
      "SEXO" = "sexo",
      "faixa_etaria"
    )
  ) %>%
  
  mutate(
    nMx = (obitos / populacao) * 1000
  )


# ============================================
# 8. GRÁFICO nMx
# ============================================


# AJUSTAR ORDEM DAS FAIXAS ETÁRIAS

nmx <- nmx %>%
  
  mutate(
    
    faixa_etaria = factor(
      faixa_etaria,
      levels = c(
        "<1",
        "1-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80+"
      )
    )
    
  )



# GRÁFICO - HOMENS

nmx_homens <- ggplot(
  nmx %>% filter(SEXO == "Masculino"),
  
  aes(
    x = faixa_etaria,
    y = nMx,
    color = factor(ano),
    group = ano
  )
) +
  
  geom_line(size = 1) +
  
  scale_y_log10(
    labels = label_number(
      decimal.mark = ","
    )
  ) +
  
  labs(
    title = "Homens",
    x = "Faixa Etária",
    y = "nMx",
    color = "Ano"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )


# GRÁFICO - MULHERES

nmx_mulheres <- ggplot(
  nmx %>% filter(SEXO == "Feminino"),
  
  aes(
    x = faixa_etaria,
    y = nMx,
    color = factor(ano),
    group = ano
  )
) +
  
  geom_line(size = 1) +
  
  scale_y_log10(
    labels = label_number(
      decimal.mark = ","
    )
  ) +
  
  labs(
    title = "Mulheres",
    x = "Faixa Etária",
    y = "nMx",
    color = "Ano"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    )
  )


# ============================================
# 9. TABELA TBM
# ============================================

library(gt)

# ORGANIZAR TABELA

tbm_tabela <- tbm %>%
  
  mutate(
    
    obitos = format(
      obitos,
      big.mark = ".",
      decimal.mark = ","
    ),
    
    populacao = format(
      round(populacao),
      big.mark = ".",
      decimal.mark = ","
    ),
    
    TBM = format(
      round(TBM, 2),
      decimal.mark = ","
    )
    
  ) %>%
  
  pivot_longer(
    cols = c(obitos, populacao, TBM),
    names_to = "Indicador",
    values_to = "Valor"
  ) %>%
  
  pivot_wider(
    names_from = ano,
    values_from = Valor
  ) %>%
  
  mutate(
    Indicador = c("Óbitos", "População", "TBM")
  )

# TABELA

tabela_tbm <- tbm_tabela %>%
  
  gt(
    rowname_col = "Indicador"
  ) %>%
  
  # "Anos" no canto superior esquerdo
  tab_stubhead(
    label = "Anos"
  ) %>%
  
  cols_label(
    `2010` = "2010",
    `2019` = "2019",
    `2021` = "2021",
    `2022` = "2022",
    `2024` = "2024"
  ) %>%
  
  # Mesmo estilo para "Anos" e cabeçalho
  tab_style(
    style = list(
      cell_fill(color = "#dfe8d5"),
      cell_text(weight = "bold")
    ),
    
    locations = list(
      cells_stubhead(),
      cells_column_labels()
    )
  ) %>%
  
  # Mesmo estilo para nomes das linhas
  tab_style(
    style = list(
      cell_fill(color = "#dfe8d5"),
      cell_text(weight = "bold")
    ),
    
    locations = cells_stub()
  ) %>%
  
  # Linha TBM azul
  tab_style(
    style = list(
      cell_fill(color = "#d9e3f0"),
      cell_text(weight = "bold")
    ),
    
    locations = cells_body(
      rows = Indicador == "TBM"
    )
  ) %>%
  
  cols_align(
    align = "center",
    columns = everything()
  )
