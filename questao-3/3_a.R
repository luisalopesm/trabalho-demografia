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
  
  filter(
    !is.na(faixa_etaria),
    faixa_etaria != "<1"
  ) %>%
  
  mutate(
    
    faixa_etaria = factor(
      faixa_etaria,
      levels = c(
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
    title = "Taxas Específicas de Mortalidade (nMx) - Homens",
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

ggsave(
  "nmx_homens.png",
  nmx_homens,
  width = 7,
  height = 3.5,
  dpi = 200
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
    title = "Taxas Específicas de Mortalidade (nMx) - Mulheres",
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

ggsave(
  "nmx_mulheres.png",
  nmx_mulheres,
  width = 7,
  height = 3.5,
  dpi = 200
)


# ============================================
# REMOVER NAs DO SEXO E FAIXA ETÁRIA
# ============================================

nmx_plot <- nmx %>%
  
  filter(
    !is.na(SEXO),
    !is.na(faixa_etaria)
  ) %>%
  
  droplevels()


# ============================================
# FUNÇÃO PARA GERAR OS GRÁFICOS
# ============================================

fazer_grafico <- function(ano_escolhido){
  
  ggplot(
    nmx_plot %>% filter(ano == ano_escolhido),
    
    aes(
      x = faixa_etaria,
      y = nMx,
      color = SEXO,
      group = SEXO
    )
  ) +
    
    geom_line(linewidth = 1) +
    
    scale_y_log10(
      labels = label_number(decimal.mark = ",")
    ) +
    
    labs(
      title = paste(
        "Taxas Específicas de Mortalidade (nMx) -",
        ano_escolhido
      ),
      
      x = "Faixa etária",
      y = "nMx",
      color = "Sexo"
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
  
}


# ============================================
# CRIAR GRÁFICOS
# ============================================

grafico_2010 <- fazer_grafico(2010)

grafico_2019 <- fazer_grafico(2019)

grafico_2021 <- fazer_grafico(2021)

grafico_2022 <- fazer_grafico(2022)

grafico_2024 <- fazer_grafico(2024)


# ============================================
# SALVAR IMAGENS
# ============================================

ggsave(
  "grafico_nmx_2010.png",
  grafico_2010,
  width = 7,
  height = 3.5,
  dpi = 200
)

ggsave(
  "grafico_nmx_2019.png",
  grafico_2019,
  width = 7,
  height = 4,
  dpi = 200
)

ggsave(
  "grafico_nmx_2021.png",
  grafico_2021,
  width = 7,
  height = 3.5,
  dpi = 200
)

ggsave(
  "grafico_nmx_2022.png",
  grafico_2022,
  width = 7,
  height = 3.5,
  dpi = 200
)

ggsave(
  "grafico_nmx_2024.png",
  grafico_2024,
  width = 7,
  height = 3.5,
  dpi = 200
)




# ============================================
# AJUSTAR FAIXAS ETÁRIAS
# ============================================

nmx_tabela <- nmx %>%
  
  filter(
    !is.na(SEXO),
    !is.na(faixa_etaria)
  ) %>%
  
  mutate(
    
    # Juntar <1 e 1-4 em 0-4
    faixa_etaria = case_when(
      
      faixa_etaria %in% c("<1", "1-4") ~ "0-4",
      
      TRUE ~ as.character(faixa_etaria)
    )
    
  ) %>%
  
  group_by(
    ano,
    SEXO,
    faixa_etaria
  ) %>%
  
  summarise(
    
    obitos = sum(obitos, na.rm = TRUE),
    
    populacao = sum(populacao, na.rm = TRUE),
    
    .groups = "drop"
    
  ) %>%
  
  mutate(
    
    nMx = (obitos / populacao) * 1000
    
  )


# ============================================
# ORDEM DAS FAIXAS
# ============================================

ordem_faixas <- c(
  "0-4",
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


# ============================================
# ORGANIZAR ORDEM
# ============================================

nmx_tabela <- nmx_tabela %>%
  
  filter(
    faixa_etaria %in% ordem_faixas
  ) %>%
  
  mutate(
    
    faixa_etaria = factor(
      faixa_etaria,
      levels = ordem_faixas
    )
    
  )


# ============================================
# ANOS DISPONÍVEIS
# ============================================

anos <- sort(unique(nmx_tabela$ano))


# ============================================
# GERAR E SALVAR TABELAS
# ============================================

for(a in anos){
  
  # ----------------------------------------
  # Criar tabela
  # ----------------------------------------
  
  tabela_temp <- nmx_tabela %>%
    
    filter(
      ano == a
    ) %>%
    
    select(
      faixa_etaria,
      SEXO,
      obitos,
      populacao,
      nMx
    ) %>%
    
    pivot_wider(
      names_from = SEXO,
      values_from = c(
        obitos,
        populacao,
        nMx
      )
    ) %>%
    
    rename(
      
      `Grupo etário` = faixa_etaria,
      
      `Óbitos Masculino` = obitos_Masculino,
      `Óbitos Feminino` = obitos_Feminino,
      
      `População Masculino` = populacao_Masculino,
      `População Feminino` = populacao_Feminino,
      
      `nMx Masculino` = nMx_Masculino,
      `nMx Feminino` = nMx_Feminino
      
    )
  
  
  # ----------------------------------------
  # Formatar tabela
  # ----------------------------------------
  
  tabela_gt <- tabela_temp %>%
    
    gt() %>%
    
    tab_header(
      title = paste(
        "Taxas Específicas de Mortalidade (nMx) - Paraná -",
        a
      )
    ) %>%
    
    # Fundo verde claro nos nomes das colunas
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
    
    # Centralizar texto
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    
    # Formatar números
    fmt_number(
      columns = contains("Óbitos"),
      decimals = 0,
      sep_mark = "."
    ) %>%
    
    fmt_number(
      columns = contains("População"),
      decimals = 0,
      sep_mark = "."
    ) %>%
    
    fmt_number(
      columns = starts_with("nMx"),
      decimals = 4,
      dec_mark = ","
    )
  
  
  # ----------------------------------------
  # Salvar PNG
  # ----------------------------------------
  
  gtsave(
    data = tabela_gt,
    
    filename = paste0(
      "tabela_nmx_",
      a,
      ".png"
    ),
    
    zoom = 0.55
  )
  
}

