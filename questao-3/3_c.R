# =========================================================
# Mortalidade por causas em Curitiba (2010, 2021 e 2024)
# 20 grupos de causas mais frequentes - CID-10
# Estratificado por sexo e grupos etários
# Destaque para Covid-19 (B34.2)
# =========================================================

# PACOTES -------------------------------------------------
library(microdatasus)
library(tidyverse)
library(lubridate)
library(stringr)
library(forcats)

# =========================================================
# 1. BAIXANDO DADOS DO SIM
# =========================================================

anos <- c(2010, 2021, 2024)

sim_lista <- list()

for(a in anos){
  
  cat("Baixando ano:", a, "\n")
  
  dados <- fetch_datasus(
    year_start = a,
    year_end   = a,
    uf         = "PR",
    information_system = "SIM-DO"
  )
  
  dados <- process_sim(dados)
  
  sim_lista[[as.character(a)]] <- dados
}

sim <- bind_rows(sim_lista)

# =========================================================
# 2. FILTRANDO CURITIBA
# =========================================================

sim_curitiba <- sim %>%
  filter(CODMUNRES == "410690")

# =========================================================
# 3. CRIANDO VARIÁVEIS
# =========================================================

# Sexo
sim_curitiba <- sim_curitiba %>%
  mutate(
    sexo = case_when(
      SEXO %in% c("1", 1, "Masculino") ~ "Masculino",
      SEXO %in% c("2", 2, "Feminino") ~ "Feminino",
      TRUE ~ NA_character_
    )
  )

# =========================================================
# IDADE E FAIXAS ETÁRIAS
# =========================================================

sim_curitiba <- sim_curitiba %>%
  mutate(
    
    idade_anos = case_when(
      
      # menores de 1 ano
      substr(IDADE, 1, 1) %in% c("0", "1", "2", "3") ~ 0,
      
      # idade em anos
      substr(IDADE, 1, 1) == "4" ~
        as.numeric(substr(IDADE, 2, 3)),
      
      # 100 anos ou mais
      substr(IDADE, 1, 1) == "5" ~ 100,
      
      TRUE ~ NA_real_
    ),
    
    faixa_etaria = case_when(
      idade_anos < 5 ~ "<5",
      idade_anos >= 5  & idade_anos <= 14 ~ "5-14",
      idade_anos >= 15 & idade_anos <= 39 ~ "15-39",
      idade_anos >= 40 & idade_anos <= 59 ~ "40-59",
      idade_anos >= 60 ~ "60+",
      TRUE ~ NA_character_
    ),
    
    faixa_etaria = factor(
      faixa_etaria,
      levels = c("<5", "5-14", "15-39", "40-59", "60+")
    )
  )

# =========================================================
# 5. CAPÍTULOS CID-10
# =========================================================

sim_curitiba <- sim_curitiba %>%
  mutate(
    cid = CAUSABAS,
    
    grupo_causa = case_when(
      
      str_detect(cid, "^[AB]") ~
        "Doenças infecciosas e parasitárias",
      
      str_detect(cid, "^C|^D0|^D1|^D2|^D3|^D4") ~
        "Neoplasias",
      
      str_detect(cid, "^D5|^D6|^D7|^D8") ~
        "Doenças do sangue",
      
      str_detect(cid, "^E") ~
        "Doenças endócrinas/metabólicas",
      
      str_detect(cid, "^F") ~
        "Transtornos mentais",
      
      str_detect(cid, "^G") ~
        "Sistema nervoso",
      
      str_detect(cid, "^H") ~
        "Olho e ouvido",
      
      str_detect(cid, "^I") ~
        "Doenças do aparelho circulatório",
      
      str_detect(cid, "^J") ~
        "Doenças do aparelho respiratório",
      
      str_detect(cid, "^K") ~
        "Doenças do aparelho digestivo",
      
      str_detect(cid, "^L") ~
        "Doenças da pele",
      
      str_detect(cid, "^M") ~
        "Doenças osteomusculares",
      
      str_detect(cid, "^N") ~
        "Doenças geniturinárias",
      
      str_detect(cid, "^O") ~
        "Gravidez/parto/puerpério",
      
      str_detect(cid, "^P") ~
        "Afecções perinatais",
      
      str_detect(cid, "^Q") ~
        "Malformações congênitas",
      
      str_detect(cid, "^R") ~
        "Causas mal definidas",
      
      str_detect(cid, "^S|^T") ~
        "Lesões/envenenamentos",
      
      str_detect(cid, "^V|^W|^X|^Y") ~
        "Causas externas",
      
      str_detect(cid, "^Z") ~
        "Fatores de contato serviços saúde",
      
      TRUE ~ "Outras"
    )
  )

# =========================================================
# 6. DESTACANDO COVID-19
# =========================================================

sim_curitiba <- sim_curitiba %>%
  mutate(
    grupo_causa = case_when(
      cid == "B342" ~ "Covid-19 (B34.2)",
      TRUE ~ grupo_causa
    )
  )

# =========================================================
# 7. 20 CAUSAS MAIS FREQUENTES
# =========================================================

top20 <- sim_curitiba %>%
  count(grupo_causa, sort = TRUE) %>%
  slice_head(n = 20) %>%
  pull(grupo_causa)

dados_plot <- sim_curitiba %>%
  filter(grupo_causa %in% top20)

# =========================================================
# 8. TABELA RESUMO
# =========================================================

sim_curitiba <- sim_curitiba %>%
  mutate(
    ano = year(DTOBITO)
  )

# recriar depois de criar "ano"
dados_plot <- sim_curitiba %>%
  filter(grupo_causa %in% top20)

tabela_causas <- dados_plot %>%
  count(ano, sexo, faixa_etaria, grupo_causa) %>%
  rename(
    obitos = n
  )

# =========================================================
# COMPARAÇÃO TEMPORAL
# Mortalidade por causas: 2010 x 2021 x 2024
# =========================================================

grafico_temporal <- tabela_causas %>%
  
  group_by(ano, grupo_causa) %>%
  summarise(
    obitos = sum(obitos),
    .groups = "drop"
  ) %>%
  
  mutate(
    grupo_causa = reorder(grupo_causa, obitos)
  ) %>%
  
  ggplot(aes(
    x = grupo_causa,
    y = obitos,
    fill = factor(ano)
  )) +
  
  geom_col(position = "dodge") +
  
  coord_flip() +
  
  labs(
    title = "Comparação temporal da mortalidade por causas - Curitiba",
    subtitle = "2010, 2021 e 2024",
    x = "",
    y = "Número de óbitos",
    fill = "Ano"
  ) +
  
  theme_minimal(base_size = 12)

ggsave(
  filename = "grafico_comparacao_temporal.png",
  plot = grafico_temporal,
  width = 13,
  height = 8,
  dpi = 300
)

# =========================================================
# GRÁFICOS POR SEXO - BARRAS AGRUPADAS
# =========================================================

for(a in anos){
  
  grafico_sexo <- tabela_causas %>%
    
    filter(
      ano == a,
      !is.na(sexo)
    ) %>%
    
    group_by(sexo, grupo_causa) %>%
    summarise(
      obitos = sum(obitos),
      .groups = "drop"
    ) %>%
    
    mutate(
      grupo_causa = reorder(grupo_causa, obitos)
    ) %>%
    
    ggplot(aes(
      x = grupo_causa,
      y = obitos,
      fill = sexo
    )) +
    
    geom_col(position = "dodge") +
    
    coord_flip() +
    
    labs(
      title = paste("Mortalidade por causas segundo sexo - Curitiba", a),
      x = "",
      y = "Número de óbitos",
      fill = "Sexo"
    ) +
    
    theme_minimal(base_size = 12)
  
  print(grafico_sexo)
  
  ggsave(
    filename = paste0("grafico_sexo_", a, ".png"),
    plot = grafico_sexo,
    width = 12,
    height = 8,
    dpi = 300
  )
}

# =========================================================
# 11. GRÁFICOS POR FAIXA ETÁRIA
# =========================================================

library(tidytext)

# Loop para gerar um painel vertical legível por ano, mantendo as 20 causas
for(a in anos){
  
  grafico_idade_completo <- tabela_causas %>%
    filter(
      ano == a,
      !is.na(faixa_etaria)
    ) %>%
    group_by(faixa_etaria, grupo_causa) %>%
    summarise(obitos = sum(obitos), .groups = "drop") %>%
    group_by(faixa_etaria) %>%
    # Ordena as 20 causas dentro de cada faixa etária
    mutate(
      grupo_causa_ord = reorder_within(grupo_causa, obitos, within = faixa_etaria)
    ) %>%
    ungroup() %>%
    
    ggplot(aes(x = grupo_causa_ord,
               y = obitos,
               fill = grupo_causa == "Covid-19 (B34.2)")) +
    
    geom_col(show.legend = FALSE) +
    
    coord_flip() +
    
    # Organiza as faixas etárias em colunas para dar o máximo de altura vertical para o texto do eixo Y
    facet_wrap(~faixa_etaria, scales = "free", ncol = 3) +
    
    scale_x_reordered() +
    
    labs(
      title = paste("Mortalidade por causas por idade - Curitiba", a),
      x = "",
      y = "Número de óbitos"
    ) +
    
    scale_fill_manual(
      values = c("gray70", "red")
    ) +
    
    theme_minimal(base_size = 10) +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      # Aumenta o espaçamento entre as facetas para o texto não invadir o gráfico vizinho
      panel.spacing.x = unit(2, "lines"), 
      panel.spacing.y = unit(1.5, "lines"),
      axis.text.y = element_text(size = 8) # Diminui levemente a fonte para caber as 20 causas
    )
  
  print(grafico_idade_completo)
  
  # Aumentamos a altura (height) para 16 polegadas para esticar o eixo Y e dar espaço para as 20 linhas de texto
  ggsave(
    filename = paste0("grafico_idade_", a, ".png"),
    plot = grafico_idade_completo,
    width = 18,
    height = 16,
    dpi = 300
  )
}
