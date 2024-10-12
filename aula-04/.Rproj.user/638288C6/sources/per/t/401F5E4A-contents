library(tidyverse)
library(gt)
library(gtExtras)
library(patchwork)

 
dados_nbb_5temporadas <- read.csv("dados_nbb_5temporadas.csv", header = TRUE, sep = ";")
# Calcular o aproveitamento efeitivo (eFG%) e criar uma tabela ranqueando as equipes de uma temporada do NBB

temp <- "2018/19"

# Criando o eFG%

tabela.efg <- dados_nbb_5temporadas %>%
  filter(Temporada == temp) %>%
  mutate(Data = as.Date.character(Data, tryFormats = "%d/%m/%Y")) %>%
  mutate(Desfecho =ifelse(Desfecho == "V", 1, 0),
         eFG = ((A2C + (1.5 * A3C)) / (A2T+A3T)) * 100 ) %>%
        group_by(Equipe) %>%
        summarise(Vitorias = sum(Desfecho),
                eFG = mean(eFG),
                A3taxa = (sum(A3T)/(sum(A3T) + sum(A2T)))*100,
                A3T = mean(A3T),
                A2T = mean(A2T)) %>%
  ungroup() %>%
  arrange(desc(eFG)) %>%
  select(-Vitorias) %>%
  
  #Tabela
  gt() %>%
  gt_theme_538 %>%
  cols_label(eFG = "eFG%", A3taxa = "3P-R") %>%
  fmt_number(decimals = 1) %>%
  data_color(columns = c(eFG), palette = "Oranges", domain = c(49,60), na_color="white") %>%
  data_color(columns = c(A3taxa, A2T, A3T), palette = "Greys", alpha=0.5) %>%
  tab_options(data_row.padding = px(2), table.width = "250px")


grafico.efg <- dados_nbb_5temporadas %>%
  filter(Temporada == temp) %>%
  mutate(Data = as.Date.character(Data, tryFormats = "%d/%m/%Y")) %>%
  mutate(Desfecho =ifelse(Desfecho == "V", 1, 0),
         eFG = ((A2C + (1.5 * A3C)) / (A2T+A3T)) * 100 ) %>%
  group_by(Equipe) %>%
  summarise(Vitorias = sum(Desfecho),
            eFG = mean(eFG),
            A3taxa = (sum(A3T)/(sum(A3T) + sum(A2T)))*100,
            A3T = mean(A3T),
            A2T = mean(A2T)) %>%
  ungroup() %>%
  
  # Gráfico
  
  ggplot() +
  aes(Vitorias, eFG) + 
  geom_point(size = 5, alpha = .3) +
  geom_smooth(method = "lm", color = "Orange") +
  ggtitle("Relação eFG% x Vitorias", subtitle = "Temporadas do NBB")
  ylab("eFG%") +
  xlab("Número de vitórias") +
  scale_x_continuous(expand = c(0,0), n.breaks = 10) +
  theme_bw()

grafico.efg + tabela.efg 

# ggsave("grafico-tabela.png", h=5, w=7, dpi=200)

# Traçar a relação entre uma variável de jogo e o nível de sucesso das equipes do NBB