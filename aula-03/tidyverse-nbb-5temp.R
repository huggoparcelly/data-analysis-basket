
library(tidyverse)

dados_nbb_5temporadas <- read.csv("dados_nbb_5temporadas.csv", header = TRUE,
                                  sep = ";")

nbb2324 <- dados_nbb_5temporadas %>% filter(Temporada == "2023/24") %>%
  mutate(Data = as.Date.character(Data, format = "%d/%m/%Y")) %>%
  filter(Data <= "2024-04-13")

nbb2324 <- nbb2324 %>%
  mutate(Posses = ((A2T + A3T)-RBO)+ERR+(0.44*LLT))

ef.2324 <- nbb2324 %>% group_by(Equipe) %>%
  summarise(PTS = sum(PTS),
            PTSo = sum(PTSo),
            Posses = sum(Posses),
            EFO = (PTS/Posses)*100,
            EFD = (PTSo/Posses)*100,
            Net = EFO - EFD) %>%
  ungroup() %>%
  mutate(Net.Rank = rank(desc(Net)))


# Gráfico

ggplot(ef.2324) +
  aes(EFO, EFD) +
  
  geom_hline(yintercept = mean(ef.2324$EFD), linetype = "dashed") +
  geom_vline(xintercept = mean(ef.2324$EFO), linetype = "dashed") +
  geom_text(aes(label = paste("#", Net.Rank)), size=3, hjust=-1) +
  geom_label(aes(label = Equipe), size = 3) +
  
  xlab("Eficiência ofensiva") +
  ylab("Eficiência defensiva") +
  
  xlim(92,123) +
  scale_y_reverse(limits = c(123, 92)) +
  
  theme_bw() +
  theme(legend.position = "none")


# Acompanhar o PPP de uma equipe durante uma temporada

nbb2324 <- dados_nbb_5temporadas %>% filter(Temporada == "2023/24") %>%
  mutate(Data = as.Date.character(Data, format = "%d/%m/%Y")) %>%
  filter(Data <= "2024-04-13")

nbb2324 %>%
  mutate(Posses = ((A2T + A3T)-RBO)+ERR+(0.44*LLT)) %>%
  mutate(PPP = PTS/Posses) %>%
  group_by(Data, Equipe) %>%
  summarise(PPP = mean(PPP),
            Desfecho = Desfecho) %>%
  arrange(Data) %>%
  ungroup() %>%
  group_by(Equipe) %>%
  mutate(Rodada = 1:n()) %>%
  ungroup() %>%
  filter(Equipe == "UFC") %>% 
  
  ggplot() +
  aes(Rodada, PPP) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_line() +
  geom_point(aes(color = Desfecho), size=3) +
  
  scale_color_manual(values = c("red", "green")) +
  
  scale_y_continuous(limits = c(0.6, 1.5), n.breaks = 8) +
  scale_x_continuous(limits = c(1, 36), n.breaks = 18, expand = c(0,0)) +
  
  theme_light()


nbb2324 %>%
  mutate(Posses = ((A2T + A3T)-RBO)+ERR+(0.44*LLT)) %>%
  mutate(PPP = PTS/Posses) %>%
  group_by(Data, Equipe) %>%
  summarise(PPP = mean(PPP),
            RBO = mean(RBO),
            Desfecho = Desfecho) %>%
  arrange(Data) %>%
  ungroup() %>%
  group_by(Equipe) %>%
  mutate(Rodada = 1:n()) %>%
  ungroup() %>%
  group_by(Desfecho) %>%
  summarise(RBO = mean(RBO)) %>%
  ungroup() %>%
  view()
