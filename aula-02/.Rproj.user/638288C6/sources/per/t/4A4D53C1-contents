
df <- read.csv("dados_ldb.csv", 
                  header = TRUE, sep = ",")

df
View(df)

# Calculando PPP(pontos por posse) ofensivo e defensivo
df$ppp <- df$pts / df$posses
df$pspp <- df$pts.sofridos / df$posses

# Calculando a eficiência ofenciva, defensiva e o net rating (saldo)
df$efo <- df$ppp * 100
df$efd <- df$pspp * 100
df$net <- df$efo - df$efd

# Filtrar as equipes da série ouro da LDB 24
subset(df, serie == "ouro")

# O maior PPP da série prata da LDB 24
subset(subset(df, serie == "prata"), ppp == max(ppp))[c("equipe", "serie", "ppp", "net")]

# Eficiência ofensiva e defensiva média de cada série da LDB
ofensiva <- aggregate(efo ~ serie, FUN = mean, data = df)
ofensiva
defensiva <- aggregate(efd ~ serie, FUN = mean, data = df)
media.efd

eficiencia <- merge(ofensiva, defensiva, by = "serie")
eficiencia


# Gráfico

# Instalando ggplot
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2) 

# Plotando o gráfico

ggplot(eficiencia) +
  aes(serie, efo, fill = efo) +
  geom_col() + 
  geom_label(aes(label = round(efo, 1)), fill = 'white') +
  
  xlab("Série") + 
  ylab("Eficiência ofensiva") +
  ylim(0, 130) + 
  
  theme_bw() +
  theme(legend.position = "none")
