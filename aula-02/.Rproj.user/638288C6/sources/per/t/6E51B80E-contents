vetor <- c(5, 6, 7, 8, 9)

data.frame(coluna1 = c(1,2,3), coluna2 = c("A", "B", "C"))

mean(vetor)


#equipe, categoria, num_arremesso_3pts

vetor.equipe <- c("Lakers", "Nets", "Giants", 
                  "Franca", "Celtics", "Minas",
                  "Pinheiros", "Facisa", "Flamengo")

vetor.categoria <- c("U18", "U18", "U18",
                     "U22", "U22", "U22",
                     "Adulto", "Adulto", "Adulto")

vetor.a3t <- c(18.5, 19.4, 20.3, 25, 28.3, 19.1, 31.1, 34.5, 42.1)

table <- data.frame(equipe = vetor.equipe,
                    categoria = vetor.categoria,
                    a3t = vetor.a3t)

table[4,3]
table[4,"a3t"]

table[,"a3t"]
table[1,]


subset(table, equipe == "Facisa")
subset(table, categoria == "U18")


subset(table, a3t == max(a3t))
subset(table, a3t == min(a3t))


mean(table$a3t)
sd(table$a3t)

# Agrupamento a3t por categoria e calculando a média para cada categoria
media <- aggregate(a3t ~ categoria, FUN = mean, data = table)
# Agrupamento a3t por categoria e calculando o desvio padrão para cada categoria
dp <- aggregate(a3t ~ categoria, FUN = sd, data = table)
# Criação da tabela com media e desvio padrão
media.dp <- merge(media, dp, by = "categoria")
# Renomear as colunas
names(media.dp) <- c("categoria", "media", "dp")


