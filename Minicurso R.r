

# Gráficos têm um papel fundamental na organização de dados na estatística, com eles, a interpretação desses dados torna-se mais eficiente.


sessionInfo()

nomes <- c("João", "Pedro", "Paulo")
salario <- c(2300, 2600, 2200)
vendas <- c(105, 120, 130)

loja <- data.frame(nomes, salario, vendas)

loja

View(loja)

plot(loja$vendas, loja$salario)

plot(loja$vendas, loja$salario, type = "b")

#type : 
#"p" para pontos, 
#"l" para linhas, 
#"b" para pontos e linhas, 
#"c" para linhas descontinuas nos pontos, 
#"o" para pontos sobre linhas, 
#"n" para nenhum gráfico, apenas a janela.


plot(loja$vendas, loja$salario, type = "p",
     col.axis = "red", 
     pch = 16,
     xlab = "Vendas", 
     ylab = "Salario", 
     main = "Salario x Vendas",
     xlim = c(100,135),
     ylim = c(2000,2700),
     cex.axis = 1, 
     cex.main = 2, 
     col.main = 1, 
     cex.lab = 1)


text(105, 2350, labels = "João")
text(120, 2650, labels = "Paulo")
text(130, 2300, labels = "Pedro")
text(vendas, salario + 60, labels = nomes)


install.packages("Hmisc")  # para visualizar as cores

library(Hmisc)

show.col()

ron_2020 <- c(400,500,1100,100,300,350,250,900,800,775,900,400)
ron_2021 <- c(450,550,100,300,500,400,500,600,750,900,750,800)
mes <- c(01,02,03,04,05,06,07,08,09,10,11,12)

investimento <- data.frame(mes,ron_2020,ron_2021)

investimento


plot(investimento$mes,col = 54, investimento$ron_2020, type = "l", ylim = c(100,1200), main = "Investimentos no ano de 2021")


plot(investimento$mes, investimento$ron_2020,type = "n", main = "2021", xlab = "Mês", ylab = "Valor", ylim = c(0,1300),
     xlim = c(1,12), las = 1, col = "black", cex.lab = 1, cex.main = 1,cex.axis = 1, lwd = 2)

legend(x = "topleft", legend = "Investimentos em R$", lty = 1, lwd = 1, bty = "n", col = "black")


plot(investimento$mes, investimento$ron_2020,type = "l", main = "2021", xlab = "Mês", ylab = "Valor", ylim = c(0,1300),
     xlim = c(1,12), las = 1, col = "red", cex.lab = 1, cex.main = 1,cex.axis = 1)

legend(x = "topleft", legend = "Investimentos em R$", lty = 1, lwd = 1, bty = "n", col = "red")

#type : 
#"p" para pontos, 
#"l" para linhas, 
#"b" para pontos e linhas, 
#"c" para linhas descontinuas nos pontos, 
#"o" para pontos sobre linhas, 
#"n" para nenhum gráfico, apenas a janela.

plot(investimento$mes,
     investimento$ron_2020,
     type = "l",
     main = "Investimentos no ano de 2021",
     xlab = "mês",
     ylab = "ss",
     col = "red",
     las=1, #rotacionar os valores dos eixos 
     ylim = c(0,1300),
     xlim = c(1,12),
     adj = 0.2,  # 0 a 1
     cex.lab = 1, #tamanho do x,ylab
     cex.axis = 1)

grid(nx = NULL, ny = NULL,  # grade
     lty = 3, 
     lwd = 1, 
     col = "gray")

segments(x0 = 3, y0 = 100,  #linha
         x1 = 3, y1 = 1100)

polygon(x = c(2, 4, 4, 2), y = c(1200, 1200, 1000, 1000),  # poligono
        col = adjustcolor("gray", alpha.f = 0.5),
        density = NULL,
        border = NA)

arrows(x0 = 2, y0 = 1170, 
       x1 = 2.75, y1 = 1115, 
       length = 0.1)

text(x = 2, y = 1200, 
     labels = "Maior valor",
     cex = 0.95)

idade = c(10,15,9,25,35,15,39,17,85,47,36,21,45,84,25,20,30,49,35,
          15,40,7,5,16,110,45,69,26,34,54,71,23,54,86,24,54,63,75,65)

idade_populacao <- data.frame(idade)

head(idade_populacao, 10)

hist(idade_populacao$idade, ylim = c(0,9), xlim = c(0,110), col = "red", breaks = c(0,10,20,30,40,50,60,70,80,90,100,110),
     main = "Titulo", border = "white")

box(bty = "l")

sort(idade)


hist(idade_populacao$idade,
     main = "Idade da População \nCampina Grande",
     ylab = "Frequencia",
     xlab = "Idade",
     ylim = c(0,8),
     xlim = c(0,120),
     las = 1,
     border = "white",
     breaks = c(0,10,20,30,40,50,60,70,80,90,100,110),
     col = "red") # intervalos)

box(bty = "l")  #bty - Altera o tipo de contorno usado envolta do gráfico. (l ou só box)

sort(idade, decreasing = F)

hist(idade_populacao$idade,
     prob = TRUE, #prob - Argumento lógico para especificar se densidade deve ser mostrada.
     las = 1,
     main = "Idade da população \n Campina Grande - PB",
     col = "red",
     xlab = "label eixo x",
     ylab = "label eixo y",
     border = "green",
     breaks = c(0,10,20,30,40,50,60,70,80,90,100,110), # intervalos
     xlim = c(0,120))



#install.packages("ade4")

library(ade4)

require(ade4) 
data(tortues)

head(tortues)

dados <- rbind(tapply(tortues[, 1], tortues[, 4], mean), 
               tapply(tortues[, 2], tortues[, 4], mean), 
               tapply(tortues[, 1], tortues[, 4], mean))

rownames(dados) <- names(tortues)[1:3] 

barplot(dados, 
        beside = TRUE, 
        width = 0.7, 
        ylim = c(0, 180), 
        las = 1,
        legend = c("Comprimento","Largura", "Altura"), 
        args.legend = list(x = "top", 
                           bty = "n", 
                           ncol = 3), 
        names = c("Machos","Fêmeas"), 
        main = "Tartarugas")



dados <- data.frame(Mulheres = c(0.328, 0.461, 0.159, 0.052), 
                    Homens  = c(0.351, 0.464, 0.146, 0.039)) 
rownames(dados) <- c("0-19", "20-49", "50-69", "70+")
dados <- as.matrix(dados)
dados

barplot(dados, 
        col = c("white", "black", "grey", "black"), 
        density = c(0, 30, 50, 200), 
        ylab = "Proporção", 
        las = 1,
        main = "População Brasileira (2010)",
        ylim = c(0, 1), xlim = c(0, 3.5),
        legend = rownames(dados), 
        args.legend = list(x = "right", 
                           bty = "n", 
                           title = "Faixa etária"))

axis(side = 1, label = FALSE, tck = 0)



#install.packages("data.table") # pacote para manipulação de dados

library(data.table)


# https://www.rdocumentation.org/


# definir o endereco do conjunto de dados e baixa-lo


local <- "C:/Users/User/Desktop/csv/sigefcamposproducaodesementes.csv"


# https://dados.agricultura.gov.br/sv/dataset/dados-referentes-ao-controle-da-producao-de-sementes-sigef/resource/6ab20c11-73a0-4ab0-8e13-2420d48dd6f5


dados <- fread(file = local) 


#todo o conjunto de dados

View(dados)

# Primeiras 6 linhas

head(dados)

# Ultimas 6 linhas

tail(dados)

# informações

str(dados)

# estatistica descritiva basica

summary(dados)

#install.packages("dplyr")  Manipulação de dados
library(dplyr)

#install.packages("ggplot2") # Gráficos
library(ggplot2)


hist(dados$Area) # pacote base do r (grafico de frequencia)

options(scipen = 999) # Desabilita notação cientifica

hist(dados$Area, xlim = c(0,2000)) 

hist(dados$Area, ylim = c(0,400000), xlim = c(0,2000)) 

hist(dados$Area, ylim = c(0,400000), xlim = c(0,2000), col = "black") 

hist(dados$Area, ylim = c(0,400000), xlim = c(0,2000), col = "black", border = "white") 

hist(dados$Area, ylim = c(0,400000), xlim = c(0,2000), col = "black", border = "white", xlab = "Área") 

hist(dados$Area, ylim = c(0,400000), xlim = c(0,2000), col = "black", border = "white", xlab = "Área", ylab = "Frequência")

hist(dados$Area, ylim = c(0,400000), xlim = c(0,2000), col = "black", border = "white", xlab = "Área", ylab = "Frequência", main = "Histograma da Área", cex.lab = 1.2)

hist(dados$Area, ylim = c(0,400000), xlim = c(0,2000), col = "black", border = "white", xlab = "Área", ylab = "Frequência", main = "Histograma da Área", cex.lab = 1.2, cex.axis = 1.2)

hist(dados$Area, ylim = c(0,400000), xlim = c(0,2000), col = "black", border = "white", xlab = "Área", ylab = "Frequência", main = "Histograma da Área", cex.lab = 1.2, cex.axis = 1.2, col.axis = "red")

hist(dados$Area, 
     ylim = c(0,400000), 
     xlim = c(0,2000), 
     col = "black", 
     border = "white", 
     xlab = "Área", 
     ylab = "Frequência", 
     main = "Histograma da Área", 
     cex.lab = 1.2, 
     cex.axis = 1.2, 
     col.axis = "red", 
     cex.main = 3)

box(bty = "l")


# https://lhmet.github.io/adar-ebook/par%C3%A2metros-gr%C3%A1ficos.html


valores_area <- data.frame(table(dados$Area)) # cria uma tabela de frequencia

hist | barplot | boxplot | pie
  
# https://www.rdocumentation.org/packages/graphics/versions/3.6.2

ggplot(data = dados) + geom_histogram(mapping = aes(x = Area))


histograma_area <- ggplot(data = dados) + geom_histogram(mapping = aes(x = Area)) + 
                      xlim(c(0,600)) + 
                      ylim(c(0,100000)) + 
                      ggtitle("Frequência do tamanho das áreas") + 
                      theme_bw() +
                      theme(plot.title = element_text(hjust = 0.5)) +
                      xlab("rótulo eixo x") + 
                      ylab("rótulo eixo y")

install.packages("plotly")
library(plotly)

grafico_inter <- ggplotly(histograma_area)

grafico_inter              

# mapping: esse parametro vai receber os eixos que você vai trabalhar nesse caso só o x
# aes: essa função realiza o mapeamento dos valores que serão plotados no graficos, ou seja, ela organiza os valores que serão plotados



ggplot(data = dados) + geom_boxplot(aes(x = UF , y = Area, fill = UF)) + theme_bw()


ggplot(data = dados) + 
  geom_bar(mapping = aes(x = Status), stat = "count") 


ggplot(data = dados) + 
  geom_bar(mapping = aes(x = Status), stat = "count") + 
  coord_flip()


# stat = "count" faz a contagem automatica dos dados


category.freq_status <- data.frame(table(dados$Status))

ggplot(data = category.freq_status) + 
  geom_bar(mapping = aes(x = Var1, y = Freq), stat = "identity") + 
  coord_flip()

# stat = "identity" não faz a contagem automatica dos dados

ggplot(data = category.freq_status) + 
  geom_bar(mapping = aes(x = reorder(Var1,Freq), y = Freq), stat = "identity") + 
  coord_flip()


ggplot(data = category.freq_status) + 
  geom_bar(mapping = aes(x = reorder(Var1,-Freq), y = Freq), stat = "identity") +
  coord_flip()


top3 <- category.freq_status[(order(-category.freq_status$Freq)),]
top3 <- category.freq_status[1:3,]

ggplot(data = top3) + 
  geom_bar(mapping = aes(x = reorder(Var1,Freq), y = Freq), stat = "identity") + 
  coord_flip()


category.freq_uf <- data.frame(table(dados$UF))

ggplot(data = category.freq_uf) + 
  geom_bar(mapping = aes(x = reorder(Var1,Freq), y = Freq, fill = Freq), stat = "identity") + 
  coord_flip() +
  ggtitle("Frequência do tamanho dos estados") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("rótulo eixo x") + 
  ylab("rótulo eixo y")
  


freq_status <- data.frame(table(dados$UF))
top3_status <- freq_status[(order(-freq_status$Freq)),]
top3_status <- top3_status[1:3,]

pizza <- ggplot(data = category.freq_uf) + 
  geom_bar(aes(x = "", y = Freq, fill = Var1), stat = "identity", width = 1) +
  ggtitle("Frequência do tamanho dos estados") + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("rótulo eixo x") + 
  ylab("rótulo eixo y") +
  coord_polar(theta = "y", start = 0 )




require("grDevices") # for colours
x <- y <- seq(-4*pi, 4*pi, length.out = 27)
r <- sqrt(outer(x^2, y^2, "+"))

image(z = z <- cos(r^2)*exp(-r/6), col = gray.colors(33))

image(z, axes = FALSE, main = "Math can be beautiful ...", xlab = expression(cos(r^2) * e^{-r/6}))
contour(z, add = TRUE, drawlabels = FALSE)

image(t(volcano)[ncol(volcano):1,])

x <- 10*(1:nrow(volcano))
y <- 10*(1:ncol(volcano))
image(x, y, volcano, col = hcl.colors(100, "terrain"), axes = FALSE)
contour(x, y, volcano, levels = seq(90, 200, by = 5),
        add = TRUE, col = "brown")
axis(1, at = seq(100, 800, by = 100))
axis(2, at = seq(100, 600, by = 100))
box()
title(main = "Maunga Whau Volcano", font.main = 4)



#https://rpubs.com/LKrukrubo/bar_charts_box_plots_and_histograms
#https://lhmet.github.io/adar-ebook/par%C3%A2metros-gr%C3%A1ficos.html
#https://vanderleidebastiani.github.io/tutoriais/Graficos_com_R.html#Fun%C3%A7%C3%B5es_b%C3%A1sicas