####################### Interactive Graphics ####################### 
library(dplyr)
library(highcharter)
library(ggplot2)
library(plotly)

####### Escrever gráfico interativo com highcharter
highchart() %>% 
  hc_title(text = "Gráfico highcharter") %>%                       # título
  hc_add_series_scatter(iris$Sepal.Width, iris$Sepal.Length) %>%   # aes
  hc_xAxis(title = list(text = "Sepal.Width")) %>%                 # nome do eixo x
  hc_yAxis(title = list(text = "Sepal.Length"))                    # nome do eixo y  




####### Escrever gráfico interativo com ggplot / ggplotly
p <- ggplot(iris,aes(Sepal.Width,Sepal.Length)) +                 # escreve em ggplot
  geom_point(colour = "#ff4dff", show.legend = "F", size = 1) +   # cor, legenda e tamanho dos pontos 
  labs(x = "Sepal.Width",y = "Sepal.Length")                      # nome dos eixos 

ggplotly(p)                                                       #faz o gráfico ficar interativo  




####### Para montar ggplotly com uma linha no meio 
x2<- max(c(iris$Sepal.Length,iris$Sepal.Width))           # pega o ponto máx entre as variáveis 
P <- ggplot(iris,aes(Sepal.Width,Sepal.Length)) +         # escolhe a base e as variáveis
  geom_point(aes(colour = factor(Species)),               # escolhe como os pontos serão coloridos
             size = 2) +                                  # tamanho dos pontos 
  labs(x = "Sepal.Width",y = "Sepal.Length",              # nome dos eixos  
       title = "Gráfico ggplotly", colour = "Species") +  # título do gráfico e título das cores
  geom_segment(aes(x = 0, y = 0, xend = x2, yend = x2),   # monta a linha (começando no 0,0)
               colour = "#737373", size = 0.3)            # cor e largura da linha
ggplotly(P, height = 600, width = 640)                    # plota o gráfico interativo e escolhe tamanho




####### Gráfico ggplotly com cores em gradiente 
df <- data.frame(                          
  x = iris$Sepal.Length,                     # escolhe as variáveis
  y = iris$Petal.Length,
  Sepal_Lenght = iris$Sepal.Length)          # escolhe a variável que vai receber a escala de cor

p <- ggplot(df, aes(x, y)) +                 # cria um ggplot 
  geom_point(aes(colour = Sepal_Lenght)) +
  scale_colour_gradient(low = "#33ccff",     # a "menor cor" 
                        high = "#ff99cc") +  # a "maior cor"
  theme(axis.text.x=element_blank(),         # pode botar o título tirando essa parte e inserindo + labs(title = "Nome do Título")
        axis.title.x=element_blank(),        # retira o nome do eixo x
        axis.title.y=element_blank())        # retira o nome do eixo y

ggplotly(p,  width = 700, height = 550)      # plota de maneira interativa e escolhe tamanho  




####### Gráfico ggplotly com tooltip modificada 
p <- ggplot(iris,aes(Sepal.Width,Sepal.Length)) +                # cria ggplot normal                
  geom_point(colour = "#79a6d2",                                 # cor para os pontos 
             size = 1,                                           # tamanho dos pontos
             aes(text = paste0("Sepal: ", Sepal.Length, "\n",    # texto para a tooltip            
                               "Petal: ", Petal.Length, "\n",
                               "Species: ", iris$Species))) +   
  labs(x = "Width",y = "Length") +                               # nome dos eixos         
  theme_bw()                                                     # troca o fundo cinza por branco

ggplotly(p, tooltip = "text", height = 500, width = 540)         # plota gráfico interativo  
                                                                 # com tooltip escolhida e tamanho do gráfico

