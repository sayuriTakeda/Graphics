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


####### Gráfico ggplot scale_fill_gradient (geom_col)
g <- ggplot(fluxo_tab_2, aes(fluxo_tab_2$nomes, fluxo_tab_2$valor))
g + geom_col(aes(fill = fluxo_tab_2$valor)) +  
  labs(x = "Período",y = "%") + 
  geom_text(aes(label = valor, y = valor + 0.05)) +              # para inserir os números de cada barra
  labs(title = "Título") + 
  theme(plot.title = element_text(hjust = 0.5), panel.background = element_blank()) +  # deixa o título no meio e o fundo branco
  scale_fill_gradient(low = "#538cc6", high = "#204060", guide = F)


####### Gráfico ggplot position = "dodge" (geom_col)
# Criação do dataframe
a <- tab_1_dash %>% transmute(regiao, p1 = cliente, p2 = cliente_2)
a %<>% gather(key = período, value = valores, -regiao) %>% filter(regiao != "-------")
# Criação do gráfico
graph <- 
  a %>% 
  ggplot(aes(x = regiao, y = valores, fill = período)) + 
  geom_col(position = "dodge", 
           aes(text = paste0("região: ", regiao, "\n",     # texto para a tooltip            
                             "valor: ", valores))) +
  labs(x = "Região", y = "Quantidade") +
  labs(title = "Fluxo de Clientes - Regiões") +     
  theme(plot.title = element_text(hjust = 0.5),            # título no meio 
        axis.text.x = element_text(angle = 20, hjust = 1),
        axis.text.y=element_blank(),                       # para retirar os números do eixo y (tooltip mostra)
        plot.margin=unit(c(0,0,0,2), "cm"))                # para os nomes do eixo x não ficarem cortados 

ggplotly(graph, tooltip = "text", height = 400, width = 700)  # para ficar interativo 


####### Para ordenar conforme o axis com num e não com factor (ordem alfab)
teste <- rev(df_cat_pos$CAT)                               # depois de ordenar a tabela (dev serve para fazer o contrário)
df_cat_pos$CAT <- factor(df_cat_pos$CAT, levels = teste)   # o level será a ordem do vetor teste 
p <- df_cat_pos %>%                                        # ggplot normal   
  ggplot(aes(`var_repr(%)`,CAT))+
  geom_point()
p


####### geom_line e point (scale_x_date break 1 month)
p <- ggplot(base_cpf_mes_ano_ant, aes(x = data, y = qtd))+ 
geom_point(colour = "#24478f") +                              # fica com pontos na linha  
geom_line(colour = "#85a3e0") +
labs(x = "Data",y = "", title = "Clientes Mês") +
theme(plot.margin=unit(c(0,0,1,1), "cm"),
axis.text.x = element_text(angle = 20, hjust = 1)) +          # angulo do eixo x 
ylim((min(base_cpf_mes$qtd) - 100000),                        # arruma o axis
     (max(base_cpf_mes$qtd) + 100000)) +                      
scale_x_date(date_breaks = "1 month", date_labels = "%b %y")  # pula de mês em mês (ex: out 2017, nov 2017...)
ggplotly(p)                                                   # para ficar interativo


####### ggplotly in Shiny
output$clientes_mes <- renderPlotly({ 
    p <- ggplot(base_cpf_mes, aes(x = mes, y = valor, color = ano, group = ano))+  # uma linha para cada ano
    geom_line(aes(text = paste0("Mês: ", mes, "\n",                                # texto para tooltip
    "Valor: ", valor))) +
    labs(x = "Mês",y = "", title = "Clientes Mês", colour = "") +                  # tira o título da legenda
    theme(plot.margin=unit(c(0,0,1,1), "cm")) +                                    
    ylim((min(base_cpf_mes$valor, na.rm=TRUE) - 100000), (max(base_cpf_mes$valor, na.rm=TRUE) + 100000)) + # arruma eixo
    scale_color_manual(values=c("#CC6666", "#336699")) +                           # cores das linhas 
    scale_y_continuous(labels = scales::comma)                                     # tira o formato cientifico no eixo y
    ggplotly(p, tooltip = "text")})
