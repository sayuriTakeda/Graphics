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
             size = 1) +                                  # tamanho dos pontos 
  labs(x = "Sepal.Width",y = "Sepal.Length",              # nome dos eixos  
       title = "Gráfico ggplotly", colour = "Species") +  # título do gráfico e título das cores
  geom_segment(aes(x = 0, y = 0, xend = x2, yend = x2),   # monta a linha (começando no 0,0)
               colour = "#737373", size = 0.3)            # cor e largura da linha
ggplotly(P, height = 400, width = 500)                    # plota o gráfico interativo e escolhe tamanho



####### Gráfico ggplotly com cores em gradiente 
df <- data.frame(                          
  x = iris$Sepal.Length,                     # escolhe as variáveis
  y = iris$Petal.Length,
  Sepal_Lenght = iris$Sepal.Length)          # escolhe a variável que vai receber a escala de cor

p <- ggplot(df, aes(x, y)) +                 # cria um ggplot 
  geom_point(aes(colour = Sepal_Lenght), 
             show.legend = F) +
  scale_colour_gradient(low = "#33ccff",     # a "menor cor" 
                        high = "#ff99cc") +  # a "maior cor"
  theme(axis.title.x=element_blank(),        # retira o nome do eixo x 
        axis.title.y=element_blank(),        # retira o nome do eixo y
        plot.title = element_text(hjust = 0.5)) +      
  labs(title = "Interativo + cores em gradiente") 

ggplotly(p,  width = 500, height = 350)      # plota de maneira interativa e escolhe tamanho  



####### Gráfico ggplotly com tooltip modificada 
p <- ggplot(iris,aes(Sepal.Width,Sepal.Length)) +                   # cria ggplot normal                
  geom_point(colour = "#79a6d2",                                    # cor para os pontos 
             size = 1,                                              # tamanho dos pontos
             aes(text = paste0("Sepal: ", Sepal.Length, "\n",       # texto para a tooltip            
                               "Petal: ", Petal.Length, "\n",
                               "Species: ", iris$Species))) +   
  labs(x = "Width",y = "Length", title = "Gerar para ver tooltip") +  # nome dos eixos         
  theme_bw()                                                        # troca o fundo cinza por branco

ggplotly(p, tooltip = "text", height = 400, width = 350)            # plota gráfico interativo  
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


####### Plotly (geom_point + geom_line) com tooltip modificada 
p <- ggplot(base_tornaram_inativos, aes(x = data, y = perc)) +
  geom_point(colour = "#993333", size = 1) +                               # escolhe cor e tamanho dos pontos
  geom_line(colour = "#CC6666") +                                          # escolhe cor da linha
  labs(x = "Data",y = "(%)", title = "Tornaram Inativos") +
  theme(plot.margin=unit(c(0,1,1,1), "cm"),                                # tem a margem recuada para não cortar o lab 
        axis.text.x = element_text(angle = 30, hjust = 1))+                # ângulo da legenda data   
  ylim(min(base_tornaram_inativos$perc) - 0.1, 
       max(base_tornaram_inativos$perc) + 0.1)+                            # altera o eixo y 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y")             # faz a legenda do eixo x pular de mês em mês e mod 

p <- plotly_build(p)

p$x$data[[1]]$text <- paste0("Mês: ", format.Date(base_tornaram_inativos$data, "%m/%y"), "\n",  # montar a tooltip            
                             "Perc: ", base_tornaram_inativos$perc, "\n",
                             "Valor: ", base_tornaram_inativos$valor)
p


####### ggplotly no Shiny com position = "stack" e cores modificadas
output$clientes_fieis <- renderPlotly({                                        # como é shiny tem que ter renderPlotly
    # Arrumando o eixo (por algum bug estava pegando 1 mês a mais antes e depois)
    intervalo_datas <- sort(unique(base_mantiveram_tornaram_fieis$datas))
    label_datas <- format.Date(intervalo_datas, "%b/%y")                       # formato como Oct/2017 ...
    # Criação do gráfico
    graph <- base_mantiveram_tornaram_fieis %>% 
      ggplot(aes(x = datas, y = valor, fill = tipo)) +                         # vai colorir conforme o tipo 
      geom_col(position = "stack",                                             # uma barra em cima da outra   
               aes(text = paste0("data: ", format.Date(datas, "%m/%y"), "\n",  # texto para a tooltip            
                                 "valor: ", valor, "\n",
                                 "perc: ", perc))) +
      labs(x = "Data", y = "Quantidade", title = "Clientes Fiéis", fill = "") +  # fill = "" e não colour = "" 
      theme(plot.title = element_text(hjust = 0.5),                              # título no meio 
            axis.text.x = element_text(angle = 20, hjust = 1),                   # angulo das datas no eixo x
            axis.text.y=element_blank(),                                         # para retirar os números do eixo y (tooltip mostra)
            plot.margin=unit(c(0,0,1,2), "cm")) +                                # recuo da margem 
      scale_x_date(breaks = intervalo_datas, date_labels = label_datas) +        # o label vai ser como definido no início
      scale_fill_manual(values = c("#5c85d6", "#666699"))                        # para alterar as cores do preenchimento 
    
    ggplotly(graph, tooltip = "text")                                            # deixar interativo 
  })


####### bubble plot e ggplot geom_point com gapminder
library(gapminder)                                                         # base de dados 

gap_with_colors <-                                                         # esolhe uma cor para cada country
  data.frame(gapminder,
             cc = I(country_colors[match(gapminder$country,
                                         names(country_colors))]))

# bubble plot, focus just on Africa and Europe in 2007
keepers <- with(gap_with_colors,                                           # filtra só o ano de 2007 na base
                 year == 2007)
plot(lifeExp ~ gdpPercap, gap_with_colors,
     subset = keepers, log = "x", pch = 21,
     cex = sqrt(gap_with_colors$pop[keepers]/pi)/3000,                     # tamanho
     bg = gap_with_colors$cc[keepers])                                     # cor  

# movie
library(gganimate)                                                         # para fazer com animação
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, 
                           color = continent, frame = year)) +             # frame vai ser ano 
  geom_point() +
  scale_x_log10()

gganimate(p)                                                               # gera o gráfico com animate     


####### scale_fill_hue para alterar as cores do fill
p <- base %>% ggplot(aes(SEMANA, PECAS, fill = SKU_NOME)) +
  geom_col() +
  labs(x = "Semanas",y = "Quantidade", title = "Teste") +
  theme(plot.title = element_text(hjust = 0.5)) +                     # ajusta o titulo no meio
  theme(legend.position="none") +                                     # retira a legenda do fill
  scale_fill_hue(l=30, c=70)                                          # altera as cores
ggplotly(p)


####### facet_grid
p <- base_rbind %>% ggplot(aes(SEMANA, VENDA, colour = factor(NOME))) + 
  geom_line(show.legend = F) +                                        # oculta a legenda do colour
  labs(colour = "", title = "Title") +
  theme(plot.title = element_text(hjust = 0.5),                       # ajusta título 
        panel.background = element_blank())                           # fundo branco

p + facet_grid(NOME~., scales = "free")                               # cada grid por nome


####### geom_point colorido por coluna_teste
p <- base %>% arrange(CONVERSAO_ATUAL) %>% 
  ggplot(aes(HOR_PROCESSAMENTO, CONVERSAO_ATUAL)) + 
  geom_line() +
  geom_point(aes(colour = factor(coluna_teste)))
ggplotly(p)


####### geom_line + geom_point com gradient color
cc <- scales::seq_gradient_pal("#00cc00", "#e60000")(seq(0,1,length.out=52))

p <- a %>% ggplot(aes(PVM, share, colour = factor(SEMANA))) + 
  geom_line(show.legend = T) +
  geom_point(aes(text = paste0("PVM: ", PVM, "\n",
                               "SHARE: ", share, "\n",
                               "SEMANA: ", SEMANA, "\n",
                               "SKU: ", SKU_NOME))) +
  labs(colour = "semana") +
  scale_colour_manual(values=cc)
ggplotly(p, tooltip = "text")


####### cada linha do gráfico em colunas diferentes na tabela
a_b %>% ggplot(aes(SEMANA)) +
  geom_line(aes(y = SOMA_A, colour = MARCA_TAMANHO)) +
  geom_line(aes(y = SOMA_B, colour = MARCA_TAMANHO_B))


####### geom_bar com vjust e position_dodge
df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("D0.5", "D1", "D2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))

ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=len), vjust=-0.8, color="black",   # vjust altura do numero 
            position = position_dodge(0.9), size=3.5)+   # altera a posição eixo x 
  scale_fill_brewer(palette="Paired")+
  theme_minimal()


####### inserir ponto (big.mark) em eixo y do ggplot
BASE_QTDE_COMPRAS_MES %>%                      
  ggplot(aes(x = DATA, y = QTD_COMPRAS)) +
  geom_line(colour = "#24478f") +
  theme_light() + 
  labs(x = "Data", y = "Quantidade de compras") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +  # eixo de mês em mês (abr 18 .. mar 19)
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) # big.mark eixo y


####### inserir geom_text na coluna
tabela <- iris %>% 
  group_by(Species) %>% 
  summarise(n = n())

tabela %>% 
  ggplot(aes(Species, n)) +
  geom_col(fill = "#006666", alpha = 0.5) +
  geom_text(aes(label = n),  vjust = -0.5) + #position=position_dodge(width=0.9)
  theme_minimal() +
  labs(x = "Especie", y = "Quantidade") +
  ylim(0,100)
                     
