#' Actogram Plot
#' Representation of animal activities rhythms plot
#' @name doubleplot
#' @param timestamp a vector of data and time in time stamp format ("yyyy-mm-hh:mm:ss").
#' @param rythm a numeric vector related to activities variable for each timestamp.
#' @param n_lines number of lines
#' @param dark_beg dark begins
#' @param dark_end dark ends
#' @param cold_beg cold begins
#' @param cold_end cold ends
#' @section Details:  O argumento n_lines pode ser utilizado para selecionar um conjunto dos dados desejados, tendo-se em mente que como trata-se de dados temporais em sequência, n_lines se refere à quantidade de observações contidas no intervalo 1:n_lines, ou seja, sempre a partir da primeira observação fornecida.
#' @return an doble actogram plot and ritm table
#' @section References:   MARQUES, M. D.; GOLOMBEK, D.; MORENO, C. Adaptação Temporal. In: MARQUES, N. &  MENNA-BARRETO, L. Cronobiologia: princípios e aplicações. São Paulo: Editora Universidade de São Paulo, 1999. p.45-62.
#' @examples
#' 
#' 
#' \dontrun{
#' day2 <- sort(as.POSIXct("1995-03-31 00:00:00") + runif(n=55, min=0, max=(3600*24)))
#' day3 <- sort(as.POSIXct("1995-04-01 00:00:00") + runif(n=45, min=0, max=(3600*24)))
#' day4 <- sort(as.POSIXct("1995-04-02 00:00:00") + runif(n=50, min=0, max=(3600*24)))
#' day5 <- sort(as.POSIXct("1995-04-03 00:00:00") + runif(n=60, min=0, max=(3600*24)))
#' day6 <- sort(as.POSIXct("1995-04-04 00:00:00") + runif(n=40, min=0, max=(3600*24)))
#' day7 <- sort(as.POSIXct("1995-04-05 00:00:00") + runif(n=53, min=0, max=(3600*24)))
#' day8 <- sort(as.POSIXct("1995-04-06 00:00:00") + runif(n=47, min=0, max=(3600*24)))
#' day9 <- sort(as.POSIXct("1995-04-07 00:00:00") + runif(n=55, min=0, max=(3600*24)))
#' day10 <- sort(as.POSIXct("1995-04-08 00:00:00") + runif(n=45, min=0, max=(3600*24)))
#' timestamp <- c(day1, day2, day3, day4, day5, day6, day7, day8, day9, day10)
#' rythm <- abs(rnorm(n=500, mean=50, sd=20))
#' dark_beg <- c(rep("00:00:00",150),rep("06:00:00",110),rep("18:00:00",93),rep("00:00:00",147))
#' dark_end <- c(rep("00:00:00",150),rep("18:00:00",110),rep("06:00:00",93),rep("23:59:59",147))
#' cold_beg <- c(rep("00:00:00",150),rep("06:00:00",110),rep("18:00:00",93),rep("04:00:00",147))
#' cold_end <- c(rep("00:00:00",150),rep("18:00:00",110),rep("06:00:00",93),rep("20:00:00",147))
#' doubleplot(timestamp=timestamp,rythm=rythm,n_lines=500,dark_beg=dark_beg,dark_end=dark_end,cold_beg=cold_beg,cold_end=cold_end)
#' doubleplot(timestamp=timestamp,rythm=rythm,n_lines=353,dark_beg=dark_beg,dark_end=dark_end)
#' doubleplot(timestamp=timestamp,rythm=rythm,n_lines=500,dark_beg=dark_beg,dark_end=dark_end,cold_beg=cold_beg,cold_end=cold_end, bar_width=600, graph_font="AvantGarde", hot_color="lightcoral", x_title_size = 5.5, y_lables_size=2.8, first_day=15, plot_legend=F)
#' }
#' 
#'
#' @encoding UTF-8
doubleplot <- function(timestamp, rythm, n_lines, dark_beg, dark_end, 
                      cold_beg=rep("00:00:00",n_lines), cold_end=rep("00:00:00",n_lines),
                      bar_width=300, 
                      graph_font="serif", 
                      hot_color="red3", cold_color="blue4",
                      x_title ="Time (Zt)", 
                      x_title_size = 4.8, x_lables_size = 3.8,
                      y_title ="Day of experiment", 
                      y_title_size = 4.8, y_lables_size = 3.8, 
                      legend_labels_size = 3.8,
                      graph_title = NULL,
                      graph_title_size = 6,
                      first_day = 1,
                      plot_legend = TRUE) 
          {                                           
           libraries <- unlist( c("lubridate",       
                                "hms",              
                                "ggplot2",
                                "gridExtra")
                               )       
           required <- unlist(lapply(X = libraries, 
                                     FUN = require, 
                                     character.only = TRUE)) 
           needed <- libraries[required==FALSE]
           if(length(needed)>0){               
               install.packages(needed) 
               lapply(X = needed, FUN = require, character.only = TRUE)
           }
      
          #require(gdtools)               #Instala e carrega o pacote "gdtools" para checar se a fonte escolhida pelo usuário existe. 
          #require(knitr)                 #Instala e carrega o pacote knitr para criar tabelas ao final.
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -            
#Verificação de parâmetros
          #Verificação 1: Igualdade de tamanho entre timestamp e rythm.
          if(length(timestamp)!=length(rythm))                        #Se timestamp e rythm possuem comprimentos diferentes:
            {stop("'timestamp' and 'rythm' have different lengths.")} #Para e retorna "''timestamp' and 'rythm' have different lengths.".

          #Verificação 2: Transformação correta do 'Time stamp'.          
          timestamp <- ymd_hms(timestamp,tz="")                #Transforma o objeto timestamp no formato mês-dia-ano + hora:minuto:segundo + fuso horário. Se houver valores fora do formato, são transformados em NA.
          if(sum(is.na(timestamp))>0)                          #Se um ou mais valores no objeto timestamp forem transformados em NA:
            {stop("'timestamp' contains out-of-format data.")} #Para e retorna "'timestamp' contains out-of-format data."".

          #Verificação 3: Presença de valores negativos em rythm.           
          if(sum(rythm<0)>0)                                                                         #Se rythm contiver valor(es) negativo(s):
            {message("Warning: Be careful, your rythm data has negative value(s). You might want to check.")} #Avisa ao usuário "Be careful, your rythm data has negative value(s). You might want to check".

          #Verificação 4: Valor adequado de números fornecidos (n_lines e bar_width).           
          n_lines <- as.integer(n_lines)                      #Transforma n_lines em número inteiro, possibilitando arredondamentos.
          if(n_lines <= 0)                                    #Se n_lines for negativo:
            {stop("'n_lines' must be integer and positive.")} #Para e retorna "'n_lines' must be integer and positive.".  
          
          bar_width <- as.integer(bar_width)                      #Transforma bar_width em número inteiro, possibilitando arredondamentos.
          if(bar_width <= 0)                                    #Se bar_width for negativo:
            {stop("'bar_width' must be integer and positive.")} #Para e retorna "'bar_width' must be integer and positive.".      

          #Verificação 5: Transformação correta dos horários fornecidos para ciclos de claro-escuro (obrigatório) e correspondência com n_lines escolhido pelo usuário.  
          if(length(dark_beg)!=length(dark_end))                          #Se dark_beg e dark_end possuem comprimentos diferentes:
            {stop("'dark_beg' and 'dark_end' have different lengths.")}   #Para e retorna "'dark_beg' and 'dark_end' have different lengths.".
          D.beg <- as_hms(dark_beg)                                       #Transforma o objeto dark_beg no formato hora:minuto:segundo e guarda no objeto D.beg. Se houver valores fora do permitido, por exemplo uma anotação "25:00:00", são transformados em NA.
          D.end <- as_hms(dark_end)                                       #Transforma o objeto dark_end no formato hora:minuto:segundo e guarda no objeto D.end. Se houver valores fora do permitido, por exemplo uma anotação "25:00:00", são transformados em NA.
          if(sum(is.na(D.beg))>0 | sum(is.na(D.end))>0)                   #Se um ou mais valores nos objetos D.beg/D.end forem transformados em NA:
            {stop("Out-of-format Light-Dark regime(s).")}                 #Para e retorna "Out-of-format Light-Dark regime(s).".
          if(length(dark_beg)<n_lines & length(dark_end)<n_lines)         #Se dark_beg e dark_end possuirem comprimento menor que n_lines escolhido:
            {stop("'dark_beg' and 'dark_end' are shorter than n_lines.")} #Para e retorna "'dark_beg' and 'dark_end' are shorter than n_lines.".
          
          #Verificação 6: Transformação correta dos horários fornecidos para ciclos de quente-frio (opcional) e correspondência com n_lines escolhido pelo usuário.              
          if(length(cold_beg)!=length(cold_end))                          #Se cold_beg e cold_end foram especificados e possuem comprimentos diferentes:
            {stop("'cold_beg' and 'cold_end' have different lengths.")}   #Para e retorna "'cold_beg' and 'cold_end' have different lengths.".
          if(missing(cold_beg)==T & missing(cold_end)==T)                 #Caso cold_beg e cold_end não sejam especificados pelo usuário:
            {                                                             #Indica as operações a serem seguidas.
            cold_beg.c <- rep("00:00:00",n_lines)                         #Cria cold_beg.c com hora zero repetida pelo número de linhas escolhido.
            cold_end.c <- rep("00:00:00",n_lines)                         #Cria cold_end.c com hora zero repetida pelo número de linhas escolhido.
            C.beg <- as_hms(cold_beg.c)                                   #Transforma o objeto cold_beg.c no formato hora:minuto:segundo.
            C.end <- as_hms(cold_end.c)                                   #Transforma o objeto cold_end.c no formato hora:minuto:segundo.
            } else                                                        #Finaliza operações indicadas e do contrário (cold_beg e cold_end especificados):
            {                                                             #Indica as operações a serem seguidas.
            C.beg <- as_hms(cold_beg)                                     #Transforma o objeto cold_beg informado no formato hora:minuto:segundo e guarda no objeto C.beg. Se houver valores fora do permitido, por exemplo uma anotação "25:00:00", ã transformados em NA.
            C.end <- as_hms(cold_end)                                     #Transforma o objeto cold_end  informado no formato hora:minuto:segundo e guarda no objeto C.end. Se houver valores fora do permitido, por exemplo uma anotação "25:00:00", ã transformados em NA.
            if(sum(is.na(C.beg))>0 | sum(is.na(C.end))>0)                 #Se um ou mais valores nos objetos C.beg ou C.end forem transformados em NA:
            {stop("Out-of-format Hot-Cold regime(s).")}                   #Para e retorna "Out-of-format Hot-Cold regime(s) em cold_beg.".
            if(length(cold_beg)<n_lines & length(cold_end)<n_lines)       #Se cold_beg e cold_end possuirem comprimento menor que n_lines escolhido:
            {stop("'cold_beg' and 'cold_end' are shorter than n_lines.")} #Para e retorna "'cold_beg' ou 'cold_end' are shorter than n_lines.".
            }                                                             #Finaliza operações indicadas.

          #Verificação 7: os valores fornecidos para tamanho das réguas e legenda são numéricos
          if(is.numeric(x_title_size)==F | is.numeric(x_lables_size)==F | is.numeric(y_title_size)==F | is.numeric(y_lables_size)==F | is.numeric(legend_labels_size)==F | is.numeric(graph_title_size)==F | is.numeric(first_day)==F)
          #Se algum dos argumentos de tamanho ou o de first_day receber valor não numérico
          {stop("Some number(s) you chose is(are) not numeric.")}         #Para e retorna "Some number(s) you chose is(are) not numeric."

          #NÃO SEI SE PRECISA MESMO DESSA #Verificação 8: os títulos fornecidos para os eixos são caracteres
          #if(is.character(x_title)==F | is.character(y_title)==F)         #Se algum dos argumentos de título receber valor que não é da classe de caracteres
          #{stop("Some title(s) you chose is(are) not character.")}        #Para e retorna "Some number(s) you chose is(are) not character."

          #Verificação X: Conferir se a fonte escolhida pelo usuário existe.
            #if(font_family_exists(graph_font)==F)                         #Se a fonte não existe.
            #{stop("The 'graph_font' you choose doesn't exists.")}         #Para e retorna "The 'graph_font' you choose doesn't exists.".

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -              
#Agora que todos os parâmetros foram verificados, podemos começar!
#Preparando dados para plotagem do actograma:  
          mydata <- data.frame(date(timestamp[1:n_lines]), as_hms(timestamp[1:n_lines]), rythm[1:n_lines], D.beg[1:n_lines],D.end[1:n_lines],C.beg[1:n_lines], C.end[1:n_lines]) #Cria o dataframe 'mydata' combinando a data extraída de timestamp, a hora extraída de timestamp, rythm, D.beg, D.end, C.beg e C.end, todos indo do primeiro valor àquele na posição de valor igual a n_lines. Com isso, o dataframe mydata, que será a base para alimentar nosso actograma, pode ter número de linhas (e, portanto, de observações consideradas) variável.
          colnames(mydata) <- c("Date","Time","Rythm","Dark_begin", "Dark_end", "Cold_begin", "Cold_end") #Altera os nomes das colunas de 'mydata' para "Date","Time","Rythm","Dark_begin", "Dark_end", "Cold_begin" e "Cold_end".
          days_contained <- unique(mydata$Date)                             #Cria o objeto "days_contained" que é todas as datas de mydata, compiladas apenas uma vez.
          mydata$Temperature <- rep(NA,nrow(mydata))                        #Adiciona uma coluna Temperature a mydata, com um NA para cada linha.
          for(a in 1:nrow(mydata))                                          #Cria um ciclo com contador a que vai de 1 ao número de linhas em mydata.                                                              
          {                                                                 #Indica início do ciclo.
          if(mydata[a,6]<mydata[a,7])                                       #Se o horário de Cold_begin da linha for menor que Cold_end (na escala de 24 horas), isso quer dizer que a duração do frio não inclui a meia noite. Nesse tipo de padrão, queremos demarcar uma única "faixa" de frio dentro do dia de 24 horas, que está no intervalo Cold_begin <= x <= Cold_end. Então:
                {                                                           #Indica as operações a serem seguidas.
                if(mydata[a,2] < mydata[a,6])                               #Se a hora da linha for menor que Cold_begin:
                  {mydata[a,8]<- paste0("hot")}                             #Escreve 'hot' na coluna Temperature da linha correspondente.
                if(mydata[a,2] >= mydata[a,6] & mydata[a,2] <= mydata[a,7]) #Se hora estiver entre Cold_begin e Cold_end:
                  {mydata[a,8] <- paste0("cold")}                           #Escreve 'cold' na coluna Temperature da linha correspondente.
                if(mydata[a,2] > mydata[a,7])                               #Se hora for maior que Cold_end:
                  {mydata[a,8] <- paste0("hot")}                            #Escreve 'hot' na coluna Temperature da linha correspondente.
                }                                                           #Finaliza as operações indicadas.
          if(mydata[a,6]>mydata[a,7])                                       #Se o horário de Cold_begin da linha for maior que Cold_end (na escala de 24 horas), isso quer dizer que a duração do frio inclui a meia noite (que é o ponto de quebra do eixo x no actograma). Nesse tipo de padrão, queremos demarcar duas "faixas" de frio dentro do dia de 24 horas, que estão nos intervalos Cold_begin <= x <= 23:59:59 e meia-noite <= x <= Cold_end. Então:
               {                                                            #Indica as operações a serem seguidas.
               if(mydata[a,2] < mydata[a,7])                                #Se a hora da linha for menor que Cold_end:
                 {mydata[a,8]<- paste0("cold")}                             #Escreve 'cold' na coluna Temperature da linha correspondente.
               if(mydata[a,2] >= mydata[a,7] & mydata[a,2] <= mydata[a,6])  #Se hora estiver entre Cold_end e Cold_begin:
                 {mydata[a,8] <- paste0("hot")}                             #Escreve 'hot' na coluna Temperature da linha correspondente.
               if(mydata[a,2] > mydata[a,6])                                #Se hora for maior que Cold_begin:
                 {mydata[a,8] <- paste0("cold")}                            #Escreve 'cold' na coluna Temperature da linha correspondente.
               }                                                            #Finaliza as operações indicadas.
          if(mydata[a,6]==mydata[a,7])                                      #Por fim, se o horário de Cold_begin da linha for igual a Cold_end, como é o caso do default para quando o usuário não fornece cold_beg nem cold_end, ou também possível para o caso de o usuário querer fornecer tanto regime quente-frio quanto de temperature constante no mesmo actograma:
                {mydata[a,8]<- paste0("constant")}                          #Escreve 'constant' na coluna Temperature da linha correspondente.
          }                                                                 #Finaliza o ciclo.
          mydata$Temperature <- as.factor(mydata$Temperature)               #Transforma as inserções feitas em Temperature em fatores. Isso é importante para poder colorir o actograma de aoordo com os regimes de temperatura informados.
#- - -         
#Preparando o actograma:
          #Definir o layout e valores m�ximos de eixos para os actogramas
          lines_in_plot <- seq(from=1, to=(length(days_contained)+1)) #Cria sequência indo de 1 ao número de dias total do actograma. Deve haver um a mais para ser o espaço em branco típico de actogramas (ora na primeira linha e ora na última).
          mat_layout <- as.matrix(lines_in_plot)                      #Transforma a sequência em matriz, para ser usada mais adiante em arrangeGrobs(). A partir daqui vou utilizar as funções do pacote gridExtra para plotar os gráficos de cada dia de acordo com a sequência contida em mat_layout. Isso, pois a função layout() base do R possui um limite de plotagem de no máximo 200 linhas que pode ser inconveniente para o usuário.
          xmax = as.numeric(as_hms("23:59:59"))                       #Define um objeto 'xmax' equivalente a 23h:59m:59s de forma numérica para ser operável em relação ao eixo x do actograma.
          ymax = max(mydata$Rythm)                                    #Define um objeto 'ymax' equivalente valor máximo de rythm presente no dataframe mydata. Assim, todas as linhas do actograma terão a mesma altura de acordo com o conjunto dos dados.
          ymax = as.numeric(ymax)                                     #Transforma ymax em objeto numérico. Pré requisito para scale_y_continuous().
#- - -
          #Criar os plots
                 #Vamos formar um actograma que é os ritmos plotados em barras, e os horários de claro/escuro demarcados com retângulo cinza ao fundo. As diferentes possibilidades de ciclo de temperatura (cíclica ou constante) serão representadas pelas cores das barras com base no fator contido na coluna Temperature.
             plots2 <- rep(NA,(length(days_contained)+1))  #Para a 2ª "coluna" do actograma, cria o objeto plots2, com (número de days_contained+1) vezes "NA".
             plots2 <- as.list(plots2)                     #Transforma plots2 em lista, para ser usada mais adiante em arrangeGrobs().
             plots2[[(length(days_contained)+1)]] <- ggplot() + theme_void()        #Cria um gráfico vazio na última posição 1 de plots2, típico de actogramas como o que faremos.         
             for(d in 1:(length(days_contained)))        #Cria ciclo com contador d indo de 1 a days_contained, para preencher os gráficos acima do vazio criado.
             {                                             #Indica início do ciclo.
             assign(paste0("df",d), mydata[mydata$Date==days_contained[d],]) #Cria um novo dataframe contendo apenas os dados da data contida em days_contained na posição do contador, de forma a separar o conjunto de dados por data em novos dataframes numerados df1, df2, df3 etc. Aqui, queremos que df1 contenha valores da primeira data de days_contained, por isso, utilizamos (d-1). 
             if(get(paste0("df",d))[1,4]<get(paste0("df",d))[1,5]) #Caso a fase de escuro declarada não contenha meia-noite - quando Dark_begin (coluna 4) do df é menor que Dark_end (coluna 5) do df -, cria apenas 1 retângulo cinza ao fundo. Como Dark_begin e Dark_end serão iguais para todas as linhas do dataframe daquele dia, podemos utilizar apenas o valor da primeira linha [1,x].
                     {                                                     #Indica as operações a serem seguidas.
                     plots2[[d]] <- ggplot(get(paste0("df",d)),        #Cria um plot a partir de df do número do contador-1, armazenando em plots1.  
                                           aes(x =Time, y =Rythm)          #Utilizando a coluna Time como x e Rythm como y.
                                           ) +                             #Fecha a função ggplot() e sinaliza o início de uma próxima.
                                    geom_rect(aes(xmin = (Dark_begin[1]-500), #Adiciona um retângulo que começa em x no valor de Dark_begin -500s para poder se alinhar ao início da barra.
                                                  xmax = (Dark_end[1]+500),   #Termina em x no valor de Dark_end + 500s para poder se alinhar ao fim da barra.
                                                  ymin = -Inf,             #"Começa" em -infinito em y.
                                                  ymax = Inf),             #"Termina" em infinito em y.
                                              fill = "grey",           #Preenchido com a cor cinza.
                                              alpha = 0.5) +           #Com opacidade de 50%.
                                    geom_bar(aes(fill=Temperature),        #Adiciona gráfico de barras, cor de acordo com Temperature.
                                             stat="identity",              #Com altura das barras equivalentes aos seus valores em y. 
                                             width=bar_width) +                 #Largura das barras = 1000 segundos.
                                    scale_fill_manual(values = c("hot" = hot_color,       #dados quentes em Temperature terão a cor escolhida pelo usuário em hot_color.
                                                             "cold" = cold_color,         #dados frios em Temperature terão a cor escolhida pelo usuário em cold_color.
                                                             "constant" = "black")) +  #dados constantes em Temperature serão pretos.
                                    theme_void() +                      #Utilizando o tema pronto void, que tem um padrão de fundo vazio.
                                    geom_hline(yintercept = 0,               #Adiciona linha horizontal no intercepto 0 em y.
                                           linetype="solid", color = "black", size=0.5) +  #Sólida, preta e com espessura 0.5. 
                                    theme(plot.margin= unit(c(0.9,0,0,0), "pt")) +      #Estabelece margens superior, direita, inferior e esquerda, nessa ordem, em pontos.
                                    theme(axis.title=element_blank())  +   #Remove título dos eixos.
                                    theme(axis.text=element_blank())  +    #Remove anotações dos eixos.
                                    theme(axis.ticks=element_blank()) +    #Remove marcações dos eixos.
                                    theme(legend.position="none") +        #Remove legenda.
                                    scale_x_continuous(limits = c(-500,(xmax+500)),    #Estabelece limite do eixo x indo de -500 segundos(metade da largura de uma barra, para que valores existentes às 00:00:00 possam aparecer) a xmax+500(pelo mesmo motivo).
                                                       expand = c(0, 0)) + #Inicia o gráfico na origem zero-zero.
                                    scale_y_continuous(limits = c(0,ymax), #Estabelece limite do eixo y indo de 0 a ymax.
                                                       expand = c(0, 0))   #Inicia o gráfico na origem zero-zero.
                     }                                                     #Finaliza operações indicadas.
          if(get(paste0("df",d))[1,4]>get(paste0("df",d))[1,5])    #Caso a fase de escuro no df do contador (d-1) contenha meia-noite (quando Dark_begin é maior que Dark_end), cria 2 retângulos cinzas ao fundo.
                     {                                                     #Indica as operações a serem seguidas.
                     plots2[[d]] <- ggplot(get(paste0("df",d)),        #Cria um plot a partir de df do número do contador-1, armazenando em plots1.
                                           aes(x =Time, y =Rythm)          #Utilizando a coluna Time como x e Rythm como y.
                                           ) +                             #Fecha a função ggplot() e sinaliza o início de uma próxima.
                                    geom_rect(aes(xmin = -Inf,             #Adiciona um retângulo que começa em x -500 segundos.
                                                  xmax = Dark_end[1],      #Termina em x no valor de D_end.
                                                  ymin = -Inf,             #"Começa" em -infinito em y.
                                                  ymax = Inf),             #"Termina" em infinito em y
                                              fill = "grey",               #Preenchido com a cor cinza.
                                              alpha = 0.5) +               #Com opacidade de 50%.
                                    geom_rect(aes(xmin = Dark_begin[1],    #Adiciona um retângulo que começa em x no valor de Dark_begin.
                                                  xmax = Inf,             #Termina em x no valor 23:59:59. 
                                                  ymin = -Inf,             #"Começa" em -infinito em y.
                                                  ymax = Inf),             #"Termina" em infinito em y.
                                              fill = "grey",               #Preenchido com a cor cinza.
                                              alpha = 0.5) +               #Com opacidade de 50%.
                                    geom_bar(aes(fill=Temperature),        #Adiciona gráfico de barras, cor de acordo com Temperature.
                                             stat="identity",              #Com altura das barras equivalentes aos seus valores em y. 
                                             width=bar_width) +                 #Largura das barras = 1000 segundos.
                                    scale_fill_manual(values = c("hot" = hot_color,       #dados quentes em Temperature terão a cor escolhida pelo usuário em hot_color.
                                                             "cold" = cold_color,         #dados frios em Temperature terão a cor escolhida pelo usuário em cold_color.
                                                             "constant" = "black")) +  #dados constantes em Temperature serão pretos.
                                    theme_void() +                      #Utilizando o tema pronto void, que tem um padrão de fundo vazio.
                                    geom_hline(yintercept = 0,               #Adiciona linha horizontal no intercepto 0 em y.
                                           linetype="solid", color = "black", size=0.5) +  #Sólida, preta e com espessura 0.5. 
                                    theme(plot.margin= unit(c(0.9,0,0,0), "pt")) +      #Estabelece margens superior, direita, inferior e esquerda em pontos.
                                    theme(axis.title=element_blank())  +   #Remove título dos eixos.
                                    theme(axis.text=element_blank())  +    #Remove anotações dos eixos.
                                    theme(axis.ticks=element_blank()) +    #Remove marcações dos eixos.
                                    theme(legend.position="none") +        #Remove legenda.
                                    scale_x_continuous(limits = c(-500,(xmax+500)),    #Estabelece limite do eixo x indo de -500 segundos(metade da largura de uma barra, para que valores existentes às 00:00:00 possam aparecer) a xmax+500 (pelo mesmo motivo).
                                                       expand = c(0, 0)) + #Inicia o gráfico na origem zero-zero.
                                    scale_y_continuous(limits = c(0,ymax), #Estabelece limite do eixo y indo de 0 a ymax.
                                                       expand = c(0, 0))   #Inicia o gráfico na origem zero-zero.         
                     }                                                     #Finaliza as operações indicadas.
           if(get(paste0("df",d))[1,4]==get(paste0("df",d))[1,5])  #Caso o regime de claro/escuro do df seja de claro constante (quando Dark_begin é igual Dark_end), não cria retângulo cinza ao fundo.
                     {                                                     #Indica as operações a serem seguidas.
                     plots2[[d]] <- ggplot(get(paste0("df",d)),        #A partir de df do número do contador-1, armazenando em plots1. Desejamos que o segundo gráfico de plots1 seja referente ao df1, e assim por diante, por isso utilizamos (d-1).  
                                           aes(x =Time, y =Rythm)          #Utilizando a coluna Time como x e Rythm como y.
                                           ) +                             #Fecha a função ggplot() e sinaliza o início de uma próxima.
                                    geom_bar(aes(fill=Temperature),        #Adiciona gráfico de barras, cor de acordo com Temperature.
                                             stat="identity",              #Com altura das barras equivalentes aos seus valores em y. 
                                             width=bar_width) +                 #Largura das barras = 1000 segundos.
                                    scale_fill_manual(values = c("hot" = hot_color,       #dados quentes em Temperature terão a cor escolhida pelo usuário em hot_color.
                                                             "cold" = cold_color,         #dados frios em Temperature terão a cor escolhida pelo usuário em cold_color.
                                                             "constant" = "black")) +  #dados constantes em Temperature serão pretos.
                                    theme_void() +                      #Utilizando o tema pronto void, que tem um padrão de fundo vazio.
                                    geom_hline(yintercept = 0,               #Adiciona linha horizontal no intercepto 0 em y.
                                           linetype="solid", color = "black", size=0.5) +  #Sólida, preta e com espessura 0.5. 
                                    theme(plot.margin= unit(c(0.9,0,0,0), "pt")) +      #Estabelece margens superior, direita, inferior e esquerda em pontos.
                                    theme(axis.title=element_blank())  +   #Remove título dos eixos.
                                    theme(axis.text=element_blank())  +    #Remove anotações dos eixos.
                                    theme(axis.ticks=element_blank()) +    #Remove marcações dos eixos.
                                    theme(legend.position="none") +        #Remove legenda.
                                    scale_x_continuous(limits = c(-500,(xmax+500)),    #Estabelece limite do eixo x indo de -500 segundos(metade da largura de uma barra, para que valores existentes às 00:00:00 possam aparecer) a xmax+500 (pelo mesmo motivo).
                                                       expand = c(0, 0)) + #Inicia o gráfico na origem zero-zero.
                                    scale_y_continuous(limits = c(0,ymax), #Estabelece limite do eixo y indo de 0 a ymax.
                                                       expand = c(0, 0))   #Inicia o gráfico na origem zero-zero.         
                     }                                                     #Finaliza as operações indicadas.
           }                                                               #Finaliza o ciclo (2ª coluna do actograma).
             
          plots1 <- rep(NA,(length(days_contained)+1))  #Para a 1ª "coluna" do actograma, cria o objeto plots1, com (número de days_contained+1) vezes "NA".
          plots1 <- as.list(plots1)                     #Transforma plots1 em lista, para ser usada mais adiante em arrangeGrobs().
          plots1[[1]] <- ggplot() + theme_void() #Cria um gráfico vazio na primeira posição de plots1, típico de actogramas como o que faremos.
          for(d in 2:(length(days_contained)+1))          #Para contador d indo de 2 ao número de dias existentes+1 (Porque nesse caso o gráfico vazio é o primeiro da coluna).
          {                                             #Indica início do ciclo.
          plots1[[d]] <- plots2[[(d-1)]]                #Copia para a posição d em plots1 o plot já criado em plots2, que está deslocado uma linha para cima na posição d-1 de plots2.
          }                                             #Indica fim do ciclo (1ª coluna do actograma).
#- - -
          #Adicionando escalas externas e legenda
             #Régua lateral em dias:
                  x1 <- rep(1,(length(days_contained)+2))                 #Cria objeto x1 com valores de 1 para número de dias existentes + 2 (para as pontas). Esses valores serão a base para criar os traços horizontais da régua lateral.                 
                  y1 <- rev(seq(0,(ymax*(length(days_contained)+1)),by=ymax)) #Cria y1 com sequência reversa de 0 até ymax*(length(days_contained)+1), ou seja, de 0 até a altura méxima dos gréficos para cada dia empilhados, intervalados pelo valor de ymax. 
                  scale1 <- data.frame(x1,y1)                              #Une x1 e y1 em um dataframe scale1.
                  scale_days <- ggplot(scale1,aes(x=x1,y=y1)) +            #Cria o gráfico scale_days, com base em x1 e y1 de scale1.
                               geom_segment(x=0.75,xend=1,                 #Adiciona linhas com 0.25 de tamanho, no canto direito.
                                            y=y1,yend=y1,                  #Horizontais, com mesmo y cada.
                                            linetype="solid",              #Do tipo linha sólida.
                                            color = "black",               #Na cor preta.
                                            size=0.4)+                     #Espessura 0.4.
                               geom_vline(xintercept = 0.75,               #Adiciona segmento vertical no intercepto 0.75 em x.
                                          linetype="solid",                #Do tipo linha sólida.
                                          color = "black",                 #Na cor preta.
                                          size=0.5) +                      #Espessura 0.5.
                               theme_void() +                              #Utilizando o tema pronto void, que tem um padrão vazio.
                               theme(plot.margin=unit(c(0,5,0,7), "pt"))+ #Estabelece margens superior, direita, inferior e esquerda em pontos.
                               theme(axis.title=element_blank())  +        #Remove título dos eixos.
                               theme(axis.text=element_blank())  +         #Remove anotações dos eixos.
                               theme(axis.ticks=element_blank()) +         #Remove marcações dos eixos.
                               scale_x_continuous(limits = c(0,1),         #Estabelece limite do eixo x indo de 0 a 1.
                                                  expand = c(0,0)) +       #Inicia o gráfico na origem zero-zero.
                               scale_y_continuous(limits = c(0,(ymax*(length(days_contained)+1))), #Estabelece limite do eixo y indo de 0 a altura máxima dos gráficos empilhados.
                                                  expand = c(0, 0)) +      #Inicia o gráfico na origem zero-zero.
                               annotate("text", x = 0.15, y = ((ymax*(length(days_contained)+1))/2), #Adiciona texto na posição x=0.15 e y = metade da altura máxima dos gráficos empilhados. 
                                        label = y_title,                   #Escreve o y_title escolhido, default 'Day of experiment'.
                                        angle = 90,                        #Rotacionada a 90°.
                                        size = y_title_size,               #Com tamanho escolhido pelo usuário.
                                        family = graph_font)               #Fonte = graph_font.
                  for(d in (length(days_contained):0))                     #Cria ciclo para escrever o número do dia ao lado de cada gráfico, indo do número referente a quantidade de dias no gráfico até 0.
                     {                                                     #Inicia o ciclo.
                     scale_days <- scale_days +                            #Adiciona ao gráfico scale_days.
                                  annotate("text", x = 0.55, y = (((d)*ymax)+(ymax/2)), #Texto em x=0.55 e y= meio da altura do dia na pilha dos gráficos, que é seu ponto + metade de sua altura máxima de y.
                                  label = paste0((length(days_contained)+(first_day-1))-d),#Escreve o número do dia, que é obtido pela expressão (length(days_contained)+(first_day-1)) - o contador. Se first_day =1 (default) por exemplo, isso gera um resultado de escrita 0, 1, 2, 3(...) de baixo para cima, pois o contador começa escrevendo no alto.
                                  size = y_lables_size,                    #Com tamanho escolhido pelo usuário.
                                  family=graph_font)                       #Fonte = graph_font.
                     }                                                     #Finaliza o ciclo.
                  #Régua inferior em horas:
                  x2 <- seq(0,48)                                          #Cria objeto x2 com valores de 0 a 48. 
                  y2 <- rep(1,49)                                          #Cria objeto y2 com 49 vezes de 1. Esses valores serão a base para criar os traços verticais da régua inferior.
                  scale2 <- data.frame(x2,y2)                              #Une x2 e y2 em um dataframe scale2.
                  scale_hours <- ggplot(scale2,aes(x=x2,y=y2)) +           #Cria o gráfico scale_hours, com base em x2 e y2 de scale2.
                                geom_segment(x=x2,xend=x2,y=0.7,yend=0.85, #Adiciona linhas horizontais de altura 0.15.
                                             linetype="solid", color = "black", size=0.4)+ #Sólidas, pretas e com espessura 0.5.
                                geom_segment(x=0,xend=0,y=0.7,yend=1,     #Adiciona linha acima do anterior na posição x=0.
                                             linetype="solid", color = "black", size=0.4)+ #Sólida, preta e com espessura 0.5.
                                geom_segment(x=24,xend=24,y=0.7,yend=1,   #Adiciona linha acima do anterior na posição x=24.
                                             linetype="solid", color = "black", size=0.4)+ #Sólida, preta e com espessura 0.5.
                                geom_segment(x=48,xend=48,y=0.7,yend=1,   #Adiciona linha acima do anterior na posição x=48.
                                             linetype="solid", color = "black", size=0.4)+ #Sólida, preta e com espessura 0.5.
                                geom_hline(yintercept = 0.7,               #Adiciona linha horizontal no intercepto 0.7 em y.
                                           linetype="solid", color = "black", size=0.5) +  #Sólida, preta e com espessura 0.5.
                                theme_void() +                             #Utilizando o tema pronto void, que tem um padrão vazio.
                                theme(plot.margin=unit(c(5,0,0,0), "pt"))+#Estabelece margens superior, direita, inferior e esquerda em pontos.
                                theme(axis.title=element_blank())  +       #Remove título dos eixos.
                                theme(axis.text=element_blank())  +        #Remove anotações dos eixos.
                                theme(axis.ticks=element_blank()) +        #Remove marcações dos eixos.
                                scale_x_continuous(limits = c(0,48),       #Estabelece limite do eixo x indo de 0 a 48.
                                                   expand = c(0,0)) +      #Inicia o gráfico na origem zero-zero.
                                scale_y_continuous(limits = c(0,1),        #Estabelece limite do eixo y indo de 0 a 1.
                                                   expand = c(0, 0)) +     #Inicia o gráfico na origem zero-zero.
                                annotate("text", x = (length(x2)/2), y = 0.2, #Adiciona texto com x no meio da régua e y=0.2.
                                         label = x_title,size=x_title_size,family=graph_font) + #Escreve x_title (default 'Time (Zt)') em tamanho e fonte escolhidos pelo usuário, que significa Zeitgeber time na área de Cronobiologia.
                                annotate("text", x = 0.3, y = 0.55,           #Adiciona texto com x=0.3 da régua e y=0.55 (Porque não aparece se for escrito em x=0 por conta dos limites do gráfico).
                                         label = "0",size=x_lables_size,family=graph_font) + #Escreve '0' em tamanho e fonte escolhidos pelo usuário.
                                annotate("text", x = 47.6, y = 0.55,          #Adiciona texto com x=47.6 da régua e y=0.55 (Porque não aparece se for escrito em x=48 por conta dos limites do gráfico).
                                         label = "48",size=x_lables_size,family=graph_font)  #Escreve '48' em tamanho e fonte escolhidos pelo usuário.
                  for(d in (c(12,24,36)))                                  #Cria ciclo para a escrever a hora nas posições 12, 24 e 36.
                     {                                                     #Indica início do ciclo.
                     scale_hours <- scale_hours +                          #Adiciona ao gráfico scale_hours.
                                   annotate("text", x = (d), y = 0.55,     #Texto em x no valor do contador e y=0.55.
                                            label = paste0(d),             #Escreve o valor do contador.
                                            size=x_lables_size,            #Com tamanho escolhido pelo usuário.
                                            family=graph_font)             #Com fonte = graph_font.
                     }                                                     #Finaliza o ciclo.
                  #Legenda:
                  x3 <- seq(0,48)                                          #Cria objeto x3 com valores de 0 a 48. 
                  y3 <- rep(1,49)                                          #Cria objeto y3 com 49 vezes de 1. Esses valores serão a base para criar os traços verticais da régua inferior.
                  scale3 <- data.frame(x3,y3)                              #Une x3 e y3 em um dataframe scale3.
                  legend <- ggplot(scale3,aes(x=x3,y=y3)) +                #Cria o gráfico legend, com base em x3 e y3 de scale3.
                             theme_void() +                                #Utilizando o tema pronto void, que tem um padrão vazio.
                             theme(plot.margin=unit(c(0,0,5,0), "pt"))+    #Estabelece margens superior, direita, inferior e esquerda em pontos.
                             theme(axis.title=element_blank()) +           #Remove título dos eixos.
                             theme(axis.text=element_blank())  +           #Remove anotações dos eixos.
                             theme(axis.ticks=element_blank()) +           #Remove marcações dos eixos.
                             scale_x_continuous(limits = c(0,48),          #Estabelece limite do eixo x indo de 0 a 48.
                                                expand = c(0,0)) +         #Inicia o gráfico na origem zero-zero.
                             scale_y_continuous(limits = c(0,1),           #Estabelece limite do eixo y indo de 0 a 1.
                                                expand = c(0, 0)) +        #Inicia o gráfico na origem zero-zero.
                             annotate("text", x=6, y=0.8,                  #Texto em x=5 e y=0.8.
                                      label = "Photoperiodic Regime:",     #Escreve "Photoperiodic Regime:".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font)                   #Com fonte = graph_font.
                  constant_light <- mydata[,4] == mydata[,5]               #Cria o objeto constant_light, que contém os resultados lógicos da igualdade entre os elementos de Dark_beg e Dark_end.
                  if(sum(constant_light) == nrow(mydata))                  #Se a quantidade de linhas em que a luz é constante é a mesma de mydata (todos os regimes nos dados são de luz constante).
                    {                                                      #Inicia o ciclo.
                      legend <- legend +                                   #Adiciona ao gráfico legend
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0.4,                      #De espessura 0.4
                                          fill = "white" ) +               #Preenchido em branco.
                                annotate("text", x=9, y=0.55,              #Texto em x no valor 9 e y=0.55.
                                      label = "Constant light (L:L)",      #Escreve "Constant light (L:L)".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font)                   #Com fonte = graph_font.  
                    }                                                      #Finaliza o ciclo.
                  constant_dark <- mydata[,4]==as_hms("00:00:00") & mydata[,5]==as_hms("23:59:59") #Cria o objeto constant_dark, que contém os resultados lógicos para Dark_beg = "00:00:00" e Dark_end = "23:59:59".
                  if(sum(constant_dark) == nrow(mydata))                  #Se a quantidade de linhas em que a luz é constante é a mesma de mydata (todos os regimes nos dados são de luz constante).
                    {                                                      #Inicia o ciclo.
                      legend <- legend +                                   #Adiciona ao gráfico legend
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0.4,                      #De espessura 0.4
                                          fill = "grey" ) +                #Preenchido em cinza.
                                annotate("text", x=9.1, y=0.55,            #Texto em x no valor 9.1 e y=0.55.
                                      label = "Constant dark (D:D)",       #Escreve "Constant dark (D:D)".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font)                   #Com fonte = graph_font.  
                    }                                                      #Finaliza o ciclo.
                  if(sum(constant_light)==0 & sum(constant_dark)==0)       #Se não houver nenhuma linha de claro constante nem de escuro constante
                    {                                                      #Inicia o ciclo.
                      legend <- legend +                                   #Adiciona ao gráfico legend
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0.4,                      #De espessura 0.4
                                          fill = "white",                  #Sem preenchimento. 
                                          alpha = 1 ) +                    #Opacidade 100%
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 3.25,                 #Termina em x no valor 3.25. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0,                        #De espessura 0.
                                          fill = "white" ) +               #Preenchido em branco.
                                geom_rect(aes(xmin = 3.25,                 #Um retângulo que começa em x no valor 3.25.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0,                        #De espessura 0.
                                          fill = "grey" ) +                #Preenchido em cinza. 
                                annotate("text", x=8.6, y=0.55,            #Texto em x no valor 8.6 e y=0.55.
                                      label = "Light-Dark (L:D)",          #Escreve "Light-Dark (L:D)".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font)                   #Com fonte = graph_font.  
                    }                                                      #Finaliza o ciclo.
                  if(sum(constant_light)>0 & sum(constant_dark)==(nrow(mydata)-sum(constant_light))) #Se houver regimes de LL e DD apenas
                  {                                                      #Inicia o ciclo.
                      legend <- legend +                                   #Adiciona ao gráfico legend
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0.4,                      #De espessura 0.4
                                          fill = "white" ) +               #Preenchido em branco.
                                annotate("text", x=9, y=0.55,              #Texto em x no valor 9 e y=0.55.
                                      label = "Constant light (L:L)",      #Escreve "Constant light (L:L)".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font) +                 #Com fonte = graph_font.
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.25,                 #Começa em em y 0.25.
                                              ymax = 0.45),                #Termina em em y 0.45.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0.4,                      #De espessura 0.4
                                          fill = "grey" ) +                #Preenchido em cinza.
                                annotate("text", x=9.1, y=0.35,            #Texto em x no valor 9.1 e y=0.35.
                                      label = "Constant dark (D:D)",       #Escreve "Constant dark (D:D)".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font)                   #Com fonte = graph_font.   
                    }                                                      #Finaliza o ciclo.
                  if(sum(constant_light)>0 & sum(constant_light)<nrow(mydata) & sum(constant_dark)==0) #Se houver regimes de LL e LD apenas
                    {                                                      #Inicia o ciclo.
                      legend <- legend +                                   #Adiciona ao gráfico legend
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0.4,                      #De espessura 0.4
                                          fill = "white",                  #Sem preenchimento. 
                                          alpha = 1 ) +                    #Opacidade 100%
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 3.25,                 #Termina em x no valor 3.25. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0,                        #De espessura 0.
                                          fill = "white" ) +               #Preenchido em branco.
                                geom_rect(aes(xmin = 3.25,                 #Um retângulo que começa em x no valor 3.25.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0,                        #De espessura 0.
                                          fill = "grey" ) +                #Preenchido em cinza. 
                                annotate("text", x=8.6, y=0.55,            #Texto em x no valor 8.6 e y=0.55.
                                      label = "Light-Dark (L:D)",          #Escreve "Light-Dark (L:D)".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font) +                 #Com fonte = graph_font.
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.25,                 #Começa em em y 0.25.
                                              ymax = 0.45),                #Termina em em y 0.45.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0.4,                      #De espessura 0.4
                                          fill = "white" ) +               #Preenchido em branco.
                                annotate("text", x=9, y=0.35,              #Texto em x no valor 9 e y=0.35.
                                      label = "Constant light (L:L)",      #Escreve "Constant light (L:L)".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font)                   #Com fonte = graph_font.  
                    }                                                      #Finaliza o ciclo. 
                  if(sum(constant_dark)>0 & sum(constant_dark)<nrow(mydata) & sum(constant_light)==0) #Se houver regimes de DD e LD apenas
                    {                                                      #Inicia o ciclo.
                      legend <- legend +                                   #Adiciona ao gráfico legend
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0.4,                      #De espessura 0.4
                                          fill = "white",                  #Sem preenchimento. 
                                          alpha = 1 ) +                    #Opacidade 100%
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 3.25,                 #Termina em x no valor 3.25. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0,                        #De espessura 0.
                                          fill = "white" ) +               #Preenchido em branco.
                                geom_rect(aes(xmin = 3.25,                 #Um retângulo que começa em x no valor 3.25.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0,                        #De espessura 0.
                                          fill = "grey" ) +                #Preenchido em cinza. 
                                annotate("text", x=8.6, y=0.55,            #Texto em x no valor 8.6 e y=0.55.
                                      label = "Light-Dark (L:D)",          #Escreve "Light-Dark (L:D)".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font) +                 #Com fonte = graph_font.
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.25,                 #Começa em em y 0.25.
                                              ymax = 0.45),                #Termina em em y 0.45.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0.4,                      #De espessura 0.4
                                          fill = "grey" ) +                #Preenchido em cinza.
                                annotate("text", x=9.1, y=0.35,            #Texto em x no valor 9.1 e y=0.35.
                                      label = "Constant dark (D:D)",       #Escreve "Constant dark (D:D)".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font)                   #Com fonte = graph_font.  
                    }                                                      #Finaliza o ciclo.
                  if(sum(constant_light)>0 &sum(constant_dark)>0 & sum(c(constant_light,constant_dark))<nrow(mydata)) #Se houver regimes de LL, DD e LD.
                    {                                                      #Inicia o ciclo.
                      legend <- legend +                                   #Adiciona ao gráfico legend
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0.4,                      #De espessura 0.4
                                          fill = "white",                  #Sem preenchimento. 
                                          alpha = 1 ) +                    #Opacidade 100%
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 3.25,                 #Termina em x no valor 3.25. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0,                        #De espessura 0.
                                          fill = "white" ) +               #Preenchido em branco.
                                geom_rect(aes(xmin = 3.25,                 #Um retângulo que começa em x no valor 3.25.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.45,                 #Começa em em y 0.45.
                                              ymax = 0.65),                #Termina em em y 0.65.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0,                        #De espessura 0.
                                          fill = "grey" ) +                #Preenchido em cinza. 
                                annotate("text", x=8.6, y=0.55,            #Texto em x no valor 8.6 e y=0.55.
                                      label = "Light-Dark (L:D)",          #Escreve "Light-Dark (L:D)".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font) +                 #Com fonte = graph_font.
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.25,                 #Começa em em y 0.25.
                                              ymax = 0.45),                #Termina em em y 0.45.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0.4,                      #De espessura 0.4
                                          fill = "white" ) +               #Preenchido em branco.
                                annotate("text", x=9, y=0.35,              #Texto em x no valor 9 e y=0.35.
                                      label = "Constant light (L:L)",      #Escreve "Constant light (L:L)".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font) +                 #Com fonte = graph_font.
                                geom_rect(aes(xmin = 2.5,                  #Um retângulo que começa em x no valor 2.5.
                                              xmax = 4,                    #Termina em x no valor 4. 
                                              ymin = 0.05,                 #Começa em em y 0.05.
                                              ymax = 0.25),                #Termina em em y 0.25.
                                          colour = "black",                #Contornado em linha na com a cor preta.
                                          size = 0.4,                      #De espessura 0.4
                                          fill = "grey" ) +                #Preenchido em cinza.
                                annotate("text", x=9.1, y=0.15,            #Texto em x no valor 9.1 e y=0.15.
                                      label = "Constant dark (D:D)",       #Escreve "Constant dark (D:D)".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font)                   #Com fonte = graph_font.  
                    }                                                      #Finaliza o ciclo.  

                  if(missing(cold_beg)==F & missing(cold_end)==F)          #Caso haja cold_beg e cold_end declarados:
                  {                                                        #Indica as operações a serem seguidas.
                  legend <- legend +                                       #Adiciona ao gráfico legenda
                             annotate("text", x=30, y=0.8,                 #Texto em x=30 e y=0.8.
                                      label = "Temperature Regime:",       #Escreve "Temperature Regime:".
                                      size=legend_labels_size,             #Com tamanho escolhido pelo usuário.
                                      family=graph_font)                   #Com fonte = graph_font.
                             if(sum(mydata[,6]!=mydata[,7]) == nrow(mydata)) #Se o número de linhas com cold_beg diferente de cold_end for igual ao número total de linhas (há ciclo hot:cold em todas as linhas)
                             {                                             #Início do ciclo
                              legend <- legend +                           #Adiciona ao gráfico legend
                                        geom_rect(aes(xmin = 26.5,         #Um retângulo que começa em x no valor 26.5.
                                                      xmax = 27.25,        #Termina em x no valor 27.25. 
                                                      ymin = 0.45,         #Começa em em y 0.45.
                                                      ymax = 0.65),        #Termina em em y 0.65.
                                                 colour = "black",         #Contornado em linha na com a cor preta.
                                                 size = 0.4,               #Com tamanho de linha 0.4.
                                                 fill = hot_color) +       #Preenchido com a cor escolhida pelo usuário em hot_color.
                                       geom_rect(aes(xmin = 27.25,         #Adiciona um retângulo que começa em x no valor 27.5.
                                                     xmax = 28,            #Termina em x no valor 28.
                                                     ymin = 0.45,          #Começa em em y 0.45.
                                                     ymax = 0.65),         #Termina em em y 0.65.
                                                 colour = "black",         #Contornado em linha na com a cor preta.
                                                 size = 0.4,               #Com tamanho de linha 0.4.
                                                 fill = cold_color) +      #Preenchido com a cor escolhida pelo usuário em hot_color.
                                       annotate("text", x=33, y=0.55,      #Texto em x no valor 33 e y=0.55.
                                                label = "Hot-Cold (H:C)",  #Escreve "Hot-Cold (H:C)".
                                                size=legend_labels_size,   #Com tamanho escolhido pelo usuário.
                                                family=graph_font)         #Com fonte = graph_font.
                             }                                             #Finaliza o ciclo                  
                             else                                          #Caso contrário (ciclo de temperatura não equivale ao total de linhas)
                             {                                             #Início do ciclo
                                legend <- legend +                         #Adiciona ao gráfico legend
                                          geom_rect(aes(xmin = 26.5,       #Um retângulo que começa em x no valor 26.5.
                                                      xmax = 27.25,        #Termina em x no valor 27.25. 
                                                      ymin = 0.45,         #Começa em em y 0.45.
                                                      ymax = 0.65),        #Termina em em y 0.65.
                                                 colour = "black",         #Contornado em linha na com a cor preta.
                                                 size = 0.4,               #Com tamanho de linha 0.4.
                                                 fill = hot_color) +       #Preenchido com a cor escolhida pelo usuário em hot_color.
                                       geom_rect(aes(xmin = 27.25,         #Adiciona um retângulo que começa em x no valor 27.5.
                                                     xmax = 28,            #Termina em x no valor 28.
                                                     ymin = 0.45,          #Começa em em y 0.45.
                                                     ymax = 0.65),         #Termina em em y 0.65.
                                                 colour = "black",         #Contornado em linha na com a cor preta.
                                                 size = 0.4,                 #Com tamanho de linha 0.4.
                                                 fill = cold_color) +      #Preenchido com a cor escolhida pelo usuário em hot_color. 
                                       annotate("text", x=33, y=0.55,      #Texto em x no valor 33 e y=0.55.
                                                label = "Hot-Cold (H:C)",  #Escreve "Hot-Cold (H:C)".
                                                size=legend_labels_size,   #Com tamanho escolhido pelo usuário.
                                                family=graph_font) +       #Com fonte = graph_font.
                                          geom_rect(aes(xmin = 26.5,       #Um retângulo que começa em x no valor 26.5.
                                                      xmax = 28,           #Termina em x no valor 28. 
                                                      ymin = 0.25,         #Começa em em y 0.25.
                                                      ymax = 0.45),        #Termina em em y 0.45.
                                                 colour = "black",         #Contornado em linha na com a cor preta.
                                                 fill = "black" ) +        #Preenchido em preto.
                                          annotate("text", x=34, y=0.35,   #Texto em x no valor 34 e y=0.35.
                                                   label = "Constant temperature",#Escreve "Constant temperature".
                                                   size=legend_labels_size,#Com tamanho escolhido pelo usuário.
                                                   family=graph_font)      #Com fonte = graph_font.     
                                }                                          #Fim do ciclo                                        
                  }                                                        #Finaliza as operações indicadas

#- - -
          #Desenhar o gráfico final
          acto1 <- arrangeGrob(grobs=plots1, layout_matrix=mat_layout)     #Faz o arranjo dos objetos gráficos contidos em plots1 de acordo com a mat_layout e guarda em acto1.
          acto2 <- arrangeGrob(grobs=plots2, layout_matrix=mat_layout)     #Faz o arranjo dos objetos gráficos contidos em plots2 de acordo com a mat_layout e guarda em acto2.
          title <- ggplot(scale3,aes(x=x3,y=y3)) +                         #Cria o gráfico title, com base em x3 e y3 de scale3.
                             theme_void() +                                #Utilizando o tema pronto void, que tem um padrão vazio.
                             theme(plot.margin=unit(c(0,0,3,0), "pt"))+    #Estabelece margens superior, direita, inferior e esquerda em pontos.
                             theme(axis.title=element_blank()) +           #Remove título dos eixos.
                             theme(axis.text=element_blank())  +           #Remove anotações dos eixos.
                             theme(axis.ticks=element_blank()) +           #Remove marcações dos eixos.
                             scale_x_continuous(limits = c(0,48),          #Estabelece limite do eixo x indo de 0 a 48.
                                                expand = c(0,0)) +         #Inicia o gráfico na origem zero-zero.
                             scale_y_continuous(limits = c(0,1),           #Estabelece limite do eixo y indo de 0 a 1.
                                                expand = c(0, 0)) +        #Inicia o gráfico na origem zero-zero.
                             annotate("text", x=24, y=0.5,                 #Texto em x=24 e y=0.5.
                                      label = graph_title,                 #Escreve graph_title fornecido pelo usuário.
                                      size=graph_title_size,               #Com tamanho escolhido pelo usuário.
                                      family=graph_font)                   #Com fonte = graph_font.
           #Se quisermos colocar todos os elementos
             # NA           titulo        titulo
             # scale_days   acto1         acto2
             # NA           scale_hours   scale_hours
             # NA           legend        legend
             if(missing(graph_title)==F & plot_legend==TRUE)               #Se houver definição para graph_title pelo usuário e legenda mantida (default)
             {                                                             #Inicia o ciclo 
             grid.arrange(title,scale_days,acto1,acto2,scale_hours,legend, #Desenha os arranjos seguindo a ordem escrita.
                       widths=c(2,10,10),                                  #Com larguras de 2(scale_days) e 10(title, acto1, acto2 e legend).
                       heights=c(1.5,10,1.5,1.5),                          #Com alturas de 10(scale_days, acto1 e acto2) e 1.5(title,scale_hours e legend)
                       layout_matrix =cbind(c(NA,2,NA,NA),c(1,3,5,6),c(1,4,5,6)), #Na ordem da matriz cbind(c(NA,2,NA,NA),c(1,3,5,6),c(1,4,5,6)).
                       right="")                                           #Cria espaço em branco à direita da grade.  
             }                                                             #Finaliza o ciclo

             if(missing(graph_title)==T & plot_legend==TRUE)               #Caso não haja graph_title e plot_legend mantida (default)
             {                                                             #Inicia o ciclo 
               grid.arrange(scale_days,acto1,acto2,scale_hours,legend,     #Desenha os arranjos seguindo a ordem escrita.
                       widths=c(2,10,10),                                  #Com larguras de 2(scale_days) e 10(acto1, acto2 e legend).
                       heights=c(10,1.5,1.5),                              #Com alturas de 10(scale_days, acto1 e acto2) e 1.5(scale_hours e legend)
                       layout_matrix =cbind(c(1,NA,NA),c(2,4,5),c(3,4,5)), #Na ordem da matriz cbind(c(1,NA,NA),c(2,4,5),c(3,4,5)).
                       top="",right="")                                    #Cria espaço em branco acima e à direita da grade.   
             }                                                             #Finaliza o ciclo

             if(missing(graph_title)==F & plot_legend==FALSE)              #Caso haja graph_title mas não legenda
             {                                                             #Inicia o ciclo 
             grid.arrange(title,scale_days,acto1,acto2,scale_hours,        #Desenha os arranjos seguindo a ordem escrita.
                       widths=c(2,10,10),                                  #Com larguras de 2(scale_days) e 10(title, acto1 e acto2).
                       heights=c(1.5,10,1.5),                              #Com alturas de 10(scale_days, acto1 e acto2) e 1.5(title e scale_hours)
                       layout_matrix =cbind(c(NA,2,NA),c(1,3,5),c(1,4,5)), #Na ordem da matriz cbind(c(NA,2,NA),c(1,3,5),c(1,4,5)).
                       right="")                                           #Cria espaço em branco à direita da grade.   
             }                                                             #Finaliza o ciclo

             if(missing(graph_title)==T & plot_legend==FALSE)              #Caso não haja graph_title nem legenda
             {                                                             #Inicia o ciclo 
             grid.arrange(scale_days,acto1,acto2,scale_hours,              #Desenha os arranjos seguindo a ordem escrita.
                       widths=c(2,10,10),                                  #Com larguras de 2(scale_days) e 10(title, acto1 e acto2).
                       heights=c(10,1.5),                                  #Com alturas de 10(scale_days, acto1 e acto2) e 1.5(title e scale_hours)
                       layout_matrix =cbind(c(1,NA),c(2,4),c(3,4)),        #Na ordem da matriz cbind(c(1,NA),c(2,4),c(3,4)).
                       top="",right="")                                    #Cria espaço em branco acima e à direita da grade.   
             }                                                             #Finaliza o ciclo
          
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
#Retornando os resultados no terminal
#message("\n--- Doubleplot succeeded! ---\n\n","Actogram for data contained in: ", length(days_contained), " days.\n") #Cria o objeto num_of_days com a frase que indica a quantidade de dias no n_lines fornecido pelo usuário.

         light <- rep(NA,length(days_contained))  #Cria o objeto light, com NA vezes o número de dias existentes.
         dark <- rep(NA,length(days_contained))   #Cria o objeto dark, com NA vezes o número de dias existentes.
         for(d in 1:(length(days_contained)))     #Cria ciclo com contador d indo de 1 ao número de dias existentes.
             {                                    #Indica in�cio do ciclo.
             dark[d] <- round(abs((unique(get(paste0("df",(d)))[,5])-unique(get(paste0("df",(d)))[,4]))/3600)) #Subtrai o horário de Dark_begin de Dark_end daquele dia (que devem ser os mesmos para todas as linhas do df em questão, por isso podemos usar unique), divide por 3600 para transformar em horas, transforma em valor absoluto e arredonda, para chegar ao valor de duração do escuro em horas. Atribui essa duração à posição dark[d] referente a data em que ocorre.
             light[d] <- 24-dark[d]               #Calcula a duração do claro para cada data a partir da complementariedade nas 24h diárias, e guarda em light na posição da data correspondente.
             }                                    #Finaliza o ciclo.
         Photo_Regime <- data.frame(days_contained,paste0(light,":",dark)) #Cria um novo dataframe Photo_Regime, com as datas contidas em days_contained e as durações de horas de claro e de escuro dessa data combinadas, expressas no formato "(duração de claro em horas):(duração de escuro em horas)", tradicionalmente escrito na cronobiologia como "L:D".
          colnames(Photo_Regime) <- c("Date","Light:Dark")         #Altera os nomes das colunas no dataframe para "Date" e"L:D".
          
if(missing(cold_beg)==T & missing(cold_end)==T)   #Se não houver cold_beg ou cold_end declarados:
          {                                       #Indica as operações a serem feitas.
          final_text <- noquote(capture.output(cat("--- Doubleplot succeeded! ---\n\n",
          "Actogram for data contained in: ", length(days_contained), " days.\n\n",
          "No Hot-Cold cycle(s) declared.\n",
          "You can check the Light-Dark Regime(s) in the generated table.\n\n",
          "--- End ---\n")))                                                                       #Cria em texts três frases, pulando linhas.
          #regimes <- print(kable(Photo_Regime, align = "c"))#Imprime no terminal uma tabela, com | separando colunas, do dataframe Photo_Regime, armazenado em regimes.
          View(Photo_Regime)
          } else                                  #Finaliza as operações indicadas.Do contário:
          {                                       #Indica as operações a serem feitas.
          hot <- rep(NA,length(days_contained))   #Cria o objeto hot, com NA vezes o número de dias existentes.
          cold <- rep(NA,length(days_contained))  #Cria o objeto cold, com NA vezes o número de dias existentes.
          for(d in 1:(length(days_contained)))    #Cria ciclo com contador d indo de 1 ao número de dias existentes.
             {                                    #Indica início do ciclo.
             cold[d] <- round(abs((unique(get(paste0("df",(d)))[,7])-unique(get(paste0("df",(d)))[,6]))/3600)) #Subtrai o á de Cold_begin de Cold_end daquele dia (que devem ser os mesmos para todas as linhas do df em questão, por isso podemos usar unique), divide por 3600 para transformar em horas, transforma em valor absoluto e arredonda, para chegar ao valor de duração do frio em horas. Atribui essa duração à posição cold[d] referente a data em que ocorre.
             hot[d] <- 24-cold[d]                 #Calcula a duração do hot para cada data a partir da complementariedade nas 24h diárias, e guarda em hot na posição da data correspondente.
             }                                    #Finaliza o ciclo.
          Photo_Temperature_Regime <- data.frame(days_contained,paste0(light,":",dark),paste0(hot,":",cold)) #Cria um novo dataframe, com as datas contidas em days_contained e as durações de horas de claro e de escuro (L:D) dessa data e as durações das horas de quente e de frio combinadas expressas no formato "(duração de quente em horas):(duração de frio em horas)", tradicionalmente escrito na cronobiologia como "H:C".
          colnames(Photo_Temperature_Regime) <- c("Date","Light:Dark","Hot:Cold")#Altera os nomes das colunas no dataframe para "Date", "L:D" e "H:C".
          final_text <- noquote(capture.output(cat("--- Doubleplot succeeded! ---\n\n",
          "Actogram for data contained in: ", length(days_contained), " days.\n\n",
          "You can check the Light-Dark and Hot-Cold Regimes in the generated table.\n\n", 
          "--- End ---\n")))  #Cria o objeto texts para armazenar a frase escrita.
          #regimes <- print(kable(Photo_Temperature_Regime, align = "c")) #Imprime no terminal uma tabela, com | separando colunas, do dataframe Photo_Temperature_Regime, armazenado em regimes.
          View(Photo_Temperature_Regime)
          }                                       #Finaliza as operações indicadas.

#results <- c(num_of_days, texts)    #Cria o objeto results para armazenar num_of_days, phrases e regimes numa lista.
return(final_text)                                #Retorna results no terminal.
          }
