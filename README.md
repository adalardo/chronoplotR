# R-chronoplot-package
Pacote que contém o código da função doubleplot(), que permite representar ritmos biológicos em conjunto com informações ambientais de claro-escuro e quente-frio, em actograma duplo.

## O que a função faz?
Retorna graficamente dados biológicos ritmicos no formato típico de actograma, comumente utilizado na área de estudos cronobiológicos.
O actograma permite combinar os dados de um mesmo indivíduo a diferentes regimes ambientais de fotoperíodo (obrigatório) e de temperatura(opcional). 
A função retorna:
  1. Actograma em duplo-plot
  2. Número de dias presente no actograma gerado
  3. Tabela detalhando os regimes cícilos de claro-escuro e de quente-frio declarados para cada data.
  
## Pseudocódigo
  ### Etapas de verificação
	1. Igualdade de tamanho dos vetores de data-hora e de ritmo
	2. Transformação correta do 'timestamp'
	3. Presença de valores negativos no vetor de ritmo
	4. Valor positivo e maior que zero no número de linhas que devem ser lidas pela função (escolhido pelo usuário)
	5. Transformação correta dos horários fornecidos para ciclos de claro-escuro (obrigatório) e correspondência com número de linhas escolhido pelo usuário  
	6. Transformação correta dos horários fornecidos para ciclos de quente-frio (opcional) e correspondência com número de linhas escolhido pelo usuário
	
  ### Manipulação dos dados fornecidos pelo usuário
	1. Organiza os vetores fornecidos pelo usuário num dataframe de ordem conhecida
	2. Caso informado ciclo de quente-frio, atribui-se a classificação da fase para cada linha do conjunto de dados
	
  ### Desenho do actograma
	1. Definir layout e valores máximos e mínimos dos eixos para o actograma
	2. Desenho do primeiro plot
	3. Desenho do segundo plot, que repete o primeiro plot, deslocado
	
  ### Desenho da legenda
	1. Escala externa, à esquerda, do número de dias no contidos na imagem
	2. Escala externa, inferior, das horas do dia (Zt)
	3. Legenda identificando os regimes de fotoperíodo possíveis (CC, CE, EE) e, quando declarado, o regime de temperatura
	
  ### Retorno de resultados
	1. Organização da imagem final do actograma duplo com escalas e legendas
	2. Número de dias contidos no actograma são informados no terminal
	3. Tabela é gerada detalhando os regimes cícilos de claro-escuro e de quente-frio declarados para cada data
