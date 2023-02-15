# Estatística Computacional         #
# Curso: Estatística                #
# Prof. Ricardo                     #
#-----------------------------------#

tosscoin <- function(times)
{
  temp <- list()
  
  for (i in 1:times)
  {
    temp[[i]] <- c("C","K")
  }
  
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE) #manter distante os outros atributos
  
  names(res) <- c(paste(rep("Lan�amento: ", times), 1:times, sep = "")) ## cria o dataframe e renomeia as colunas
  
  return(res)
}

# testando a fun�ao
x <- tosscoin(3)
head(x)



####### Atividades 

# Exerc�cio 1. 
# Crie uma rotina em R para descrever o espa�o amostral referente ao experimento de lan�ar dados em duas
# situa��es: a primeira deve envolver os lan�amentos de um dado, e a segunda deve envolver o lan�amento de dois ou mais
# dados.


# Lan�amento de um dado
dado <- function(times)
{
  eventos <- list()
  
  for (i in 1:times)
  {
    eventos[[i]] <-c("I","II","III","IV","V","VI")
  }
  
  resposta <- expand.grid(eventos, KEEP.OUT.ATTRS = FALSE) #manter distante os outros atributos
  
  names(resposta) <- c(paste(rep("Lan�amento: ", times), 1:times, sep = "")) ## cria o dataframe e renomeia as colunas
  
  return(resposta)
  
}

y <- dado(2)

# Lan�amento de dois ou mais dados
dados < - function(times,lancamento)
{
  evento <- list()
  
  for (j in 1:lancamento)
  {
    for (i in 1:times)
  }
  vezes[[i]] <-c("I","II","III","IV","V","VI")
  
}




# Exerc�cio 2.
# Crie uma rotina em R para descrever o espa�o amostral referente ao experimento de retirar uma carta de um baralho 
# em duas situa��es: a primeira considerando o baralho padr�o de 52 cartas, e a segunda considerando os dois "coringas"
# que formam um baralho de 54 cartas.








# Defini��o Matem�tica de Probabilidade
# Exemplo de um baralho
cards <- function(jokers =  FALSE, makespace = FALSE)
{
  x <- c(2:10, "J","Q","K","A")
  y <- c("Club","Diamond","Heart","Spade")
  res <- expand.grid(rank = x, suit = y)
  if (jokers){
    levels(res$rank) <- c(levels(res$rank),"Jokers") #indica os niveis da vari�vel
    res <- rbind(res, data.frame(rank = c("Joker", "Joker"),
                                 suit = c(NA,NA)))
  }
  if (makespace){
    res$probs <- rep(1, dim(res)[1])/dim(res)[1]
  }
  return(res)
}

# Espa�o de probabilidade do experimento
S <- cards(makespace = TRUE)

#Evento A: Todas as cartas de copas
A <- subset(S, suit =="Heart")

# Evento B: Todas as cartas com valores 7 a 9
B <- subset(S, rank)


# Fun��o para o c�lculo de probabilidades baseadas nos Axiomas de Kolmogorov

prob <- function(x, event= NULL, ...)
{
  if (is.null(x$probs))
  {
    
    message("O espa�o de probabilidades n�o cont�m a coluna das probabilidades")
    stop("Reveja o espa�o de probabilidade constru�do")
  }
  if(missing(event)) # se estiver faltando o evento
  {
    
    r <- TRUE # n�o colocou o evento. o r coloca o evento empirico
  }
  else
  {
    e <- substitute(event)
    r <- eval(e,x, parent.frame())
    if(!is.logical(r))
      stop("'event' deve ser avaliado como l�gico")
    r <- r & !is.na(r)
    if(!isTRUE(all.equal(sum(x$probs),1)))
      warning("'space' n�o tem probabilidade igual a 1.")
  }
  
  A < - x[r, ]
  
  p <- sum(A$probs)
  
  return(p)
}

# Atividades

# Exerc�cio 3
# Considere o experimento do lan�amento de dois dados. Crie uma rotina em R para descrever o espa�o de probabilidade 
# desse experimento, e , usando a fun��o 'prob' calcule a probabilidade do evento X= "a soma dos valores obtidos nas
# faces dos dados � menor do que 7".

# Exerc�cio 4
# Consisdere o experimento do lan�amento de uma moeda 5 vezes. Crie uma rotina em R para descrever o espa�o de 
# probabilidade desse experimento, e, usando a fun��o 'prob' calcule a probabilidade do evento Y = "obter pelo menos 5
# faces caras no lan�amento de uma moeda 5 vezes".


### Proxima aula: probbilidade condicional
