# Teste se a soma de n valores entre 0 e 1 ao quadrado
# é maior ou igual a 1 sobre n, sendo n = tamanho da amostra.
# 
# Obs: Caso queira vizualizar os valores resultantes basta transformar os `if` 
# em comentarios, e retornar os atuais comentarios para linhas de codigo,
# (com excessao deste texto).

testeS <- function(n){
  x <- runif(n)
  Soma.p <- function(p){
    s <- 0
    for (v in p) {
      s <- s + v^2
      
    }
    if(s >= 1/n){
      return(T)
    }else{
      return(F)
    }
    #return(s)                
  }
  
  #print(paste("Soma do quadrado de valores  = ",Soma.p(x/sum(x))))
  #print(paste("Resultado de 1/n = ",1/n))
  print(paste("Soma do quadrado de p é maior que 1/n ?",Soma.p(x/sum(x))))
  }
testeS(99999)