#Verifica se a porcentagem do numero de vezes em que a media populacional cai dentro
#dos intervalos de confiança condiz com o nível de confiança desejado. 'dist.normal

#niveis de confiança:
#90% -> intervalo1 c(0.05), intervalo2 c(0.95)
#95% -> intervalo1 c(0.025), intervalo2 c(0.975)
#99% -> intervalo1 c(0.005), intervalo2 c(0.995)

med.porcent.dentro <- function(realizaçoes){
  
  porcent.dentro <- function(n,m,v){
    #contadores
    ta.dentro <- 0
    ta.fora <- 0
    
    for(intervalo in 1:100){
      normal <- rnorm(n,m,v)
      media <- mean(normal)
      variancia <- var(normal)
      intervalo1 <- media+qt(c(0.025),df=n-1)*sqrt(variancia/
                                                     length(normal))
      intervalo2 <- media+qt(c(0.975),df=n-1)*sqrt(variancia/
                                                     length(normal))
      
      #checar se a media da distribuição esta no intervalo
      if(m > intervalo1 & m < intervalo2){
        
        ta.dentro <- ta.dentro + 1
      }
      else{
        ta.fora <- ta.fora +1
      }
      
    }
    
    #retorna a porcentagem de vezes que a media de entrada pertence aos intervalos
    return((ta.dentro/(ta.dentro + ta.fora))*100)    
  }
  
  s <- c() #vetor nulo para estocar valores 
  #estoca as porcentagens geradas
  for (i in 1:realizaçoes) {
    s[i] <- porcent.dentro(100,2,2) # chamando a função porcent.dentro com 'n',media e variancia desejadas
  }
  print(mean(s)) #imprimi a media das porcentagens estocadas em s
}

#chamando função med.porcent.dentro com o numero de realizações(porcentagens) 
#em que se deseja calcular a media
med.porcent.dentro(100) 