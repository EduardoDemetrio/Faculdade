# 1.1 encontre as probabilidades 
### Lançamento de um dado:

## Sair a face 5;
probabilidades <- function(x, total, prob='classica' ,mutualmente = TRUE){
  if (prob =='classica'){
  if (mutualmente){
    return (x/total)
    }
  else {
    return("prob_intersecao")
  }}
  else{
    print('teste')
  }}

probabilidades(c(1),c(6), prob = 'classica', mutualmente = TRUE)
probabilidades(2,6,mutualmente = TRUE)


dado1 <- sample(1:6, 1E6, replace = TRUE)
mean(dado1 == 5)
1/6


probabilid <- function(dados, x, prob=TRUE, pvalor=TRUE ){
  set.seed(123)
  n = 10000
  if (dados==1){
  l1 <- sample(1:6, n, replace = TRUE)
  return(mean(l1==x))}
  
  if (dados == 2){
    l1 <- sample(1:6, n, replace = TRUE)
    l2 <- sample(1:6, n, replace= TRUE)
    return(mean((l1+l2)==x))
  }
  else{
    l1 <- sample(1:6, n, replace = TRUE)
    l2 <- sample(1:6, n, replace= TRUE)
    l3 <- sample(1:6, n, replace=TRUE)
    return(mean((l1+l2+l3)==x))
  }
}
probabilid()

dados <- 2
set.seed(123)
n = 10000
l1<-c()
for (i in length(dados)){
  li <- sample(1:6, n, replace = TRUE)
  l1 <- c(li,l1)}
l1

#--------------------------#
#### Exercicio plus da aula

# em duas amostras n=5, X={2,7,4,3,2} e Y={1,2,3,6,5}

x <- c(2,7,4,3,2)
y <- c(1,2,3,6,5)

## Somatório de i =1 a n de xi yi
sum(x+y)

## somatório de xi - yi de 2 a 4
sum(x[2:4]- y[2:4])

## Somatório de Xi - Yn-i-1 de 1 a n
sum(x-rev(y))

#### Porque só um sum quando falamos até n?
##### R: Porque no R tratamos de um vetor então é para n

## Somatório de Yi^i
sum(y^(y))

