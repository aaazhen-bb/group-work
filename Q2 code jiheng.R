# Project

# 2
currentasset=250000
n=1000
# n is numeber of customers
outcome=function(n,alpha,beta,current){
  u=runif(10000)
  v=beta*(1/(1-u)**(1/alpha)-1)
  annual=rep(6000,len=n)
  customers=-annual
  for (i in 1:n){
    prob=runif(1)
    if (prob<0.1){
      current=current+annual[i]-v[i]
      customers[i]=customers[i]+v[i]
    }
    else 
      current=current+annual[i]
  }
  result=data.frame(current,customers)
}

count=0
for (j in 1:10000){
  outcome_q2=outcome(1000,alpha,beta,currentasset)
  if (outcome_q2$current[1]<0)
    count=count+1
}
prob_bankrupt=count/10000


