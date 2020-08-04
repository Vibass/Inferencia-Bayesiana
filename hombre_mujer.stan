data{
  int <lower=0> N;              //n�mero de observaciones
  real <lower=0> xi[N];         //proporci�n de hombres en cada parroquia
  real <lower=0> unomenosxi[N]; //proporci�n de mujeres en cada parroquia
  int <lower=0> Ti[N];          //n�mero de personas que apoyan a AP 
  int <lower=0> ni[N];          //n�mero de votantes en cada parroquia
}
parameters{ 
  real <lower=0,upper=1>  betab[N];
  real <lower=0,upper=1>  betaw[N];
  real <lower=0>  cb;
  real <lower=0>  db;
  real <lower=0>  cw;
  real <lower=0>  dw;
}
transformed parameters{
  real <lower=0>  theta[N];
  for (n in 1:N){
    theta[n]=xi[n]*betab[n]+unomenosxi[n]*betaw[n];
  }
}
model{
  cb~exponential(0.5);  //par�metro shape1 de betab
  db~exponential(0.5);  //par�metro shape2 de betab  
  cw~exponential(0.5);  //par�metro shape1 de betaw
  dw~exponential(0.5);  //par�metro shape2 de betaw    
  for(n in 1:N){
    betab[n]~beta(cb,db);  // proporci�n de hombres que apoyan a AP
    betaw[n]~beta(cw,dw);  // proporci�n de mujeres que apoyan a AP
  }
  for (n in 1:N){
    Ti[n]~binomial(ni[n],theta[n]);  
  }
}
