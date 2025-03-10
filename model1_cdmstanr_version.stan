data{
  
  int<lower=1> N; 
  array[N] real math;
  array[N] real homework;
  
  int<lower=1> J; 
  array[N] int<lower=1, upper=J> schid; 
  
}

parameters{
  
  vector[2] beta;  // beta[1] es para el intercepto, beta[2] para homework
  real<lower=0> sigma_u0; 
  real<lower=0> sigma_e;  
  vector[J] u0;  // Efectos aleatorios por escuela
}

model{
  
  real mu; 
  
  beta[1] ~ normal(0,10);  // Priori de beta[1] (intercepto)
  beta[2] ~ normal(0,10);  // Priori de beta[2] (coef. de homework)
  sigma_e ~ gamma(1,0.5);  // Priori de sigma_e
  sigma_u0 ~ gamma(1,0.5);  // Priori de sigma_u0
  u0 ~ normal(0, sigma_u0);  // Efectos aleatorios de las escuelas
  
  for (i in 1:N){
    mu = beta[1] + u0[schid[i]] + beta[2] * homework[i];  // Modelo de la media
    math[i] ~ normal(mu, sigma_e);  // Verosimilitud
  }
}

generated quantities {
  // simulaciones posteriores
  
  array[N] real y_rep;
  
  for (n in 1:N){
    
    y_rep[n] = normal_rng(beta[1] + u0[schid[n]] + beta[2] * homework[n], sigma_e);
  }
  
  
  vector[N] log_lik;  
  
  for (n in 1:N) {
    // Calculamos la log-verosimilitud usando los parámetros estimados
    log_lik[n] = normal_lpdf(math[n] | beta[1] + u0[schid[n]] + beta[2] * homework[n], sigma_e); 
  }
}

