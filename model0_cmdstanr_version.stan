data{
  int<lower=1> N;              // Número de observaciones
  array[N] real math;          // Rendimiento en matemáticas de cada observación
  int<lower=1> J;              // Número de colegios
  array[N] int<lower=1, upper=J> schid;  // Indice de colegio para cada observación
}

parameters{
  real beta0;                  // Intercepto global
  real<lower=0> sigma_u0;      // Desviación estándar de los efectos aleatorios entre colegios
  real<lower=0> sigma_e;       // Desviación estándar del error residual
  vector[J] u0;                // Efectos aleatorios de cada colegio
}

model{
  real mu; 
  
  // Priors
  beta0 ~ normal(0, 10);  
  sigma_e ~ gamma(1, 0.5);    
  sigma_u0 ~ gamma(1, 0.5);   
  u0 ~ normal(0, sigma_u0);   // Distribución normal para los efectos aleatorios
  
  // Modelo
  for (i in 1:N) {
    mu = beta0 + u0[schid[i]];    // Mu: intercepto global + efecto aleatorio del colegio
    math[i] ~ normal(mu, sigma_e); // Modelo de regresión normal
  }
}

generated quantities {
  // Simulación de las observaciones futuras (y_rep) a partir de la distribución posterior
  array[N] real y_rep;
  
  // Simulamos las observaciones de 'math' para cada n
  for (n in 1:N) {
    y_rep[n] = normal_rng(beta0 + u0[schid[n]], sigma_e);
  }
  
  // Para calcular el log-likelihood (útil para el LOO)
  vector[N] log_lik;
  
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(math[n] | beta0 + u0[schid[n]], sigma_e);
  }
}
