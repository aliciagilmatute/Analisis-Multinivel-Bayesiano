data {
  int<lower=1> N; 
  array[N] real math;
  array[N] real homework;
  int<lower=1> J; 
  array[N] int<lower=1, upper=J> schid; 
}

parameters {
  vector[2] beta;  
  vector<lower=0>[2] sigma_u; 
  real<lower=0> sigma_e;  
  cholesky_factor_corr[2] L_u;
  matrix[2,J] z_u;
}

transformed parameters {
  matrix[2,J] u;
  u = diag_pre_multiply(sigma_u, L_u) * z_u;
}

model {
  real mu; 
  
  // Priors
  L_u ~ lkj_corr_cholesky(1);   
  to_vector(z_u) ~ normal(0, 1);
  beta ~ normal(0, 10);
  sigma_u ~ normal(0, 1); 
  sigma_e ~ gamma(1, 0.5); 
  
  for (i in 1:N) {
    mu = beta[1] + u[1, schid[i]] + (beta[2] + u[2, schid[i]]) * homework[i];
    math[i] ~ normal(mu, sigma_e);
  }
}

generated quantities {
  matrix[2,2] R;
  R = L_u * L_u';
  
  vector[J] u0;  
  
  for (j in 1:J) {
    u0[j] = u[1,j];  // Calcula intercepto por colegio  ****(beta[1] + u[1,j];)
  }
  
  array[N] real y_rep;
  
  for (n in 1:N){
    
     y_rep[n] = normal_rng(beta[1] + u[1, schid[n]] + (beta[2] + u[2, schid[n]]) * homework[n], sigma_e);
  }
  
  
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(math[n] | beta[1] + u[1, schid[n]] + (beta[2] + u[2, schid[n]]) * homework[n], sigma_e); 
  }
}
