data {
  int<lower=0> N; 
  int<lower=0> n_age;
  int<lower=0> n_age_edu;  
  int<lower=0> n_edu; 
  int<lower=0> n_region_full; 
  int<lower=0> n_state; 
  int<lower=0,upper=n_age> age[N];
  int<lower=0,upper=n_age_edu> age_edu[N];
  vector<lower=0,upper=1>[N] black;
  int<lower=0,upper=n_edu> edu[N];
  vector<lower=0,upper=1>[N] female;
  int<lower=0,upper=n_region_full> region_full[N];
  int<lower=0,upper=n_state> state[N];
  vector[N] v_prev_full;
  int<lower=0,upper=1> y[N];
} 
parameters {
  vector[n_age] a_raw;
  vector[n_edu] b_raw;
  vector[n_age_edu] c_raw;
  vector[n_state] d_raw;
  vector[n_region_full] e_raw;
  vector[5] beta_raw;
  real<lower=0,upper=100> sigma_a;
  real<lower=0,upper=100> sigma_b;
  real<lower=0,upper=100> sigma_c;
  real<lower=0,upper=100> sigma_d;
  real<lower=0,upper=100> sigma_e;
}
transformed parameters {
  vector[N] y_hat;
  vector[n_age] a;
  vector[n_edu] b;
  vector[n_age_edu] c;
  vector[n_state] d;
  vector[n_region_full] e;
  vector[5] beta;
  
  a <- a_raw * sigma_a;
  b <- b_raw * sigma_b;
  c <- c_raw * sigma_c;
  d <- d_raw * sigma_d;
  e <- e_raw * sigma_e;
  beta <- beta_raw * 100;

  for (i in 1:N)
    y_hat[i] <- beta[1] + beta[2] * black[i] + beta[3] * female[i]
                + beta[5] * female[i] * black[i] 
                + beta[4] * v_prev_full[i] + a[age[i]] + b[edu[i]] 
                + c[age_edu[i]] + d[state[i]] + e[region_full[i]];
} 
model {
  a_raw ~ normal (0, 1);
  b_raw ~ normal (0, 1);
  c_raw ~ normal (0, 1);
  d_raw ~ normal (0, 1);
  e_raw ~ normal (0, 1);
  beta_raw ~ normal(0, 1);
  y ~ bernoulli_logit(y_hat);
}
