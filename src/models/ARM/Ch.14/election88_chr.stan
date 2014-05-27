data {
  int<lower=0> N; 
  int<lower=0> n_state; 
  vector<lower=0,upper=1>[N] black;
  vector<lower=0,upper=1>[N] female;
  int<lower=1,upper=n_state> state[N];
  int<lower=0,upper=1> y[N];
} 
parameters {
  vector[n_state] a_raw;
  vector[2] b_raw;
  real<lower=0,upper=100> sigma_a;
  real mu_a;
}
transformed parameters {
  vector[N] y_hat;
  vector[n_state] a;
  vector[2] b;

  a <- 100 * mu_a + a_raw * sigma_a;
  b <- b_raw * 100;
  for (i in 1:N)
    y_hat[i] <- b[1] * black[i] + b[2] * female[i] + a[state[i]];
} 
model {
  mu_a ~ normal(0, 1);
  a_raw ~ normal (0, 1);
  b_raw ~ normal (0, 1);
  y ~ bernoulli_logit(y_hat);
}
