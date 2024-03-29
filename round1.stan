
    data {
    
    //Index variables
    int n_plot; // Number of plots
    int n; // length of the data frame
    
    //Response variable
    real<lower = 0> y[n]; // Observations of coral size of each individual coral
    
    // Predictor variables
    real<lower=0, upper=1> S[n];  // 1-overlap where the overlap includes both live and dead coral. An index of now much free space an individual coral has to grow into.
    int T[n]; // treatment (either removal (T =1) or retention (T=0))
    int plot[n];

    }
    
    parameters {
    
    real alpha_0; // Reference level of the treatment effects (e.g. the effect of retention, when T=0)
    real beta_S; // Slope effect of space availability on individual coral growth. Does not vary across plots or treatments.
    real beta_T;
    real beta_X;
    real<lower=0> w;
    real<lower=0> v;
    real<lower=0> sigma;
    vector[n_plot] gamma;
    real x0;
    }
    
    transformed parameters{
    real r[n];
    real x_hat[n];
  
    for(i in 1:n){
        r[i] = alpha_0 + gamma[plot[i]] + beta_T*T[i] + beta_S*S[i] + beta_X*S[i]*T[i];
    }
    
    x_hat[1] = x0; 
    
    for(i in 2:n){
    x_hat[i] = x_hat[i-1] + r[i]; 
    }
    
    
    }
    
    model {
    
    real x[n];
    
    //priors

    alpha_0 ~ normal(0, 10);
    beta_T ~ normal(0, 10);
    beta_S ~ normal(0, 10);
    beta_X ~ normal(0, 10);
    x0 ~ normal(1, 0.001);
    
    for(p in 1:n_plot){
      gamma[p] ~ normal(0, sigma);
    }
    
    w ~ exponential(1);
    v ~ exponential(1);
    
    // likelihood
    
    x ~ normal(x_hat, v);
    y ~ normal(x, w);

    }
