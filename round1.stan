
    data {
    
    int n_plot; // Number of plots
    int n_time; // Number of time points
    int n_genet; // Number of colonies
    int N; // Number of rows in the plot level data frame
    int N_c; // Number of rows in the genet related matices
    vector[N_c] plot_c; // 
    real<lower=0> lcc[n_plot]; // total live coral cover in each plot at each time point
    real<lower=0, upper=1> S[n_genet, n_time];  // 1-overlap where the overlap includes both live and dead coral. An index of now much free space an individual coral has to grow into. 
    real<lower=0> size_t[n_genet,n_time];  // the size (cm2) of each coral colony in each plot at each time point
    int T[n_plot]; // treatment (either removal or retention)
    real<lower=0> lcc_t0[n_plot]; // initial total cover of live coral

    }
    
    parameters {
    
    real beta_0; 
    real beta_1;
    real beta_0_c;
    real beta_1_c;
    real beta_2_c;
    real<lower=0> sigma;

    }
    
    transformed parameters{
    
    matrix[n_genet, n_time] beta_rem;
    vector[N] beta_rem_sum;
    vector[N] mu_c;
  
    for(j in 2:n_time){
      for(i in 1:n_genet){
        beta_rem[plot_c[i,j]] = beta_0_c + beta_1_c*S[plot_c[i,j-1]] + beta_2_c*size_t[plot_c[i,j-1]];
      }
    }
    
    for(k in 1:N){
      for(j in 1:n_time){
        beta_rem_sum[N] = sum(beta_rem[plot[,j]]);
        mu_c[plot[k]] = beta_0 + beta_1*lcc_t0[k] + beta_rem_sum[k,j]*T[k];
      }
    }

    }
    
    
    model {
    
    //priors

    beta_0 ~ normal(0, 10);
    beta_1 ~ normal(0, 10);
    beta_0_c ~ normal(0, 10);
    beta_1_c ~ normal(0, 10);
    beta_2_c ~ normal(0, 10);
    sigma ~ gamma(1, 1);
    
    // likelihood
    
    lcc ~ normal(mu_c, sigma);

    }
