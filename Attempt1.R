library(tidyverse)


sink("round1.stan")
cat("
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
    
    real beta_rem[n_genet, n_time, n_plot];
    real beta_rem_sum[n_plot, n_time];
    real mu_c[n_plot];
  
    for(k in 1:n_plot){
      for(j in 2:n_time){
        for(i in 1:n_genet){
          beta_rem[i,j,k] = beta_0_c + beta_1_c*S[i,j-1,k] + beta_2_c*size_t[i,j-1,k]];
        }
      }
    }
    for(k in 1:N){
      for(j in 1:n_time){
        beta_rem_sum[N] = sum(beta_rem[,j,k]);
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

    }",fill = TRUE)
sink()



mod <- rstan::stan_model('round1.stan')

df <- read.csv("bart_bayesian.csv")

df_plotscale <- df %>%
  group_by(plot, treatment, initial_live) %>%
  summarize(t1 = sum(august_2019_area), 
            t2 = sum(july_2022_area)) #%>%
  # pivot_longer(cols = t1:t2)

df_size <- df %>%
    group_by(plot, treatment) %>%
    rename(t1 = august_2019_area,
           t2 = july_2022_area) %>%
    select(-c(total_dead_overlap, initial_live)) %>%
  arrange(treatment, plot, genet)

size_mat <- df_size %>%
  ungroup() %>% 
  select(t1, t2) %>%
  as.matrix()

plot_c <- df_size %>%
  ungroup() %>%
  select(plot)

plot_c <- as.vector(plot_c$plot)


# S_mat <- df %>%
#   group_by(plot, treatment) %>%
#   rename(t1 = august_2019_area, 
#          t2 = july_2022_area) %>%
#   select(-c(total_dead_overlap, initial_live))

stan_data = list("P"= length(unique(df$plot)),
                 "C" = length(unique),
                 "lcc" = length(df$initial), 
                 "S" = df$mc, 
                 "size_t" = df$mr,
                 "T" = length(unique(df$id))
) # named list

stanfit <- sampling(poissonNC, data = stan_data, chains = 4, thin = 3,
                    iter = 20000,
                    seed = 123123, control = list(adapt_delta = 0.99))


