library(rstan)
library(tidyverse)
N_pred = 100
N_samp = 30
x = seq(0,4, length.out = N_pred)
y = sin(x) + rnorm(N_pred, 0, 0.25)
set <- sample.int(Npred, N_samp)

ggplot(tibble(x=x,y=y), aes(x=x,y=y)) + geom_point() + geom_smooth()

gp_model <- stan_model("stan_models/gp_simple_latent.stan")

sim_data_model <- stan_model("stan_models/sim_gp_latent.stan")


dat_list <- list(N = 2000, alpha = 1, length_scale = 0.15, sigma = sqrt(0.1))
set <- sample(1:dat_list$N,size = 30, replace = F)
draw <- sampling(sim_data_model,iter=1,algorithm='Fixed_param', chains = 1, data = dat_list,
                 seed = 363360090)
samps <- rstan::extract(draw)

plt_df = with(samps,data.frame(x = x[1,], y = y[1,], f = f[1,]))


ggplot(data = plt_df[set,], aes(x=x, y=y)) + 
  geom_point(aes(colour = 'Realized data')) + 
  geom_line(data = plt_df, aes(x = x, y = f, colour = 'Latent mean function')) +
  theme_bw() + theme(legend.position="bottom") +
  scale_color_manual(name = '', values = c('Realized data'='black','Latent mean function'='red')) +
  xlab('X') + 
  ylab('y') +
  ggtitle(paste0('N=',length(set),' from length-scale = 0.15, alpha = 1, sigma = 0.32'))


stan_data <- list(N = length(set), N_pred = dat_list$N - length(set),
                  zeros =rep(0,length(set)), x = samps$x[1,set], y = samps$y[1,set],
                  x_pred = samps$x[1,-set], f_pred = samps$f[1,-set])


comp_gp_mod_lat <- stan_model('stan_models/gp_simple_latent.stan')

gp_mod_lat <- sampling(comp_gp_mod_lat, data = stan_data, cores = 4, chains = 4, iter = 2000, control = list(adapt_delta = 0.95))
samps_gp_mod_lat <- rstan::extract(gp_mod_lat)
post_pred <- data.frame(x = stan_data$x_pred,
                        pred_mu = colMeans(samps_gp_mod_lat$f_pred))
plt_df_rt = data.frame(x = stan_data$x_pred, f = t(samps_gp_mod_lat$f_pred))
plt_df_rt_melt = reshape2::melt(plt_df_rt,id.vars = 'x')
p <- ggplot(data = plt_df[set,], aes(x=x, y=y)) + 
  geom_line(data = plt_df_rt_melt, aes(x = x, y = value, group = variable, colour = 'Posterior mean functions'), alpha = 0.15) + theme_bw() + theme(legend.position="bottom") +
  geom_point(aes(colour = 'Realized data')) + 
  geom_line(data = plt_df, aes(x = x, y = f, colour = 'Latent mean function')) +
  geom_line(data = post_pred, aes(x = x, y = pred_mu, colour = 'Posterior mean function')) +
  theme_bw() + theme(legend.position="bottom") +
  scale_color_manual(name = '', values = c('Realized data'='black','Latent mean function'='red','Posterior mean functions'= 'blue','Posterior mean function'='green')) +
  xlab('X') + 
  ylab('y') +
  ggtitle(paste0('N=',length(set),' from length-scale = 0.15, alpha = 1, sigma = 0.32'))
p
