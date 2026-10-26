################################################################################
# 02_analysis.R
#
# Produces Tables 1-3, Figures 1-2, and online Appendix A (online betting) and
# B (Covid control) figures for:
# Nedved, J.D. and Ferreira Neto, A.B. (2024). Sports betting cannibalization:
# Evidence from Pennsylvania. Economics Letters 245, 112031.
#
# This is the script used for the published results (originally
# regression-code-amir-rr.R). Changes from the original are limited to file
# paths, marked "# [repo]". The analysis code is unchanged.
################################################################################

library(tidyverse)
library(fixest)
library(did)
library(modelsummary)

# [repo] Run from the repository root. The original script used setwd() to a local folder.
# [repo] To run on the dataset rebuilt by 01_build_gaming_data.R, change data_file to
# [repo] "data/derived/gaming_data_rebuilt.rds".
data_file <- "data/final/gaming_data.rds"
dir.create("output", showWarnings = FALSE)

df_raw <- read_rds(data_file)

month_year_table <- tibble(
  date = seq(as.Date("2015-07-01"), as.Date("2023-06-01"), by = "1 month"),
  month_year = seq(1:96))

### 
# up to 2020-02 
# from 2020-07

# everything if 0 between 2020-03 - 2020-07 return 0.1
CPI_202306 <- 0.7834163

df <- df_raw %>% 
  mutate(cpi_new = cpi_adjustment/CPI_202306,
         real_gtr = real_gtr/cpi_adjustment*cpi_new,
         real_handle = real_handle/cpi_adjustment*cpi_new,
         real_table_rev = real_table_rev/cpi_adjustment*cpi_new,
         drop = ifelse((real_table_rev==0 & real_gtr==0 & real_handle==0) &
                         (!paste0(year,"-",month)%in%c("2020-04","2020-05","2020-06")),1,0),
         drop = ifelse(casino%in%c("mohegan - lehigh valley","parx at malvern","parx shippensburg",
                                   "south philadelphia race and sportsbook","oaks race and sportsbook"),1,drop)) %>%
  filter(drop==0) %>%
   ### drop unopened casinos and sports books only
  arrange(casino, year, month) %>%
  mutate(date = lubridate::my(paste0(month,"-",year))) %>%
  left_join(., month_year_table, by="date") %>%
  mutate(real_handle2 = ifelse(real_handle == 0 & date%in%c(lubridate::ym("2020-03"):lubridate::ym("2020-07")), 
                                                            0.01, 
                                                            real_handle),
         real_gtr2 = ifelse(real_gtr == 0 & date%in%c(lubridate::ym("2020-03"):lubridate::ym("2020-07")), 0.01, real_gtr),
         real_table_rev2 = ifelse(real_table_rev == 0 & date%in%c(lubridate::ym("2020-03"):lubridate::ym("2020-07")), 0.01, real_gtr),
         active_handle = as.numeric(!real_handle2==0)) %>%
  group_by(casino, active_handle) %>%
  mutate(group = min(month_year)) %>%
  ungroup() %>%
  group_by(casino) %>%
  mutate(group = max(group)) %>%
  ungroup() %>%
  mutate(diff_c = month_year-group,
         treat = as.numeric(diff_c>=0),
         idname = as.numeric(as.factor(casino)),
         real_gtr = real_gtr/100000,
         real_handle = real_handle/100000,
         real_table_rev = real_table_rev/100000,
         gtr_sine = log(real_gtr+(real_gtr^2+1)^0.5),
         handle_sine = log(real_handle+(real_handle^2+1)^0.5),
         table_sine = log(real_table_rev+(real_table_rev^2+1)^0.5))

###########
ols_gtr <- df%>%
  do(broom::tidy(feols(gtr_sine ~ i(diff_c, ref = c(-1, -1000))|casino+month_year, 
                       cluster = ~casino, data = .), conf.int = TRUE)) %>% 
  mutate(t =  as.double(str_replace_all(term, c("diff_c::" = "", ":treat" = "")))) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  mutate(method = "OLS")



### SUN & ABRAHAMS
sa_gtr <-df %>% 
  do(broom::tidy(feols(gtr_sine ~ sunab(group, month_year)|casino+month_year, data = .,
                       cluster = ~ casino))) %>% 
  mutate(t =  as.double(str_replace(term, "month_year::", "")),
         conf.low = estimate - (qnorm(0.975)*std.error),
         conf.high = estimate + (qnorm(0.975)*std.error)) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  mutate(method = "SUNAB")


### CSA

csa.est<- att_gt(yname= 'gtr_sine',
                 tname= 'month_year',
                 idname = 'idname',
                 gname = 'group',
                 clustervars = 'idname',
                 control_group = 'notyettreated',
                 data = df, 
                 panel = F) 

csa_grt <- aggte(csa.est, type = "dynamic", na.rm = TRUE) %>% 
  tidy() %>% 
  rename(t = event.time) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  mutate(method = "CSA")


coefs <- bind_rows(ols_gtr, sa_gtr, csa_grt) 

plot_grt <- coefs %>% 
  ggplot(aes(x = t, y = estimate, color = method)) + 
  geom_point(aes(x = t, y = estimate), position = position_dodge2(width = 0.8), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), position = position_dodge2(width = 0.8), size = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", size = .25) +
  # scale_color_manual(name="Estimation Method", values= met.brewer("Cross", 8, "discrete")) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs( y="ATT", x = "Relative time in months") + 
  guides(col = guide_legend(nrow = 3))
plot_grt




### table revenue
ols_tr <- df%>%
  do(broom::tidy(feols(table_sine ~ i(diff_c, ref = c(-1, -1000))|casino+month_year, 
                       cluster = ~casino, data = .), conf.int = TRUE)) %>% 
  mutate(t =  as.double(str_replace_all(term, c("diff_c::" = "", ":treat" = "")))) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  mutate(method = "OLS")



### SUN & ABRAHAMS
sa_tr <-df %>% 
  do(broom::tidy(feols(table_sine ~ sunab(group, month_year)|casino+month_year, data = .,
                       cluster = ~ casino))) %>% 
  mutate(t =  as.double(str_replace(term, "month_year::", "")),
         conf.low = estimate - (qnorm(0.975)*std.error),
         conf.high = estimate + (qnorm(0.975)*std.error)) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  mutate(method = "SUNAB")


### CSA

csa.esttr<- att_gt(yname= 'table_sine',
                   tname= 'month_year',
                   idname = 'idname',
                   gname = 'group',
                   clustervars = 'idname',
                   control_group = 'notyettreated',
                   data = df, 
                   panel = F) 

csa_tr <- aggte(csa.esttr, type = "dynamic", na.rm = TRUE) %>% 
  tidy() %>% 
  rename(t = event.time) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  mutate(method = "CSA")


coefs_tr <- bind_rows(ols_tr, sa_tr, csa_tr) 

plot_tr <- coefs_tr %>% 
  ggplot(aes(x = t, y = estimate, color = method)) + 
  geom_point(aes(x = t, y = estimate), position = position_dodge2(width = 0.8), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), position = position_dodge2(width = 0.8), size = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", size = .25) +
  # scale_color_manual(name="Estimation Method", values= met.brewer("Cross", 8, "discrete")) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative time in months") + 
  guides(col = guide_legend(nrow = 3)) 
plot_tr

png("output/gtr-sine-rr.png",res=300, width = 8 , height=4, units = "in")
plot_grt
dev.off()


png("output/table_revenue-sine-rr.png",res=300, width = 8 , height=4, units = "in")
plot_tr
dev.off()



##########

df_reg_est <- df %>%
  group_by(casino) %>%
  mutate(treat_casino = as.numeric(sum(treat)>1),
         post = as.numeric(date >= lubridate::mdy("05-01-2018")))



ols_gtr_reg <- feols(gtr_sine ~  treat|casino+month_year, cluster = ~casino, data=df)
ols_tr_reg <- feols(table_sine ~ treat|casino+month_year, cluster = ~casino, data=df)

csa_grt_reg <-  aggte(csa.est, type = "simple", na.rm = TRUE)
csa_tr_reg <-aggte(csa.esttr, type = "simple", na.rm = TRUE)



sa_gtr_reg <- feols(gtr_sine ~ sunab(group, month_year)|casino+month_year, data = df,
                    cluster = ~ casino)

sa_tr_reg <- feols(table_sine ~ sunab(group, month_year)|casino+month_year, data = df,
                   cluster = ~ casino)


sa_gtr_agg <- aggregate(sa_gtr_reg, c("ATT" = "month_year::[^-]"))
sa_tr_agg <- aggregate(sa_tr_reg, c("ATT" = "month_year::[^-]"))





ols_gtr_reg <- feols(gtr_sine ~ i(diff_c, ref = c(-1, -1000))|casino+month_year, data = df,
                     cluster = ~ casino)

ols_tr_reg <- feols(table_sine ~ i(diff_c, ref = c(-1, -1000))|casino+month_year, data = df,
                    cluster = ~ casino)


ols_gtr_agg <- aggregate(ols_gtr_reg, c("ATT" = "diff_c::[^-]"))
ols_tr_agg <- aggregate(ols_tr_reg, c("ATT" = "diff_c::[^-]"))



library(modelsummary)
datasummary(All(df) ~ Mean + SD + Min + Max, data=df, output = "latex")
df <- df %>%mutate(post=as.numeric(year>=2018 & month >= 05))
datasummary_balance(~post, data=df, output="latex")
datasummary_balance(~treat, data=df, output="latex")

t.test(real_gtr  ~ post, data = df)
t.test(real_table_rev  ~ post, data = df)
t.test(real_gtr  ~ treat, data = df)
t.test(real_table_rev  ~ treat, data = df)

###########
#online betting
online_raw <- read_csv("data/raw/online.csv")  # [repo] path

df_ob <- left_join(df, online_raw, by="casino") %>%
  filter(!is.na(online_bet)) %>%
  mutate(online_bet = lubridate::mdy(online_bet),
         online_bet_my = lubridate::mdy(paste(
           lubridate::month(online_bet),
           1,
           lubridate::year(online_bet),sep = "/")))%>%
  group_by(casino) %>%
  mutate(group_online = if_else(date==online_bet_my, month_year, NA)) %>%
  fill(group_online, .direction = "downup") %>%
  ungroup() %>%
  mutate(diff_c_online = month_year-group_online,
         treat_online = as.numeric(diff_c_online>=0))
  

### GRT
ols_gtr_o <- df_ob %>%
  do(broom::tidy(feols(gtr_sine ~ i(diff_c_online, ref = c(-1, -1000))|casino+month_year, 
                       cluster = ~casino, data = .), conf.int = TRUE)) %>% 
  mutate(t =  as.double(str_replace_all(term, c("diff_c_online::" = "", ":treat_online" = "")))) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  mutate(method = "OLS")



### SUN & ABRAHAMS
sa_gtr_o <-df_ob %>% 
  do(broom::tidy(feols(gtr_sine ~ sunab(group_online, month_year)|casino+month_year, data = .,
                       cluster = ~ casino))) %>% 
  mutate(t =  as.double(str_replace(term, "month_year::", "")),
         conf.low = estimate - (qnorm(0.975)*std.error),
         conf.high = estimate + (qnorm(0.975)*std.error)) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  mutate(method = "SUNAB")


### CSA

csa.est_o<- att_gt(yname= 'gtr_sine',
                 tname= 'month_year',
                 idname = 'idname',
                 gname = 'group_online',
                 clustervars = 'idname',
                 control_group = 'notyettreated',
                 data = df_ob, 
                 panel = F) 

csa_grt_o <- aggte(csa.est_o, type = "dynamic", na.rm = TRUE) %>% 
  tidy() %>% 
  rename(t = event.time) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  mutate(method = "CSA")


coefs <- bind_rows(ols_gtr_o, sa_gtr_o, csa_grt_o) 

plot_grt_o <- coefs %>% 
  ggplot(aes(x = t, y = estimate, color = method)) + 
  geom_point(aes(x = t, y = estimate), position = position_dodge2(width = 0.8), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), position = position_dodge2(width = 0.8), size = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", size = .25) +
  # scale_color_manual(name="Estimation Method", values= met.brewer("Cross", 8, "discrete")) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs( y="ATT", x = "Relative time in months") + 
  guides(col = guide_legend(nrow = 3)) 
plot_grt_o




### table revenue
ols_tr_o <- df_ob%>%
  do(broom::tidy(feols(table_sine ~ i(diff_c_online, ref = c(-1, -1000))|casino+month_year, 
                       cluster = ~casino, data = .), conf.int = TRUE)) %>% 
  mutate(t =  as.double(str_replace_all(term, c("diff_c_online::" = "", ":treat_online" = "")))) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  mutate(method = "OLS")



### SUN & ABRAHAMS
sa_tr_o <-df_ob %>% 
  do(broom::tidy(feols(table_sine ~ sunab(group_online, month_year)|casino+month_year, data = .,
                       cluster = ~ casino))) %>% 
  mutate(t =  as.double(str_replace(term, "month_year::", "")),
         conf.low = estimate - (qnorm(0.975)*std.error),
         conf.high = estimate + (qnorm(0.975)*std.error)) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  mutate(method = "SUNAB")


### CSA

csa.esttr_o<- att_gt(yname= 'table_sine',
                   tname= 'month_year',
                   idname = 'idname',
                   gname = 'group_online',
                   clustervars = 'idname',
                   control_group = 'notyettreated',
                   data = df_ob, 
                   panel = F) 

csa_tr_o <- aggte(csa.esttr_o, type = "dynamic", na.rm = TRUE) %>% 
  tidy() %>% 
  rename(t = event.time) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  mutate(method = "CSA")


coefs_tr <- bind_rows(ols_tr_o, sa_tr_o, csa_tr_o) 

plot_tr_o <- coefs_tr %>% 
  ggplot(aes(x = t, y = estimate, color = method)) + 
  geom_point(aes(x = t, y = estimate), position = position_dodge2(width = 0.8), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), position = position_dodge2(width = 0.8), size = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", size = .25) +
  # scale_color_manual(name="Estimation Method", values= met.brewer("Cross", 8, "discrete")) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative time in months") + 
  guides(col = guide_legend(nrow = 3)) 
plot_tr_o

png("output/gtr-sine-online.png",res=300, width = 8 , height=4, units = "in")
plot_grt_o
dev.off()


png("output/table_revenue-sine-online.png",res=300, width = 8 , height=4, units = "in")
plot_tr_o
dev.off()

#################################################################################

#covid
covid_raw <- read_csv("data/raw/covid.csv")  # [repo] path

covid <- covid_raw %>%
  janitor::clean_names() %>%
  select(date,new_cases,population_2019, county_fips_code) %>%
  rename(fips = county_fips_code) %>%
  mutate(date = lubridate::mdy(date),
         month = lubridate::month(date),
         year = lubridate::year(date)) %>%
  group_by(fips, month, year) %>%
  summarise(
    cases = sum(new_cases, na.rm = T),
    population = mean(population_2019, na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(cases_pop = cases/population)

df_cov <- left_join(df_ob, covid, by = c("fips","month","year")) %>%
  mutate(cases = ifelse(is.na(cases),0,cases),
         cases_pop = ifelse(is.na(cases_pop),0,cases_pop))



################################################################################



### GRT
ols_gtr_c <- df_cov%>%
  do(broom::tidy(feols(gtr_sine ~ i(diff_c, ref = c(-1, -1000))+cases|casino+month_year, 
                  cluster = ~casino, data = .), conf.int = TRUE)) %>% 
     mutate(t =  as.double(str_replace_all(term, c("diff_c::" = "", ":treat" = "")))) %>% 
     filter(t >= -6 & t <= 12) %>% 
     select(t, estimate, conf.low, conf.high) %>% 
     # add in data for year -1
     bind_rows(tibble(t = -1, estimate = 0, 
                                           conf.low = 0, conf.high = 0
                          )) %>% 
     mutate(method = "OLS")



### SUN & ABRAHAMS
sa_gtr_c <-df_cov %>% 
     do(broom::tidy(feols(gtr_sine ~ sunab(group, month_year)+cases|casino+month_year, data = .,
                                                   cluster = ~ casino))) %>% 
     mutate(t =  as.double(str_replace(term, "month_year::", "")),
                       conf.low = estimate - (qnorm(0.975)*std.error),
                       conf.high = estimate + (qnorm(0.975)*std.error)) %>% 
     filter(t >= -6 & t <= 12) %>% 
     select(t, estimate, conf.low, conf.high) %>% 
     mutate(method = "SUNAB")
  

### CSA

#csa.est_c<- att_gt(yname= 'gtr_sine',
#                 tname= 'month_year',
#                 idname = 'idname',
#                 gname = 'group',
#                 clustervars = 'idname',
#                 control_group = 'notyettreated',
##                 xformla = ~cases,
#                 data = df_cov %>% filter(month_year%in%c(55:95)), 
#                 panel = F) 
#
#csa_grt_c <- aggte(csa.est_c, type = "dynamic", na.rm = TRUE) %>% 
#  tidy() %>% 
#  rename(t = event.time) %>% 
#  filter(t >= -6 & t <= 12) %>% 
#  select(t, estimate, conf.low, conf.high) %>% 
#  mutate(method = "CSA")


coefs_c <- bind_rows(ols_gtr_c, sa_gtr_c) 

plot_grt_c <- coefs_c %>% 
  ggplot(aes(x = t, y = estimate, color = method)) + 
  geom_point(aes(x = t, y = estimate), position = position_dodge2(width = 0.8), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), position = position_dodge2(width = 0.8), size = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", size = .25) +
 # scale_color_manual(name="Estimation Method", values= met.brewer("Cross", 8, "discrete")) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs( y="ATT", x = "Relative time in months") + 
  guides(col = guide_legend(nrow = 3)) 
plot_grt_c




### table revenue
ols_tr_c <- df_cov%>%
  do(broom::tidy(feols(table_sine ~ i(diff_c, ref = c(-1, -1000))+cases|casino+month_year, 
                       cluster = ~casino, data = .), conf.int = TRUE)) %>% 
  mutate(t =  as.double(str_replace_all(term, c("diff_c::" = "", ":treat" = "")))) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  # add in data for year -1
  bind_rows(tibble(t = -1, estimate = 0, 
                   conf.low = 0, conf.high = 0
  )) %>% 
  mutate(method = "OLS")



### SUN & ABRAHAMS
sa_tr_c <-df_cov %>% 
  do(broom::tidy(feols(table_sine ~ sunab(group, month_year)+cases|casino+month_year, data = .,
                       cluster = ~ casino))) %>% 
  mutate(t =  as.double(str_replace(term, "month_year::", "")),
         conf.low = estimate - (qnorm(0.975)*std.error),
         conf.high = estimate + (qnorm(0.975)*std.error)) %>% 
  filter(t >= -6 & t <= 12) %>% 
  select(t, estimate, conf.low, conf.high) %>% 
  mutate(method = "SUNAB")


### CSA

#csa.esttr_c<- att_gt(yname= 'table_sine',
#                  tname= 'month_year',
#                  idname = 'idname',
#                  gname = 'group',
#                  clustervars = 'idname',
#                  control_group = 'notyettreated',
#                  xformla = ~cases,
#                  data = df_cov , 
#                  panel = F) 

#csa_tr_c <- aggte(csa.esttr_c, type = "dynamic", na.rm = TRUE) %>% 
#  tidy() %>% 
#  rename(t = event.time) %>% 
#  filter(t >= -6 & t <= 12) %>% 
#  select(t, estimate, conf.low, conf.high) %>% 
#  mutate(method = "CSA")


coefs_tr_c <- bind_rows(ols_tr_c, sa_tr_c) 

plot_tr_c <- coefs_tr_c %>% 
  ggplot(aes(x = t, y = estimate, color = method)) + 
  geom_point(aes(x = t, y = estimate), position = position_dodge2(width = 0.8), size = 1) +
  geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), position = position_dodge2(width = 0.8), size = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = .25, alpha = 0.75) + 
  geom_vline(xintercept = -0.5, linetype = "dashed", size = .25) +
  # scale_color_manual(name="Estimation Method", values= met.brewer("Cross", 8, "discrete")) +
  theme_bw() + theme(legend.position= 'bottom') +
  labs(y="ATT", x = "Relative time in months") + 
  guides(col = guide_legend(nrow = 3)) 
plot_tr_c

png("output/gtr-sine-covid.png",res=300, width = 8 , height=4, units = "in")
plot_grt_c
dev.off()


png("output/table_revenue-sine-covid.png",res=300, width = 8 , height=4, units = "in")
plot_tr_c
dev.off()

