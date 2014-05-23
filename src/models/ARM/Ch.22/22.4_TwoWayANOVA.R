library(rstan)
library(ggplot2)

## Read the pilots data & define variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/pilots

pilots <- read.table ("pilots.dat", header=TRUE)
attach (pilots)
group.names <- as.vector(unique(group))
scenario.names <- as.vector(unique(scenario))
n.group <- length(group.names)
n.scenario <- length(scenario.names)
successes <- NULL
failures <- NULL
group.id <- NULL
scenario.id <- NULL
for (j in 1:n.group){
  for (k in 1:n.scenario){
    ok <- group==group.names[j] & scenario==scenario.names[k]
    successes <- c (successes, sum(recovered[ok]==1,na.rm=T))
    failures <- c (failures, sum(recovered[ok]==0,na.rm=T))
    group.id <- c (group.id, j)
    scenario.id <- c (scenario.id, k)
  }
}

y <- successes/(successes+failures)
y.mat <- matrix (y, n.scenario, n.group)
sort.group <- order(apply(y.mat,2,mean))
sort.scenario <- order(apply(y.mat,1,mean))

group.id.new <- sort.group[group.id]
scenario.id.new <- sort.scenario[scenario.id]
y.mat.new <- y.mat[sort.scenario,sort.group]

scenario.abbr <- c("Nagoya", "B'ham", "Detroit", "Ptsbgh", "Roseln", "Chrlt", "Shemya", "Toledo")

## Model fit
## M1 <- lmer (y ~ 1 + (1 | group.id) + (1 | scenario.id))
if (!exists("anova_pilots.sm")) {
    if (file.exists("anova_pilots.sm.RData")) {
        load("anova_pilots.sm.RData", verbose = TRUE)
    } else {
        rt <- stanc("anova_pilots.stan", model_name = "pilots")
        anova_pilots.sm <- stan_model(stanc_ret = rt)
        save(anova_pilots.sm, file = "anova_pilots.sm.RData")
    }
}

dataList.1 <- list(N=length(y),y=y,n_groups=n.group,n_scenarios=n.scenario,group_id=group.id,scenario_id=scenario.id)
anova_pilots.sf1 <- sampling(anova_pilots.sm, dataList.1)
print(anova_pilots.sf1,pars = c("a", "b", "sigma_y", "lp__", "s_a", "s_b", "s_y"))

anova.df <- summary(anova_pilots.sf1, c("s_y", "s_b", "s_a"))$summary
anova.df <- data.frame(anova.df,
                       Source = factor(rownames(anova.df),
                                       levels = rownames(anova.df),
                                       labels = c("error", "airport", "treatment")),
                       df = with(dataList.1, c(N-n_scenarios-n_groups+1,  n_scenarios-1, n_groups-1)))

# (close to) Figure 22.5
p <- ggplot(anova.df, aes(x = Source, y = X50., ymin = X2.5., ymax = X97.5.)) +
     geom_linerange()+ # 95% interval
     geom_linerange(aes(ymin = X25., ymax = X75.), size = 1)+ # 50% interval
     geom_point(size = 2)+ # Median
     scale_x_discrete("Source (df)",
                      labels = with(anova.df, paste0(Source, " (", df, ")"))) +
     scale_y_continuous("Est. sd of coefficients",
                        breaks = seq(0, .4, by=.1), # Breaks from 0 to 0.8
                        limits = c(0, max(anova.df$X97.5) + 0.005),
                        expand = c(0,0)) + # Remove y padding
     coord_flip() +
     theme_bw()
print(p)

# Compare to classic ANOVA
summary(aov(y ~ factor(group.id) + factor(scenario.id)))
