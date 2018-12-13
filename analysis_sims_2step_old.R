require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(RColorBrewer)

setwd('/Users/adam/Me/Psychology/Projects/Dezfouli/git')

se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

#----- Setup -----#
real <- T
if (real) {
  df <- read.csv('Behavioral/2step/v1/data.csv') %>% tbl_df
} else {
  df <- read.csv('Simulations/data/2step/sims_MFMB_noAS.csv') %>% tbl_df
}

## Exclusion
if (real) {
  df.demo <- read.csv('Behavioral/2step/v1/data_demo.csv')
  exclude.subj <- df.demo$subject[(df.demo$reading_time / 60000) < 1]
  df <- df %>% filter(!(df$subject %in% exclude.subj))
  df.demo.exclude = df.demo %>% filter(!(df.demo$subject %in% exclude.subj))
}

subjlist = unique(as.character(df$subject))

# nstates = length(unique(df$S2)) + 1
# nactions = 2
# curvals = matrix(nrow = nstates, ncol = nactions)
# for (i in 1:nrow(df)) {
#   df$prev1[i] = curvals[1,df$Action1[i]]
#   df$prev2[i] = curvals[df$S2[i],df$Action2[i]]
#   
#   curvals[1,df$Action1[i]] = df$Re[i]
#   curvals[df$S2[i],df$Action2[i]] = df$Re[i]
#   
#   if (df$S2[i] == 4) {
#     otherAction = ifelse(df$Action2[i] == 1, 2, 1)
#     curvals[df$S2[i],otherAction] = df$Re[i]
#   }
#   
#   if (i < nrow(df) && df$subject[i] != df$subject[i+1]) {
#     curvals = matrix(nrow = nstates, ncol = nactions)
#   }
# }

if (real) {
  df.crits <- df %>%
    mutate(
      last.common = factor(lag(common), c(1, 0), c('Common', 'Rare')),
      sameS2 = factor(lag(S2) == S2, c(T, F), c("Same S2", "Different S2")),
      stay1 = Action1 == lag(Action1),
      stay2 = Action2 == lag(Action2),
      stay1.fac = factor(Action1 == lag(Action1), c(T, F), c("Same action1", "Different action1")),
      stay2.fac = factor(Action2 == lag(Action2), c(T, F), c("Same action2", "Different action2")),
      rt1.fac = factor(rt1 < median(rt1), c(T, F), c("RT1 fast", "RT1 slow")),
      last.reinf = lag(Re),
      #last.reinf.fac = factor(ifelse(lag(Re) == 0, 0, ifelse(lag(Re) > 0, 1, -1)), c(1, 0, -1), c("+", "0", "-")),
      last.reinf.fac = factor(last.reinf >= 0, c(T,F), c("+", "-")), last.reinf.abs = abs(lag(Re)),
      #last.pe1 = lag(Re) - lag(prev1),
      #last.pe2 = lag(Re) - lag(prev2),
      last.pe1.fac = factor(last.pe1 >= 0, c(T,F), c("+", "-")),
      last.pe2.fac = factor(last.pe1 >= 0, c(T,F), c("+", "-")),
      subject_id = as.numeric(subject)
      ) %>%
   filter(round != min(round, na.rm = T) & practice != 1 & timeouts == 0)
} else {
  df.crits <- df %>% 
    mutate(
      last.common = factor(lag(S2) == (lag(Action1) + 1), c(T, F), c("Common", "Rare")),
      sameS2 = factor(lag(S2) == S2, c(T, F), c("Same S2", "Different S2")),
      stay1 = Action1 == lag(Action1),
      stay2 = Action2 == lag(Action2),
      stay1.fac = factor(Action1 == lag(Action1), c(T, F), c("Same action1", "Different action1")),
      stay2.fac = factor(Action2 == lag(Action2), c(T, F), c("Same action2", "Different action2")),
      last.reinf = lag(Re),
      #last.reinf.fac = factor(ifelse(lag(Re) == 0, 0, ifelse(lag(Re) > 0, 1, -1)), c(1, 0, -1), c("+", "0", "-")),
      last.reinf.fac = factor(last.reinf >= 0, c(T,F), c("+", "-")),
      last.reinf.abs = abs(lag(Re))
      ) %>%
    filter(round != min(round, na.rm = T))
}

#----- Tests -----#

## Test 1: Stay1 ~ Common X Reinf
# Plot
df.bysubj <- df.crits %>% group_by(last.pe1.fac, last.common, subject) %>%
  summarize(stay1 = mean(stay1))

df.agg <- df.bysubj %>%
  summarize(stay1.mean = mean(stay1), stay1.se = se(stay1))

ggplot(df.agg, aes(x = last.pe1.fac, y = stay1.mean, group = last.common, fill = last.common)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay1.mean + stay1.se, ymin = stay1.mean - stay1.se), width = .5, position = dodge, colour = "white") +
  guides(fill = F) +
  labs(x = "", y = "") +
  coord_cartesian(ylim=c(0, 1)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.background = element_rect(colour = "black", fill = "black"),
        panel.grid = element_blank())

# Models
model.daw.mm <- glmer(stay1 ~ last.reinf.fac * last.common + (1 + last.reinf.fac * last.common | subject),
                      family = binomial, data = df.crits %>% filter(last.reinf.fac != '0'))
model.daw.mm.null <- glmer(stay1 ~ last.reinf.fac + last.common + (1 + last.reinf.fac * last.common | subject), family = binomial,
                           data = df.crits %>% filter(last.reinf.fac != '0'))
model.daw.mm.null2 <- glmer(stay1 ~ last.reinf.fac:last.common + last.common + (1 + last.reinf.fac * last.common | subject), family = binomial,
                           data = df.crits %>% filter(last.reinf.fac != '0'))
anova(model.daw.mm, model.daw.mm.null)
anova(model.daw.mm, model.daw.mm.null2)
model.daw <- lm(stay1 ~ last.reinf.fac * last.common, data = df.bysubj,
                 contrasts = list(last.reinf.fac = contr.sum, last.common = contr.sum))
summary(model.daw)


## Test 2: Stay2 ~ Stay1 x Last.Reinf | Different S2, Last Common
# Plot
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2' & last.common == 'Common') %>%
  group_by(stay1.fac, last.reinf.fac, subject) %>%
  summarize(stay2 = mean(stay2))

df.agg <- df.bysubj %>%
  summarize(stay2.mean = mean(stay2), stay2.se = se(stay2))

ggplot(df.agg, aes(x = last.reinf.fac, y = stay2.mean, group = stay1.fac, fill = stay1.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay2.mean + stay2.se, ymin = stay2.mean - stay2.se), width = .5, position = dodge) +
  guides(fill = F) +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(color = 'white', fill = NA)) +
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown")) +
  ylim(0,.7)

# Model
model.test3.mm <- glmer(stay2 ~ last.reinf.fac * stay1.fac + (1 + last.reinf.fac * stay1.fac | subject),
                      family = binomial, data = df.crits)
model.test3.mm.null <- glmer(stay2 ~ last.reinf.fac + stay1.fac + (1 + last.reinf.fac * stay1.fac | subject), family = binomial,
                           data = df.crits)
model.test3.mm.null2 <- glmer(stay2 ~ last.reinf.fac:stay1.fac + stay1.fac + (1 + last.reinf.fac * stay1.fac | subject), family = binomial,
                            data = df.crits)
anova(model.daw.mm, model.daw.mm.null)
anova(model.daw.mm, model.daw.mm.null2)
model.test3 <- lm(stay2 ~ stay1.fac * last.reinf.fac, data = df.bysubj, 
                  contrasts = list(last.reinf.fac = contr.sum, stay1.fac = contr.sum))
summary(model.test3)


## Test 3: Stay2 ~ Stay1 x Last.Reinf | Different S2, Last Rare
# Plot
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2' & last.common == 'Rare') %>%
  group_by(stay1.fac, last.reinf.fac, subject) %>%
  summarize(stay2 = mean(stay2))

df.agg <- df.bysubj %>%
  summarize(stay2.mean = mean(stay2), stay2.se = se(stay2))

ggplot(df.agg, aes(x = stay1.fac, y = stay2.mean, group = last.reinf.fac, fill = last.reinf.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay2.mean + stay2.se, ymin = stay2.mean - stay2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "")) +
  labs(x = "", y = "Prob of repeating action 2")

# Model
model.test4 <- lm(stay2 ~ stay1.fac * last.reinf.fac, data = df.bysubj, 
                  contrasts = list(last.reinf.fac = contr.sum, stay1.fac = contr.sum))
summary(model.test4)


## Tests 2 & 3 combined
# Plot
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2') %>%
  group_by(stay1.fac, last.reinf.fac, last.common, subject) %>%
  summarize(stay2 = mean(stay2))

df.agg <- df.bysubj %>%
  summarize(stay2.mean = mean(stay2), stay2.se = se(stay2))

ggplot(df.agg, aes(x = stay1.fac, y = stay2.mean, group = last.reinf.fac, fill = last.reinf.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = stay2.mean + stay2.se, ymin = stay2.mean - stay2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "Last reinforcement")) +
  labs(x = "", y = "Prob of repeating action 2") +
  facet_wrap(~ last.common)

# Model
model.test34 <- lm(stay2 ~ stay1.fac * last.reinf.fac * last.common, data = df.bysubj, 
                  contrasts = list(last.reinf.fac = contr.sum, stay1.fac = contr.sum, last.common = contr.sum))
summary(model.test34)


## RT2

# After common
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2' & last.common == 'Common') %>%
  group_by(stay1.fac, last.reinf.fac, stay2.fac, subject) %>%
  summarize(rt2 = mean(rt2))

df.agg <- df.bysubj %>%
  summarize(rt2.mean = mean(rt2), rt2.se = se(rt2))

ggplot(df.agg, aes(x = last.reinf.fac, y = rt2.mean, group = stay2.fac, fill = stay2.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rt2.mean + rt2.se, ymin = rt2.mean - rt2.se), width = .5, position = dodge) +
  guides(fill = F) +
  labs(x = "", y = "") +
  facet_wrap(~ stay1.fac) + ylim(0,1000) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                                                  axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                                                  panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(fill = 'white'))+
  scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))

# differences version
df.bysubj2 = df.bysubj %>% filter(stay2.fac == 'Same action2')
for (i in 1:nrow(df.bysubj2)) {
  other = df.bysubj$subject == df.bysubj2$subject[i] & df.bysubj$last.reinf.fac == df.bysubj2$last.reinf.fac[i] & df.bysubj$stay1.fac == df.bysubj2$stay1.fac[i] & df.bysubj$stay2.fac == 'Different action2'
  if (any(other)) {
    df.bysubj2$rt2.diff[i] = df.bysubj$rt2[other] - df.bysubj2$rt2[i] 
  }
}

df.agg <- df.bysubj2 %>%
  summarize(rt2.mean = mean(rt2.diff), rt2.se = se(rt2.diff))

ggplot(df.agg, aes(x = last.reinf.fac, y = rt2.mean, group = stay1.fac, fill = stay1.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rt2.mean + rt2.se, ymin = rt2.mean - rt2.se), width = .5, position = dodge) +
  guides(fill = F) + ylim(-100,400) +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 2),
        panel.background = element_rect(fill = 'white'))+
  scale_fill_manual(values = c("Same action1" = "#3D9970", "Different action1" = "brown"))
  
model.rt = lmer(rt2 ~ stay1.fac * stay2.fac * last.reinf.fac + (1 + stay1.fac * stay2.fac * last.reinf.fac | subject), data = df.crits %>% filter(sameS2 == 'Different S2' & last.common == 'Common'))
summary(model.rt)

# After rare
df.bysubj <- df.crits %>% filter(last.reinf.fac != '0' & sameS2 == 'Different S2' & last.common == 'Rare') %>%
  group_by(stay1.fac, last.reinf.fac, stay2.fac, subject) %>%
  summarize(rt2 = mean(rt2))

df.agg <- df.bysubj %>%
  summarize(rt2.mean = mean(rt2), rt2.se = se(rt2))

ggplot(df.agg, aes(x = stay1.fac, y = rt2.mean, group = stay2.fac, fill = stay2.fac)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rt2.mean + rt2.se, ymin = rt2.mean - rt2.se), width = .5, position = dodge) +
  guides(fill = guide_legend(title = "Stage 2 choice")) +
  labs(x = "", y = "RT of action 2") +
  facet_wrap(~ last.reinf.fac)

## Make modeling CSV
if (real) {
  write.table(df.crits %>% select(Action1, S2, Action2, Re, subject_id), 'Behavioral/dez_2step/v1/data_fitting.csv',
              row.names = F, col.names = F, sep = ",")
}