### colors ----
color_meta1 <- "#A04088"
color_meta2 <- "#C1C544"
color_meta3 <- "#00CCB4"
color_all_metas <- "#043565"
colors_meta <- c(color_meta1, color_meta2, color_meta3)
color_learningstrat1 <- "#5C3EB6"
color_learningstrat2 <- "#008F6B"
color_learningstrat3 <- "#ED31B1"
color_learningstrat4 <- "#FF990A"
colors_learningstrat <- c(color_learningstrat1, color_learningstrat2, color_learningstrat3, color_learningstrat4)


### labels ----
labels_learningstrat <- c("Success", "Similarity", "Age", "Conformity")
labels_metatypes <- c("Fixed Learner", "Flexibile Learner", "Integrative Learner")
labs_legend_learningstrat <- "Learning Strategy"
labs_legend_meta <- "Type of Meta Learning"


### load files ----
chain_success <- read_csv("data/2024-06-17 10-47-34_chainSuccess/results.csv")
chain_Similarity <- read_csv("data/2024-06-17 10-32-38_chainSimilarity/results.csv")
chain_age <- read_csv("data/2024-06-16 22-12-09_chainAge/results.csv")
chain_conformity <- read_csv("data/2024-06-16 21-59-44_chainConf/results.csv")
chain_fixed <- read_csv("data/2024-06-17 10-53-28_chainFixed/results.csv")
chain_flexible <- read_csv("data/2024-06-17 11-05-30_chainFlexible/results.csv")
chain_integrative <- read_csv("data/2024-06-17 11-14-38_chainIntegrative/results.csv")
chain_all <- read_csv("data/2024-06-15 19-27-28_chainAllMetas/results.csv")

flat_success <- read_csv("data/2024-06-16 19-43-03_flatSuccess/results.csv")
flat_similarity <- read_csv("data/2024-06-16 20-55-35_flatSim/results.csv")
flat_age <- read_csv("data/2024-06-16 21-32-09_flatAge/results.csv")
flat_conformity <- read_csv("data/2024-06-16 21-50-10_flatConf/results.csv")
flat_fixed <- read_csv("data/2024-06-16 16-32-12_flatFixed/results.csv")
flat_flexible <- read_csv("data/2024-06-16 16-55-34_flatFlexible/results.csv")
flat_integrative <- read_csv("data/2024-06-16 17-46-35_flatIntegrative/results.csv")
flat_all <- read_csv("data/2024-06-15 21-01-29_flatAllMetas/results.csv")

### get Social Learning Success Rates for all Simulations ----

get_SLSuccessRate <- function(dat){
  dat %>%
    filter(!is.na(SLsucess)) %>%
    reframe(meanSuccessRate = mean(SLsucess))
  
}
SLSuccessRates <- data.frame(ChainSuccess = get_SLSuccessRate(chain_success), 
                             ChainSimilarity = get_SLSuccessRate(chain_Similarity), 
                             ChainAge = get_SLSuccessRate(chain_age),
                             ChainConformity = get_SLSuccessRate(chain_conformity),
                             ChainFixed = get_SLSuccessRate(chain_fixed), 
                             ChainFlexible = get_SLSuccessRate(chain_flexible), 
                             ChainIntegrative = get_SLSuccessRate(chain_integrative),
                             ChainAllMetas = get_SLSuccessRate(chain_all),
                             FlatSuccess = get_SLSuccessRate(flat_success), 
                             FlatSimilarity = get_SLSuccessRate(flat_similarity), 
                             FlatAge = get_SLSuccessRate(flat_age),
                             FlatConformity = get_SLSuccessRate(flat_conformity),
                             FlatFixed = get_SLSuccessRate(flat_fixed), 
                             FlatFlexible = get_SLSuccessRate(flat_flexible), 
                             FlatIntegrative = get_SLSuccessRate(flat_integrative),
                             FlatAllMetas = get_SLSuccessRate(flat_all)
)

names(SLSuccessRates) <- c("ChainSuccess", "ChainSimilarity", "ChainAge", "ChainConformity", 
                           "ChainFixed", "ChainFlexible", "ChainIntegrative", "ChainAllMetas", 
                           "FlatSuccess", "FlatSimilarity", "FlatAge", "FlatConformity", "FlatFixed", 
                           "FlatFlexible", "FlatIntegrative", "FlatAllMetas")
write.csv2(SLSuccessRates, file = "Social Learning Success Rates.txt", row.names = FALSE)

get_SLSuccessRate_overTime <- function(dat){
  dat %>%
    filter(!is.na(SLsucess)) %>%
    group_by(Timestep) %>%
    reframe(meanSuccessRate = mean(SLsucess), #
            sdSuccessRate = sd(SLsucess))
  
}

SLSuccess <- get_SLSuccessRate_overTime(chain_success) %>%
  mutate(Source = "Success")
SLSimilarity <- get_SLSuccessRate_overTime(chain_Similarity) %>%
  mutate(Source = "Similarity")
SLAge <- get_SLSuccessRate_overTime(chain_age) %>%
  mutate(Source = "Age")
SLConformity <- get_SLSuccessRate_overTime(chain_conformity) %>%
  mutate(Source = "Conformity")
SLFixed <- get_SLSuccessRate_overTime(chain_fixed) %>%
  mutate(Source = "Fixed")
SLFlexible <- get_SLSuccessRate_overTime(chain_flexible) %>%
  mutate(Source = "Flexible")
SLIntegrative <- get_SLSuccessRate_overTime(chain_integrative) %>%
  mutate(Source = "Integrative")
SLAll <- get_SLSuccessRate_overTime(chain_all) %>%
  mutate(Source = "All Meta Learners")


SLSuccessRateOverTime <- rbind(SLSuccess, SLSimilarity, SLAge, SLConformity, 
                               SLFixed, SLFlexible, SLIntegrative, SLAll)

SLSuccessRateOverTime %>%
  ggplot(aes(y = meanSuccessRate, x = Timestep, 
             color = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity", "Fixed", "Flexible", "Integrative", "All Meta Learners"), labels = c(labels_learningstrat, labels_metatypes, "All Meta Learners")),
             fill = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity", "Fixed", "Flexible", "Integrative", "All Meta Learners"), labels = c(labels_learningstrat, labels_metatypes, "All Meta Learners"))
  )) +
  geom_point(alpha = 0.05) +
  geom_smooth(linewidth = 1) +
  scale_color_manual(values = c(colors_learningstrat, colors_meta, color_all_metas)) +
  scale_fill_manual(values = c(colors_learningstrat, colors_meta, color_all_metas)) +
  labs(color = "Simulation", fill = "Simulation") +
  theme_classic()

### check age-number of skill correlation ----
chain_age %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Age,
       NumberSkills,
       .direction = "down") %>%
  select(Age, NumberSkills, Round, Timestep) %>%
  group_by(Round, Timestep) %>%
  reframe(CorrAgeSkill = cor(Age, NumberSkills)) %>%
  ggplot(aes(x = Timestep, y = CorrAgeSkill)) +
  geom_line()

chain_age %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Age,
       NumberSkills,
       .direction = "down") %>%
  select(Age, NumberSkills, Round, Timestep) %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  reframe(CorrAgeSkill = cor(Age, NumberSkills)) %>%
  reframe(meanCor = mean(CorrAgeSkill), 
          sdCor = sd(CorrAgeSkill))

chain_Similarity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Age,
       NumberSkills,
       .direction = "down") %>%
  select(Age, NumberSkills, Round, Timestep) %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  reframe(CorrAgeSkill = cor(Age, NumberSkills)) %>%
  reframe(meanCor = mean(CorrAgeSkill), 
          sdCor = sd(CorrAgeSkill))

chain_success %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Age,
       NumberSkills,
       .direction = "down") %>%
  select(Age, NumberSkills, Round, Timestep) %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  reframe(CorrAgeSkill = cor(Age, NumberSkills)) %>%
  reframe(meanCor = mean(CorrAgeSkill), 
          sdCor = sd(CorrAgeSkill))
### check attempted skill per Learning Strategy ----

## constrained tree 
payoff_ChainSuccess <- chain_success %>%
  mutate(Source = "Success")
payoff_ChainSimilarity <- chain_Similarity %>%
  mutate(Source = "Similarity")
payoff_ChainAge <-  chain_age %>%
  mutate(Source = "Age")
payoff_ChainConformity <-  chain_conformity %>%
  mutate(Source = "Conformity")

payoffChainLS <- rbind(payoff_ChainSuccess, payoff_ChainSimilarity, payoff_ChainAge, payoff_ChainConformity)

payoffChainLS %>%
  filter(!is.na(AttemptedSkill)) %>%
  group_by(Source, Round, ID) %>%
  mutate(LearningStep = 1:n()) %>%
  group_by(Source, LearningStep) %>% 
  reframe(AttemptedSkill = mean(AttemptedSkill)) %>%
  ggplot(aes(x = LearningStep, y = AttemptedSkill, color = factor(Source, levels = labels_learningstrat ,labels = labels_learningstrat))) +
  geom_line(linewidth = 2) +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color = "Simulation", x = "Learning Attempt", y = "Mean Attempted Skill Level") +
  xlim(c(0, 75))

ggsave("AttemptedSkillChainLS.png", width = 15, height = 12.5, units = "cm")

## unconstrained tree 
payoff_FlatSuccess <- flat_success %>%
  mutate(Source = "Success")
payoff_FlatSimilarity <- flat_similarity %>%
  mutate(Source = "Similarity")
payoff_FlatAge <-  flat_age %>%
  mutate(Source = "Age")
payoff_FlatConformity <-  flat_conformity %>%
  mutate(Source = "Conformity")

payoffFlatLS <- rbind(payoff_FlatSuccess, payoff_FlatSimilarity, payoff_FlatAge, payoff_FlatConformity)

payoffFlatLS %>%
  filter(!is.na(AttemptedSkill)) %>%
  group_by(Source, Round, ID) %>%
  mutate(LearningStep = 1:n()) %>%
  group_by(Source, LearningStep) %>% 
  reframe(AttemptedSkill = mean(AttemptedSkill)) %>%
  ggplot(aes(x = LearningStep, y = AttemptedSkill, color = factor(Source, levels = labels_learningstrat ,labels = labels_learningstrat))) +
  geom_line(linewidth = 2) +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color = "Simulation", x = "Learning Attempt", y = "Mean Attempted Skill Level") +
  xlim(c(0, 75))

ggsave("AttemptedSkillFlatLS.png", width = 15, height = 12.5, units = "cm")
### Beta Distributions in one plot ----

## chain tree
integrative_weights <- chain_integrative %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(PayoffLearningSuccess,
       PayoffLearningFails,
       SimilarityLearningSuccess,
       SimilarityLearningFails,
       AgeLearningSuccess,
       AgeLearningFails,
       ConformityLearningSuccess,
       ConformityLearningFails,
       .direction = "down") %>%
  ungroup() %>%  
  filter(Timestep == max(Timestep)) %>%
  summarise_at(vars(PayoffLearningSuccess:ConformityLearningFails), list(Mean = mean))

theta = seq(0, 1, length.out = 1e3)  
Integrative_betas <- data.frame(Theta = theta, 
                                Success = dbeta(theta,
                                                1+integrative_weights$PayoffLearningSuccess_Mean, 
                                                1+integrative_weights$PayoffLearningFails_Mean
                                ),
                                Similarity = dbeta(theta,
                                                   1+integrative_weights$SimilarityLearningSuccess_Mean, 
                                                   1+integrative_weights$SimilarityLearningFails_Mean),
                                Age = dbeta(theta, 
                                            1+integrative_weights$AgeLearningSuccess_Mean,
                                            1+integrative_weights$AgeLearningFails_Mean),
                                Conformity = dbeta(theta, 
                                                   1+integrative_weights$ConformityLearningSuccess_Mean,
                                                   1+integrative_weights$ConformityLearningFails_Mean)
) %>%
  pivot_longer(Success:Conformity,
               names_to = "Strategy", 
               values_to = "Density"
  ) %>%
  mutate(Strategy = factor(Strategy, levels = labels_learningstrat , labels = labels_learningstrat))


## Beta distributions integrative and flexible learner 

flexible_weights <- chain_flexible %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(PayoffLearningSuccess,
       PayoffLearningFails,
       SimilarityLearningSuccess,
       SimilarityLearningFails,
       AgeLearningSuccess,
       AgeLearningFails,
       ConformityLearningSuccess,
       ConformityLearningFails,
       .direction = "down") %>%
  ungroup()%>%
  filter(Timestep == max(Timestep)) %>%
  summarise_at(vars(PayoffLearningSuccess:ConformityLearningFails), list(Mean = mean))



Flexible_Betas <- data.frame(Theta = theta, 
                             Success = dbeta(theta,
                                             1+flexible_weights$PayoffLearningSuccess_Mean, 
                                             1+flexible_weights$PayoffLearningFails_Mean
                             ),
                             Similarity = dbeta(theta,
                                                1+flexible_weights$SimilarityLearningSuccess_Mean, 
                                                1+flexible_weights$SimilarityLearningFails_Mean),
                             Age = dbeta(theta, 
                                         1+flexible_weights$AgeLearningSuccess_Mean,
                                         1+flexible_weights$AgeLearningFails_Mean),
                             Conformity = dbeta(theta, 
                                                1+flexible_weights$ConformityLearningSuccess_Mean,
                                                1+flexible_weights$ConformityLearningFails_Mean)
) %>%
  pivot_longer(Success:Conformity,
               names_to = "Strategy", 
               values_to = "Density"
  ) %>%
  mutate(Strategy = factor(Strategy, levels = labels_learningstrat, labels = labels_learningstrat))

Flexible_Betas <- Flexible_Betas %>%
  mutate(Learner = "flexible")
Integrative_betas <- Integrative_betas %>%
  mutate(Learner = "integrative")

all_Betas <- rbind.data.frame(Flexible_Betas, Integrative_betas)

all_Betas %>%
  ggplot(aes(x = Theta, y = Density, color = Strategy)) +
  geom_vline(xintercept= SLSuccessRates$ChainSuccess, linetype="dashed", color = color_learningstrat1) +
  geom_vline(xintercept= SLSuccessRates$ChainSimilarity, linetype="dashed", color = color_learningstrat2) +
  geom_vline(xintercept= SLSuccessRates$ChainAge, linetype="dashed", color = color_learningstrat3) +
  geom_vline(xintercept= SLSuccessRates$ChainConformity, linetype="dashed", color = color_learningstrat4) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = c(colors_learningstrat, colors_learningstrat)) +
  labs(color=labs_legend_learningstrat, x = "Estimated Probability of Success") +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  facet_wrap(~factor(Learner, levels = c("flexible", "integrative"), 
                     labels = c("Flexible Learner", "Integrative Learner"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        strip.background = element_rect(
          color="black", 
          fill="white")) +
  scale_x_continuous(breaks = seq(0,1,0.1))

ggsave("ChainallBetas.png", width = 30, height = 13, units = "cm")

## flat tree
integrative_weights <- flat_integrative %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(PayoffLearningSuccess,
       PayoffLearningFails,
       SimilarityLearningSuccess,
       SimilarityLearningFails,
       AgeLearningSuccess,
       AgeLearningFails,
       ConformityLearningSuccess,
       ConformityLearningFails,
       .direction = "down") %>%
  ungroup() %>%  
  filter(Timestep == max(Timestep)) %>%
  summarise_at(vars(PayoffLearningSuccess:ConformityLearningFails), list(Mean = mean))

theta = seq(0, 1, length.out = 1e3)  
Integrative_betas <- data.frame(Theta = theta, 
                                Success = dbeta(theta,
                                                1+integrative_weights$PayoffLearningSuccess_Mean, 
                                                1+integrative_weights$PayoffLearningFails_Mean
                                ),
                                Similarity = dbeta(theta,
                                                   1+integrative_weights$SimilarityLearningSuccess_Mean, 
                                                   1+integrative_weights$SimilarityLearningFails_Mean),
                                Age = dbeta(theta, 
                                            1+integrative_weights$AgeLearningSuccess_Mean,
                                            1+integrative_weights$AgeLearningFails_Mean),
                                Conformity = dbeta(theta, 
                                                   1+integrative_weights$ConformityLearningSuccess_Mean,
                                                   1+integrative_weights$ConformityLearningFails_Mean)
) %>%
  pivot_longer(Success:Conformity,
               names_to = "Strategy", 
               values_to = "Density"
  ) %>%
  mutate(Strategy = factor(Strategy, levels = labels_learningstrat , labels = labels_learningstrat))


## Beta distributions integrative and flexible learner 

flexible_weights <- flat_flexible %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(PayoffLearningSuccess,
       PayoffLearningFails,
       SimilarityLearningSuccess,
       SimilarityLearningFails,
       AgeLearningSuccess,
       AgeLearningFails,
       ConformityLearningSuccess,
       ConformityLearningFails,
       .direction = "down") %>%
  ungroup()%>%
  filter(Timestep == max(Timestep)) %>%
  summarise_at(vars(PayoffLearningSuccess:ConformityLearningFails), list(Mean = mean))



Flexible_Betas <- data.frame(Theta = theta, 
                             Success = dbeta(theta,
                                             1+flexible_weights$PayoffLearningSuccess_Mean, 
                                             1+flexible_weights$PayoffLearningFails_Mean
                             ),
                             Similarity = dbeta(theta,
                                                1+flexible_weights$SimilarityLearningSuccess_Mean, 
                                                1+flexible_weights$SimilarityLearningFails_Mean),
                             Age = dbeta(theta, 
                                         1+flexible_weights$AgeLearningSuccess_Mean,
                                         1+flexible_weights$AgeLearningFails_Mean),
                             Conformity = dbeta(theta, 
                                                1+flexible_weights$ConformityLearningSuccess_Mean,
                                                1+flexible_weights$ConformityLearningFails_Mean)
) %>%
  pivot_longer(Success:Conformity,
               names_to = "Strategy", 
               values_to = "Density"
  ) %>%
  mutate(Strategy = factor(Strategy, levels = labels_learningstrat, labels = labels_learningstrat))

Flexible_Betas <- Flexible_Betas %>%
  mutate(Learner = "flexible")
Integrative_betas <- Integrative_betas %>%
  mutate(Learner = "integrative")

all_Betas <- rbind.data.frame(Flexible_Betas, Integrative_betas)

all_Betas %>%
  ggplot(aes(x = Theta, y = Density, color = Strategy)) +
  geom_vline(xintercept= SLSuccessRates$FlatSuccess, linetype="dashed", color = color_learningstrat1) +
  geom_vline(xintercept= SLSuccessRates$FlatSimilarity, linetype="dashed", color = color_learningstrat2) +
  geom_vline(xintercept= SLSuccessRates$FlatAge, linetype="dashed", color = color_learningstrat3) +
  geom_vline(xintercept= SLSuccessRates$FlatConformity, linetype="dashed", color = color_learningstrat4) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = c(colors_learningstrat, colors_learningstrat)) +
  labs(color=labs_legend_learningstrat, x = "Estimated Probability of Success") +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  facet_wrap(~factor(Learner, levels = c("flexible", "integrative"), 
                     labels = c("Flexible Learner", "Integrative Learner"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        strip.background = element_rect(
          color="black", 
          fill="white")) +
  scale_x_continuous(breaks = seq(0,1,0.1))

ggsave("FlatallBetas.png", width = 30, height = 13, units = "cm")
### Denisties between Rounds ----

# flat tree
skills_success <- flat_success %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Success")

skills_success %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  mutate(MeanSkills = mean(NumberSkills), 
         medianSkills = median(NumberSkills)) %>%
  ggplot(aes(y = NumberSkills, x = factor(Round))) +
  geom_boxplot() +
  theme_classic()


skills_success %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 
ggsave("FlatSuccessDensities.png", width = 30, height = 13, units = "cm")


skills_success %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  reframe(MeanSkills = mean(NumberSkills), 
          medianSkills = median(NumberSkills)) %>%
  filter(medianSkills > 35) %>%
  nrow()


skills_similarity <- flat_similarity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Similarity")

skills_similarity %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 
ggsave("FlatSimilarityDensities.png", width = 30, height = 13, units = "cm")


skills_similarity %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  mutate(MeanSkills = mean(NumberSkills), 
         medianSkills = median(NumberSkills)) %>%
  ggplot(aes(y = medianSkills, x = factor(Round))) +
  geom_point() +
  theme_classic()

skills_similarity %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  reframe(MeanSkills = mean(NumberSkills), 
          medianSkills = median(NumberSkills)) %>%
  filter(medianSkills > 35) %>%
  nrow()

skills_age <- flat_age %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Age")

skills_age %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 

ggsave("FlatAgeDensities.png", width = 30, height = 13, units = "cm")


skills_age %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  mutate(MeanSkills = mean(NumberSkills), 
         medianSkills = median(NumberSkills)) %>%
  ggplot(aes(y = medianSkills, x = factor(Round))) +
  geom_point() +
  theme_classic()

skills_age %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  reframe(MeanSkills = mean(NumberSkills), 
          medianSkills = median(NumberSkills)) %>%
  filter(medianSkills > 35) %>%
  nrow()

skills_conformity <- flat_conformity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Conformity")

skills_conformity %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 

ggsave("FlatConformityDensities.png", width = 30, height = 13, units = "cm")


skills_conformity %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  mutate(MeanSkills = mean(NumberSkills), 
         medianSkills = median(NumberSkills)) %>%
  ggplot(aes(y = medianSkills, x = factor(Round))) +
  geom_point() +
  theme_classic()


skills_conformity %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  reframe(MeanSkills = mean(NumberSkills), 
          medianSkills = median(NumberSkills)) %>%
  filter(medianSkills > 35) %>%
  nrow()

skills_fixed <- flat_fixed %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Fixed")

skills_fixed %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 

ggsave("FlatFixedDensities.png", width = 30, height = 13, units = "cm")

skills_flexible <- flat_flexible %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Flexible")

skills_flexible %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 

ggsave("FlatFlexibleDensities.png", width = 30, height = 13, units = "cm")


skills_integrative <- flat_integrative %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Integrative")

skills_integrative %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 

ggsave("FlatIntegrativeDensities.png", width = 30, height = 13, units = "cm")



skills_all <- flat_all %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "All")

skills_all %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 

ggsave("FlatAllMetasDensities.png", width = 30, height = 13, units = "cm")

### chain tree

skills_success <- chain_success %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Success")

skills_success %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  mutate(MeanSkills = mean(NumberSkills), 
         medianSkills = median(NumberSkills)) %>%
  ggplot(aes(y = NumberSkills, x = factor(Round))) +
  geom_boxplot() +
  theme_classic()


skills_success %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 
ggsave("chainSuccessDensities.png", width = 30, height = 13, units = "cm")


skills_success %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  reframe(MeanSkills = mean(NumberSkills), 
          medianSkills = median(NumberSkills)) %>%
  filter(medianSkills < 5) %>%
  nrow()


skills_similarity <- chain_similarity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Similarity")

skills_similarity %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 
ggsave("chainSimilarityDensities.png", width = 30, height = 13, units = "cm")


skills_similarity %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  mutate(MeanSkills = mean(NumberSkills), 
         medianSkills = median(NumberSkills)) %>%
  ggplot(aes(y = medianSkills, x = factor(Round))) +
  geom_point() +
  theme_classic()

skills_similarity %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  reframe(MeanSkills = mean(NumberSkills), 
          medianSkills = median(NumberSkills)) %>%
  filter(medianSkills > 35) %>%
  nrow()

skills_age <- chain_age %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Age")

skills_age %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 

ggsave("chainAgeDensities.png", width = 30, height = 13, units = "cm")


skills_age %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  mutate(MeanSkills = mean(NumberSkills), 
         medianSkills = median(NumberSkills)) %>%
  ggplot(aes(y = medianSkills, x = factor(Round))) +
  geom_point() +
  theme_classic()

skills_age %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  reframe(MeanSkills = mean(NumberSkills), 
          medianSkills = median(NumberSkills)) %>%
  filter(medianSkills > 35) %>%
  nrow()

skills_conformity <- chain_conformity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Conformity")

skills_conformity %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 

ggsave("chainConformityDensities.png", width = 30, height = 13, units = "cm")


skills_conformity %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  mutate(MeanSkills = mean(NumberSkills), 
         medianSkills = median(NumberSkills)) %>%
  ggplot(aes(y = medianSkills, x = factor(Round))) +
  geom_point() +
  theme_classic()


skills_conformity %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  reframe(MeanSkills = mean(NumberSkills), 
          medianSkills = median(NumberSkills)) %>%
  filter(medianSkills > 35) %>%
  nrow()

skills_fixed <- chain_fixed %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Fixed")

skills_fixed %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 

ggsave("chainFixedDensities.png", width = 30, height = 13, units = "cm")

skills_flexible <- chain_flexible %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Flexible")

skills_flexible %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 

ggsave("chainFlexibleDensities.png", width = 30, height = 13, units = "cm")


skills_integrative <- chain_integrative %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Integrative")

skills_integrative %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 

ggsave("chainIntegrativeDensities.png", width = 30, height = 13, units = "cm")



skills_all <- chain_all %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "All")

skills_all %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  group_by(Round) %>%
  ggplot(aes(x=NumberSkills, color = factor(Round))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"), 
        legend.position = "none") 

ggsave("chainAllMetasDensities.png", width = 30, height = 13, units = "cm")
### compare simulations ----
## Learning Strats ----
# skills ----

### constrained tree
# skills at last timestep boxplots
skills_success <- chain_success %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Success")

skills_similarity <- chain_Similarity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Similarity")

skills_age <- chain_age %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Age")

skills_conformity <- chain_conformity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Conformity")

All_skills <- rbind(skills_success, skills_similarity, skills_age, skills_conformity)

# plot skills per Sim boxplot
All_skills %>%
  filter(Timestep == max(Timestep)) %>%
  mutate(Source = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity"), labels = labels_learningstrat)) %>%
  group_by(Source, Round) %>%
  ggplot(aes(y = NumberSkills, x = Source)) +
  geom_boxplot(color = colors_learningstrat) +
  labs(y = "Number of Skills", x = "Simulation") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  ylim(c(0, 40))

ggsave("AllSkillsLSChain.png", width = 15, height = 12.5, units = "cm")

# skills over time
All_skills %>%
  filter(Timestep %in% c(0, 1000, 2000, 3000, 4000, 5000)) %>%
  group_by(Source, Round, Timestep) %>%
  ggplot(aes(x = factor(Timestep), 
             color = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity"), labels = labels_learningstrat), 
             y = NumberSkills,
             fill = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity"), labels = labels_learningstrat)
  )) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.6,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    alpha = 0.5, 
    scale = 0.5
  ) +
  geom_boxplot(
    width = 0.3,
    # removing outliers
    outlier.color = NA,
    alpha = 0.2, 
    position = position_dodge(width = 0.5)
  ) +
  theme_classic() +
  scale_color_manual(values = colors_learningstrat, guide = "none") +
  scale_fill_manual(values = colors_learningstrat) +
  labs(fill = "Simulation", y = "Number of Skills", x = "Timesteps") +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )
ggsave("SimulationComparisonRainbowPlotSkillsLSChain.png", width = 30, height = 13, units = "cm")

### unconstrained tree
# skills at last timestep boxplots
skills_success <- flat_success %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Success")

skills_similarity <- flat_similarity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Similarity")

skills_age <- flat_age %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Age")

skills_conformity <- flat_conformity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Conformity")

All_skills <- rbind(skills_success, skills_similarity, skills_age, skills_conformity)

# plot skills per Sim boxplot
All_skills %>%
  filter(Timestep == max(Timestep)) %>%
  mutate(Source = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity"), labels = labels_learningstrat)) %>%
  group_by(Source, Round) %>%
  ggplot(aes(y = NumberSkills, x = Source)) +
  geom_boxplot(color = colors_learningstrat) +
  labs(y = "Number of Skills", x = "Simulation") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  ylim(c(0, 40))

ggsave("AllSkillsLSFlat.png", width = 15, height = 12.5, units = "cm")

# skills over time
All_skills %>%
  filter(Timestep %in% c(0, 1000, 2000, 3000, 4000, 5000)) %>%
  group_by(Source, Round, Timestep) %>%
  ggplot(aes(x = factor(Timestep), 
             color = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity"), labels = labels_learningstrat), 
             y = NumberSkills,
             fill = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity"), labels = labels_learningstrat)
  )) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.6,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    alpha = 0.5, 
    scale = 0.5
  ) +
  geom_boxplot(
    width = 0.3,
    # removing outliers
    outlier.color = NA,
    alpha = 0.2, 
    position = position_dodge(width = 0.5)
  ) +
  theme_classic() +
  scale_color_manual(values = colors_learningstrat, guide = "none") +
  scale_fill_manual(values = colors_learningstrat) +
  labs(fill = "Simulation", y = "Number of Skills", x = "Timesteps") +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )
ggsave("SimulationComparisonRainbowPlotSkillsLS.png", width = 30, height = 13, units = "cm")
# Payoffs ----
## constrained tree
payoff_success <- chain_success %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Success")

payoff_similarity <- chain_Similarity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Similarity")

payoff_age <- chain_age %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Age")

payoff_conformity <- chain_conformity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Conformity")

All_payoffs <- rbind(payoff_success, payoff_similarity, payoff_age, payoff_conformity)

# boxplot with total payoffs at last timestep
All_payoffs %>%
  filter(Timestep == max(Timestep)) %>%
  mutate(Source = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity"), labels = labels_learningstrat)) %>%
  group_by(Source, Round) %>%
  summarise_at(vars(Payoff), list(total=sum)) %>%
  ggplot(aes(x = Source, y = total)) +
  geom_boxplot(color = colors_learningstrat) +
  labs(y = "Total Payoff", x = "Simulation") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) 

ggsave("AllPayoffsLSFlatChain.png", width = 13, height = 12.5, units = "cm")

# payoffs over time
All_payoffs %>%
  filter(Timestep %in% c(0, 1000, 2000, 3000, 4000, 5000)) %>%
  group_by(Source, Round, Timestep) %>%
  reframe(TotalPayoff = sum(Payoff)) %>%
  ggplot(aes(x = factor(Timestep), 
             color = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity"), labels = labels_learningstrat), 
             y = TotalPayoff,
             fill = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity"), labels = labels_learningstrat)
  )) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.6,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    alpha = 0.5, 
    scale = 0.5
  ) +
  geom_boxplot(
    width = 0.3,
    # removing outliers
    outlier.color = NA,
    alpha = 0.2, 
    position = position_dodge(width = 0.5)
  ) +
  theme_classic() +
  scale_color_manual(values = colors_learningstrat, guide = "none") +
  scale_fill_manual(values = colors_learningstrat) +
  labs(fill = "Simulation", y = "Total Payoff", x = "Timesteps") +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )
ggsave("SimulationComparisonRainbowPlotPayoffsLSChain.png", width = 30, height = 13, units = "cm")

### unconstrained tree
payoff_success <- flat_success %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Success")

payoff_similarity <- flat_similarity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Similarity")

payoff_age <- flat_age %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Age")

payoff_conformity <- flat_conformity %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Conformity")

All_payoffs <- rbind(payoff_success, payoff_similarity, payoff_age, payoff_conformity)

# boxplot with total payoffs at last timestep
All_payoffs %>%
  filter(Timestep == max(Timestep)) %>%
  mutate(Source = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity"), labels = labels_learningstrat)) %>%
  group_by(Source, Round) %>%
  summarise_at(vars(Payoff), list(total=sum)) %>%
  ggplot(aes(x = Source, y = total)) +
  geom_boxplot(color = colors_learningstrat) +
  labs(y = "Total Payoff", x = "Simulation") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) 

ggsave("AllPayoffsLSFlat.png", width = 13, height = 12.5, units = "cm")

# payoffs over time
All_payoffs %>%
  filter(Timestep %in% c(0, 1000, 2000, 3000, 4000, 5000)) %>%
  group_by(Source, Round, Timestep) %>%
  reframe(TotalPayoff = sum(Payoff)) %>%
  ggplot(aes(x = factor(Timestep), 
             color = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity"), labels = labels_learningstrat), 
             y = TotalPayoff,
             fill = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity"), labels = labels_learningstrat)
  )) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.6,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    alpha = 0.5, 
    scale = 0.5
  ) +
  geom_boxplot(
    width = 0.3,
    # removing outliers
    outlier.color = NA,
    alpha = 0.2, 
    position = position_dodge(width = 0.5)
  ) +
  theme_classic() +
  scale_color_manual(values = colors_learningstrat, guide = "none") +
  scale_fill_manual(values = colors_learningstrat) +
  labs(fill = "Simulation", y = "Total Payoff", x = "Timesteps") +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )
ggsave("SimulationComparisonRainbowPlotPayoffsLSFlat.png", width = 30, height = 13, units = "cm")
## Meta Strats ----
# skills ----
### constrained tree
skills_fixed <- chain_fixed %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Fixed")

skills_flexible <- chain_flexible %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Flexible")

skills_integrative <- chain_integrative %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Integrative")

skills_allMetas <- chain_all %>% 
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "AllMetas")

All_skillsML <- rbind(skills_fixed, skills_flexible, skills_integrative, skills_allMetas)

# boxplot skills at the end of simulations
All_skillsML %>%
  filter(Timestep == max(Timestep)) %>%
  mutate(Source = factor(Source, levels = c("Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_metatypes, "All Meta Learners"))) %>%
  group_by(Source, Round) %>%
  ggplot(aes(y = NumberSkills, x = Source)) +
  geom_boxplot(color = c(colors_meta, color_all_metas)) +
  labs(y = "Mean Number of Skills", x = "Simulation") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  ylim(c(0,40))

ggsave("SimulationsSkillsMetaChain.png", width = 15, height = 12.5, units = "cm")

# skills over time
All_skillsML %>%
  filter(Timestep %in% c(0, 1000, 2000, 3000, 4000, 5000)) %>%
  group_by(Source, Round, Timestep) %>%
  ggplot(aes(x = factor(Timestep), 
             color = factor(Source, levels = c("Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_metatypes, "All Meta Learners")), 
             y = NumberSkills,
             fill = factor(Source, levels = c("Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_metatypes, "All Meta Learners"))
  )) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.6,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    alpha = 0.5, 
    scale = 0.5
  ) +
  geom_boxplot(
    width = 0.3,
    # removing outliers
    outlier.color = NA,
    alpha = 0.2, 
    position = position_dodge(width = 0.5)
  ) +
  theme_classic() +
  scale_color_manual(values = c(colors_meta, color_all_metas), guide = "none") +
  scale_fill_manual(values = c(colors_meta, color_all_metas)) +
  labs(fill = "Simulation", y = "Number of Skills", x = "Timesteps") +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )
ggsave("SimulationComparisonRainbowPlotSkillsMLChain.png", width = 30, height = 13, units = "cm")

### unconstrained tree
skills_fixed <- flat_fixed %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Fixed")

skills_flexible <- flat_flexible %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Flexible")

skills_integrative <- flat_integrative %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "Integrative")

skills_allMetas <- flat_all %>% 
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills, 
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  mutate(Source = "AllMetas")

All_skillsML <- rbind(skills_fixed, skills_flexible, skills_integrative, skills_allMetas)

# boxplot skills at the end of simulations
All_skillsML %>%
  filter(Timestep == max(Timestep)) %>%
  mutate(Source = factor(Source, levels = c("Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_metatypes, "All Meta Learners"))) %>%
  group_by(Source, Round) %>%
  ggplot(aes(y = NumberSkills, x = Source)) +
  geom_boxplot(color = c(colors_meta, color_all_metas)) +
  labs(y = "Mean Number of Skills", x = "Simulation") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  ylim(c(0,40))

ggsave("SimulationsSkillsMetaFlat.png", width = 15, height = 12.5, units = "cm")

# skills over time
All_skillsML %>%
  filter(Timestep %in% c(0, 1000, 2000, 3000, 4000, 5000)) %>%
  group_by(Source, Round, Timestep) %>%
  ggplot(aes(x = factor(Timestep), 
             color = factor(Source, levels = c("Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_metatypes, "All Meta Learners")), 
             y = NumberSkills,
             fill = factor(Source, levels = c("Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_metatypes, "All Meta Learners"))
  )) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.6,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    alpha = 0.5, 
    scale = 0.5
  ) +
  geom_boxplot(
    width = 0.3,
    # removing outliers
    outlier.color = NA,
    alpha = 0.2, 
    position = position_dodge(width = 0.5)
  ) +
  theme_classic() +
  scale_color_manual(values = c(colors_meta, color_all_metas), guide = "none") +
  scale_fill_manual(values = c(colors_meta, color_all_metas)) +
  labs(fill = "Simulation", y = "Number of Skills", x = "Timesteps") +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )
ggsave("SimulationComparisonRainbowPlotSkillsMLFlatChain.png", width = 30, height = 13, units = "cm")
# payoffs ----
## constrained tree
payoff_fixed <- chain_fixed %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Fixed")

payoff_flexible <- chain_flexible %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Flexible")

payoff_integrative <- chain_integrative %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Integrative")

payoff_allMetas <- chain_all %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "AllMetas")

All_payoffs_ML <- rbind(payoff_fixed, payoff_flexible, payoff_integrative, payoff_allMetas)

All_payoffs_ML %>%
  filter(Timestep == max(Timestep)) %>%
  mutate(Source = factor(Source, levels = c("Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_metatypes, "All Meta Learners"))) %>%
  group_by(Source, Round) %>%
  reframe(TotalPayoff = sum(Payoff)) %>%
  ggplot(aes(x = Source, y = TotalPayoff)) +
  geom_boxplot(color = c(colors_meta, color_all_metas)) +
  labs(y = "Total Payoff", x = "Simulation") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )

ggsave("PayoffsMLSimulationsChain.png", width = 15, height = 12.5, units = "cm")

# payoffs over time
All_payoffs_ML %>%
  filter(Timestep %in% c(0, 1000, 2000, 3000, 4000, 5000)) %>%
  group_by(Source, Round, Timestep) %>%
  reframe(TotalPayoff = sum(Payoff)) %>%
  ggplot(aes(x = factor(Timestep), 
             color = factor(Source, levels = c("Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_metatypes, "All Meta Learners")), 
             y = TotalPayoff,
             fill = factor(Source, levels = c("Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_metatypes, "All Meta Learners"))
  )) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.6,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    alpha = 0.5, 
    scale = 0.5
  ) +
  geom_boxplot(
    width = 0.3,
    # removing outliers
    outlier.color = NA,
    alpha = 0.2, 
    position = position_dodge(width = 0.5)
  ) +
  theme_classic() +
  scale_color_manual(values = c(colors_meta, color_all_metas), guide = "none") +
  scale_fill_manual(values = c(colors_meta, color_all_metas)) +
  labs(fill = "Simulation", y = "Total Payoff", x = "Timesteps") +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )
ggsave("SimulationComparisonRainbowPlotPayoffsMLChain.png", width = 30, height = 13, units = "cm")

## unconstrained tree

payoff_fixed <- flat_fixed %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Fixed")

payoff_flexible <- flat_flexible %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Flexible")

payoff_integrative <- flat_integrative %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "Integrative")

payoff_allMetas <- flat_all %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(Payoff, 
       .direction = "down") %>%
  select(Timestep, Payoff) %>%
  mutate(Source = "AllMetas")

All_payoffs_ML <- rbind(payoff_fixed, payoff_flexible, payoff_integrative, payoff_allMetas)

All_payoffs_ML %>%
  filter(Timestep == max(Timestep)) %>%
  mutate(Source = factor(Source, levels = c("Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_metatypes, "All Meta Learners"))) %>%
  group_by(Source, Round) %>%
  reframe(TotalPayoff = sum(Payoff)) %>%
  ggplot(aes(x = Source, y = TotalPayoff)) +
  geom_boxplot(color = c(colors_meta, color_all_metas)) +
  labs(y = "Total Payoff", x = "Simulation") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )

ggsave("PayoffsMLSimulationsFlat.png", width = 15, height = 12.5, units = "cm")

# payoffs over time
All_payoffs_ML %>%
  filter(Timestep %in% c(0, 1000, 2000, 3000, 4000, 5000)) %>%
  group_by(Source, Round, Timestep) %>%
  reframe(TotalPayoff = sum(Payoff)) %>%
  ggplot(aes(x = factor(Timestep), 
             color = factor(Source, levels = c("Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_metatypes, "All Meta Learners")), 
             y = TotalPayoff,
             fill = factor(Source, levels = c("Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_metatypes, "All Meta Learners"))
  )) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # move to the right
    justification = -0.6,
    # remove the slub interval
    .width = 0,
    point_colour = NA,
    alpha = 0.5, 
    scale = 0.5
  ) +
  geom_boxplot(
    width = 0.3,
    # removing outliers
    outlier.color = NA,
    alpha = 0.2, 
    position = position_dodge(width = 0.5)
  ) +
  theme_classic() +
  scale_color_manual(values = c(colors_meta, color_all_metas), guide = "none") +
  scale_fill_manual(values = c(colors_meta, color_all_metas)) +
  labs(fill = "Simulation", y = "Total Payoff", x = "Timesteps") +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )
ggsave("SimulationComparisonRainbowPlotPayoffsMLFlat.png", width = 30, height = 13, units = "cm")


### all Simulations in one plot ----
# skills
AllSim_skills <- rbind(All_skills, All_skillsML)

AllSim_skills %>%
  filter(Timestep == max(Timestep)) %>%
  mutate(Source = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity", "Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_learningstrat, labels_metatypes, "All Meta Learners"))) %>%
  group_by(Source, Round) %>%
  ggplot(aes(y = NumberSkills, x = Source)) +
  geom_boxplot(color = c(colors_learningstrat, colors_meta, color_all_metas)) +
  labs(y = "Number of Skills", x = "Simulation") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  ylim(c(0, 40))

ggsave("AllSimulationsSkillss.png", width = 30, height = 12.5, units = "cm")

AllSimulations_Skills <- AllSim_skills %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Source) %>%
  reframe(medianSkills = median(NumberSkills), 
          lowerquantSkills = as.numeric(quantile(NumberSkills)[2]),
          upperquantSkills = as.numeric(quantile(NumberSkills)[4])
  )

write.csv2(AllSimulations_Skills, file = "AllSimulationsSkills.txt", row.names = FALSE)

# pop development skills 

AllSim_skills %>%
  group_by(Round) %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  ggplot(aes(x=NumberSkills, color = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity", "Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_learningstrat, labels_metatypes, "All Meta Learners")))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  scale_color_manual(values = c(colors_learningstrat, colors_meta, color_all_metas)) +
  labs(color="Simulation", y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) 

ggsave("AllSimPopDev.png", width = 30, height = 13, units = "cm")

# payoffs
AllSim_payoffs <- rbind(All_payoffs, All_payoffs_ML)

p <- AllSim_payoffs %>%
  filter(Timestep == max(Timestep)) %>%
  mutate(Source = factor(Source, levels = c("Success", "Similarity", "Age", "Conformity", "Fixed", "Flexible", "Integrative", "AllMetas"), labels = c(labels_learningstrat, labels_metatypes, "All Meta Learners"))) %>%
  group_by(Source, Round) %>%
  reframe(TotalPayoff = sum(Payoff)) %>%
  ggplot(aes(y = TotalPayoff, x = Source)) +
  geom_boxplot(color = c(colors_learningstrat, colors_meta, color_all_metas)) +
  labs(y = "Total Payoff", x = "Simulation") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) + ylim(c(0, 20000))
gg.y_gap(p, y_segment_start = 20000, y_segment_end = 40000, break_step = 5000)
ggsave("AllSimulationsPayoffs.png", width = 30, height = 12.5, units = "cm")

AllSimulations_Payoffs <- AllSim_payoffs %>%
  filter(Timestep == max(Timestep)) %>%
  group_by(Source, Round) %>%
  reframe(TotalPayoff = sum(Payoff)) %>%
  group_by(Source) %>%
  reframe(medianPayoff = median(TotalPayoff), 
          lowerquantPayoff = as.numeric(quantile(TotalPayoff)[2]),
          upperquantPayoff = as.numeric(quantile(TotalPayoff)[4])
  )

write.csv2(AllSimulations_Payoffs, file = "AllSimulationsPayoffs.txt", row.names = FALSE)