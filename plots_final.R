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
### check how many rounds individuals learn ----
overview %>%
  filter(Timestep >= 1) %>%
  group_by(Round, ID) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = n)) +
  geom_density(linewidth = 1) +
  labs(x = "Number of Learning Rounds", y = "Density") +
  theme_classic() + 
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  )

ggsave("LearningRounds.png", width = 13, height = 12.5, units = "cm", path = data_folder)

LearningRounds <- overview %>%
  filter(Timestep >= 1) %>%
  group_by(Round, ID) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  reframe(MeanLearningRounds = mean(n),
          SDLearningRounds = sd(n))

write.csv2(LearningRounds, file = paste0(data_folder, "/LearningRounds.txt"), row.names = FALSE)

## control how many eligible teachers inds find per skill level ----
overview %>%
  filter(Timestep >= 1) %>%
  group_by(NumberSkills) %>%
  reframe(EligibleTeachers = mean(EligibleTeachers)) %>%
  ggplot() +
  geom_line(aes(x = NumberSkills, y = EligibleTeachers), linewidth = 1.5) +
  theme_classic() +
  labs(y = "Number of Eligible Teachers", x = "Number of Skills") +
  geom_hline(yintercept=1, linetype="dashed", color = "grey")

ggsave("EligibleTeachers.png", width = 13, height = 12.5, units = "cm", path = data_folder)

# Eligible Teachers per Meta Learning Type
if (ML_on == "all"){
  overview %>%
    filter(Timestep >= 1) %>%
    group_by(NumberSkills, MetaStrat) %>%
    reframe(EligibleTeachers = mean(EligibleTeachers)) %>%
    ggplot() +
    geom_line(aes(x = NumberSkills, 
                  y = EligibleTeachers,
                  color = factor(MetaStrat, labels = labels_metatypes))
              , linewidth = 1) +
    theme_classic() +
    scale_color_manual(values = colors_meta) +
    labs(y = "Number of Eligible Teachers", x = "Number of Skills", color = labs_legend_meta) +
    geom_hline(yintercept=1, linetype="dashed", color = "grey")
  
  ggsave("EligibleTeachersMetaLearners.png", width = 13, height = 12.5, units = "cm", path = data_folder)
}

# Eligible Teachers per Learning Strategy
if (ML_on == "all" | ML_on == 1 & LS_on == "all"){
  overview %>%
    filter(Timestep >= 1) %>%
    filter(MetaStrat == 1) %>%
    group_by(NumberSkills, LearningStrat) %>%
    reframe(EligibleTeachers = mean(EligibleTeachers)) %>%
    ggplot() +
    geom_line(aes(x = NumberSkills, 
                  y = EligibleTeachers,
                  color = factor(LearningStrat, labels = labels_learningstrat))
              , linewidth = 1) +
    theme_classic() +
    scale_color_manual(values = colors_learningstrat) +
    labs(y = "Number of Eligible Teachers", x = "Number of Skills", color = labs_legend_learningstrat) +
    geom_hline(yintercept=1, linetype="dashed", color = "grey")
  
  ggsave("EligibleTeachersLS.png", width = 13, height = 12.5, units = "cm", path = data_folder)
}



## check skill attempts per Learning Strategy ----
if (ML_on == "all" | ML_on == 1 & LS_on == "all"){
overview %>%
  filter(Timestep > 0, 
         MetaStrat == 1,
         !is.na(AttemptedSkill)) %>%
  group_by(Round, ID) %>%
  mutate(LearningStep = 1:n()) %>%
  group_by(LearningStrat, LearningStep) %>% 
  reframe(AttemptedSkill = mean(AttemptedSkill)) %>%
  ggplot(aes(x = LearningStep, y = AttemptedSkill, color = factor(LearningStrat, labels = labels_learningstrat))) +
  geom_line(linewidth = 2) +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat, y = "Attempted Skill", x = "Learning Attempt")

ggsave("LearningAttemptsSkillLevel.png", width = 17, height = 12.5, units = "cm", path = data_folder)


overview %>%
  filter(MetaStrat == 1,
         !is.na(AttemptedSkill)) %>%
  group_by(Round, ID) %>%
  mutate(LearningStep = 1:n()) %>%
  group_by(LearningStrat, LearningStep) %>% 
  reframe(Payoff = mean(Payoff)) %>%
  ggplot(aes(x = LearningStep, y = Payoff, color = factor(LearningStrat, labels = labels_learningstrat))) +
  geom_line(linewidth = 2) +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat, x = "Learning Attempt", y = "Mean Payoff of Attempted Skill")

ggsave("LearningAttemptsPayoff.png", width = 17, height = 12.5, units = "cm", path = data_folder)
}
### Social Learning Success Rates ----

# per Meta Learning Type
if(ML_on == "all"){
overview %>%
  group_by(MetaStrat) %>%
  filter(!is.na(SLsucess)) %>%
  reframe(SLsucess = mean(SLsucess)) %>%
  ggplot(aes(x = factor(MetaStrat, labels=labels_metatypes), y = SLsucess)) +
  geom_col(fill = colors_meta) +
  labs(x = labs_legend_meta, y = "Social Learning Success Rate") +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  ylim(c(0,1))

ggsave("SLSuccessMeta.png", width = 12.5, height = 12.5, units = "cm", path = data_folder)

SL_successMeta <- overview %>%
  group_by(MetaStrat) %>%
  filter(!is.na(SLsucess)) %>%
  reframe(SLsucessMean = mean(SLsucess), 
          SLsucessSD = sd(SLsucess))

write.csv2(SL_successMeta, file = paste0(data_folder, "/SLSuccessRatesMeta.txt"), row.names = FALSE)

}

# per Learning Strat
if(ML_on == "all" | ML_on == 1 & LS_on == "all"){
overview %>%
  group_by(LearningStrat) %>%
  filter(!is.na(SLsucess)) %>%
  filter(MetaStrat == 1) %>%
  reframe(SLsucess = mean(SLsucess)) %>%
  ggplot(aes(x = factor(LearningStrat, labels=labels_learningstrat), y = SLsucess)) +
  geom_col(fill = colors_learningstrat) +
  labs(x = labs_legend_learningstrat, y = "Social Learning Success Rate") +
  theme_classic()+
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  ylim(c(0,1))
  
ggsave("SLSuccessLearningStrat.png", width = 12.5, height = 12.5, units = "cm", path = data_folder)

SL_successLStrats <- overview %>%
  group_by(LearningStrat) %>%
  filter(!is.na(SLsucess)) %>%
  filter(MetaStrat == 1) %>%
  reframe(SLsucessMean = mean(SLsucess), 
          SLsucessSD = sd(SLsucess))

write.csv2(SL_successLStrats, file = paste0(data_folder, "/SLSuccessRatesLearningStrats.txt"), row.names = FALSE)
}
### differences Meta Learning Types ------------------------
if(ML_on == "all"){
  
  ### Number of Skills
  overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(NumberSkills, 
         MetaStrat,
         .direction = "down") %>%
    filter(Timestep == max(Timestep)) %>%
    group_by(MetaStrat, Round) %>%
    ggplot(aes(x = factor(MetaStrat, labels = labels_metatypes), y = NumberSkills)) +
    geom_boxplot(color = colors_meta) +
    labs(y = "Number of Skills", x = labs_legend_meta) +
    theme_classic() + 
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    ) +
    ylim(c(0,40))
  ggsave("NumberSkillsMeta.png", width = 13, height = 12.5, units = "cm", path = data_folder)
  
  SkillsMeta <- overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(NumberSkills, 
         MetaStrat,
         .direction = "down") %>%
    filter(Timestep == max(Timestep)) %>%
    group_by(MetaStrat) %>%
    reframe(medianML = median(NumberSkills), 
            lowerquant = as.numeric(quantile(NumberSkills)[2]),
            upperquant = as.numeric(quantile(NumberSkills)[4])
    )
  
  write.csv2(SkillsMeta, file = paste0(data_folder, "/SkillsMeta.txt"), row.names = FALSE)
  
  # plot with density distributions and boxplots over time
  overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(NumberSkills, 
         MetaStrat,
         .direction = "down") %>%
    filter(Timestep %in% c(0, 1000, 2000, 3000, 4000, 5000)) %>%
    group_by(MetaStrat, Round, Timestep) %>%
    ggplot(aes(x = factor(Timestep), 
               color = factor(MetaStrat, labels = labels_metatypes), 
               y = NumberSkills,
               fill = factor(MetaStrat, labels = labels_metatypes))) +
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
    scale_color_manual(values = colors_meta, guide = "none") +
    scale_fill_manual(values = colors_meta) +
    labs(fill = labs_legend_meta, y = "Number of Skills", x = "Timesteps") +
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    )
  ggsave("RainbowPlotSkillsMeta.png", width = 30, height = 13, units = "cm", path = data_folder)
  
  ### Payoffs 
  # boxplot with total payoffs at last timestep
  overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(Payoff, 
         MetaStrat,
         .direction = "down") %>%
    filter(Timestep == max(Timestep)) %>%
    group_by(MetaStrat, Round) %>%
    summarise_at(vars(Payoff), list(total=sum)) %>%
    ggplot(aes(x = factor(MetaStrat, labels = labels_metatypes), y = total)) +
    geom_boxplot(color = colors_meta) +
    labs(y = "Total Payoff", x = labs_legend_meta) +
    theme_classic() + 
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    ) +
    ylim(c(0, 6500))
  ggsave("TotalPayoffMeta.png", width = 13, height = 12.5, units = "cm", path = data_folder)
  
  PayoffMeta <- overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(Payoff, 
         MetaStrat,
         .direction = "down") %>%
    filter(Timestep == max(Timestep)) %>%
    group_by(MetaStrat, Round) %>%
    summarise_at(vars(Payoff), list(total=sum)) %>%
    reframe(medianML = median(total), 
            lowerquant = as.numeric(quantile(total)[2]),
            upperquant = as.numeric(quantile(total)[4])
            )
  
  write.csv2(PayoffMeta, file = paste0(data_folder, "/TotalPayoffMeta.txt"), row.names = FALSE)
  
  # plot with density distributions and boxplots over time
  overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(Payoff, 
         MetaStrat,
         .direction = "down") %>%
    filter(Timestep %in% c(0, 1000, 2000, 3000, 4000, 5000)) %>%
    group_by(MetaStrat, Round, Timestep) %>%
    reframe(TotalPayoff = sum(Payoff)) %>%
    ggplot(aes(x = factor(Timestep), 
               color = factor(MetaStrat, labels = labels_metatypes), 
               y = TotalPayoff,
               fill = factor(MetaStrat, labels = labels_metatypes))) +
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
    scale_color_manual(values = colors_meta, guide = "none") +
    scale_fill_manual(values = colors_meta) +
    labs(fill = labs_legend_meta, y = "Total Payoff", x = "Timesteps") +
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    )
  ggsave("RainbowPlotPayoffsMeta.png", width = 30, height = 13, units = "cm", path = data_folder)
}



### differences Learning Strategies ----
if(ML_on == "all" | ML_on == 1 & LS_on == "all"){
  ### Number of Skills
  overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(NumberSkills, 
         MetaStrat,
         LearningStrat,
         .direction = "down") %>%
    filter(Timestep == max(Timestep)) %>%
    filter(MetaStrat == 1) %>%
    group_by(MetaStrat, Round) %>%
    ggplot(aes(x = factor(LearningStrat, labels = labels_learningstrat), y = NumberSkills)) +
    geom_boxplot(color = colors_learningstrat) +
    labs(y = "Number of Skills", x = labs_legend_learningstrat) +
    theme_classic() + 
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    ) +
    ylim(c(0,40))
  ggsave("NumberSkillsLS.png", width = 13, height = 12.5, units = "cm", path = data_folder)
  
  SkillsLS <- overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(NumberSkills, 
         MetaStrat,
         LearningStrat,
         .direction = "down") %>%
    filter(Timestep == max(Timestep)) %>%
    filter(MetaStrat == 1) %>%
    group_by(LearningStrat) %>%
    reframe(medianLS = median(NumberSkills), 
            lowerquant = as.numeric(quantile(NumberSkills)[2]),
            upperquant = as.numeric(quantile(NumberSkills)[4])
    )
  
  write.csv2(SkillsLS, file = paste0(data_folder, "/SkillsLS.txt"), row.names = FALSE)
  
  # plot with density distributions and boxplots over time
  overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(NumberSkills, 
         MetaStrat,
         LearningStrat,
         .direction = "down") %>%
    filter(Timestep %in% c(0, 1000, 2000, 3000, 4000, 5000)) %>%
    filter(MetaStrat == 1) %>%
    group_by(LearningStrat, Round, Timestep) %>%
    ggplot(aes(x = factor(Timestep), 
               color = factor(LearningStrat, labels = labels_learningstrat), 
               y = NumberSkills,
               fill = factor(LearningStrat, labels = labels_learningstrat))) +
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
    labs(fill = labs_legend_learningstrat, y = "Number of Skills", x = "Timesteps") +
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    )
  ggsave("RainbowPlotSkillsLS.png", width = 30, height = 13, units = "cm", path = data_folder)
  
  ### Payoffs 
  # boxplot with total payoffs at last timestep
  overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(Payoff, 
         MetaStrat,
         LearningStrat,
         .direction = "down") %>%
    filter(Timestep == max(Timestep)) %>%
    filter(MetaStrat == 1) %>%
    group_by(LearningStrat, Round) %>%
    summarise_at(vars(Payoff), list(total=sum)) %>%
    ggplot(aes(x = factor(LearningStrat, labels = labels_learningstrat), y = total)) +
    geom_boxplot(color = colors_learningstrat) +
    labs(y = "Total Payoff", x = labs_legend_learningstrat) +
    theme_classic() + 
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    ) +
    ylim(c(0, 6500))
  ggsave("TotalPayoffLS.png", width = 13, height = 12.5, units = "cm", path = data_folder)
  
  PayoffLS <- overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(Payoff, 
         MetaStrat,
         LearningStrat,
         .direction = "down") %>%
    filter(Timestep == max(Timestep)) %>%
    filter(MetaStrat == 1) %>%
    group_by(LearningStrat, Round) %>%
    summarise_at(vars(Payoff), list(total=sum)) %>%
    reframe(medianLS = median(total), 
            lowerquant = as.numeric(quantile(total)[2]),
            upperquant = as.numeric(quantile(total)[4])
    )
  
  write.csv2(PayoffLS, file = paste0(data_folder, "/TotalPayoffLS.txt"), row.names = FALSE)
  
  # plot with density distributions and boxplots over time
  overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(Payoff, 
         MetaStrat,
         LearningStrat,
         .direction = "down") %>%
    filter(Timestep %in% c(0, 1000, 2000, 3000, 4000, 5000)) %>%
    filter(MetaStrat == 1) %>%
    group_by(LearningStrat, Round, Timestep) %>%
    reframe(TotalPayoff = sum(Payoff)) %>%
    ggplot(aes(x = factor(Timestep), 
               color = factor(LearningStrat, labels = labels_learningstrat), 
               y = TotalPayoff,
               fill = factor(LearningStrat, labels = labels_learningstrat))) +
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
    labs(fill = labs_legend_learningstrat, y = "Total Payoff", x = "Timesteps") +
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    )
  ggsave("RainbowPlotPayoffsLS.png", width = 30, height = 13, units = "cm", path = data_folder)
}
### Flexible Learner----
if(ML_on == "all" | ML_on == 2){
  # use of learning strategies for flexible learner over age 
  overview %>%
    filter(MetaStrat == 2) %>%
    group_by(Age, LearningStrat) %>%
    filter(!is.na(LearningStrat))%>%
    summarize(Count_all = n()) %>%
    mutate(LearningStratUse = Count_all/sum(Count_all)) %>%
    ggplot() +
    geom_line(aes(x = Age, y = LearningStratUse, color = factor(LearningStrat, labels=labels_learningstrat)), linewidth = 2) +
    scale_color_manual(values = colors_learningstrat) +
    labs(color=labs_legend_learningstrat, y = "Use of Learning Strategy in %") +
    geom_hline(yintercept=0.25, linetype="dashed", color = "grey") +
    theme_classic() + 
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    )
  
  ggsave("FlexibleLearnerOverAge.png", width = 13, height = 12.5, units = "cm", path = data_folder)
 
  # use of learning strategies for flexible learner over timesteps 
  overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(MetaStrat,
         LearningStrat, 
         Timestep,
         .direction = "down") %>%
    filter(MetaStrat == 2) %>%
    group_by(Timestep, LearningStrat) %>%
    filter(!is.na(LearningStrat))%>%
    summarize(Count_all = n()) %>%
    mutate(LearningStratUse = Count_all/sum(Count_all)) %>%
    ggplot() +
    geom_line(aes(x = Timestep, y = LearningStratUse, color = factor(LearningStrat, labels=labels_learningstrat)), linewidth =2) +
    scale_color_manual(values = colors_learningstrat) +
    labs(color=labs_legend_learningstrat, y = "Use of Learning Strategy in %") +
    geom_hline(yintercept=0.25, linetype="dashed", color = "grey") +
    theme_classic() + 
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    )
  
  ggsave("FlexibleLearnerOverTimestep.png", width = 15, height = 12.5, units = "cm", path = data_folder)
}




### Integrative Learner ----

if(ML_on == "all" | ML_on == 3){
  # Weights for Successes and Fails over Timesteps
  time_overview <- overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(MetaStrat,
         Age, 
         PayoffLearningSuccess,
         PayoffLearningFails,
         SimilarityLearningSuccess,
         SimilarityLearningFails,
         AgeLearningSuccess,
         AgeLearningFails,
         ConformityLearningSuccess,
         ConformityLearningFails,
         .direction = "down") %>%
    filter(MetaStrat == 3) %>%
    group_by(Timestep) %>%
    summarise_at(vars(12:19), list(Mean = mean))
  
  p_Weights <- time_overview %>%
    pivot_longer(
      cols = 2:9,
      names_to = "Strat",
      values_to = "Weights"
    ) %>%
    mutate(outcome = ifelse(grepl("Success_Mean$", Strat), "pos", "fail"))
  
  p_Weights$Strat <- factor(p_Weights$Strat, 
                            levels=c("PayoffLearningSuccess_Mean", "SimilarityLearningSuccess_Mean", "AgeLearningSuccess_Mean", "ConformityLearningSuccess_Mean", "PayoffLearningFails_Mean", "SimilarityLearningFails_Mean", "AgeLearningFails_Mean", "ConformityLearningFails_Mean"), 
                            labels=c("Success_pos", "Similarity_pos", "Age_pos", "Conformity_pos","Success_neg", "Similarity_neg", "Age_neg", "Conformity_neg"))
  p_Weights$Strat <- fct_collapse(p_Weights$Strat, Success = c("Success_pos", "Success_neg"), Similarity = c("Similarity_pos", "Similarity_neg"), Age = c("Age_pos", "Age_neg"), Conformity = c("Conformity_pos","Conformity_neg"))
  
  
  p_Weights %>%
    ggplot() +
    geom_line(aes(x = Timestep, y = Weights, color = Strat), linewidth = 1.5) +
    scale_color_manual(values = c(colors_learningstrat, colors_learningstrat)) +
    labs(color=labs_legend_learningstrat) +
    theme_bw() +
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    ) +
    facet_wrap(~factor(outcome, levels = c("pos", "fail"), labels = c("Successful Attempts", "Failed Attempts"))) +
    theme(strip.text.x = element_text(size = 10, face = "bold"))
  
  ggsave("IntegrativeLearnerSuccessFailsOverTimestep.png", width = 17, height = 12.5, units = "cm", path = data_folder)
  
  ## one weight for each Learning Strategy over timesteps
  time_overview %>%
    mutate(Weight_Sucess = (PayoffLearningSuccess_Mean + 1) / (PayoffLearningSuccess_Mean + 2 + PayoffLearningFails_Mean),
           Weight_Sim = (SimilarityLearningSuccess_Mean + 1) / (SimilarityLearningSuccess_Mean + 2 + SimilarityLearningFails_Mean),
           Weight_Age = (AgeLearningSuccess_Mean + 1) / (AgeLearningSuccess_Mean + 2 + AgeLearningFails_Mean),
           Weight_Conf = (ConformityLearningSuccess_Mean + 1) / (ConformityLearningSuccess_Mean + 2 + ConformityLearningFails_Mean)) %>%
    select(Timestep, Weight_Sucess, Weight_Sim, Weight_Age, Weight_Conf) %>%
    pivot_longer(
      cols = 2:5, 
      names_to = "Strat",
      values_to = "Weights"
    ) %>%
    ggplot(aes(x = Timestep, y = Weights, color = factor(Strat, levels = c("Weight_Sucess", "Weight_Sim", "Weight_Age", "Weight_Conf"),labels = labels_learningstrat))) +
    geom_line(linewidth = 1.5) +
    scale_color_manual(values = colors_learningstrat) +
    theme_classic() +
    labs(color = labs_legend_learningstrat) +
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    ) 
  
  ggsave("IntegrativeLearnerWeightsOverTimestep.png", width = 15, height = 12.5, units = "cm", path = data_folder)
  
  ## one weight for each Learning Strategy over Age
  time_overview_age <- overview %>%
    filter(MetaStrat == 3) %>%
    group_by(Age) %>%
    summarise_at(vars(12:19), list(Mean = mean))
  
  time_overview_age %>%
    mutate(Weight_Sucess = (PayoffLearningSuccess_Mean + 1) / (PayoffLearningSuccess_Mean + 2 + PayoffLearningFails_Mean),
           Weight_Sim = (SimilarityLearningSuccess_Mean + 1) / (SimilarityLearningSuccess_Mean + 2 + SimilarityLearningFails_Mean),
           Weight_Age = (AgeLearningSuccess_Mean + 1) / (AgeLearningSuccess_Mean + 2 + AgeLearningFails_Mean),
           Weight_Conf = (ConformityLearningSuccess_Mean + 1) / (ConformityLearningSuccess_Mean + 2 + ConformityLearningFails_Mean)) %>%
    select(Age, Weight_Sucess, Weight_Sim, Weight_Age, Weight_Conf) %>%
    pivot_longer(
      cols = 2:5, 
      names_to = "Strat",
      values_to = "Weights"
    ) %>%
    ggplot(aes(x = Age, y = Weights, color = factor(Strat, levels = c("Weight_Sucess", "Weight_Sim", "Weight_Age", "Weight_Conf"),labels = labels_learningstrat))) +
    geom_line(linewidth = 1.5) +
    scale_color_manual(values = colors_learningstrat) +
    theme_classic() +
    labs(color = labs_legend_learningstrat) +
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    ) 
  
  ggsave("IntegrativeLearnerWeightsOverAge.png", width = 15, height = 12.5, units = "cm", path = data_folder)
}
### Beta Distributions for Integrative and Flexible learner ----
theta = seq(0, 1, length.out = 1e3) 
# Integrative Learner
if(ML_on == 3){
  lastTimestep_weights <- time_overview %>%
    filter(Timestep == max(Timestep)) 
  
  theta = seq(0, 1, length.out = 1e3)  
  Integrative_betas <- data.frame(Theta = theta, 
                                  Payoff = dbeta(theta,
                                                 1+lastTimestep_weights$PayoffLearningSuccess_Mean, 
                                                 1+lastTimestep_weights$PayoffLearningFails_Mean
                                  ),
                                  Similarity = dbeta(theta,
                                                     1+lastTimestep_weights$SimilarityLearningSuccess_Mean, 
                                                     1+lastTimestep_weights$SimilarityLearningFails_Mean),
                                  Age = dbeta(theta, 
                                              1+lastTimestep_weights$AgeLearningSuccess_Mean,
                                              1+lastTimestep_weights$AgeLearningFails_Mean),
                                  Conformity = dbeta(theta, 
                                                     1+lastTimestep_weights$ConformityLearningSuccess_Mean,
                                                     1+lastTimestep_weights$ConformityLearningFails_Mean)
  ) %>%
    pivot_longer(Payoff:Conformity,
                 names_to = "Strategy", 
                 values_to = "Density"
    ) %>%
    mutate(Strategy = factor(Strategy, levels = c("Payoff", "Similarity", "Age", "Conformity") , labels = labels_learningstrat))
  
  
  ## Beta distributions integrative and flexible learner 
  
  Integrative_betas %>%
    ggplot(aes(x = Theta, y = Density, color = Strategy)) +
    geom_line(linewidth = 2) +
    scale_color_manual(values = colors_learningstrat) +
    labs(color=labs_legend_learningstrat) +
    theme_classic() 
  
  ggsave("IntegrativeBetas.png", width = 15, height = 12.5, units = "cm", path = data_folder)
  
}
# Flexible Learner
if(ML_on == 2){
  time_overview_flex <- overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(MetaStrat,
         Age, 
         PayoffLearningSuccess,
         PayoffLearningFails,
         SimilarityLearningSuccess,
         SimilarityLearningFails,
         AgeLearningSuccess,
         AgeLearningFails,
         ConformityLearningSuccess,
         ConformityLearningFails,
         .direction = "down") %>%
    filter(MetaStrat == 2) %>%
    group_by(Timestep) %>%
    summarise_at(vars(12:19), list(Mean = mean)) %>%
    filter(Timestep == max(Timestep))
  
  
  Flexible_Betas <- data.frame(Theta = theta, 
                               Payoff = dbeta(theta,
                                              1+time_overview_flex$PayoffLearningSuccess_Mean, 
                                              1+time_overview_flex$PayoffLearningFails_Mean
                               ),
                               Similarity = dbeta(theta,
                                                  1+time_overview_flex$SimilarityLearningSuccess_Mean, 
                                                  1+time_overview_flex$SimilarityLearningFails_Mean),
                               Age = dbeta(theta, 
                                           1+time_overview_flex$AgeLearningSuccess_Mean,
                                           1+time_overview_flex$AgeLearningFails_Mean),
                               Conformity = dbeta(theta, 
                                                  1+time_overview_flex$ConformityLearningSuccess_Mean,
                                                  1+time_overview_flex$ConformityLearningFails_Mean)
  ) %>%
    pivot_longer(Payoff:Conformity,
                 names_to = "Strategy", 
                 values_to = "Density"
    ) %>%
    mutate(Strategy = factor(Strategy, levels = c("Payoff", "Similarity", "Age", "Conformity") , labels = labels_learningstrat))
  
  Flexible_Betas %>%
    ggplot(aes(x = Theta, y = Density, color = Strategy)) +
    geom_line(linewidth = 2) +
    scale_color_manual(values = colors_learningstrat) +
    labs(color=labs_legend_learningstrat) +
    theme_classic() 
  ggsave("FlexibleLearnerBetaDistr.png", width = 15, height = 12.5, units = "cm", path = data_folder)
  
}
if(ML_on == "all"){
  lastTimestep_weights <- time_overview %>%
    filter(Timestep == max(Timestep)) 
  
  theta = seq(0, 1, length.out = 1e3)  
  Integrative_betas <- data.frame(Theta = theta, 
                                  Payoff = dbeta(theta,
                                                 1+lastTimestep_weights$PayoffLearningSuccess_Mean, 
                                                 1+lastTimestep_weights$PayoffLearningFails_Mean
                                  ),
                                  Similarity = dbeta(theta,
                                                     1+lastTimestep_weights$SimilarityLearningSuccess_Mean, 
                                                     1+lastTimestep_weights$SimilarityLearningFails_Mean),
                                  Age = dbeta(theta, 
                                              1+lastTimestep_weights$AgeLearningSuccess_Mean,
                                              1+lastTimestep_weights$AgeLearningFails_Mean),
                                  Conformity = dbeta(theta, 
                                                     1+lastTimestep_weights$ConformityLearningSuccess_Mean,
                                                     1+lastTimestep_weights$ConformityLearningFails_Mean)
  ) %>%
    pivot_longer(Payoff:Conformity,
                 names_to = "Strategy", 
                 values_to = "Density"
    ) %>%
    mutate(Strategy = factor(Strategy, levels = c("Payoff", "Similarity", "Age", "Conformity") , labels = labels_learningstrat))
  
  
  ## Beta distributions integrative and flexible learner 
  
  Integrative_betas %>%
    ggplot(aes(x = Theta, y = Density, color = Strategy)) +
    geom_line(linewidth = 2) +
    scale_color_manual(values = colors_learningstrat) +
    labs(color=labs_legend_learningstrat) +
    theme_classic() +
    geom_vline(xintercept= SL_successLStrats$SLsucessMean[1], linetype="dashed", color = color_learningstrat1) +
    geom_vline(xintercept= SL_successLStrats$SLsucessMean[2], linetype="dashed", color = color_learningstrat2) +
    geom_vline(xintercept= SL_successLStrats$SLsucessMean[3], linetype="dashed", color = color_learningstrat3) +
    geom_vline(xintercept= SL_successLStrats$SLsucessMean[4], linetype="dashed", color = color_learningstrat4)
  
  ggsave("IntegrativeBetas.png", width = 15, height = 12.5, units = "cm", path = data_folder)
  
  time_overview_flex <- overview %>%
    group_by(Round) %>%
    complete(Timestep, ID) %>%
    group_by(Round, ID) %>%
    fill(MetaStrat,
         Age, 
         PayoffLearningSuccess,
         PayoffLearningFails,
         SimilarityLearningSuccess,
         SimilarityLearningFails,
         AgeLearningSuccess,
         AgeLearningFails,
         ConformityLearningSuccess,
         ConformityLearningFails,
         .direction = "down") %>%
    filter(MetaStrat == 2) %>%
    group_by(Timestep) %>%
    summarise_at(vars(12:19), list(Mean = mean)) %>%
    filter(Timestep == max(Timestep))
  
  
  Flexible_Betas <- data.frame(Theta = theta, 
                               Payoff = dbeta(theta,
                                              1+time_overview_flex$PayoffLearningSuccess_Mean, 
                                              1+time_overview_flex$PayoffLearningFails_Mean
                               ),
                               Similarity = dbeta(theta,
                                                  1+time_overview_flex$SimilarityLearningSuccess_Mean, 
                                                  1+time_overview_flex$SimilarityLearningFails_Mean),
                               Age = dbeta(theta, 
                                           1+time_overview_flex$AgeLearningSuccess_Mean,
                                           1+time_overview_flex$AgeLearningFails_Mean),
                               Conformity = dbeta(theta, 
                                                  1+time_overview_flex$ConformityLearningSuccess_Mean,
                                                  1+time_overview_flex$ConformityLearningFails_Mean)
  ) %>%
    pivot_longer(Payoff:Conformity,
                 names_to = "Strategy", 
                 values_to = "Density"
    ) %>%
    mutate(Strategy = factor(Strategy, levels = c("Payoff", "Similarity", "Age", "Conformity") , labels = labels_learningstrat))
  
  Flexible_Betas %>%
    ggplot(aes(x = Theta, y = Density, color = Strategy)) +
    geom_line(linewidth = 2) +
    scale_color_manual(values = colors_learningstrat) +
    labs(color=labs_legend_learningstrat) +
    theme_classic() +
    geom_vline(xintercept= SL_successLStrats$SLsucessMean[1], linetype="dashed", color = color_learningstrat1) +
    geom_vline(xintercept= SL_successLStrats$SLsucessMean[2], linetype="dashed", color = color_learningstrat2) +
    geom_vline(xintercept= SL_successLStrats$SLsucessMean[3], linetype="dashed", color = color_learningstrat3) +
    geom_vline(xintercept= SL_successLStrats$SLsucessMean[4], linetype="dashed", color = color_learningstrat4)
  
  ggsave("FlexibleLearnerBetaDistr.png", width = 15, height = 12.5, units = "cm", path = data_folder)
  
  # both Learner Betas in one plot
  
  Flexible_Betas <- Flexible_Betas %>%
    mutate(Learner = "flexible")
  Integrative_betas <- Integrative_betas %>%
    mutate(Learner = "integrative")
  
  all_Betas <- rbind.data.frame(Flexible_Betas, Integrative_betas)
  
  all_Betas %>%
    ggplot(aes(x = Theta, y = Density, color = Strategy)) +
    geom_vline(xintercept= SL_successLStrats$SLsucessMean[1], linetype="dashed", color = color_learningstrat1) +
    geom_vline(xintercept= SL_successLStrats$SLsucessMean[2], linetype="dashed", color = color_learningstrat2) +
    geom_vline(xintercept= SL_successLStrats$SLsucessMean[3], linetype="dashed", color = color_learningstrat3) +
    geom_vline(xintercept= SL_successLStrats$SLsucessMean[4], linetype="dashed", color = color_learningstrat4) +
    geom_line(linewidth = 2) +
    scale_color_manual(values = c(colors_learningstrat, colors_learningstrat)) +
    labs(color=labs_legend_learningstrat) +
    theme_bw() +
    theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
          axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
          axis.text.x=element_text(size=10, vjust=0.5),
          axis.text.y=element_text(size=10, hjust=0.5)
    ) +
    facet_wrap(~factor(Learner, levels = c("flexible", "integrative"), 
                       labels = c("Flexible Learner", "Integrative Learner"))) +
    theme(strip.text.x = element_text(size = 10, face = "bold")) +
    scale_x_continuous(breaks = seq(0,1,0.1))
  
  ggsave("allBetas.png", width = 30, height = 13, units = "cm", path = data_folder)
}







### Success Rate Estimates Flexible and Integrative Learner ----
# Integratvive Learner
if(ML_on == "all" | ML_on == 3){
  SREstimateIntLearner <- time_overview %>%
    mutate(Weight_Sucess = (PayoffLearningSuccess_Mean + 1) / (PayoffLearningSuccess_Mean + 2 + PayoffLearningFails_Mean),
           Weight_Sim = (SimilarityLearningSuccess_Mean + 1) / (SimilarityLearningSuccess_Mean + 2 + SimilarityLearningFails_Mean),
           Weight_Age = (AgeLearningSuccess_Mean + 1) / (AgeLearningSuccess_Mean + 2 + AgeLearningFails_Mean),
           Weight_Conf = (ConformityLearningSuccess_Mean + 1) / (ConformityLearningSuccess_Mean + 2 + ConformityLearningFails_Mean)) %>%
    filter(Timestep == max(Timestep)) %>%
    select(Weight_Sucess, Weight_Sim, Weight_Age, Weight_Conf)
  
  write.csv2(SREstimateIntLearner, file = paste0(data_folder, "/SuccessRateEstimatesIntegrativeLearner.txt"), row.names = FALSE)
}

# Flexible Learner
if(ML_on == "all" | ML_on == 2){
  SREstimateFlexLearner <- time_overview_flex %>%
    mutate(Weight_Sucess = (PayoffLearningSuccess_Mean + 1) / (PayoffLearningSuccess_Mean + 2 + PayoffLearningFails_Mean),
           Weight_Sim = (SimilarityLearningSuccess_Mean + 1) / (SimilarityLearningSuccess_Mean + 2 + SimilarityLearningFails_Mean),
           Weight_Age = (AgeLearningSuccess_Mean + 1) / (AgeLearningSuccess_Mean + 2 + AgeLearningFails_Mean),
           Weight_Conf = (ConformityLearningSuccess_Mean + 1) / (ConformityLearningSuccess_Mean + 2 + ConformityLearningFails_Mean)) %>%
    filter(Timestep == max(Timestep)) %>%
    select(Weight_Sucess, Weight_Sim, Weight_Age, Weight_Conf)
  
  write.csv2(SREstimateFlexLearner, file = paste0(data_folder, "/SuccessRateEstimatesFlexibleLearner.txt"), row.names = FALSE)
}


### population development ---- 
# whole population
overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(NumberSkills,
       .direction = "down") %>%
  select(Timestep, NumberSkills) %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  ggplot(aes(x=NumberSkills)) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  labs(y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  ylim(c(0, 0.1))
ggsave("PopDev.png", width = 30, height = 13, units = "cm", path = data_folder)

# first and last timestep per Meta Learning Type
if(ML_on == "all"){
overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat,
       NumberSkills,
       .direction = "down") %>%
  select(Timestep, NumberSkills, MetaStrat) %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  ggplot(aes(x=NumberSkills, color = factor(MetaStrat, labels = labels_metatypes))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  scale_color_manual(values = colors_meta) +
  labs(color=labs_legend_meta, y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"))
  
ggsave("PopDevMeta.png", width = 30, height = 13, units = "cm", path = data_folder)
}

# first and last timestep per learning strategy 
if(ML_on == "all" | ML_on == 1 & LS_on == "all"){
overview %>%
  group_by(Round) %>%
  complete(Timestep, ID) %>%
  group_by(Round, ID) %>%
  fill(MetaStrat,
       LearningStrat,
       NumberSkills,
       .direction = "down") %>%
  filter(MetaStrat == 1) %>%
  select(Timestep, NumberSkills, LearningStrat) %>%
  filter(Timestep == 0 | Timestep == max(Timestep)) %>%
  ggplot(aes(x=NumberSkills, color = factor(LearningStrat, labels = labels_learningstrat))) + 
  geom_density(linewidth = 1) +
  theme_bw() +
  theme(axis.title.x = element_text(margin = margin(t = 15), size = 12, face = "bold"),
        axis.title.y = element_text(margin = margin(r = 15), size = 12, face = "bold"),
        axis.text.x=element_text(size=10, vjust=0.5),
        axis.text.y=element_text(size=10, hjust=0.5)
  ) +
  scale_color_manual(values = colors_learningstrat) +
  labs(color=labs_legend_learningstrat, y = "Density", x = "Number of Skills") +
  facet_wrap(~factor(Timestep, labels = c("Start", "End"))) +
  theme(strip.text.x = element_text(size = 10, face = "bold"))
  
ggsave("PopDevLStrat.png", width = 30, height = 13, units = "cm", path = data_folder)
}
















