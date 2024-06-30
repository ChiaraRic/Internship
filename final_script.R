# get current time in correct format for folder name
current_time <- Sys.time() %>% 
  str_replace_all(":", "-") %>%
  substr(1,19)

# create data path names
data_folder <- paste0("data/", current_time)
sim_settings_path <- paste0(data_folder, "/sim_settings.json")
results_path <- paste0(data_folder, "/results.csv")
payoff_path <- paste0(data_folder, "/payoffs.csv")

# create data folder for sim settings and sim results
dir.create(data_folder)

# create json file with sim settings
# sim_dat <- fromJSON(file = "myJSON.json")
# population = sim_dat$population_size

###
# set up population
population <- 1:100
skills <- 40
timesteps <- 5000
rounds <- 100
reset_rate <- 0.05	
social_learning <- 0.99
skill_probs <- c(rep((1/skills)*2, 0.25*skills), rep(1/skills, 0.5*skills), rep((1/skills)/2, 0.25*skills))
skill_tree <- "flat" # determines which skill tree is used; can be chain (constrained) or flat (unconstrained)
LS_on <- 1 # determines which Learning Strategies are implemented; can be 1 (success),2 (similarity),3 (age), 4 (conformity), or "all"
ML_on <- 1 # determines which Meta Learning Types are implemented; can be 1 (fixed), 2 (flexible), 3 (integrative), or "all"

###
sim_settings <-list(population = length(population),
                    skills = skills,
                    timesteps = timesteps, 
                    rounds = rounds,
                    reset_rate = reset_rate,
                    social_learning = social_learning,
                    skill_probs = skill_probs, 
                    skill_tree = skill_tree, 
                    LearningStrategies = LS_on, 
                    MetaLearning = ML_on
)
write(toJSON(sim_settings), file = sim_settings_path)

# get functions and vectors 
source("functions_finalfinal.R")

LearningStrategies <- select_LearningStrategies(LS_on)
MetaLearningTypes <- select_MetaLearning(ML_on)
# start simulation
for (r in 1 : rounds) {
  ID <- append(ID, population)
  t_ID <- c()
  # vectors to collect data from all rounds and 
  r_payoffs <- create_payoffs()
  r_skills <- sample(seq(1:skills), length(population), prob = skill_probs, replace = TRUE)
  r_ages <- r_skills
  r_learning_strat <- rep(NA, length(population))
  r_meta_strat <- resample(MetaLearningTypes, length(population), replace = TRUE, prob = rep(1/length(MetaLearningTypes), length(MetaLearningTypes)))
  r_learning_strat[which(r_meta_strat == 1)] <- resample(LearningStrategies, length(which(r_meta_strat == 1)), replace = TRUE, prob = rep(1/length(LearningStrategies), length(LearningStrategies)))
  r_teacher_ID <- c()
  r_strat1_success <- rep(0, length(population))
  r_strat1_failure <- rep(0, length(population))
  r_strat2_success <- rep(0, length(population))
  r_strat2_failure <- rep(0, length(population))
  r_strat3_success <- rep(0, length(population))
  r_strat3_failure <- rep(0, length(population))
  r_strat4_success <- rep(0, length(population))
  r_strat4_failure <- rep(0, length(population))
  r_teacher_ID <- rep(NA, length(population))
  r_skill_attempted <- rep(NA, length(population))
  r_SL_rate <- rep(0, length(population))
  r_SL_success<- rep(NA, length(population))
  r_n_resets <- rep(0, length(population))
  r_IL_rate<- rep(0, length(population))
  r_IL_success <- rep(NA, length(population))
  r_learner_payoffs <- sapply(r_skills, function(s) sum(r_payoffs[1:s])) 
  r_eligible_teachers <- rep(NA, length(population))
  skill_matrix <- outer(1:skills, r_skills, "<=") * 1
  # create vcetors to track for each time step
  t_skills <- r_skills
  t_ages <- r_ages
  t_learning_strat <- r_learning_strat
  t_strat1_success <- r_strat1_success
  t_strat1_failure <- r_strat1_failure
  t_strat2_success <- r_strat2_success
  t_strat2_failure <- r_strat2_failure
  t_strat3_success <- r_strat3_success
  t_strat3_failure <- r_strat3_failure
  t_strat4_success <- r_strat4_success
  t_strat4_failure <- r_strat4_failure
  t_teacherID <- r_teacher_ID
  t_skill_attempt <- r_skill_attempted
  t_SL_rate <- r_SL_rate
  t_SL_success <- r_SL_success
  t_resets <- r_n_resets
  t_IL_rate <- r_IL_rate
  t_IL_success <- r_IL_success
  t_learner_payoffs <- r_learner_payoffs
  t_eligible_teachers <- r_eligible_teachers
  for (t in 1:timesteps) {
    # reset lists for new timestep
    t_IL_success <- rep(NA, length(population))
    t_SL_success <- rep(NA, length(population))
    t_teacherID <- rep(NA, length(population))
    t_skill_attempt <- rep(NA, length(population))
    t_eligible_teachers <- rep(NA, length(population))
    t_learning_strat[which(r_meta_strat == 2)] <- NA
    # select learner
    individual <- select_learner(population, t_skills, skills)
    # select teachers 
    teacher_select <- find_skilled_teachers(skill_matrix, population, individual)
    skilled_teacher <- unlist(teacher_select[1])
    individual <- unlist(teacher_select[2])
    t_eligible_teachers[individual] <- length(skilled_teacher) 
    
    # select if learner is being reset, doing individual, or social learning 
    state <- state_learning(reset_rate)
    if(state == "reset"){
      t_skills[individual] <- 1
      t_ages[individual] <- 0
      t_strat1_success[individual] <- 0
      t_strat1_failure[individual] <- 0
      t_strat2_success[individual] <- 0
      t_strat2_failure[individual] <- 0
      t_strat3_success[individual] <- 0
      t_strat3_failure[individual] <- 0
      t_strat4_success[individual] <- 0
      t_strat4_failure[individual] <- 0
      t_resets[individual] <- t_resets[individual]+1
      t_learner_payoffs[individual] <- r_payoffs[1]
      t_IL_rate[individual] <- 0
      t_SL_rate[individual] <- 0
      skill_matrix[2:skills, individual] <- 0
    }
    if(state == "IL"){ # individual learning 
      t_IL_rate[individual] <- t_IL_rate[individual] + 1
      t_skill_attempt[individual] <- individual_learning(skill_matrix, individual)
        if(is_learnable(skill_tree, t_skill_attempt, individual) == TRUE){ # IL success
          t_IL_success[individual] <- 1
          t_learner_payoffs[individual] <- t_learner_payoffs[individual] + r_payoffs[t_skills[individual] + 1]
          t_skills[individual] <- t_skills[individual] + 1
          skill_matrix[t_skill_attempt[individual], individual] <- 1
        } else { # IL success
          t_IL_success[individual] <- 0 
        }
      }
    if(state == "SL"){
      t_SL_rate[individual] <- t_SL_rate[individual] + 1
      if(r_meta_strat[individual] == 1){ # fixed meta-strategy
        learningstrat <- t_learning_strat[individual]
        select_teacher_behavior <- fixed_strategy(learningstrat, t_learner_payoffs, skilled_teacher, skill_matrix, t_skills, individual, t_ages, skills)
        t_teacherID[individual] <- unlist(select_teacher_behavior[1])
        t_skill_attempt[individual] <- unlist(select_teacher_behavior[2])
        if(is_learnable(skill_tree, t_skill_attempt, individual) == TRUE){ 
          t_learner_payoffs[individual] <- t_learner_payoffs[individual] + r_payoffs[t_skill_attempt[individual]]
          t_skills[individual] <- t_skills[individual] + 1 # this is number of skills
          t_SL_success[individual] <- 1
          skill_matrix[t_skill_attempt[individual], individual] <- 1
        } else t_SL_success[individual] <- 0
      }
      if(r_meta_strat[individual] == 2){ # flexible meta-strategy
        flex_learner <- flexible_strategy(t_strat1_success, t_strat2_success, t_strat3_success, t_strat4_success,
                                          t_strat1_failure, t_strat2_failure, t_strat3_failure, t_strat4_failure,
                                          individual)
        strat_weights <- unlist(flex_learner[1])
        learningstrat <- unlist(flex_learner[2])
        t_learning_strat[individual] <- learningstrat
        select_teacher_behavior <- fixed_strategy(learningstrat, t_learner_payoffs, skilled_teacher, skill_matrix, t_skills, individual, t_ages, skills) #new version skills
        t_teacherID[individual] <- unlist(select_teacher_behavior[1])
        t_skill_attempt[individual] <- unlist(select_teacher_behavior[2])
        if(is_learnable(skill_tree, t_skill_attempt, individual) == TRUE){ 
          t_learner_payoffs[individual] <- t_learner_payoffs[individual] + r_payoffs[t_skill_attempt[individual]]
          t_skills[individual] <- t_skills[individual] + 1 # this is number of skills
          t_SL_success[individual] <- 1
          skill_matrix[t_skill_attempt[individual], individual] <- 1
          new_weights <- update_weights_success(learningstrat, t_strat1_success, t_strat2_success, t_strat3_success, t_strat4_success,
                                                individual, r_payoffs, t_skill_attempt)
          t_strat1_success[individual] <- unlist(new_weights[1])
          t_strat2_success[individual] <- unlist(new_weights[2])
          t_strat3_success[individual] <- unlist(new_weights[3])
          t_strat4_success[individual] <- unlist(new_weights[4])
          
        } else {
          t_SL_success[individual] <- 0
          new_weights <- update_weights_failure(learningstrat, t_strat1_failure, t_strat2_failure, t_strat3_failure, t_strat4_failure,
                                                individual, r_payoffs, t_skill_attempt)
          t_strat1_failure[individual] <- unlist(new_weights[1])
          t_strat2_failure[individual] <- unlist(new_weights[2])
          t_strat3_failure[individual] <- unlist(new_weights[3])
          t_strat4_failure[individual] <- unlist(new_weights[4])
        }
      } 
      if(r_meta_strat[individual] == 3){
        integrative_learner <- integrative_strategy(t_skills, skilled_teacher, individual, t_learner_payoffs,
                                                    t_ages, skill_matrix, t_strat1_success, t_strat2_success, t_strat3_success, t_strat4_success,
                                                    t_strat1_failure, t_strat2_failure, t_strat3_failure, t_strat4_failure)
        t_teacherID[individual] <- unlist(integrative_learner[1])
        t_skill_attempt[individual] <- unlist(integrative_learner[2])
        teacher_score <- integrative_learner[3] %>%
          unlist() 
        weights_payoff <- unlist(integrative_learner[4])
        weights_similarity <- unlist(integrative_learner[5])
        weights_age <- unlist(integrative_learner[6])
        weights_conformity <- unlist(integrative_learner[7])
        if(is_learnable(skill_tree, t_skill_attempt, individual) == TRUE){ # this must be changed based on skill tree
          t_learner_payoffs[individual] <- t_learner_payoffs[individual] + r_payoffs[t_skill_attempt[individual]]
          t_skills[individual] <- t_skills[individual] + 1 # this is number of skills
          t_SL_success[individual] <- 1
          skill_matrix[t_skill_attempt[individual], individual] <- 1
          new_weights <- update_integrative_success(weights_payoff, weights_similarity, weights_age, weights_conformity,
                                                    t_strat1_success, t_strat2_success, t_strat3_success, t_strat4_success,
                                                    r_payoffs, individual, t_skill_attempt)
          t_strat1_success[individual] <- unlist(new_weights[1])
          t_strat2_success[individual] <- unlist(new_weights[2])
          t_strat3_success[individual] <- unlist(new_weights[3])
          t_strat4_success[individual] <- unlist(new_weights[4])
          
        } else {
          t_SL_success[individual] <- 0
          new_weights <- update_integrative_failure(weights_payoff, weights_similarity, weights_age, weights_conformity,
                                                    t_strat1_failure, t_strat2_failure, t_strat3_failure, t_strat4_failure,
                                                    r_payoffs, individual, t_skill_attempt)
          t_strat1_failure[individual] <- unlist(new_weights[1])
          t_strat2_failure[individual] <- unlist(new_weights[2])
          t_strat3_failure[individual] <- unlist(new_weights[3])
          t_strat4_failure[individual] <- unlist(new_weights[4])
        }
        
        
      }
    }  
    t_ages[individual] <- t_ages[individual]+1  
    # save results from this timestep
    t_ID <- append(t_ID, population[individual])
    r_skills <- append(r_skills, t_skills[individual])
    r_ages <- append(r_ages, t_ages[individual])
    r_meta_strat <- append(r_meta_strat, r_meta_strat[individual])
    r_learning_strat <- append(r_learning_strat, t_learning_strat[individual])
    r_strat1_success <- append(r_strat1_success, t_strat1_success[individual])
    r_strat2_success <- append(r_strat2_success, t_strat2_success[individual])
    r_strat3_success <- append(r_strat3_success, t_strat3_success[individual])
    r_strat4_success <- append(r_strat4_success, t_strat4_success[individual])
    r_strat1_failure <- append(r_strat1_failure, t_strat1_failure[individual])
    r_strat2_failure <- append(r_strat2_failure, t_strat2_failure[individual])
    r_strat3_failure <- append(r_strat3_failure, t_strat3_failure[individual])
    r_strat4_failure <- append(r_strat4_failure, t_strat4_failure[individual])
    r_teacher_ID <- append(r_teacher_ID, t_teacherID[individual])
    r_skill_attempted <- append(r_skill_attempted, t_skill_attempt[individual])
    r_SL_rate <- append(r_SL_rate, t_SL_rate[individual])
    r_SL_success <- append(r_SL_success, t_SL_success[individual])
    r_IL_rate <- append(r_IL_rate, t_IL_rate[individual])
    r_IL_success <- append(r_IL_success, t_IL_success[individual])
    r_n_resets <- append(r_n_resets, t_resets[individual])
    r_learner_payoffs <- append(r_learner_payoffs, t_learner_payoffs[individual])
    r_eligible_teachers <- append(r_eligible_teachers, t_eligible_teachers[individual])
    cat("timestep = ", t,"round = ", r, "\n")
  }
  
  n_skills <- append(n_skills, r_skills)
  ages <- append(ages, r_ages) 
  meta_strat <- append(meta_strat, r_meta_strat)
  learning_strat <- append(learning_strat, r_learning_strat)
  payoffs <- append(payoffs, r_payoffs) 
  learner_payoffs <- append(learner_payoffs, r_learner_payoffs)
  strat1_success <- append(strat1_success, r_strat1_success)
  strat1_failure <- append(strat1_failure, r_strat1_failure)
  strat2_success <- append(strat2_success, r_strat2_success)
  strat2_failure <- append(strat2_failure, r_strat2_failure)
  strat3_success <- append(strat3_success, r_strat3_success)
  strat3_failure <- append(strat3_failure, r_strat3_failure)
  strat4_success <- append(strat4_success, r_strat4_success)
  strat4_failure <- append(strat4_failure, r_strat4_failure)
  teacher_ID <- append(teacher_ID, r_teacher_ID)
  skill_attempted <- append(skill_attempted, r_skill_attempted)
  SL_rate <- append(SL_rate, r_SL_rate)
  SL_success <- append(SL_success, r_SL_success)
  n_resets <- append(n_resets, r_n_resets)
  IL_rate <- append(IL_rate, r_IL_rate)
  IL_success <- append(IL_success, r_IL_success)
  eligible_teachers <- append(eligible_teachers, r_eligible_teachers)
  ID <- append(ID, t_ID)
}


overview <- data.frame(Round = n_rounds, 
                       Timestep = n_timestep, 
                       ID = ID, 
                       MetaStrat = meta_strat, 
                       LearningStrat = learning_strat, 
                       NumberSkills = n_skills,
                       Age = ages,
                       Payoff = learner_payoffs,
                       ILrate = IL_rate, 
                       ILsucess = IL_success,
                       SLrate = SL_rate,
                       SLsucess = SL_success,
                       PayoffLearningSuccess = strat1_success, 
                       PayoffLearningFails = strat1_failure, 
                       SimilarityLearningSuccess = strat2_success, 
                       SimilarityLearningFails = strat2_failure,
                       AgeLearningSuccess = strat3_success, 
                       AgeLearningFails = strat3_failure,
                       ConformityLearningSuccess = strat4_success, 
                       ConformityLearningFails = strat4_failure,
                       EligibleTeachers = eligible_teachers,
                       Teacher = teacher_ID,
                       AttemptedSkill = skill_attempted,
                       Resets = n_resets
)


num_columns <- ceiling(length(payoffs)/skills)
payoff_matrix <- matrix(payoffs, ncol = num_columns)
PayoffMatrix <- as.data.frame(payoff_matrix)
colnames(PayoffMatrix) <- paste0("Payoff_Round", 1:num_columns)

write.csv(overview, file = results_path, row.names = FALSE)
write.csv(PayoffMatrix, file = payoff_path, row.names = FALSE)

