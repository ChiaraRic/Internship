library(tidyverse)
library(hrbrthemes)
library(viridis)
library(stringr)
library(rjson)
library(forcats)
library(gganimate)
library(transformr)
library(readr)
library(bayesrules)
library(MARSS)
library(gdata)
library(gifski)
library(tidyquant)
library(ggdist)
# devtools::install_github("lukekorthals/gg-ygap")
library(ggygap)
library(readr)



### initiate vectors ----
vec_timesteps <- c(rep(0, length(population)), 1:timesteps)
n_timestep <- rep(vec_timesteps, rounds) 
n_rounds <- c()
for (rnd in 1:rounds) {
  vec_rounds <- c(rep(rnd, length(population)), rep(rnd, timesteps))
  n_rounds <- append(n_rounds, vec_rounds)
}
n_skills <- c()
meta_strat <- c()
learning_strat <- c()
payoffs <- c()
learner_payoffs <- c()
ages <- c() 
strat1_success <- c()
strat1_failure <- c()
strat2_success <- c()
strat2_failure <- c()
strat3_success <- c()
strat3_failure <- c()
strat4_success <- c()
strat4_failure <- c()
teacher_ID <- c()
skill_attempted <- c()
SL_rate <- c()
SL_success <- c()
n_resets <- c()
IL_rate <- c()
IL_success <- c()
eligible_teachers <- c()
t_ID <- c()
ID <- c()

### functions ----

create_payoffs <- function(){
  r_payoffs <- c()
  for (s in 1:skills){
    r_payoffs[s] <- runif(n = 1, min = 0.1, max = 1) * s
  }
  r_payoffs
}

select_LearningStrategies <- function(LearningStrat){
  if(LearningStrat == 1){
    return(1)
  } else if(LearningStrat == 2){
    return(2)
  } else if(LearningStrat == 3){
    return(3)
  } else if(LearningStrat == 4){
    return(4)
  } else return(1:4)
}

select_MetaLearning <- function(MetaLearningTypes){
  if(MetaLearningTypes == 1){
    return(1)
  } else if(MetaLearningTypes == 2){
    return(2)
  } else if(MetaLearningTypes == 3){
    return(3)
  } else return(1:3)
}

# select individual who learns that round
select_learner <- function(population, t_skills, skills) {
  individual <- sample(population, 1)
  if (t_skills[individual] == skills) {
    new_learner <- TRUE
    while (new_learner == TRUE) {
      individual <- sample(population, 1)
      if (t_skills[individual] < skills)
        new_learner <- FALSE
    }
  }
  individual
}

# select teachers to learn from
find_skilled_teachers <- function(skill_matrix, population, individual, resample_limit = 10) {
  for (attempt in 1:resample_limit) {
    rest_pop <- population[-individual]
    
    # Sample 10 potential teachers from the rest of the population
    potential_teachers <- sample(rest_pop, 10, replace = FALSE)
    
    # Find skills the individual does not have
    learner_open_skills <- which(skill_matrix[, individual] == 0)
    
    # Find skilled teachers
    skilled_teachers <- sapply(potential_teachers, function(teacher) {
      any(skill_matrix[learner_open_skills, teacher] == 1)
    })
    
    # Select skilled teachers
    skilled_teachers <- potential_teachers[skilled_teachers]
    
    if (length(skilled_teachers) > 0) {
      return(list(skilled_teachers, individual))
    }
  }
  
  # If resample limit is reached, select a new individual and repeat
  new_individual <- sample(population, 1)
  return(find_skilled_teachers(skill_matrix, population, new_individual, resample_limit))
}


# what does the learner do this round: reset, IL, or SL
state_learning <- function(reset_rate){
  s <- runif(1)
  r <- runif(1) 
  if(r <= reset_rate){ # r <= reset_rate*t_ages[individual]
    state <- "reset"
  } else if (s < social_learning){
    state <- "SL"
  } else if (s >= social_learning) state <- "IL"
}

# determine what can be successfully learnt based on skill tree 
is_learnable <- function(skill_tree, t_skill_attempt, individual){
  if (skill_tree == "flat"){
    if (skill_matrix[t_skill_attempt[individual], individual] == 0) {
      learnable <- TRUE
    } else learnable <- FALSE 
  } else if (skill_tree == "chain"){
    if (skill_matrix[t_skill_attempt[individual], individual] == 0 && skill_matrix[(t_skill_attempt[individual] - 1), individual] == 1) {
      learnable <- TRUE
    } else learnable <- FALSE 
  }
  return(learnable)  
}

# Individual learning 
individual_learning <- function(skill_matrix, individual){
  learnable_traits <- which(skill_matrix[, individual] == 0)
  learnable_traits <- resample(learnable_traits,1)
  selected_trait <- learnable_traits
}

# social learning strategies
payoff_based <- function(t_learner_payoffs, skilled_teacher){
  payoff_teachers <- t_learner_payoffs[skilled_teacher]
  teacher_highest_payoff <- c(skilled_teacher[payoff_teachers == max(payoff_teachers)])
  selected_teacher <- resample(teacher_highest_payoff, 1)
}
# similar traits
similarity_based <- function(t_skills, skilled_teacher, individual){
  similarity_score <- c(t_skills[skilled_teacher] - t_skills[individual])
  similar_teacher <- c(skilled_teacher[similarity_score == min(similarity_score)])
  selected_teacher <- resample(similar_teacher, 1)
}
# similar age
age_based <- function(t_ages, skilled_teacher, individual){
  teacher_age <- t_ages[skilled_teacher]
  learner_age <- t_ages[individual]
  age_difference <- c(teacher_age - learner_age)
  similar_age_teacher <- c(skilled_teacher[age_difference == min(age_difference)])    
  similar_age_teacher <- resample(similar_age_teacher,1)
}
# conformity, weigh common traits 
conformity_based <- function(skills, skilled_teacher, skill_matrix){
  skill_sum <- c()
  for (s in 2: skills){
    skill_s_sum <- sum(skill_matrix[s, skilled_teacher] == 1)
    skill_sum <- append(skill_sum, skill_s_sum)
  }
  conf_traits <- which(skill_sum == max(skill_sum)) + 1
  conf_traits <- resample(conf_traits, 1)
  teacher_conf <- skilled_teacher[which(skill_matrix[conf_traits,skilled_teacher] == 1)]
  teacher_conf <- resample(teacher_conf, 1)
  return(teacher_conf)
} 

determine_skill_range <- function(skill_matrix, selected_teacher, individual){
  skills_individual <- which(skill_matrix[,individual] == 1)
  skills_teacher <- which(skill_matrix[,selected_teacher] == 1)
  skill_range <- skills_teacher[which(!(skills_teacher %in% skills_individual))]
  return(skill_range)
}


# meta-strategies

## fixed meta learner 
fixed_strategy <- function (learningstrat, t_learner_payoffs, skilled_teacher, skill_matrix, t_skills, individual, t_ages, skills){
  if (learningstrat == 1){
    selected_teacher <- payoff_based(t_learner_payoffs, skilled_teacher)
    skill_range <- determine_skill_range(skill_matrix, selected_teacher, individual)
    observed_behavior <- resample(skill_range, 1)
  }else if(learningstrat == 2){
    selected_teacher <- similarity_based(t_skills, skilled_teacher, individual)
    skill_range <- determine_skill_range(skill_matrix, selected_teacher, individual)
    observed_behavior <- resample(skill_range, 1)
  }else if(learningstrat == 3){
    selected_teacher <- age_based(t_ages, skilled_teacher, individual)
    skill_range <- determine_skill_range(skill_matrix, selected_teacher, individual)
    observed_behavior <- resample(skill_range, 1)
  } else if(learningstrat == 4){
    selected_teacher <- conformity_based(skills, skilled_teacher, skill_matrix)
    skill_range <- determine_skill_range(skill_matrix, selected_teacher, individual)
    observed_behavior <- resample(skill_range, 1)
  } 
  list(selected_teacher, observed_behavior)
}


## flexible meta learner
flexible_strategy <- function(t_strat1_success, t_strat2_success, t_strat3_success, t_strat4_success,
                              t_strat1_failure, t_strat2_failure, t_strat3_failure, t_strat4_failure,
                              individual){
  successes <- c(t_strat1_success[individual],
                 t_strat2_success[individual],
                 t_strat3_success[individual], 
                 t_strat4_success[individual] 
  )
  failures <-  c(t_strat1_failure[individual],
                 t_strat2_failure[individual], 
                 t_strat3_failure[individual], 
                 t_strat4_failure[individual]
  )                   
  probs_learningstrat <- c()
  for(learningstrat in 1:4){
    a <- successes[learningstrat]
    b <- failures[learningstrat]
    distr_bayesian <- rbeta(n = 1, shape1 = 1 + a, shape2 = 1 + b)
    probs_learningstrat <- append(probs_learningstrat, distr_bayesian)
    
  }
  list(probs_learningstrat, which.max(probs_learningstrat))
}
update_weights_success <- function(learningstrat, t_strat1_success, t_strat2_success, t_strat3_success, t_strat4_success,
                                   individual, r_payoffs, t_skill_attempt){
  if (learningstrat == 1){
    strat1_success <- t_strat1_success[individual] + 1 * r_payoffs[t_skill_attempt[individual]]
    strat2_success <- t_strat2_success[individual] 
    strat3_success <- t_strat3_success[individual]
    strat4_success <- t_strat4_success[individual]
  }
  if (learningstrat == 2){
    strat1_success <- t_strat1_success[individual] 
    strat2_success <- t_strat2_success[individual] + 1 * r_payoffs[t_skill_attempt[individual]]
    strat3_success <- t_strat3_success[individual]
    strat4_success <- t_strat4_success[individual]
  }
  if (learningstrat == 3){
    strat1_success <- t_strat1_success[individual] 
    strat2_success <- t_strat2_success[individual] 
    strat3_success <- t_strat3_success[individual]+ 1 * r_payoffs[t_skill_attempt[individual]]
    strat4_success <- t_strat4_success[individual]
  }
  if (learningstrat == 4){
    strat1_success <- t_strat1_success[individual] 
    strat2_success <- t_strat2_success[individual] 
    strat3_success <- t_strat3_success[individual]
    strat4_success <- t_strat4_success[individual] + 1 * r_payoffs[t_skill_attempt[individual]]
  }
  list(strat1_success, strat2_success, strat3_success, strat4_success)
}
update_weights_failure <- function(learningstrat, t_strat1_failure, t_strat2_failure, t_strat3_failure, t_strat4_failure,
                                   individual, r_payoffs, t_skill_attempt){
  if (learningstrat == 1){
    strat1_failure <- t_strat1_failure[individual] + 1 * r_payoffs[t_skill_attempt[individual]]
    strat2_failure <- t_strat2_failure[individual] 
    strat3_failure <- t_strat3_failure[individual]
    strat4_failure <- t_strat4_failure[individual]
  }
  if (learningstrat == 2){
    strat1_failure <- t_strat1_failure[individual] 
    strat2_failure <- t_strat2_failure[individual] + 1 * r_payoffs[t_skill_attempt[individual]]
    strat3_failure <- t_strat3_failure[individual]
    strat4_failure <- t_strat4_failure[individual]
  }
  if (learningstrat == 3){
    strat1_failure <- t_strat1_failure[individual] 
    strat2_failure <- t_strat2_failure[individual] 
    strat3_failure <- t_strat3_failure[individual]+ 1 * r_payoffs[t_skill_attempt[individual]]
    strat4_failure <- t_strat4_failure[individual]
  }
  if (learningstrat == 4){
    strat1_failure <- t_strat1_failure[individual] 
    strat2_failure <- t_strat2_failure[individual] 
    strat3_failure <- t_strat3_failure[individual]
    strat4_failure <- t_strat4_failure[individual] + 1 * r_payoffs[t_skill_attempt[individual]]
  }
  list(strat1_failure, strat2_failure, strat3_failure, strat4_failure)
}


## integrative learner 
calculate_z_score <- function(vec){
  if (length(unique(vec)) == 1){
    score <- rep(0, length(vec))
  } else score <- zscore(vec)
  return(score)
}

calculate_score_payoff <- function(t_learner_payoffs, skilled_teacher){
  teacher_payoff <- t_learner_payoffs[skilled_teacher]
  score_payoff <- calculate_z_score(teacher_payoff)
  return(score_payoff) 
}
calculate_score_similarity <- function(t_skills, skilled_teacher, individual){
  skills_teacher <- t_skills[skilled_teacher]
  skills_learner <- t_skills[individual]
  similarity_score <- c(skills_teacher - skills_learner)
  score_similarity <- calculate_z_score(similarity_score)
  return(score_similarity)
}
calculate_score_age <- function(t_ages, skilled_teacher, individual){
  teacher_age <- c(t_ages[skilled_teacher])
  learner_age <- t_ages[individual]
  age_difference <- c(abs(teacher_age - learner_age))
  score_age <- calculate_z_score(age_difference)
  return(score_age)
}
calculate_score_conformity <- function(t_skills, skilled_teacher, individual){
  skill_sum <- c()
  for (s in 2: skills){
    skill_s_sum <- sum(skill_matrix[s, skilled_teacher] == 1)
    skill_sum <- append(skill_sum, skill_s_sum)
  }
  conf_traits <- which(skill_sum == max(skill_sum)) + 1
  conf_traits <- resample(conf_traits, 1)
  conf_teachers <- which(skill_matrix[conf_traits,skilled_teacher] == 1)
  teacher_conf <- skilled_teacher
  teacher_conf[conf_teachers] <- max(skill_sum)
  teacher_conf[!conf_teachers] <- 0
  score_conf <- calculate_z_score(teacher_conf)
  return(score_conf)
}

calculate_total_score <- function(skilled_teacher, score_payoff, score_similarity, score_age, score_conf,
                                  probs_learningstrat){
  weighted_payoff_score <- c()
  weighted_similiarity_score <- c()
  weighted_age_score <- c()
  weighted_conformity_score <- c()
  overall_score <- c()
  for (i in 1:length(skilled_teacher)){
    weighted_payoff_score[i] <- score_payoff[i] * probs_learningstrat[1]
    weighted_similiarity_score[i] <- score_similarity[i] * probs_learningstrat[2]
    weighted_age_score[i] <- score_age[i] * probs_learningstrat[3]
    weighted_conformity_score[i] <- score_conf[i] * probs_learningstrat[4]
    overall_score[i] <- weighted_payoff_score[i] - weighted_similiarity_score[i] - weighted_age_score[i] + weighted_conformity_score[i] ## was + for all
  }
  list(overall_score, weighted_payoff_score, weighted_similiarity_score, weighted_age_score, weighted_conformity_score)
}

integrative_strategy <- function(t_skills, skilled_teacher, individual, t_learner_payoffs,
                                 t_ages, skill_matrix, t_strat1_success, t_strat2_success, t_strat3_success, t_strat4_success,
                                 t_strat1_failure, t_strat2_failure, t_strat3_failure, t_strat4_failure){
  ## calculate how well each teacher performs based on each learning strategy
  score_payoff <- calculate_score_payoff(t_learner_payoffs, skilled_teacher)
  score_similarity <- calculate_score_similarity(t_skills, skilled_teacher, individual)
  score_age <- calculate_score_age(t_ages, skilled_teacher, individual)
  score_conf <-  calculate_score_conformity(t_skills, skilled_teacher, individual)
  # find out how well each learning strategy performed for the individual in the past
  probs_learningstrat <- unlist(flexible_strategy(t_strat1_success, t_strat2_success, t_strat3_success, t_strat4_success,
                                                  t_strat1_failure, t_strat2_failure, t_strat3_failure, t_strat4_failure,
                                                  individual)[1])
  # get an overall weighted score for each teacher
  overall_score <- unlist(calculate_total_score(skilled_teacher, score_payoff, score_similarity, score_age, score_conf,
                                                probs_learningstrat)[1])
  weighted_payoff_score <- unlist(calculate_total_score(skilled_teacher, score_payoff, score_similarity, score_age, score_conf,
                                                        probs_learningstrat)[2])
  weighted_similiarity_score <- unlist(calculate_total_score(skilled_teacher, score_payoff, score_similarity, score_age, score_conf,
                                                             probs_learningstrat)[3])
  weighted_age_score <- unlist(calculate_total_score(skilled_teacher, score_payoff, score_similarity, score_age, score_conf,
                                                     probs_learningstrat)[4])
  weighted_conformity_score <- unlist(calculate_total_score(skilled_teacher, score_payoff, score_similarity, score_age, score_conf,
                                                            probs_learningstrat)[5])
  # select teacher and skill to learn
  selected_teacher <- resample(skilled_teacher[overall_score == max(overall_score)], 1)
  skill_range <- determine_skill_range(skill_matrix, selected_teacher, individual)
  observed_behavior <- resample(skill_range, 1)
  list(selected_teacher, 
       observed_behavior, 
       overall_score[skilled_teacher == selected_teacher], 
       weighted_payoff_score[skilled_teacher == selected_teacher], 
       weighted_similiarity_score[skilled_teacher == selected_teacher], 
       weighted_age_score[skilled_teacher == selected_teacher], 
       weighted_conformity_score[skilled_teacher == selected_teacher])
}

update_integrative_success <- function(weights_payoff, weights_similarity, weights_age, weights_conformity,
                                       t_strat1_success, t_strat2_success, t_strat3_success, t_strat4_success,
                                       r_payoffs, individual, t_skill_attempt){
  abs_total <- abs(weights_payoff) + abs(weights_similarity)+ abs(weights_age) + abs(weights_conformity) + 10^-12
  strat1_success <- t_strat1_success[individual] + (abs(weights_payoff)/abs_total) * r_payoffs[t_skill_attempt[individual]]
  strat2_success <- t_strat2_success[individual] + (abs(weights_similarity)/abs_total) * r_payoffs[t_skill_attempt[individual]]
  strat3_success <- t_strat3_success[individual] + (abs(weights_age)/abs_total) * r_payoffs[t_skill_attempt[individual]]
  strat4_success <- t_strat4_success[individual] + (abs(weights_conformity)/abs_total) * r_payoffs[t_skill_attempt[individual]]
  list(strat1_success, strat2_success, strat3_success, strat4_success)
}

update_integrative_failure <- function(weights_payoff, weights_similarity, weights_age, weights_conformity,
                                       t_strat1_failure, t_strat2_failure, t_strat3_failure, t_strat4_failure,
                                       r_payoffs, individual, t_skill_attempt){
  abs_total <- abs(weights_payoff) + abs(weights_similarity)+ abs(weights_age) + abs(weights_conformity)+ 10^-12
  strat1_failure <- t_strat1_failure[individual] + (abs(weights_payoff)/abs_total) * r_payoffs[t_skill_attempt[individual]]
  strat2_failure <- t_strat2_failure[individual] + (abs(weights_similarity)/abs_total) * r_payoffs[t_skill_attempt[individual]]
  strat3_failure <- t_strat3_failure[individual] + (abs(weights_age)/abs_total) * r_payoffs[t_skill_attempt[individual]]
  strat4_failure <- t_strat4_failure[individual] + (abs(weights_conformity)/abs_total) * r_payoffs[t_skill_attempt[individual]]
  list(strat1_failure, strat2_failure, strat3_failure, strat4_failure)
}



