library(shiny)
library(shinydashboard)
#Function-----------------
Lxs <- c(10000.0000,9994.0000,9988.0636,9982.2006,9976.3909,9970.6346,9964.9313,9959.2613,9953.6144,9947.9807,9942.3402,9936.6730,9930.9694,9925.2094,9919.3535,9913.3821,9907.2655,9900.9645,9894.4299,9887.6126,9880.4540,9872.8954,9864.8688,9856.2863,9847.0510,9837.0661,9826.2060,9814.3359,9801.3123,9786.9534,
       9771.0789,9753.4714,9733.8865,9712.0728,9687.7149,9660.5021,9630.0522,9595.9715,9557.8179,9515.1040,9467.2906,9413.8004,9354.0040,9287.2164,9212.7143,9129.7170,9037.3973,8934.8771,8821.2612,8695.6199,8557.0118,8404.4916,8237.1329,8054.0544,7854.4508,7637.6208,7403.0084,7150.2401,6879.1673,6589.9258,
       6282.9803,5959.1680,5619.7577,5266.4604,4901.4789,4527.4960,4147.6708,3765.5998,3385.2479,3010.8395,2646.7416,2297.2976,1966.6499,1658.5545,1376.1906,1121.9889,897.5025,703.3242,539.0643,403.4023,294.2061,208.7060,143.7120,95.8476,61.7733,38.3796,22.9284,13.1359,7.1968,3.7596,1.8669,0.8784,0.3903,0.1632,0.0640,0.0234,
       0.0080,0.0025,0.0007,0.0002,0.0000,0.0000,0.0000,0.0000) 
Lx <- Lxs[-(1:3)]
Ly <- Lxs[1:length(Lx)]



#Annuity
Whole_life_annuity = function(age,interest_rate){
  discount_rate = 1/(1+interest_rate)
  survival_probabilites = Lx[-(1:(age-20+1))]/Lx[age-20+1]
  all_discount_rate = discount_rate^(1:length(survival_probabilites))
  EPV = sum(survival_probabilites*all_discount_rate)
  return(EPV)
}

Whole_life_annuity_inf = function(age, interest_rate, inflation_rate){
  j = ((1+interest_rate)/(1+inflation_rate)) - 1
  EPV = (1/(1+interest_rate))*Whole_life_annuity(age, j)
  return(EPV)
}

Term_annuity_inf_due = function(age, interest_rate, term, inflation_rate){
  v_n = (1/(1+interest_rate))^term
  n_p_x = Lx[age-20+1+term]/Lx[age-20+1]
  EPV = Term_annuity_inf(age, interest_rate, term, inflation_rate) + 1 - (v_n*n_p_x)
  return(EPV)
}

Term_annuity_due = function(age, interest_rate, term){
  v_n = (1/(1+interest_rate))^term
  n_p_x = Lx[age-20+1+term]/Lx[age-20+1]
  EPV = Term_annuity(age, interest_rate, term) + 1 - (v_n*n_p_x)
  return(EPV)
}

Term_annuity = function(age, interest_rate, term){
  v_n = (1/(1+interest_rate))^term
  n_p_x = Lx[age-20+1+term]/Lx[age-20+1]
  EPV = Whole_life_annuity(age, interest_rate) - (v_n*n_p_x*Whole_life_annuity(age+term, interest_rate))
  return(EPV)
}

Term_annuity_inf = function(age, interest_rate, term, inflation_rate){
  j = ((1+interest_rate)/(1+inflation_rate)) - 1
  EPV = (1/(1+inflation_rate))*Term_annuity(age, j, term)
  return(EPV)
}

#Assurance
Whole_life_assurance = function(age, interest_rate){
  discount_rate = 1/(1+interest_rate)
  d = interest_rate*discount_rate
  EPV = discount_rate - (d*Whole_life_annuity(age,interest_rate))
  return(EPV)
}

Whole_life_assurance_ = function(age, interest_rate){
  EPV = ((1+interest_rate)^0.5)*Whole_life_assurance(age, interest_rate)
  return(EPV)
}

Term_assurance = function(age, interest_rate, term){
  v_n = (1/(1+interest_rate))^term
  n_p_x = Lx[age-20+1+term]/Lx[age-20+1]
  EPV = Whole_life_assurance(age, interest_rate) - (v_n*n_p_x*Whole_life_assurance(age+term, interest_rate))
  return(EPV)
}

Term_assurance_ = function(age, interest_rate, term){
  EPV = ((1+interest_rate)^0.5)*Term_assurance(age, interest_rate, term)
  return(EPV)
}

Pure_endowment = function(age, interest_rate, term){
  v_n = (1/(1+interest_rate))^term
  n_p_x = Lx[age-20+1+term]/Lx[age-20+1]
  EPV = v_n*n_p_x
  return(EPV)
}

Endowment = function(age, interest_rate, term){
  EPV = Term_assurance(age,interest_rate, term) + Pure_endowment(age,interest_rate, term)
  return(EPV)
}

Endowment_ = function(age, interest_rate, term){
  force_interest_rate = log(1+interest_rate)
  v_n = (1/(1+interest_rate))^term
  n_p_x = Lx[age-20+1+term]/Lx[age-20+1]
  Term_annuity_due = 1 + Term_annuity(age, interest_rate, term-1)
  con_term_annuity = Term_annuity_due - (0.5*(1-(v_n*n_p_x)))
  EPV = 1 - (force_interest_rate*con_term_annuity)
  return(EPV)
}

#Bonus
bonus_whole = function(interest_rate, age, bonus_rate){
  discount_rate = (1/(1+interest_rate))
  all_discount_rate = discount_rate^(1:(120-age+1))
  all_bonus = (1:(120-age+1))*bonus_rate
  bonus = sum(all_discount_rate*all_bonus)
  return(bonus)
}

bonus_term = function(interest_rate, bonus_rate, term){
  discount_rate = (1/(1+interest_rate))
  all_discount_rate = discount_rate^(1:term)
  all_bonus = (1:term)*bonus_rate
  bonus = sum(all_discount_rate*all_bonus)
  return(bonus)
}

bonus_pure_endowment = function(age, interest_rate, bonus_rate, term){
  v_n = (1/(1+interest_rate))^term
  n_p_x = Lx[age-20+1+term]/Lx[age-20+1]
  bonus = 1 + (term*bonus_rate)
  EPV = bonus*v_n*n_p_x
  return(EPV)
}

bonus_endowment = function(age, interest_rate, bonus_rate, term){
  term_ass = Term_assurance(age, interest_rate, term)
  term_bonus = bonus_term(interest_rate, bonus_rate, term)
  pure_EPV = bonus_pure_endowment(age, interest_rate, bonus_rate, term)
  EPV = term_ass + term_bonus + pure_EPV
  return(EPV)
}

bonus_endowment_ = function(age, interest_rate, bonus_rate, term){
  term_ass = Term_assurance_(age, interest_rate, term)
  term_bonus = bonus_term(interest_rate, bonus_rate, term)
  pure_EPV = bonus_pure_endowment(age, interest_rate, bonus_rate, term)
  EPV = term_ass + term_bonus + pure_EPV
  return(EPV)
}

#Annuity 2
yWhole_life_annuity = function(age,interest_rate){
  discount_rate = 1/(1+interest_rate)
  survival_probabilites = Ly[-(1:(age-20+1))]/Ly[age-20+1]
  all_discount_rate = discount_rate^(1:length(survival_probabilites))
  EPV = sum(survival_probabilites*all_discount_rate)
  return(EPV)
}

yWhole_life_annuity_inf = function(age, interest_rate, inflation_rate){
  j = ((1+interest_rate)/(1+inflation_rate)) - 1
  EPV = (1/(1+interest_rate))*yWhole_life_annuity(age, j)
  return(EPV)
}

yTerm_annuity_inf_due = function(age, interest_rate, term, inflation_rate){
  v_n = (1/(1+interest_rate))^term
  n_p_x = Ly[age-20+1+term]/Ly[age-20+1]
  EPV = yTerm_annuity_inf(age, interest_rate, term, inflation_rate) + 1 - (v_n*n_p_x)
  return(EPV)
}

yTerm_annuity_due = function(age, interest_rate, term){
  v_n = (1/(1+interest_rate))^term
  n_p_x = Ly[age-20+1+term]/Ly[age-20+1]
  EPV = yTerm_annuity(age, interest_rate, term) + 1 - (v_n*n_p_x)
  return(EPV)
}

yTerm_annuity = function(age, interest_rate, term){
  v_n = (1/(1+interest_rate))^term
  n_p_x = Ly[age-20+1+term]/Ly[age-20+1]
  EPV = yWhole_life_annuity(age, interest_rate) - (v_n*n_p_x*yWhole_life_annuity(age+term, interest_rate))
  return(EPV)
}

yTerm_annuity_inf = function(age, interest_rate, term, inflation_rate){
  j = ((1+interest_rate)/(1+inflation_rate)) - 1
  EPV = (1/(1+inflation_rate))*yTerm_annuity(age, j, term)
  return(EPV)
}

#Assurance 2
yWhole_life_assurance = function(age, interest_rate){
  discount_rate = 1/(1+interest_rate)
  d = interest_rate*discount_rate
  EPV = discount_rate - (d*yWhole_life_annuity(age,interest_rate))
  return(EPV)
}

yWhole_life_assurance_ = function(age, interest_rate){
  EPV = ((1+interest_rate)^0.5)*yWhole_life_assurance(age, interest_rate)
  return(EPV)
}

yTerm_assurance = function(age, interest_rate, term){
  v_n = (1/(1+interest_rate))^term
  n_p_x = Ly[age-20+1+term]/Ly[age-20+1]
  EPV = yWhole_life_assurance(age, interest_rate) - (v_n*n_p_x*yWhole_life_assurance(age+term, interest_rate))
  return(EPV)
}

yTerm_assurance_ = function(age, interest_rate, term){
  EPV = ((1+interest_rate)^0.5)*yTerm_assurance(age, interest_rate, term)
  return(EPV)
}

yPure_endowment = function(age, interest_rate, term){
  v_n = (1/(1+interest_rate))^term
  n_p_x = Ly[age-20+1+term]/Ly[age-20+1]
  EPV = v_n*n_p_x
  return(EPV)
}

yEndowment = function(age, interest_rate, term){
  EPV = yTerm_assurance(age,interest_rate, term) + yPure_endowment(age,interest_rate, term)
  return(EPV)
}

yEndowment_ = function(age, interest_rate, term){
  force_interest_rate = log(1+interest_rate)
  v_n = (1/(1+interest_rate))^term
  n_p_x = Ly[age-20+1+term]/Ly[age-20+1]
  yTerm_annuity_due = 1 + yTerm_annuity(age, interest_rate, term-1)
  ycon_term_annuity = yTerm_annuity_due - (0.5*(1-(v_n*n_p_x)))
  EPV = 1 - (force_interest_rate*ycon_term_annuity)
  return(EPV)
}

#Bonus
ybonus_whole = function(interest_rate, age, bonus_rate){
  discount_rate = (1/(1+interest_rate))
  all_discount_rate = discount_rate^(1:(120-age+1))
  all_bonus = (1:(120-age+1))*bonus_rate
  bonus = sum(all_discount_rate*all_bonus)
  return(bonus)
}

ybonus_term = function(interest_rate, bonus_rate, term){
  discount_rate = (1/(1+interest_rate))
  all_discount_rate = discount_rate^(1:term)
  all_bonus = (1:term)*bonus_rate
  bonus = sum(all_discount_rate*all_bonus)
  return(bonus)
}

ybonus_pure_endowment = function(age, interest_rate, bonus_rate, term){
  v_n = (1/(1+interest_rate))^term
  n_p_x = Ly[age-20+1+term]/Ly[age-20+1]
  bonus = 1 + (term*bonus_rate)
  EPV = bonus*v_n*n_p_x
  return(EPV)
}

ybonus_endowment = function(age, interest_rate, bonus_rate, term){
  term_ass = yTerm_assurance(age, interest_rate, term)
  term_bonus = ybonus_term(interest_rate, bonus_rate, term)
  pure_EPV = ybonus_pure_endowment(age, interest_rate, bonus_rate, term)
  EPV = term_ass + term_bonus + pure_EPV
  return(EPV)
}

ybonus_endowment_ = function(age, interest_rate, bonus_rate, term){
  term_ass = yTerm_assurance_(age, interest_rate, term)
  term_bonus = ybonus_term(interest_rate, bonus_rate, term)
  pure_EPV = ybonus_pure_endowment(age, interest_rate, bonus_rate, term)
  EPV = term_ass + term_bonus + pure_EPV
  return(EPV)
}


#Joint
survival_probability_x = function(age){
  table = Lx[-(1:(age-20+1))]/Lx[age-20+1]
  add_table = c(1,table)
  return(add_table)
}

survival_probability_y = function(age){
  table = Ly[-(1:(age-20+1))]/Ly[age-20+1]
  add_table = c(1,table)
  return(add_table)
}

survival_probability_xy = function(agex, agey){
  k = min(120-agex+1,120-agey+1)
  x = survival_probability_x(agex)
  x_new = x[1:k]
  y = survival_probability_y(agey)
  y_new = y[1:k]
  table = x_new * y_new
  table_k = table[-((min(length(x),length(y))+1):(max(length(x),length(y))))]
  return(table_k)
}

q_x = function(age){
  vector = c()
  k = 0:(120-age)
  for (i in 1:length(k)){
    vector[i] = (Lxs[i+age-20]-Lxs[i+age-20+1])/Lxs[i+age-20]
  }
  vector
  vector_2 = replace(vector, is.nan(vector), 1)
  vector_3 = replace(vector, is.na(vector), 1)
  return(vector_3)
}

q_y = function(age){
  vector = c()
  k = 0:(120-age)
  for (i in 1:length(k)){
    vector[i] = (Ly[i+age-20]-Ly[i+age-20+1])/Ly[i+age-20]
  }
  vector
  vector_2 = replace(vector, is.nan(vector), 1)
  vector_3 = replace(vector, is.na(vector), 1)
  return(vector_3)
}

Whole_joint_life = function(agex, agey, interest_rate){
  k = min(120-agex+1,120-agey+1)
  discount_rate = 1/ (1+interest_rate)
  survival_probability = survival_probability_xy(agex, agey)
  all_discount_rate = discount_rate^(1:length(survival_probability))
  die_x = q_x(agex)
  new_die_x = die_x[1:k]
  die_y = q_y(agey)
  new_die_y = die_y[1:k]
  product = all_discount_rate*survival_probability*new_die_x*new_die_y
  vector = product[1:length(survival_probability)]
  EPV = sum(vector)
  return(EPV)
}

Whole_joint_life_ = function(agex, agey, interest_rate){
  EPV = ((1 + interest_rate)^0.5) * Whole_joint_life(agex, agey, interest_rate)
  return(EPV)
}

Term_joint_assurance = function(agex, agey, interest_rate, term){
  v_n = (1/(1+interest_rate))^term
  survival = survival_probability_xy(agex,agey)
  n_p_xy = survival[term]
  EPV = Whole_joint_life(agex, agey, interest_rate) - (v_n*n_p_xy*Whole_joint_life(agex+term, agey+term, interest_rate))
  return(EPV)
}

Term_joint_assurance_ = function(agex, agey, interest_rate, term){
  EPV = ((1+interest_rate)^0.5) * Term_joint_assurance(agex, agey, interest_rate, term)
  return(EPV)
}

Whole_joint_life_annuity = function(agex, agey, interest_rate){
  v = 1/(1+interest_rate)
  d = interest_rate * v
  EPV = (1 - d - Whole_joint_life)/(d)
  return(EPV)
}

Term_joint_annuity = function(agex, agey, interest_rate, term){
  v_n = (1/(1+interest_rate))^term
  survival = survival_probability_xy(agex,agey)
  n_p_xy = survival[term]
  due = Whole_joint_life_annuity(agex, agey, interest_rate)+1
  term_due = due - (v_n*n_p_xy*Whole_joint_life_annuity(agex+term, agey+term, interest_rate))
  EPV = (term_due - 1) + (v_n*n_p_xy)
  return(EPV)
}

Whole_joint_life_annuity_inf = function(agex, agey, interest_rate, inflation_rate){
  k = min(120-agex+1,120-agey+1)
  v = (1/(1+interest_rate))
  kpxy = survival_probability_xy(agex, agey)
  vk = v^(1:k)
  inflation = (1+inflation_rate)^(1:k)
  EPV = sum(kpxy*vk*inflation)
  return(EPV)
}

Term_joint_annuity_inf = function(agex, agey, interest_rate, term, inflation_rate){
  v_n = (1/(1+interest_rate))^term
  survival = survival_probability_xy(agex,agey)
  n_p_xy = survival[term]
  due = Whole_joint_life_annuity_inf(agex, agey, interest_rate, inflation_rate)+1
  term_due = due - (v_n*n_p_xy*Whole_joint_life_annuity_inf(agex+term, agey+term, interest_rate, inflation_rate))
  EPV = (term_due - 1) + (v_n*n_p_xy)
  return(EPV)
}

#----------------------------------------

Whole_life_assurance_reserve_single = function(age, interest_rate, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Whole_life_assurance(t[i], interest_rate) + bonus_whole(interest_rate, t[i], bonus_rate) + (claim_expense*(Whole_life_assurance(t[i], interest_rate) + bonus_whole(interest_rate, t[i], bonus_rate)))
  }
  plot(t,vector)
}

Whole_life_assurance_reserve_single_ = function(age, interest_rate, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Whole_life_assurance_(t[i], interest_rate) + bonus_whole(interest_rate, t[i], bonus_rate) + (claim_expense*(Whole_life_assurance_(t[i], interest_rate) + bonus_whole(interest_rate, t[i], bonus_rate)))
  }
  plot(t,vector)
}

Term_assurance_reserve_single = function(age, interest_rate, term, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Term_assurance(t[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term) + (claim_expense*(Term_assurance(t[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term)))
  }
  plot(t,vector)
}

Term_assurance_reserve_single_ = function(age, interest_rate, term, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Term_assurance_(t[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term) + (claim_expense*(Term_assurance_(t[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term)))
  }
  plot(t,vector)
}

Pure_endowment_reserve = function(age, interest_rate, term, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Pure_endowment(t[i], interest_rate, term) + (claim_expense*(Pure_endowment(t[i], interest_rate, term)))
  }
  plot(t,vector)
}

Endowment_reserve = function(age, interest_rate, term, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = bonus_endowment(t[i], interest_rate, bonus_rate, term) + (claim_expense*(bonus_endowment(t[i], interest_rate, bonus_rate, term)))
  }
  plot(t,vector)
}

Endowment_reserve_ = function(age, interest_rate, term, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = bonus_endowment_(t[i], interest_rate, bonus_rate, term) + (claim_expense*(bonus_endowment_(t[i], interest_rate, bonus_rate, term)))
  }
  plot(t,vector)
}

Whole_life_annuity_reserve_single = function(age, interest_rate, claim_expense, bonus_rate, inflation_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Whole_life_annuity_inf(t[i], interest_rate, inflation_rate) + (claim_expense*(Whole_life_annuity_inf(t[i], interest_rate, inflation_rate)))
  }
  plot(t,vector)
}

Term_annuity_reserve_single = function(age, interest_rate, claim_expense, bonus_rate, term, inflation_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Term_annuity_inf(t[i], interest_rate, term, inflation_rate) + (claim_expense*(Term_annuity_inf(t[i], interest_rate, term, inflation_rate)))
  }
  plot(t,vector)
}

Whole_life_assurance_reserve_joint = function(agex, agey, interest_rate, claim_expense, bonus_rate){
  vector = c()
  t1 = max(agex,agey):100
  t2 = min(agex,agey):(100-(max(agex,agey)-min(agex,agey)))
  for (i in 1:length(t)){
    vector[i] = Whole_joint_life(t1[i], t2[i], interest_rate) + bonus_whole(interest_rate, t1[i], bonus_rate) + (claim_expense*(Whole_joint_life(t1[i], t2[i], interest_rate) + bonus_whole(interest_rate, t1[i], bonus_rate)))
  }
  plot(t,vector)
}

Whole_life_assurance_reserve_joint_ = function(agex, agey, interest_rate, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Whole_joint_life_(t1[i], t2[i], interest_rate) + bonus_whole(interest_rate, t1[i], bonus_rate) + (claim_expense*(Whole_joint_life_(t1[i], t2[i], interest_rate) + bonus_whole(interest_rate, t1[i], bonus_rate)))
  }
  plot(t,vector)
}

Term_assurance_reserve_joint = function(agex, agey, interest_rate, term, claim_expense, bonus_rate){
  vector = c()
  t1 = max(agex,agey):100
  t2 = min(agex,agey):(100-(max(agex,agey)-min(agex,agey)))
  for (i in 1:length(t)){
    vector[i] = Term_joint_assurance(t1[i], t2[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term) + (claim_expense*(Term_joint_assurance(t1[i], t2[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term)))
  }
  plot(t,vector)
}

Term_assurance_reserve_joint_ = function(agex, agey, interest_rate, term, claim_expense, bonus_rate){
  vector = c()
  t1 = max(agex,agey):100
  t2 = min(agex,agey):(100-(max(agex,agey)-min(agex,agey)))
  for (i in 1:length(t)){
    vector[i] = Term_joint_assurance_(t1[i], t2[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term) + (claim_expense*(Term_joint_assurance_(t1[i], t2[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term)))
  }
  plot(t,vector)
}

Whole_life_annuity_reserve_joint = function(agex, agey, interest_rate, claim_expense, bonus_rate, inflation_rate){
  vector = c()
  t1 = max(agex,agey):100
  t2 = min(agex,agey):(100-(max(agex,agey)-min(agex,agey)))
  for (i in 1:length(t)){
    vector[i] = Whole_joint_life_annuity_inf(t1[i], t2[i], interest_rate, inflation_rate) + (claim_expense*(Whole_joint_life_annuity_inf(t1[i], t2[i], interest_rate, inflation_rate)))
  }
  plot(t,vector)
}

Term_annuity_reserve_joint = function(agex, agey, interest_rate, term, claim_expense, bonus_rate, inflation_rate){
  vector = c()
  t1 = max(agex,agey):100
  t2 = min(agex,agey):(100-(max(agex,agey)-min(agex,agey)))
  for (i in 1:length(t)){
    vector[i] = Term_joint_annuity_inf(t1[i], t2[i], interest_rate, term, inflation_rate) + (claim_expense*(Term_joint_annuity_inf(t1[i], t2[i], interest_rate, term, inflation_rate)))
  }
  plot(t,vector)
}

#----------------------------------------------------------------------

Whole_life_assurance_reserve_single_level = function(age, interest_rate, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Whole_life_assurance(t[i], interest_rate) + bonus_whole(interest_rate, t[i], bonus_rate) + (claim_expense*(Whole_life_assurance(t[i], interest_rate) + bonus_whole(interest_rate, t[i], bonus_rate))) - prem*(Whole_life_annuity(t[i], interest_rate)-1)
  }
  plot(t,vector)
}

Whole_life_assurance_reserve_single_level_ = function(age, interest_rate, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Whole_life_assurance_(t[i], interest_rate) + bonus_whole(interest_rate, t[i], bonus_rate) + (claim_expense*(Whole_life_assurance_(t[i], interest_rate) + bonus_whole(interest_rate, t[i], bonus_rate))) - prem*(Whole_life_annuity(t[i], interest_rate)-1)
  }
  plot(t,vector)
}

Term_assurance_reserve_single_level = function(age, interest_rate, term, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Term_assurance(t[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term) + (claim_expense*(Term_assurance(t[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term))) - prem*(Term_annuity_due(t[i], interest_rate, term))
  }
  plot(t,vector)
}

Term_assurance_reserve_single_level_ = function(age, interest_rate, term, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Term_assurance_(t[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term) + (claim_expense*(Term_assurance_(t[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term))) - - prem*(Term_annuity_due(t[i], interest_rate, term))
  }
  plot(t,vector)
}

Pure_endowment_reserve_level = function(age, interest_rate, term, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Pure_endowment(t[i], interest_rate, term) + (claim_expense*(Pure_endowment(t[i], interest_rate, term))) - prem*(Term_annuity_due(t[i], interest_rate, term))
  }
  plot(t,vector)
}

Endowment_reserve_level = function(age, interest_rate, term, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = bonus_endowment(t[i], interest_rate, bonus_rate, term) + (claim_expense*(bonus_endowment(t[i], interest_rate, bonus_rate, term))) - prem*(Term_annuity_due(t[i], interest_rate, term))
  }
  plot(t,vector)
}

Endowment_reserve_level_ = function(age, interest_rate, term, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = bonus_endowment_(t[i], interest_rate, bonus_rate, term) + (claim_expense*(bonus_endowment_(t[i], interest_rate, bonus_rate, term))) - prem*(Term_annuity_due(t[i], interest_rate, term))
  }
  plot(t,vector)
}

Whole_life_annuity_reserve_single_level = function(age, interest_rate, claim_expense, bonus_rate, inflation_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Whole_life_annuity_inf(t[i], interest_rate, inflation_rate) + (claim_expense*(Whole_life_annuity_inf(t[i], interest_rate, inflation_rate))) - prem*(Whole_life_annuity_inf(t[i], interest_rate, inflation_rate)-1)
  }
  plot(t,vector)
}

Term_annuity_reserve_single_level = function(age, interest_rate, claim_expense, bonus_rate, term, inflation_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Term_annuity_inf(t[i], interest_rate, term, inflation_rate) + (claim_expense*(Term_annuity_inf(t[i], interest_rate, term, inflation_rate))) - prem*(Term_annuity_inf_due(t[i], interest_rate, term, inflation_rate))
  }
  plot(t,vector)
}

Whole_life_assurance_reserve_joint_level = function(agex, agey, interest_rate, claim_expense, bonus_rate, prem){
  vector = c()
  t1 = max(agex,agey):100
  t2 = min(agex,agey):(100-(max(agex,agey)-min(agex,agey)))
  for (i in 1:length(t)){
    vector[i] = Whole_joint_life(t1[i], t2[i], interest_rate) + bonus_whole(interest_rate, t1[i], bonus_rate) + (claim_expense*(Whole_joint_life(t1[i], t2[i], interest_rate) + bonus_whole(interest_rate, t1[i], bonus_rate))) - prem*(Whole_life_annuity(t1[i], interest_rate)-1)
  }
  plot(t,vector)
}

Whole_life_assurance_reserve_joint_level_ = function(agex, agey, interest_rate, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = Whole_joint_life_(t1[i], t2[i], interest_rate) + bonus_whole(interest_rate, t1[i], bonus_rate) + (claim_expense*(Whole_joint_life_(t1[i], t2[i], interest_rate) + bonus_whole(interest_rate, t1[i], bonus_rate))) - prem*(Whole_life_annuity(t1[i], interest_rate)-1)
  }
  plot(t,vector)
}

Term_assurance_reserve_joint_level = function(agex, agey, interest_rate, term, claim_expense, bonus_rate, prem){
  vector = c()
  t1 = max(agex,agey):100
  t2 = min(agex,agey):(100-(max(agex,agey)-min(agex,agey)))
  for (i in 1:length(t)){
    vector[i] = Term_joint_assurance(t1[i], t2[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term) + (claim_expense*(Term_joint_assurance(t1[i], t2[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term))) - prem*(Term_annuity_due(t1[i], interest_rate, term))
  }
  plot(t,vector)
}

Term_assurance_reserve_joint_level_ = function(agex, agey, interest_rate, term, claim_expense, bonus_rate, prem){
  vector = c()
  t1 = max(agex,agey):100
  t2 = min(agex,agey):(100-(max(agex,agey)-min(agex,agey)))
  for (i in 1:length(t)){
    vector[i] = Term_joint_assurance_(t1[i], t2[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term) + (claim_expense*(Term_joint_assurance_(t1[i], t2[i], interest_rate, term) + bonus_term(interest_rate, bonus_rate, term))) - prem*(Term_annuity_due(t1[i], interest_rate, term))
  }
  plot(t,vector)
}

Whole_life_annuity_reserve_joint = function(agex, agey, interest_rate, claim_expense, bonus_rate, inflation_rate, prem){
  vector = c()
  t1 = max(agex,agey):100
  t2 = min(agex,agey):(100-(max(agex,agey)-min(agex,agey)))
  for (i in 1:length(t)){
    vector[i] = Whole_joint_life_annuity_inf(t1[i], t2[i], interest_rate, inflation_rate) + (claim_expense*(Whole_joint_life_annuity_inf(t1[i], t2[i], interest_rate, inflation_rate))) - prem*(Whole_life_annuity_inf(t1[i], interest_rate, inflation_rate)-1)
  }
  plot(t,vector)
}

Term_annuity_reserve_joint = function(agex, agey, interest_rate, term, claim_expense, bonus_rate, inflation_rate, prem){
  vector = c()
  t1 = max(agex,agey):100
  t2 = min(agex,agey):(100-(max(agex,agey)-min(agex,agey)))
  for (i in 1:length(t)){
    vector[i] = Term_joint_annuity_inf(t1[i], t2[i], interest_rate, term, inflation_rate) + (claim_expense*(Term_joint_annuity_inf(t1[i], t2[i], interest_rate, term, inflation_rate))) - prem*(Term_annuity_inf_due(t1[i], interest_rate, term, inflation_rate))
  }
  plot(t,vector)
}



#----------------------------------------

yWhole_life_assurance_reserve_single = function(age, interest_rate, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yWhole_life_assurance(t[i], interest_rate) + ybonus_whole(interest_rate, t[i], bonus_rate) + (claim_expense*(yWhole_life_assurance(t[i], interest_rate) + ybonus_whole(interest_rate, t[i], bonus_rate)))
  }
  plot(t,vector)
}

yWhole_life_assurance_reserve_single_ = function(age, interest_rate, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yWhole_life_assurance_(t[i], interest_rate) + ybonus_whole(interest_rate, t[i], bonus_rate) + (claim_expense*(yWhole_life_assurance_(t[i], interest_rate) + ybonus_whole(interest_rate, t[i], bonus_rate)))
  }
  plot(t,vector)
}

yTerm_assurance_reserve_single = function(age, interest_rate, term, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yTerm_assurance(t[i], interest_rate, term) + ybonus_term(interest_rate, bonus_rate, term) + (claim_expense*(yTerm_assurance(t[i], interest_rate, term) + ybonus_term(interest_rate, bonus_rate, term)))
  }
  plot(t,vector)
}

yTerm_assurance_reserve_single_ = function(age, interest_rate, term, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yTerm_assurance_(t[i], interest_rate, term) + ybonus_term(interest_rate, bonus_rate, term) + (claim_expense*(yTerm_assurance_(t[i], interest_rate, term) + ybonus_term(interest_rate, bonus_rate, term)))
  }
  plot(t,vector)
}

yPure_endowment_reserve = function(age, interest_rate, term, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yPure_endowment(t[i], interest_rate, term) + (claim_expense*(yPure_endowment(t[i], interest_rate, term)))
  }
  plot(t,vector)
}

yEndowment_reserve = function(age, interest_rate, term, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = ybonus_endowment(t[i], interest_rate, bonus_rate, term) + (claim_expense*(ybonus_endowment(t[i], interest_rate, bonus_rate, term)))
  }
  plot(t,vector)
}

yEndowment_reserve_ = function(age, interest_rate, term, claim_expense, bonus_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = ybonus_endowment_(t[i], interest_rate, bonus_rate, term) + (claim_expense*(ybonus_endowment_(t[i], interest_rate, bonus_rate, term)))
  }
  plot(t,vector)
}

yWhole_life_annuity_reserve_single = function(age, interest_rate, claim_expense, bonus_rate, inflation_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yWhole_life_annuity_inf(t[i], interest_rate, inflation_rate) + (claim_expense*(yWhole_life_annuity_inf(t[i], interest_rate, inflation_rate)))
  }
  plot(t,vector)
}

yTerm_annuity_reserve_single = function(age, interest_rate, claim_expense, bonus_rate, term, inflation_rate){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yTerm_annuity_inf(t[i], interest_rate, term, inflation_rate) + (claim_expense*(yTerm_annuity_inf(t[i], interest_rate, term, inflation_rate)))
  }
  plot(t,vector)
}

#----------------------------------------------------------------------

yWhole_life_assurance_reserve_single_level = function(age, interest_rate, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yWhole_life_assurance(t[i], interest_rate) + ybonus_whole(interest_rate, t[i], bonus_rate) + (claim_expense*(yWhole_life_assurance(t[i], interest_rate) + ybonus_whole(interest_rate, t[i], bonus_rate))) - prem*(yWhole_life_annuity(t[i], interest_rate)-1)
  }
  plot(t,vector)
}

yWhole_life_assurance_reserve_single_level_ = function(age, interest_rate, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yWhole_life_assurance_(t[i], interest_rate) + ybonus_whole(interest_rate, t[i], bonus_rate) + (claim_expense*(yWhole_life_assurance_(t[i], interest_rate) + ybonus_whole(interest_rate, t[i], bonus_rate))) - prem*(yWhole_life_annuity(t[i], interest_rate)-1)
  }
  plot(t,vector)
}

yTerm_assurance_reserve_single_level = function(age, interest_rate, term, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yTerm_assurance(t[i], interest_rate, term) + ybonus_term(interest_rate, bonus_rate, term) + (claim_expense*(yTerm_assurance(t[i], interest_rate, term) + ybonus_term(interest_rate, bonus_rate, term))) - prem*(yTerm_annuity_due(t[i], interest_rate, term))
  }
  plot(t,vector)
}

yTerm_assurance_reserve_single_level_ = function(age, interest_rate, term, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yTerm_assurance_(t[i], interest_rate, term) + ybonus_term(interest_rate, bonus_rate, term) + (claim_expense*(yTerm_assurance_(t[i], interest_rate, term) + ybonus_term(interest_rate, bonus_rate, term))) - prem*(yTerm_annuity_due(t[i], interest_rate, term))
  }
  plot(t,vector)
}

yPure_endowment_reserve_level = function(age, interest_rate, term, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yPure_endowment(t[i], interest_rate, term) + (claim_expense*(yPure_endowment(t[i], interest_rate, term))) - prem*(yTerm_annuity_due(t[i], interest_rate, term))
  }
  plot(t,vector)
}

yEndowment_reserve_level = function(age, interest_rate, term, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = ybonus_endowment(t[i], interest_rate, bonus_rate, term) + (claim_expense*(ybonus_endowment(t[i], interest_rate, bonus_rate, term))) - prem*(yTerm_annuity_due(t[i], interest_rate, term))
  }
  plot(t,vector)
}

yEndowment_reserve_level_ = function(age, interest_rate, term, claim_expense, bonus_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = ybonus_endowment_(t[i], interest_rate, bonus_rate, term) + (claim_expense*(ybonus_endowment_(t[i], interest_rate, bonus_rate, term))) - prem*(yTerm_annuity_due(t[i], interest_rate, term))
  }
  plot(t,vector)
}

yWhole_life_annuity_reserve_single_level = function(age, interest_rate, claim_expense, bonus_rate, inflation_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yWhole_life_annuity_inf(t[i], interest_rate, inflation_rate) + (claim_expense*(yWhole_life_annuity_inf(t[i], interest_rate, inflation_rate))) - prem*(yWhole_life_annuity_inf(t[i], interest_rate, inflation_rate)-1)
  }
  plot(t,vector)
}

yTerm_annuity_reserve_single_level = function(age, interest_rate, claim_expense, bonus_rate, term, inflation_rate, prem){
  vector = c()
  t = age:100
  for (i in 1:length(t)){
    vector[i] = yTerm_annuity_inf(t[i], interest_rate, term, inflation_rate) + (claim_expense*(yTerm_annuity_inf(t[i], interest_rate, term, inflation_rate))) - prem*(yTerm_annuity_inf_due(t[i], interest_rate, term, inflation_rate))
  }
  plot(t,vector)
}



#---------------------------------------------------------
product_tabs <- tabsetPanel(
  id = "product",
  type = "hidden",
  tabPanel("Whole Life Assurance",
           selectInput("timing",
                       "Payment Timing",
                       choices = c("End of Year of Death", "Immediately"))
  ),
  tabPanel("Term Assurance",
           selectInput("timing",
                       "Payment Timing",
                       choices = c("End of Year of Death", "Immediately")),
           numericInput("n",
                       "Select Term",
                       value = 10,
                       min = 1,
                       )
  ),
  tabPanel("Pure Endowment",
           numericInput("n",
                       "Select Term",
                       value = 10,
                       min = 1,
                       )
  ),
  tabPanel("Endowment",
           selectInput("timing",
                       "Payment Timing",
                       choices = c("End of Year of Death", "Immediately")),
           numericInput("n",
                       "Select Term",
                       value = 10,
                       min = 1,
                       )
  ),
  tabPanel("Whole Life Annuity"
  ),
  tabPanel("Term Annuity",
           numericInput("n",
                       "Select Term",
                       value = 10,
                       min = 1,
                       )
  )
)

policy_life <- tabsetPanel(
  id = "policy_life",
  type = "hidden",
  tabPanel("Single Life",
           sliderInput("age",
                       "Policyholder Age",
                       value = 30,
                       min = 20,
                       max = 100,
                       step = 1),
           radioButtons("table",
                        "Policy Life",
                        choices = c("Life X" ,"Life Y"))
  ),
  tabPanel("Joint Life",
           sliderInput("agex",
                       "Policyholder Age (X)",
                       value = 30,
                       min = 20,
                       max = 100,
                       step = 1),
           sliderInput("agey",
                       "Policyholder Age (Y)",
                       value = 30,
                       min = 20,
                       max = 100,
                       step = 1),
  )
)

#Header------------------
header <- dashboardHeader(title = "Insurance Pricing")

#Sidebar-----------------
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Tools", tabName = "tools")
              )
)

#Body-------------------
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tools",
            fluidRow(
              column(width = 6,
                     tabBox(width = NULL,
                            tabPanel(
                              "Personal Information",
                              selectInput("personal", "Policy Life",
                                          choices = c("Single Life", "Joint Life")),
                              policy_life,
                            ),
                            tabPanel(
                              "Insurance Product",
                              selectInput("type", "Product Type",
                                          choices = c("Whole Life Assurance", "Term Assurance",
                                                      "Pure Endowment", "Endowment", "Whole Life Annuity",
                                                      "Term Annuity")),
                              product_tabs,
                              br(),
                              numericInput("s",
                                           "Sum Assured",
                                           value = 10000,
                                           min = 1,
                                           max = NA),
                              br(),
                              numericInput("ir",
                                           "Interest Rate (0-50%)",
                                           value = 4,
                                           min = 0,
                                           max = 50),
                              br(),
                              radioButtons("payment",
                                           "Payment Type",
                                           c("Single" = "single",
                                             "Level" = "level"))
                            ),
                            tabPanel(
                              "Expenses",
                              sliderInput("initial_expense",
                                          "Initial expenses in % of the gross premium",
                                          value = 0,
                                          max = 50,
                                          min = 0),
                              sliderInput("premium_expense",
                                          "Premium expenses in % of the gross premium",
                                          value = 0,
                                          max = 50,
                                          min = 0),
                              sliderInput("claim_expense",
                                          "Claim expenses in % of the sum assured",
                                          value = 0,
                                          max = 50,
                                          min = 0)
                            ),
                            tabPanel(
                              "Extra",
                              sliderInput("bonus",
                                          "Simple Bonus Vesting in % (applicable for Assurance)",
                                          value = 0,
                                          max = 50,
                                          min = 0),
                              sliderInput("inflation",
                                          "Inflation in % (applicable for Annuity)",
                                          value = 0,
                                          max = 50,
                                          min = 0)
                            )
                      )),
              column(width = 6,
                     box(title = "Reserve", width = NULL, plotOutput("reserve")))
            ),
            fluidRow(
              column(width = 3,
                     box(title = "Premium", width = NULL, h1(textOutput("premium"))))
            ))
  )
)



#Server---------------------
server <- function(input, output, session){
  observeEvent(input$personal,{
    updateTabsetPanel(session, inputId = "policy_life", selected = input$personal)
  })
  
  observeEvent(input$type, {
    updateTabsetPanel(session, inputId = "product", selected = input$type)
  })
  
  
  
  output$premium = renderText({
    
    age = input$age
    agex = input$agex
    agey = input$agey
    interest_rate = input$ir/100
    term = input$n
    bonus_rate = input$bonus/100
    inflation_rate = input$inflation/100
    initial_expense = input$initial_expense/100
    premium_expense = input$premium_expense/100
    claim_expense = input$claim_expense/100
    
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Whole Life Assurance")
      (EPVB = Whole_life_assurance(age, interest_rate) + bonus_whole(interest_rate, age, bonus_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Whole Life Assurance")
      (EPVB = Whole_life_assurance_(age, interest_rate) + bonus_whole(interest_rate, age, bonus_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Term Assurance")
      (EPVB = Term_assurance(age, interest_rate, term) + bonus_term(interest_rate, bonus_rate, term))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Term Assurance")
      (EPVB = Term_assurance_(age, interest_rate, term) + bonus_term(interest_rate, bonus_rate, term))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$type == "Pure Endowment")
      (EPVB = Pure_endowment(age, interest_rate, term))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Endowment")
      (EPVB = bonus_endowment(age, interest_rate, bonus_rate, term))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Endowment")
      (EPVB = bonus_endowment_(age, interest_rate, bonus_rate, term))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$type == "Whole Life Annuity")
      (EPVB = Whole_life_annuity_inf(age, interest_rate, inflation_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$type == "Term Annuity")
      (EPVB = Term_annuity_inf(age, interest_rate, term, inflation_rate))
    
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Whole Life Assurance")
      (EPVB = yWhole_life_assurance(age, interest_rate) + ybonus_whole(interest_rate, age, bonus_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Whole Life Assurance")
      (EPVB = yWhole_life_assurance_(age, interest_rate) + ybonus_whole(interest_rate, age, bonus_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Term Assurance")
      (EPVB = yTerm_assurance(age, interest_rate, term) + ybonus_term(interest_rate, bonus_rate, term))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Term Assurance")
      (EPVB = yTerm_assurance_(age, interest_rate, term) + ybonus_term(interest_rate, bonus_rate, term))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$type == "Pure Endowment")
      (EPVB = yPure_endowment(age, interest_rate, term))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Endowment")
      (EPVB = ybonus_endowment(age, interest_rate, bonus_rate, term))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Endowment")
      (EPVB = ybonus_endowment_(age, interest_rate, bonus_rate, term))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$type == "Whole Life Annuity")
      (EPVB = yWhole_life_annuity_inf(age, interest_rate, inflation_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$type == "Term Annuity")
      (EPVB = yTerm_annuity_inf(age, interest_rate, term, inflation_rate))
    
    if (input$policy_life == "Joint Life" & input$timing == "End of Year of Death" & input$type == "Whole Life Assurance")
      (EPVB = Whole_joint_life(agex, agey, interest_rate) + bonus_whole(interest_rate, max(agex,agey), bonus_rate))
    if (input$policy_life == "Joint Life" & input$timing == "Immediately" & input$type == "Whole Life Assurance")
      (EPVB = Whole_joint_life_(agex, agey, interest_rate) + bonus_whole(interest_rate, max(agex,agey), bonus_rate))
    if (input$policy_life == "Joint Life" & input$timing == "End of Year of Death" & input$type == "Term Assurance")
      (EPVB = Term_joint_assurance(agex, agey, interest_rate, term) + bonus_term(interest_rate, bonus_rate, term))
    if (input$policy_life == "Joint Life" & input$timing == "Immediately" & input$type == "Term Assurance")
      (EPVB = Term_joint_assurance_(agex, agey, interest_rate, term) + bonus_term(interest_rate, bonus_rate, term))
    if (input$policy_life == "Joint Life" & input$type == "Whole Life Annuity")
      (EPVB = Whole_joint_life_annuity_inf(agex, agey, interest_rate, inflation_rate))
    if (input$policy_life == "Joint Life" & input$type == "Term Annuity")
      (EPVB = Term_joint_annuity_inf(agex, agey, interest_rate, term, inflation_rate))
    
    
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$payment == "single" & (input$type == "Whole Life Assurance" || input$type == "Whole Life Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/(1-(initial_expense+(premium_expense*Whole_life_annuity_inf(age, interest_rate, inflation_rate)))))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$payment == "single" & (input$type == "Term Assurance" || input$type == "Pure Endowment" || input$type == "Endowment" || input$type == "Term Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/(1-(initial_expense+(premium_expense*Term_annuity_inf_due(age, interest_rate, term, inflation_rate)))))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$payment == "level" & (input$type == "Whole Life Assurance" || input$type == "Whole Life Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/((Whole_life_annuity_inf(age, interest_rate, inflation_rate))-(initial_expense+(premium_expense*Whole_life_annuity_inf(age, interest_rate, inflation_rate)))))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$payment == "level" & (input$type == "Term Assurance" || input$type == "Pure Endowment" || input$type == "Endowment" || input$type == "Term Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/((Term_annuity_inf_due(age, interest_rate, term, inflation_rate))-(initial_expense+(premium_expense*Term_annuity_inf_due(age, interest_rate, term, inflation_rate)))))
    
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$payment == "single" & (input$type == "Whole Life Assurance" || input$type == "Whole Life Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/(1-(initial_expense+(premium_expense*yWhole_life_annuity_inf(age, interest_rate, inflation_rate)))))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$payment == "single" & (input$type == "Term Assurance" || input$type == "Pure Endowment" || input$type == "Endowment" || input$type == "Term Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/(1-(initial_expense+(premium_expense*yTerm_annuity_inf_due(age, interest_rate, term, inflation_rate)))))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$payment == "level" & (input$type == "Whole Life Assurance" || input$type == "Whole Life Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/((yWhole_life_annuity_inf(age, interest_rate, inflation_rate))-(initial_expense+(premium_expense*yWhole_life_annuity_inf(age, interest_rate, inflation_rate)))))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$payment == "level" & (input$type == "Term Assurance" || input$type == "Pure Endowment" || input$type == "Endowment" || input$type == "Term Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/((yTerm_annuity_inf_due(age, interest_rate, term, inflation_rate))-(initial_expense+(premium_expense*yTerm_annuity_inf_due(age, interest_rate, term, inflation_rate)))))
    
    
    if (input$policy_life == "Joint Life" & input$payment == "single" & (input$type == "Whole Life Assurance" || input$type == "Whole Life Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/(1-(initial_expense+(premium_expense*Whole_life_annuity_inf(max(agex, agey), interest_rate, inflation_rate)))))
    if (input$policy_life == "Joint Life" & input$payment == "single" & (input$type == "Term Assurance" || input$type == "Pure Endowment" || input$type == "Endowment" || input$type == "Term Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/(1-(initial_expense+(premium_expense*Term_annuity_inf_due(max(agex, agey), interest_rate, term, inflation_rate)))))
    if (input$policy_life == "Joint Life" & input$payment == "level" & (input$type == "Whole Life Assurance" || input$type == "Whole Life Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/((Whole_life_annuity_inf(max(agex,agey), interest_rate, inflation_rate))-(initial_expense+(premium_expense*Whole_life_annuity_inf(max(agex, agey), interest_rate, inflation_rate)))))
    if (input$policy_life == "Joint Life" & input$payment == "level" & (input$type == "Term Assurance" || input$type == "Pure Endowment" || input$type == "Endowment" || input$type == "Term Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/((Term_annuity_inf_due(max(agex,agey), interest_rate, term, inflation_rate))-(initial_expense+(premium_expense*Term_annuity_inf_due(max(agex, agey), interest_rate, term, inflation_rate)))))
    
    return(prem)
  })
  
  
  output$reserve = renderPlot({
    
    age = input$age
    agex = input$agex
    agey = input$agey
    interest_rate = input$ir/100
    term = input$n
    bonus_rate = input$bonus/100
    inflation_rate = input$inflation/100
    initial_expense = input$initial_expense/100
    premium_expense = input$premium_expense/100
    claim_expense = input$claim_expense/100
   
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Whole Life Assurance")
      (EPVB = Whole_life_assurance(age, interest_rate) + bonus_whole(interest_rate, age, bonus_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Whole Life Assurance")
      (EPVB = Whole_life_assurance_(age, interest_rate) + bonus_whole(interest_rate, age, bonus_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Term Assurance")
      (EPVB = Term_assurance(age, interest_rate, term) + bonus_term(interest_rate, bonus_rate, term))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Term Assurance")
      (EPVB = Term_assurance_(age, interest_rate, term) + bonus_term(interest_rate, bonus_rate, term))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$type == "Pure Endowment")
      (EPVB = Pure_endowment(age, interest_rate, term))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Endowment")
      (EPVB = bonus_endowment(age, interest_rate, bonus_rate, term))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Endowment")
      (EPVB = bonus_endowment_(age, interest_rate, bonus_rate, term))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$type == "Whole Life Annuity")
      (EPVB = Whole_life_annuity_inf(age, interest_rate, inflation_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$type == "Term Annuity")
      (EPVB = Term_annuity_inf(age, interest_rate, term, inflation_rate))
    
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Whole Life Assurance")
      (EPVB = yWhole_life_assurance(age, interest_rate) + ybonus_whole(interest_rate, age, bonus_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Whole Life Assurance")
      (EPVB = yWhole_life_assurance_(age, interest_rate) + ybonus_whole(interest_rate, age, bonus_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Term Assurance")
      (EPVB = yTerm_assurance(age, interest_rate, term) + ybonus_term(interest_rate, bonus_rate, term))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Term Assurance")
      (EPVB = yTerm_assurance_(age, interest_rate, term) + ybonus_term(interest_rate, bonus_rate, term))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$type == "Pure Endowment")
      (EPVB = yPure_endowment(age, interest_rate, term))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Endowment")
      (EPVB = ybonus_endowment(age, interest_rate, bonus_rate, term))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Endowment")
      (EPVB = ybonus_endowment_(age, interest_rate, bonus_rate, term))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$type == "Whole Life Annuity")
      (EPVB = yWhole_life_annuity_inf(age, interest_rate, inflation_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$type == "Term Annuity")
      (EPVB = yTerm_annuity_inf(age, interest_rate, term, inflation_rate))
    
    if (input$policy_life == "Joint Life" & input$timing == "End of Year of Death" & input$type == "Whole Life Assurance")
      (EPVB = Whole_joint_life(agex, agey, interest_rate) + bonus_whole(interest_rate, max(agex,agey), bonus_rate))
    if (input$policy_life == "Joint Life" & input$timing == "Immediately" & input$type == "Whole Life Assurance")
      (EPVB = Whole_joint_life_(agex, agey, interest_rate) + bonus_whole(interest_rate, max(agex,agey), bonus_rate))
    if (input$policy_life == "Joint Life" & input$timing == "End of Year of Death" & input$type == "Term Assurance")
      (EPVB = Term_joint_assurance(agex, agey, interest_rate, term) + bonus_term(interest_rate, bonus_rate, term))
    if (input$policy_life == "Joint Life" & input$timing == "Immediately" & input$type == "Term Assurance")
      (EPVB = Term_joint_assurance_(agex, agey, interest_rate, term) + bonus_term(interest_rate, bonus_rate, term))
    if (input$policy_life == "Joint Life" & input$type == "Whole Life Annuity")
      (EPVB = Whole_joint_life_annuity_inf(agex, agey, interest_rate, inflation_rate))
    if (input$policy_life == "Joint Life" & input$type == "Term Annuity")
      (EPVB = Term_joint_annuity_inf(agex, agey, interest_rate, term, inflation_rate))
    
    
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$payment == "single" & (input$type == "Whole Life Assurance" || input$type == "Whole Life Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/(1-(initial_expense+(premium_expense*Whole_life_annuity_inf(age, interest_rate, inflation_rate)))))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$payment == "single" & (input$type == "Term Assurance" || input$type == "Pure Endowment" || input$type == "Endowment" || input$type == "Term Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/(1-(initial_expense+(premium_expense*Term_annuity_inf_due(age, interest_rate, term, inflation_rate)))))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$payment == "level" & (input$type == "Whole Life Assurance" || input$type == "Whole Life Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/((Whole_life_annuity_inf(age, interest_rate, inflation_rate))-(initial_expense+(premium_expense*Whole_life_annuity_inf(age, interest_rate, inflation_rate)))))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$payment == "level" & (input$type == "Term Assurance" || input$type == "Pure Endowment" || input$type == "Endowment" || input$type == "Term Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/((Term_annuity_inf_due(age, interest_rate, term, inflation_rate))-(initial_expense+(premium_expense*Term_annuity_inf_due(age, interest_rate, term, inflation_rate)))))
    
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$payment == "single" & (input$type == "Whole Life Assurance" || input$type == "Whole Life Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/(1-(initial_expense+(premium_expense*yWhole_life_annuity_inf(age, interest_rate, inflation_rate)))))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$payment == "single" & (input$type == "Term Assurance" || input$type == "Pure Endowment" || input$type == "Endowment" || input$type == "Term Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/(1-(initial_expense+(premium_expense*yTerm_annuity_inf_due(age, interest_rate, term, inflation_rate)))))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$payment == "level" & (input$type == "Whole Life Assurance" || input$type == "Whole Life Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/((yWhole_life_annuity_inf(age, interest_rate, inflation_rate))-(initial_expense+(premium_expense*yWhole_life_annuity_inf(age, interest_rate, inflation_rate)))))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$payment == "level" & (input$type == "Term Assurance" || input$type == "Pure Endowment" || input$type == "Endowment" || input$type == "Term Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/((yTerm_annuity_inf_due(age, interest_rate, term, inflation_rate))-(initial_expense+(premium_expense*yTerm_annuity_inf_due(age, interest_rate, term, inflation_rate)))))
    
    
    if (input$policy_life == "Joint Life" & input$payment == "single" & (input$type == "Whole Life Assurance" || input$type == "Whole Life Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/(1-(initial_expense+(premium_expense*Whole_life_annuity_inf(max(agex, agey), interest_rate, inflation_rate)))))
    if (input$policy_life == "Joint Life" & input$payment == "single" & (input$type == "Term Assurance" || input$type == "Pure Endowment" || input$type == "Endowment" || input$type == "Term Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/(1-(initial_expense+(premium_expense*Term_annuity_inf_due(max(agex, agey), interest_rate, term, inflation_rate)))))
    if (input$policy_life == "Joint Life" & input$payment == "level" & (input$type == "Whole Life Assurance" || input$type == "Whole Life Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/((Whole_life_annuity_inf(max(agex,agey), interest_rate, inflation_rate))-(initial_expense+(premium_expense*Whole_life_annuity_inf(max(agex, agey), interest_rate, inflation_rate)))))
    if (input$policy_life == "Joint Life" & input$payment == "level" & (input$type == "Term Assurance" || input$type == "Pure Endowment" || input$type == "Endowment" || input$type == "Term Annuity"))
      (prem = ((input$s*EPVB)*(1+claim_expense))/((Term_annuity_inf_due(max(agex,agey), interest_rate, term, inflation_rate))-(initial_expense+(premium_expense*Term_annuity_inf_due(max(agex, agey), interest_rate, term, inflation_rate)))))
    
  
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Whole Life Assurance")
      (Whole_life_assurance_reserve_single(age, interest_rate, claim_expense, bonus_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Whole Life Assurance")
      (Whole_life_assurance_reserve_single_(age, interest_rate, claim_expense, bonus_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Term Assurance")
      (Term_assurance_reserve_single(age, interest_rate, term, claim_expense, bonus_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Term Assurance")
      (Term_assurance_reserve_single_(age, interest_rate, term, claim_expense, bonus_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$type == "Pure Endowment")
      (Pure_endowment_reserve(age, interest_rate, term, claim_expense, bonus_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Endowment")
      (Endowment_reserve(age, interest_rate, term, claim_expense, bonus_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Endowment")
      (Endowment_reserve_(age, interest_rate, term, claim_expense, bonus_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$type == "Whole Life Annuity")
      (Whole_life_annuity_reserve_single(age, interest_rate, claim_expense, bonus_rate, inflation_rate))
    if (input$table == "Life X" & input$policy_life == "Single Life" & input$type == "Term Annuity")
      (Term_annuity_reserve_single(age, interest_rate, claim_expense, bonus_rate, term, inflation_rate))
    
    
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Whole Life Assurance")
      (yWhole_life_assurance_reserve_single(age, interest_rate, claim_expense, bonus_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Whole Life Assurance")
      (yWhole_life_assurance_reserve_single_(age, interest_rate, claim_expense, bonus_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Term Assurance")
      (yTerm_assurance_reserve_single(age, interest_rate, term, claim_expense, bonus_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Term Assurance")
      (yTerm_assurance_reserve_single_(age, interest_rate, term, claim_expense, bonus_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$type == "Pure Endowment")
      (yPure_endowment_reserve(age, interest_rate, term, claim_expense, bonus_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Endowment")
      (yEndowment_reserve(age, interest_rate, term, claim_expense, bonus_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Endowment")
      (yEndowment_reserve_(age, interest_rate, term, claim_expense, bonus_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$type == "Whole Life Annuity")
      (yWhole_life_annuity_reserve_single(age, interest_rate, claim_expense, bonus_rate, inflation_rate))
    if (input$table == "Life Y" & input$policy_life == "Single Life" & input$type == "Term Annuity")
      (yTerm_annuity_reserve_single(age, interest_rate, claim_expense, bonus_rate, term, inflation_rate))
    
    
    if (input$policy_life == "Joint Life" & input$timing == "End of Year of Death" & input$type == "Whole Life Assurance")
      (Whole_life_annuity_reserve_joint(agex, agey, interest_rate, claim_expense, bonus_rate))
    if (input$policy_life == "Joint Life" & input$timing == "Immediately" & input$type == "Whole Life Assurance")
      (Whole_life_annuity_reserve_joint_(agex, agey, interest_rate, claim_expense, bonus_rate))
    if (input$policy_life == "Joint Life" & input$timing == "End of Year of Death" & input$type == "Term Assurance")
      (Term_assurance_reserve_joint(agex, agey, interest_rate, term, claim_expense, bonus_rate))
    if (input$policy_life == "Joint Life" & input$timing == "Immediately" & input$type == "Term Assurance")
      (Term_assurance_reserve_joint_(agex, agey, interest_rate, term, claim_expense, bonus_rate))
    if (input$policy_life == "Joint Life" & input$type == "Whole Life Annuity")
      (Whole_life_annuity_reserve_joint(agex, agey, interest_rate, claim_expense, bonus_rate, inflation_rate))
    if (input$policy_life == "Joint Life" & input$type == "Term Annuity")
      (Term_annuity_reserve_joint(agex, agey, interest_rate, term, claim_expense, bonus_rate, inflation_rate))
 
       
    if (input$table == "Life X" & input$payment == "level" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Whole Life Assurance")
      (Whole_life_assurance_reserve_single_level(age, interest_rate, claim_expense, bonus_rate, prem))
    if (input$table == "Life X" & input$payment == "level" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Whole Life Assurance")
      (Whole_life_assurance_reserve_single_level_(age, interest_rate, claim_expense, bonus_rate, prem))
    if (input$table == "Life X" & input$payment == "level" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Term Assurance")
      (Term_assurance_reserve_single_level(age, interest_rate, term, claim_expense, bonus_rate, prem))
    if (input$table == "Life X" & input$payment == "level" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Term Assurance")
      (Term_assurance_reserve_single_level_(age, interest_rate, term, claim_expense, bonus_rate, prem))
    if (input$table == "Life X" & input$payment == "level" & input$policy_life == "Single Life" & input$type == "Pure Endowment")
      (Pure_endowment_reserve_level(age, interest_rate, term, claim_expense, bonus_rate, prem))
    if (input$table == "Life X" & input$payment == "level" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Endowment")
      (Endowment_reserve_level(age, interest_rate, term, claim_expense, bonus_rate, prem))
    if (input$table == "Life X" & input$payment == "level" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Endowment")
      (Endowment_reserve_level_(age, interest_rate, term, claim_expense, bonus_rate, prem))
    if (input$table == "Life X" & input$payment == "level" & input$policy_life == "Single Life" & input$type == "Whole Life Annuity")
      (Whole_life_annuity_reserve_single_level(age, interest_rate, claim_expense, bonus_rate, inflation_rate, prem))
    if (input$table == "Life X" & input$payment == "level" & input$policy_life == "Single Life" & input$type == "Term Annuity")
      (Term_annuity_reserve_single_level(age, interest_rate, claim_expense, bonus_rate, term, inflation_rate, prem))
    
    
    if (input$table == "Life Y" & input$payment == "level" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Whole Life Assurance")
      (yWhole_life_assurance_reserve_single_level(age, interest_rate, claim_expense, bonus_rate, prem))
    if (input$table == "Life Y" & input$payment == "level" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Whole Life Assurance")
      (yWhole_life_assurance_reserve_single_level_(age, interest_rate, claim_expense, bonus_rate, prem))
    if (input$table == "Life Y" & input$payment == "level" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Term Assurance")
      (yTerm_assurance_reserve_single_level(age, interest_rate, term, claim_expense, bonus_rate, prem))
    if (input$table == "Life Y" & input$payment == "level" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Term Assurance")
      (yTerm_assurance_reserve_single_level_(age, interest_rate, term, claim_expense, bonus_rate, prem))
    if (input$table == "Life Y" & input$payment == "level" & input$policy_life == "Single Life" & input$type == "Pure Endowment")
      (yPure_endowment_reserve_level(age, interest_rate, term, claim_expense, bonus_rate, prem))
    if (input$table == "Life Y" & input$payment == "level" & input$policy_life == "Single Life" & input$timing == "End of Year of Death" & input$type == "Endowment")
      (yEndowment_reserve_level(age, interest_rate, term, claim_expense, bonus_rate, prem))
    if (input$table == "Life Y" & input$payment == "level" & input$policy_life == "Single Life" & input$timing == "Immediately" & input$type == "Endowment")
      (yEndowment_reserve_level_(age, interest_rate, term, claim_expense, bonus_rate, prem))
    if (input$table == "Life Y" & input$payment == "level" & input$policy_life == "Single Life" & input$type == "Whole Life Annuity")
      (yWhole_life_annuity_reserve_single_level(age, interest_rate, claim_expense, bonus_rate, inflation_rate, prem))
    if (input$table == "Life Y" & input$payment == "level" & input$policy_life == "Single Life" & input$type == "Term Annuity")
      (yTerm_annuity_reserve_single_level(age, interest_rate, claim_expense, bonus_rate, term, inflation_rate, prem))
    
    
    if (input$payment == "level" & input$policy_life == "Joint Life" & input$timing == "End of Year of Death" & input$type == "Whole Life Assurance")
      (Whole_life_annuity_reserve_joint_level(agex, agey, interest_rate, claim_expense, bonus_rate, prem))
    if (input$payment == "level" & input$policy_life == "Joint Life" & input$timing == "Immediately" & input$type == "Whole Life Assurance")
      (Whole_life_annuity_reserve_joint_level_(agex, agey, interest_rate, claim_expense, bonus_rate, prem))
    if (input$payment == "level" & input$policy_life == "Joint Life" & input$timing == "End of Year of Death" & input$type == "Term Assurance")
      (Term_assurance_reserve_joint_level(agex, agey, interest_rate, term, claim_expense, bonus_rate, prem))
    if (input$payment == "level" & input$policy_life == "Joint Life" & input$timing == "Immediately" & input$type == "Term Assurance")
      (Term_assurance_reserve_joint_level_(agex, agey, interest_rate, term, claim_expense, bonus_rate, prem))
    if (input$payment == "level" & input$policy_life == "Joint Life" & input$type == "Whole Life Annuity")
      (Whole_life_annuity_reserve_joint_level(agex, agey, interest_rate, claim_expense, bonus_rate, inflation_rate, prem))
    if (input$payment == "level" & input$policy_life == "Joint Life" & input$type == "Term Annuity")
      (Term_annuity_reserve_joint_level(agex, agey, interest_rate, term, claim_expense, bonus_rate, inflation_rate, prem))
    
  })
}


#-------------------------------------
ui <- dashboardPage(
  header,
  sidebar,
  body
)


shinyApp(ui = ui, server = server)

