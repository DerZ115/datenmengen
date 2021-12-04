#set.seed
set.seed(123)


#variant from tab1
alpha <- replicate(34656,"alpha")
delta <- replicate(8682,"delta")
variant <- sample(c(alpha, delta),replace = TRUE)

data <- data.frame(
  variant = c(alpha, delta),replace = TRUE)

#symptom status
asymptomatic_alpha <- replicate(14934,"asymptomatic")
symptomatic_alpha <- replicate(17757,"symptomatic")
unknown_alpha <- replicate(1965,"unknown")
asymptomatic_delta <- replicate(3659,"asymptomatic")
symptomatic_delta <- replicate(4334,"symptomatic")
unknown_delta <- replicate(689,"unknown")

stat_alpha <- c(asymptomatic_alpha,symptomatic_alpha,unknown_alpha)
stat_alpha <- sample(stat_alpha)
stat_delta <- c(asymptomatic_delta,symptomatic_delta,unknown_delta)
stat_delta <- sample(stat_delta)

data$stat <- ifelse(data$variant == "alpha", stat_alpha, stat_delta)

#check if it is correct
table(subset(data,variant="alpha"))

#hospitalisation
h_alpha <- replicate(764,"HOS")
h_delta <- replicate(196,"HOS")
er_alpha <- replicate(1148,"ER") #er = emergency care
er_delta <- replicate(498,"ER")

hos_alpha <- c(h_alpha,er_alpha)
hos_alpha <- sample(hos_alpha)

hos_delta <- c(h_delta,er_delta)
hos_delta <- sample(hos_delta)

#in this case random assignment isn't the best way because symptomatic disease progression would lead to higher hospitalization rates than aysmptomatic disease progression
data$hos=""
data$hos[sample(which(data$variant == "alpha"),length(hos_alpha))] <- hos_alpha
data$hos[sample(which(data$variant == "delta"),length(hos_delta))] <- hos_delta


#-----------------------------------------------
#draft
#age
age_n <- c(3564,9462,7636,9157,6885,3916,1681,584,453)
age_d <- c("<10","10-19","20-29","30-39","40-49","50-59","60-69","70-79",">80")
for (i in seq(from=1, to=length(age_d))){
  replicate(age_n[i],age_d[i])
}
