# This script import salary and retirement benefit data.


# load("Data/actives.rda")
# load("Data/retirees.rda")
# load("Data/salgrowth.hist.rda")
# load("Data/salgrowth.assume.rda")

#assign_parmsList(Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
#assign_parmsList(paramlist,        envir = environment())

## Inputs:
 # actives
 # retirees
 # salgrowth.hist
 # salgrowth.assume  
 
## Outputs
 # salary:  complete historical and prospect salary by age and ea.
 # benefit: average benefit payment in year 1, by age and ea. 



#*************************************************************************************************************
#                                        Create complete salary scale                                    #####                  
#*************************************************************************************************************

get_scale <- function(.planname_sscale.hist = planname_sscale.hist,
                      .planname_sscale.assume = planname_sscale.assume,
                      .salgrowth.hist   = salgrowth.hist,
                      .salgrowth.assume = salgrowth.assume, 
                      .paramlist = paramlist,
                      .Global_paramlist  = Global_paramlist){

# Salary scale for all starting year
  
assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(.paramlist,        envir = environment())
  
sscale_hist   <- .salgrowth.hist   %>% filter(planname == .planname_sscale.hist) %>% select(-planname)
sscale_assume <- .salgrowth.assume %>% filter(planname == .planname_sscale.assume) %>% select(-planname)

SS.all <- expand.grid(start.year = (1 - (max.age - min.age)):nyear, ea = range_ea, age = min.age:(r.max - 1)) %>% 
  filter(age >= ea, start.year + r.max - 1 - ea >= 1 ) %>%
  # arrange(start.year, ea, age) %>%
  left_join(sscale_hist) %>%
  left_join(sscale_assume) %>% 
  group_by(start.year, ea) %>% 
  mutate(year = start.year + (age - ea),
         sscale = ifelse(year < 1, sscale.hist.rate, sscale.assume.rate),
         growth.start = (1 + infl)^(start.year - 1), # assume starting salary grows at the rate of inflation for all entry ages 
         scale = cumprod(ifelse(age == ea, 1, lag(1 + sscale.hist.rate))),
         scale = ifelse(start.year <= 1, scale/scale[year == 1],
                        scale * growth.start)
  ) %>% 
  select(start.year, ea, age, year, scale)

return(SS.all)
}

SS.all <- get_scale()


#*************************************************************************************************************
#                     Supplement the inital salary table with all starting salary                        #####                  
#*************************************************************************************************************

fill_startSal <- function(.actives          = actives,
                          .planname_actives = planname_actives,
                          .paramlist        = paramlist,
                          .Global_paramlist = Global_paramlist){
  
assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(.paramlist,        envir = environment())  

sal <- actives %>% filter(planname == .planname_actives) %>% select(age, ea, salary)
#sal %>% spread(age, salary)
sal.start <- splong(sal, "ea", range_ea) %>% filter(age == ea) %>% select(-age) %>% splong("ea", range_ea) %>% mutate(age = ea)

sal <- rbind(sal, sal.start) 
 
sal <- sal[!duplicated(sal[c("age","ea")]),]
# sal %>% spread(age, salary)


# DIRTY TRICK to correct negative salary in "youngplan"
if(.planname_actives == "youngplan") sal %<>%  mutate(salary =  ifelse(salary <= 0, salary[age == 62], salary ))

if(any(sign(sal$salary) != 1)) stop("Negative value(s) in imputed starting salary.")

return(sal)

}

init_sal <- fill_startSal()
init_sal  

actives %>% filter(planname == "youngplan")

#*************************************************************************************************************
#                                        Create complete salary history                                  #####                  
#*************************************************************************************************************

get_salary <- function(.SS.all = SS.all,
                       .init_sal =  init_sal,
                       .paramlist = paramlist,
                       .Global_paramlist  = Global_paramlist){
# Inputs

assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(.paramlist,        envir = environment())  

#avgpay <- actives %>% filter(planname == .planname_actives) %>% select(age, ea, salary)
    
salary <- .SS.all %>% left_join(.init_sal) %>% 
  group_by(start.year, ea) %>% 
  mutate(sx = ifelse(start.year <= 1, salary[year == 1]* scale, 
                     salary[age == ea]* scale)) %>% 
  select(start.year, ea, age, year, sx)


return(salary)
}

salary <- get_salary() 
salary


# # Check the growth of starting salary before year 1
# fn <- function(x) (x[length(x)]/x[1])^(1/(length(x) - 1)) - 1
# 
# # starting salary over time
# start.pay <- salary %>% filter(age == ea, start.year <=1) %>% ungroup %>%  arrange(ea, year) %>% 
#   select(ea, year, sx)
# start.pay %>%  kable
# # start.pay %>% ggplot(aes(x = year + 2012, y = sx, colour = factor(ea))) + geom_line(size = 1)
# 
# 
# # average growht rates of starting salary 
# salary %>% filter(age == ea, start.year <=1) %>% ungroup %>% group_by(ea) %>%  arrange(ea, year) %>% 
#   select(ea, year, sx) %>% filter(year <= -12 ) %>% summarize(g = fn(sx) )%>% kable


# 1. Starting salary generally has in rising trend, while drops after the Great Recession.(The rising trend in the end may be due to the imputation)
# 2. The growth rates before 40 are around 3% ~ 4%, which is approximately the average inflation rate
# 3. The growth rates of starting salary are much lower for ages greater 40, and even become negative in 50s.
#    Even after taking into account that starting salary at higher entry ages have shorter history and hencea are
#    affected by the recent recession more, their growth curves are still flatter than those of lower entry ages.
# 4. Need to be carefull about the observations above since the imputed salary table is used.
# 5. If the observations above are real, we need to make different assumptions about the growth rate of starting
#    salary for different entry ages. The current assumption of a flat growth rate over all entry ages may be inappropriate.


#*************************************************************************************************************
#                               Import initial retirement benefit table from AV                          #####                  
#*************************************************************************************************************
get_benefit <- function(.planname_retirees = planname_retirees,
                        .retirees = retirees,
                        .paramlist = paramlist,
                        .Global_paramlist  = Global_paramlist){

assign_parmsList(.Global_paramlist, envir = environment())
assign_parmsList(.paramlist,        envir = environment())  

avgben <- .retirees %>% filter(planname == .planname_retirees) %>% select(age, benefit)  
    
benefit <- avgben %>% filter(age>=r.max) %>% 
           mutate(year = 1,
                  ea = r.min - 1)
# benefit %>% select(-year) %>% spread(age, benefit)

return(benefit)
}

benefit <- get_benefit()


#*************************************************************************************************************
#                               Generating inital population                                             #####                  
#*************************************************************************************************************


# actives <- actives %>% mutate(nactives = 1)
# actives 


# retirees <- retirees %>% mutate(nretirees = 1)
# retirees


get_initPop <- function (.actives          = actives,
                         .retirees         = retirees,
                         .planname_actives = planname_actives,
                         .planname_retirees= planname_retirees,
                         .paramlist        = paramlist,
                         .Global_paramlist = Global_paramlist){
  
  assign_parmsList(.Global_paramlist, envir = environment())
  assign_parmsList(.paramlist,        envir = environment()) 
  
  init_actives <- .actives %>% filter(planname == .planname_actives) %>% select(ea, age, nactives)
  init_actives <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_actives) %>% 
                  spread(age, nactives, fill = 0) %>% select(-ea) %>% as.matrix 

  
  init_retirees <- .retirees %>% filter(planname == .planname_retirees) %>% select(age, nretirees) %>% mutate(ea = r.min - 1)
  init_retirees <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_retirees) %>% 
                   spread(age, nretirees, fill = 0) %>% select(-ea) %>% as.matrix
  #init_retirees %>% spread(age, nretirees)
  
  return(list(actives = init_actives, retirees = init_retirees))
}


# if(!dev_mode) 
  init_pop <- get_initPop()

# init_pop$actives
# init_pop$retirees


# init_pop$actives <- init_pop$actives %>% as.data.frame %>% gather(age, value) %>% 
#   mutate(value = ifelse(f2n(age) >= 30 & f2n(age) < 65, 1, 0), ea = rep(20:64, 101)) %>% filter(f2n(age) >= ea ) %>% spread(age, value, fill = 0) %>% 
#   select(-ea) %>% as.matrix
  
  

  
#*************************************************************************************************************
#                            Infering ditribution of entrants from low yos actives                       #####                  
#*************************************************************************************************************
 
  
get_entrantsDist <- function(.actives          = actives,
                             .planname         = paramlist$planname_actives,
                             .range_ea         = paramlist$range_ea,
                             #.paramlist        = paramlist,
                             .Global_paramlist = Global_paramlist,
                             simple = FALSE){
# Simple imputation rule is applied under the following two circumstances:
# 1. parameter "simple" is set to TRUE
# 2. negative weights are generated by the regular rule. 
  
#   .actives          = actives
#   .paramlist        = paramlist
#   .Global_paramlist = Global_paramlist  
  
assign_parmsList(.Global_paramlist, envir = environment())
#assign_parmsList(.paramlist,        envir = environment())   
  
nact <- .actives %>% filter(planname == .planname) %>% select(age, ea, nactives)
#nact %>% spread(age, nactives)

## Distributon by simple rule
nact1 <- nact %>% filter(age - ea <= 4) %>% group_by(ea) %>% summarise(avg_ent = mean(nactives)) %>% right_join(data.frame(ea = .range_ea))
while(any(is.na(nact1$avg_ent))) nact1 %<>% mutate(avg_ent = ifelse(is.na(avg_ent), lag(avg_ent) , avg_ent))
# nact1

nact <- splong(nact, "ea", .range_ea) %>% splong("age", .range_ea) %>% filter(age >= ea)
#nact <- splong(nact, "ea", range_ea) %>% filter(age >= ea)
nact %>% spread(age, nactives)


ent <- nact %>% filter(age - ea <= 4) %>% group_by(ea) %>% summarise(avg_ent = mean(nactives))

neg_ea <- ent[which(ent$avg_ent < 0), "ea"]

if(any(ent$avg_ent < 0)){warning("Negative inferred value(s) in the following entry age(s): " , as.character(neg_ea), "\n",
                                  "  Simple imputation rule is applied")
                         ent <- nact1                          
  }

# ent %<>% mutate(avg_ent = ifelse(avg_ent < 0, 0, avg_ent))

if(simple) ent <- nact1

dist <- lowess(ent$avg_ent, f= 0.1)$y
dist <- dist/sum(dist)

return(dist)
}

entrants_dist <- get_entrantsDist()


# entrants_dist

# dist1 <- get_entrantsDist(actives, "average")
# dist2 <- get_entrantsDist(actives, "underfunded")
# get_entrantsDist(actives, "youngplan")
 
# data.frame(ea = paramlist$range_ea, average = dist1, underfunded = dist2) %>% gather(plan, pct, -ea) %>% 
# ggplot(aes(x = ea, y = pct, color = plan)) + geom_point(size = 3.5) + geom_line(linetype = 3) + theme_bw()
# plot(entrants_dist)



