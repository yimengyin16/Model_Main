# This script import salary and retirement benefit data.


## Inputs:
 # actives
 # retirees
 # salgrowth.hist
 # salgrowth.assume  
 
## Outputs
 # salary:  complete historical and prospect salary by age and ea.
 # benefit: average benefit payment in year 1, by age and ea. 


#*************************************************************************************************************
#                                       Tailoring Demographic Data                                       #####                  
#*************************************************************************************************************

tailor_demoData <- function(.paramlist = paramlist,
                            .Global_paramlist  = Global_paramlist,
                            .actives = actives,
                            .retirees = retirees){
  
assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(.paramlist,        envir = environment())

.actives  %<>% filter(planname == planname_actives,
                                  ea  %in% range_ea,
                                  age %in% range_ea)
.retirees %<>% filter(planname == planname_retirees,
                                  age >= r.min)

return(list(actives = .actives, retirees = .retirees))
}

tailored_demoData <-  tailor_demoData()
  

#*************************************************************************************************************
#                                        Create complete salary scale                                    #####                  
#*************************************************************************************************************

get_scale <- function(
                      #.salgrowth.hist   = salgrowth.hist,
                      #.salgrowth.assume = salgrowth.assume, 
                      .salgrowth = salgrowth,
                      .paramlist = paramlist,
                      .Global_paramlist  = Global_paramlist){

# This function generates a complete salary scale for all combos of starting year, entry ages and ages relevant to
# to model. 
# 
# Salary levels at year 1 are set to 1. For future workers (entry year greater than 1) whose span of career years
# do not include year 1, assumption about their starting salary levels is needed. Curretnly we assume starting salary
# grows at inflation rate. 
  
  
# Run the section below when developing new features. 
#   .salgrowth        = salgrowth
#   .paramlist = paramlist
#   .Global_paramlist  = Global_paramlist

  
assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(.paramlist,        envir = environment())

  
# sscale_hist   <- .salgrowth.hist   %>% filter(planname == .planname_sscale.hist) %>% select(-planname)
# sscale_assume <- .salgrowth.assume %>% filter(planname == .planname_sscale.assume) %>% select(-planname)
# Do not distinguish between sscale_hist and sscale_assume.

sscale <- .salgrowth %>% filter(planname == planname_sscale) %>% select(-planname)

SS.all <- expand.grid(start.year = (1 - (max.age - min.age)):nyear, ea = range_ea, age = min.age:(r.max - 1)) %>% 
  filter(age >= ea, start.year + r.max - 1 - ea >= 1 ) %>% # workers must stay in workforce at least up to year 1. 
  # arrange(start.year, ea, age) %>%
  mutate(yos = age - ea) %>% 
  left_join(sscale) %>%
  group_by(start.year, ea) %>% 
  mutate(year = start.year + (age - ea),
         growth.start = (1 + startingSal_growth)^(start.year - 1), # assume starting salary grows at the rate of inflation for all entry ages 
         scale = cumprod(ifelse(age == ea, 1, lag(1 + salgrowth))), # salgrowth is from data salgrowth
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

fill_startSal <- function(.actives          = tailored_demoData$actives,
                          .paramlist        = paramlist,
                          .Global_paramlist = Global_paramlist){
# This function generate a table of initial salary (year 1) which include all starting salary levels (age = ea)
# If the starting salary is missing from the actives data frame, spline function is used to interpolate and/or 
# extraploate the missing values. 
  
  
# Run the section below when developing new features.
#   .actives          = actives
#   .paramlist        = paramlist
#   .Global_paramlist = Global_paramlist
    
assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
assign_parmsList(.paramlist,        envir = environment())  

sal <- actives %>% select(age, ea, salary)
#x <- sal %>% spread(age, salary)

sal.start <- splong(sal, "ea", range_ea) %>% filter(age == ea) %>% select(-age) %>% splong("ea", range_ea) %>% mutate(age = ea)

sal <- rbind(sal, sal.start) 
 
sal <- sal[!duplicated(sal[c("age","ea")]),]
# sal %>% spread(age, salary)


# DIRTY TRICK to correct negative salary in "youngplan"
if(planname_actives == "youngplan") sal %<>%  mutate(salary =  ifelse(salary <= 0, salary[age == 62], salary ))

if(any(sign(sal$salary) != 1)) stop("Negative value(s) in imputed starting salary.")

return(sal)

}

init_sal <- fill_startSal()
init_sal  %>% filter(age == ea)




#*************************************************************************************************************
#                                        Create complete salary history                                  #####                  
#*************************************************************************************************************

get_salary <- function(.SS.all = SS.all,
                       .init_sal =  init_sal,
                       .paramlist = paramlist,
                       .Global_paramlist  = Global_paramlist){

# Run the section below when developing new features.
#   .SS.all = SS.all
#   .init_sal =  init_sal
#   .paramlist = paramlist
#   .Global_paramlist  = Global_paramlist
  

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
get_benefit <- function(
                        .retirees = tailored_demoData$retirees,
                        .paramlist = paramlist,
                        .Global_paramlist  = Global_paramlist){

assign_parmsList(.Global_paramlist, envir = environment())
assign_parmsList(.paramlist,        envir = environment())  

avgben <- .retirees %>% select(age, benefit)  
    
benefit <- avgben %>% 
           # filter(age>=r.max) %>% 
           mutate(year = 1,
                  ea = r.min - 1)
# benefit %>% select(-year) %>% spread(age, benefit)

return(benefit)
}

benefit <- get_benefit()


#*************************************************************************************************************
#                               Generating inital population                                             #####                  
#*************************************************************************************************************

get_initPop <- function (.actives          = tailored_demoData$actives,
                         .retirees         = tailored_demoData$retirees,
                         .paramlist        = paramlist,
                         .Global_paramlist = Global_paramlist){
# Import and standardize the total number of actives and retirees.  
  
# Run the section below when developing new features.
#   .actives          = actives
#   .retirees         = retirees
#   .paramlist        = paramlist
#   .Global_paramlist = Global_paramlist
  
    
  assign_parmsList(.Global_paramlist, envir = environment())
  assign_parmsList(.paramlist,        envir = environment()) 

    
  init_actives <- .actives %>% select(ea, age, nactives)
  init_actives <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_actives) %>% 
                  mutate(nactives = n_init_actives * nactives/sum(nactives, na.rm = TRUE)) %>% 
                  spread(age, nactives, fill = 0) %>% select(-ea) %>% as.matrix 

  
  init_retirees <- .retirees %>% select(age, nretirees) %>% mutate(ea = r.min - 1) 
  init_retirees <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_retirees) %>% 
                   mutate(nretirees = n_init_retirees * nretirees/sum(nretirees, na.rm = TRUE)) %>% 
                   spread(age, nretirees, fill = 0) %>% select(-ea) %>% as.matrix

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
 
  
get_entrantsDist <- function(.actives          = tailored_demoData$actives,
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
#   .planname = "youngplan"
#   simpe = TRUE
#   .range_ea = paramlist$range_ea
    
assign_parmsList(.Global_paramlist, envir = environment())
#assign_parmsList(.paramlist,        envir = environment())   
  
nact <- .actives %>% select(age, ea, nactives)
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

# save(entrants_dist, file = "entrants_dist.average.RData")



