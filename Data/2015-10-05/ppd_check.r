

library("pdata")
summary(ppd)

glimpse(ppd)
count(ppd, planf)

df <- ppd %>% filter(planf!="Safety") %>%
  group_by(ppd_id) %>%
  mutate(planf=as.character(planf),
         adminf=as.character(adminf),
         activepch=actives_tot / actives_tot[match(fy-1, fy)] * 100 - 100,
         retireepch=beneficiaries_tot / beneficiaries_tot[match(fy-1, fy)] * 100 - 100,
         # ?? rets_growth=beneficiaries_ServiceRetirees / beneficiaries_ServiceRetirees[match(fy-1, fy)] * 100 - 100,
         poptot=actives_tot + beneficiaries_tot,
         activepchtot=(actives_tot -actives_tot[match(fy-1, fy)]) / poptot[match(fy-1, fy)] * 100,
         retireepchtot=(beneficiaries_tot -beneficiaries_tot[match(fy-1, fy)]) / poptot[match(fy-1, fy)] * 100,
         xcfpct2=(contrib_tot + expense_net) / MktAssets_net * 100,
         apratio=MktAssets_net / payroll,
         abratio=actives_tot / beneficiaries_ServiceRetirees,
         assetsb=MktAssets_net/1e6,
         realreturnassumption=InvestmentReturnAssumption_GASB - InflationAssumption_GASB,
         cashfixed=FixedIncome_tot + CashAndShortTerm,
         benassets=expense_net / MktAssets_net) %>%
  ungroup

# medians for all
df %>% filter(fy==2012) %>%
  select(assetsb, ActiveAge_avg, ActFundedRatio_GASB, PercentReqContPaid, activepch, retireepch, xcfpct2, abratio, apratio) %>%
  summarise_each(funs(median(., na.rm=TRUE)))

# 1 plan and medians for all other (non-safety)
compare <- function(myppdid) {
  mdns <- df %>% filter(fy==2012) %>%
    mutate(PlanName=ifelse(ppd_id==myppdid, PlanName, "Other"), ppd_id=ifelse(ppd_id==myppdid, ppd_id, 0)) %>%
    group_by(ppd_id, PlanName) %>%
    select(assetsb, ActiveAge_avg, ActFundedRatio_GASB, PercentReqContPaid, activepch, retireepch, xcfpct2, abratio, apratio) %>%
    summarise_each(funs(median(., na.rm=TRUE))) %>%
    kable(digits=3)

  yrs <- df %>% filter(ppd_id==myppdid) %>%
    arrange(fy) %>%
    select(ppd_id, PlanName, fy, assetsb, ActiveAge_avg, ActFundedRatio_GASB, PercentReqContPaid, activepch, retireepch, xcfpct2, abratio, apratio) %>%
    kable(digits=3)
  return(list(mdns=mdns, yrs=yrs))
}

# look at a quintile
getquint <- function(var, quint) {
  df %>% filter(fy==2012) %>%
    mutate(ntile=ntile(.[[var]], 5)) %>%
    filter(ntile==quint) %>%
    select(ppd_id, PlanName, planf, assetsb, ActiveAge_avg, ActFundedRatio_GASB, PercentReqContPaid, activepch, retireepch, xcfpct2, abratio, apratio, ntile) %>%
    arrange_(var) %>%
    kable(digits=2)
}

# average plan ####
compare(6)$mdns
compare(6)$yrs

getquint("xcfpct2", 5) # doesn't work for ActiveAge_avg -- see special code below


# older plan ####
getquint("apratio", 5)
getquint("abratio", 5)
getquint("xcfpct2", 1)

ppdid <- 43 # 43 93 23
compare(ppdid)$mdns
compare(ppdid)$yrs


var <- "ActiveAge_avg"
tmp <- df %>% filter(fy==2012, ActiveAge_avg>0, ActiveAge_avg<100) %>% ungroup %>%
  filter_(!is.na(var)) %>%
  mutate(ntile=ntile(.[[var]], 5)) %>%
  #filter(ntile==5) %>%
  select(ppd_id, PlanName, planf, assetsb, ActiveAge_avg, ActFundedRatio_GASB, PercentReqContPaid, activepch, retireepch, xcfpct2, abratio, apratio, ntile) %>%
  arrange_(var)
tmp %>% filter(ntile==1) %>% kable(digits=2)
glimpse(tmp)


# immature plan ####
ppdid <- 37
compare(ppdid)$mdns
compare(ppdid)$yrs

getquint("apratio", 1)
getquint("abratio", 1)
getquint("xcfpct2", 5)






