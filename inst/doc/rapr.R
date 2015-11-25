## ----global, include=FALSE-----------------------------------------------
# load rapr R package
library(rapr)

# load packages for vignette
library(plyr)
library(dplyr)
library(ggplot2)
library(RandomFields)

# set cache globally
knitr::opts_chunk$set(cache=TRUE)

# use built-in solutions or Gurobi?
use.Gurobi=FALSE
if (use.Gurobi & !is.GurobiInstalled())
	use.Gurobi=FALSE

# set seed for reproducibility
set.seed(500)

## ---- eval=use.Gurobi----------------------------------------------------
#  # make prioritisations
#  sim_mrs_amount <- update(
#  	sim_ru,
#  	amount.target=c(0.2,0.2,0.2),
#  	space.target=c(0,0,0)
#  )
#  
#  sim_mrs_space <- update(
#  	sim_ru,
#  	amount.target=c(0.2,0.2,0.2),
#  	space.target=c(0.85, 0.85, 0.85)
#  )

## ---- fig.height=3.5, fig.width=8, fig.cap="Prioritisations were generated using amount-based targets (20\\%), and with additional space-based targets (85\\%). These are compared to the Queensland reserve network. Data represent means and standard errors for the four species in each prioritisation."----
# define standard error function
se=function(x){sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))}

# create a table to store the values for the 3 prioritisations
cs_results <- data.frame(
	name=rep(rep(c('Amount-based prioritisation', 
		'Amount+space-based prioritsation', 'Australian reserve network'),
		each=4),3),
	variable=rep(c('Amount', 'Geographic space', 'Environmental space'), each=12),
	species=colnames(amount.held(cs_rs_amount)),
	value=c(
		amount.held(cs_rs_amount)[1,], amount.held(cs_rs_space)[1,],
			amount.held(cs_rs_aus)[1,],
		space.held(cs_rs_amount, space=2)[1,], space.held(cs_rs_space, space=2)[1,], 
			space.held(cs_rs_aus, space=2)[1,],
		space.held(cs_rs_amount, space=1)[1,], space.held(cs_rs_space, space=1)[1,],
			space.held(cs_rs_aus, space=1)[1,]
	)
) %>% group_by(
	name,
	variable
) %>% summarise(
	mean=mean(value),
	se=se(value)
)

# plot the performance metrics
ggplot(aes(x=variable, y=mean, fill=name), data=cs_results) +
	geom_bar(position=position_dodge(0.9), stat='identity') +
	geom_errorbar(
		aes(ymin=mean-se, ymax=mean+se), position=position_dodge(0.9),
		width=0.2
	) +
	xlab('Property of species') +
	ylab('Proportion held in\nselected planning units (%)') +
	scale_fill_discrete(
		name=''
	) +
	theme_classic() +
	theme(legend.position='bottom',legend.direction='horizontal')

