## ----global, include=FALSE-----------------------------------------------
# load rapr R package
library(rapr)

# set cache globally
knitr::opts_chunk$set(cache=TRUE)

# use built-in solutions or Gurobi?
use.Gurobi=TRUE
if (use.Gurobi & !is.GurobiInstalled())
	use.Gurobi=FALSE

# set seed for reproducibility
set.seed(500)

## ---- eval=use.Gurobi----------------------------------------------------
# solve problem to identify prioritisation
sim_rs_s1_amount <- solve(sim_ru_s1)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  sim_rs_s1_amount <- solve(sim_ru_s1, b=81:100)

## ---- eval=use.Gurobi----------------------------------------------------
# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s2_amount <- update(sim_ru_s2, amount.target=0.2, space.target=0, solve=TRUE)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # update amount targets to 20% and space targets to 0% and solve it
#  sim_rs_s2_amount <- update(sim_ru_s2, amount.target=0.2, space.target=0, solve=FALSE)
#  sim_rs_s2_amount <-solve(
#  	sim_rs_s2_amount,
#  	b=c(35L, 36L, 44L, 45L, 46L, 55L, 56L, 57L, 65L, 66L)
#  )

## ---- eval={use.Gurobi}--------------------------------------------------
# update amount targets to 20% and space targets to 0% and solve it
sim_rs_s3_amount <- update(sim_ru_s3, amount.target=0.2, space.target=0)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # update amount targets to 20% and space targets to 0% and solve it
#  sim_rs_s3_amount <- update(sim_ru_s3, amount.target=0.2, space.target=0, solve=FALSE)
#  sim_rs_s3_amount <- solve(
#  	sim_rs_s3_amount,
#  	b=c(53L, 63L, 64L, 72L, 73L, 74L, 83L, 84L)
#  )

## ---- eval=use.Gurobi----------------------------------------------------
# make new prioritisation
sim_rs_s1_space <- update(sim_rs_s1_amount, amount.target=0.2, space.target=0.85)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make new prioritisation
#  sim_rs_s1_space <- update(sim_rs_s1_amount, amount.target=0.2, space.target=0.85, solve=FALSE)
#  sim_rs_s1_space <- solve(sim_rs_s1_space, b=c(53L, 63L, 64L, 72L, 73L, 74L, 83L, 84L))

## ---- eval=use.Gurobi----------------------------------------------------
# make new prioritisation
sim_rs_s2_space <- update(sim_rs_s2_amount, amount.target=0.2, space.target=0.85)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make new prioritisation
#  sim_rs_s2_space <- update(sim_rs_s2_amount, amount.target=0.2, space.target=0.85, solve=FALSE)
#  sim_rs_s2_space <- solve(sim_rs_s2_space, b=c(23L, 25L, 28L, 44L, 46L, 47L, 53L, 56L, 59L, 65L, 66L, 73L, 78L, 86L))
#  

## ---- eval=use.Gurobi----------------------------------------------------
# make new prioritisation
sim_rs_s3_space <- update(sim_rs_s3_amount, amount.target=0.2, space.target=0.85)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make new prioritisation
#  sim_rs_s3_space <- update(sim_rs_s3_amount, amount.target=0.2, space.target=0.85, solve=FALSE)
#  sim_rs_s3_space <- solve(sim_rs_s3_space, b=c(18L, 29L, 42L, 55L, 63L, 64L, 72L, 74L, 76L, 84L))

## ---- eval=use.Gurobi----------------------------------------------------
# make prioritisations
sim_mrs_amount <- update(sim_ru, amount.target=c(0.2,0.2,0.2), space.target=c(0,0,0))

sim_mrs_space <- update(sim_ru, amount.target=c(0.2,0.2,0.2), space.target=c(0.85, 0.85, 0.85))

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make prioritisations
#  sim_mrs_amount <- update(sim_ru, amount.target=c(0.2,0.2,0.2), space.target=c(0,0,0), solve=FALSE)
#  sim_mrs_amount <- solve(sim_mrs_amount, b=c(14L, 35L, 36L, 46L, 47L, 53L, 54L, 55L, 57L, 62L, 63L, 65L, 69L, 71L, 72L, 73L, 74L, 75L, 83L, 93L))
#  
#  sim_mrs_space <- update(sim_ru, amount.target=c(0.2,0.2,0.2), space.target=c(0.85, 0.85, 0.85), solve=FALSE)
#  sim_mrs_space <- solve(sim_mrs_space, b=c(3L, 16L, 17L, 22L, 25L, 30L, 36L, 38L, 43L, 46L, 56L, 62L, 64L, 66L, 69L, 70L, 73L, 85L, 87L, 92L, 100L))

## ---- eval={use.Gurobi}--------------------------------------------------
# make new prioritisation with probability threshold of 0.5 for each species
sim_mrs_space2 <- solve(
	prob.subset(
		sim_mrs_space,
		species=1:3,
		threshold=c(0.5,0.5,0.5)
	)
)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make new prioritisation with probability threshold of 0.5 for each species
#  sim_mrs_space2 <- solve(
#  	prob.subset(
#  		sim_mrs_space,
#  		species=1:3,
#  		threshold=c(0.5,0.5,0.5)
#  	),
#  	b=c(
#  		7L, 12L, 20L, 28L, 29L, 34L, 36L, 37L, 48L, 52L,
#  		53L, 55L, 56L, 64L, 67L, 70L, 72L, 75L, 84L, 89L
#  	)
#  )

## ---- eval=use.Gurobi----------------------------------------------------
# make new prioritisation using reliable formulation
sim_mrs_space3 <- update(sim_mrs_space, formulation='reliable', max.r.level=1L)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # make new prioritisation using reliable formulation
#  sim_mrs_space3 <- update(sim_mrs_space, formulation='reliable', max.r.level=1L, solve=FALSE)
#  sim_mrs_space3 <- solve(
#  	sim_mrs_space3,
#  	b=c(13L, 14L, 17L, 19L, 20L, 21L, 26L, 35L, 49L, 51L, 53L, 55L,
#  		63L, 67L, 70L, 74L, 84L, 87L, 89L, 92L
#  	)
#  )

## ---- eval=use.Gurobi----------------------------------------------------
# update prioritisation
sim_mrs_amount_blm <- update(sim_mrs_amount, BLM=100)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  sim_mrs_amount_blm <- update(sim_mrs_amount, BLM=100, solve=FALSE)
#  sim_mrs_amount_blm <- solve(
#  	sim_mrs_amount_blm,
#  	b=c(52L, 53L, 54L, 55L, 56L, 62L, 63L, 64L, 65L, 66L, 72L, 73L,
#  		74L, 75L, 76L, 82L, 83L, 84L, 85L, 86L
#  	)
#  )

## ---- eval=use.Gurobi, markup='hide'-------------------------------------
# create vector with distance metrics
dist.metrics <- c(
	'euclidean', 'bray', 'manhattan','gower',
	'canberra', 'mahalanobis',
	'jaccard', 'kulczynski'
)

# generate solutions
solutions <- list()
for (i in dist.metrics) {
	solutions[[i]] <- update(sim_ru_gp, distance.metric=i)
}

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # create vector with distance metrics
#  dist.metrics <- c(
#  	'euclidean', 'bray', 'manhattan','gower',
#  	'canberra', 'mahalanobis',
#  	'jaccard', 'kulczynski'
#  )
#  
#  ## create list with selections
#  # dput(lapply(solutions, function(x){which(selections(x)==1)}))
#  selections <- structure(list(euclidean = c(6L, 9L, 10L, 22L, 25L), bray = c(17L,
#  18L, 22L, 23L), manhattan = c(6L, 10L, 17L, 23L), gower = c(6L,
#  10L, 17L, 23L), canberra = c(6L, 10L, 17L, 20L, 23L), mahalanobis = c(1L,
#  10L, 11L, 17L, 23L), jaccard = c(7L, 17L, 18L, 23L), kulczynski = c(16L,
#  17L, 18L, 23L)), .Names = c("euclidean", "bray", "manhattan",
#  "gower", "canberra", "mahalanobis", "jaccard", "kulczynski"))
#  
#  # generate solutions
#  solutions <- list()
#  for (i in dist.metrics) {
#  	solutions[[i]] <- update(sim_ru_gp, distance.metric=i, solve=FALSE)
#  	solutions[[i]] <- solve(solutions[[i]],b=selections[[i]])
#  }

## ---- eval=use.Gurobi----------------------------------------------------
# generate prioritisation
cs_rs_amount <- solve(cs_rs_amount)

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  cs_rs_amount <- solve(
#  	cs_rs_amount,
#  	b=c(4L, 7L, 14L, 19L, 25L, 26L, 36L, 37L, 43L, 44L, 52L, 53L, 61L,
#  		81L, 86L, 103L, 104L, 115L, 116L, 117L, 120L, 132L, 133L, 134L,
#  		135L, 136L, 137L, 138L, 152L, 153L, 154L, 155L, 156L, 173L, 174L,
#  		175L, 192L, 193L, 194L, 195L, 212L, 213L, 214L, 215L, 233L, 234L,
#  		235L, 236L, 237L, 524L, 525L, 552L, 553L, 554L, 574L, 577L, 578L,
#  		579L, 580L, 591L, 592L, 593L, 594L, 595L, 596L, 603L, 604L, 605L,
#  		617L, 618L, 619L, 620L, 621L, 622L, 629L, 630L, 631L, 632L, 633L,
#  		634L, 635L, 642L, 643L, 644L, 645L, 646L, 647L, 648L, 649L, 655L,
#  		656L, 657L, 658L, 659L, 667L, 668L, 669L, 670L, 671L, 672L, 673L,
#  		675L, 680L, 681L, 682L, 683L, 685L, 692L, 693L, 694L, 695L, 696L,
#  		697L, 698L, 699L, 700L, 701L, 704L, 705L, 706L, 707L, 708L, 709L,
#  		710L, 722L, 723L, 724L, 725L, 726L, 727L, 730L, 731L, 732L, 733L,
#  		734L, 735L
#  	)
#  )

## ---- eval={use.Gurobi}--------------------------------------------------
# make amount- and space-based prioritisation
cs_rs_space <- update(cs_rs_amount, space.target=0.85)

## ---- eval={!use.Gurobi}, include=FALSE----------------------------------
#  # make amount- and space-based prioritisation
#  cs_rs_space <- update(cs_rs_amount, space.target=0.85, solve=FALSE)
#  cs_rs_space <- solve(
#  	cs_rs_space,
#  	b=c(7L, 14L, 19L, 25L, 36L, 39L, 44L, 52L, 53L, 81L, 86L, 103L,
#  		104L, 112L, 116L, 117L, 120L, 133L, 134L, 135L, 136L, 137L, 138L,
#  		148L, 152L, 153L, 155L, 156L, 162L, 163L, 166L, 172L, 173L, 174L,
#  		175L, 192L, 193L, 194L, 195L, 198L, 212L, 213L, 214L, 215L, 221L,
#  		233L, 234L, 235L, 236L, 237L, 305L, 319L, 347L, 410L, 440L, 463L,
#  		468L, 495L, 501L, 508L, 518L, 525L, 526L, 552L, 553L, 555L, 562L,
#  		565L, 574L, 577L, 578L, 591L, 594L, 603L, 604L, 617L, 618L, 619L,
#  		620L, 621L, 622L, 624L, 626L, 629L, 630L, 631L, 635L, 642L, 643L,
#  		644L, 645L, 646L, 647L, 648L, 655L, 656L, 657L, 658L, 664L, 667L,
#  		668L, 669L, 670L, 671L, 672L, 673L, 675L, 676L, 680L, 681L, 682L,
#  		683L, 692L, 693L, 694L, 695L, 696L, 697L, 698L, 700L, 701L, 704L,
#  		705L, 706L, 707L, 708L, 709L, 710L, 722L, 724L, 727L, 730L, 731L,
#  		732L, 733L, 734L, 735L, 755L
#  	)
#  )

## ---- eval=use.Gurobi, markup='hide'-------------------------------------
# create empty list to store solutions
solutions <- list()

# generate solutions
for (i in seq_along(space.targets))
	solutions[[i]] <- update(cs_rs_space, space.target=rep(space.targets[i],4))

## ---- eval=!use.Gurobi, include=FALSE------------------------------------
#  # create empty list to store solutions
#  solutions <- list()
#  
#  # create selection list with optimal selections
#  selections <- list(c(4L, 7L, 14L, 19L, 25L, 36L, 37L, 43L, 44L, 52L, 53L, 61L,
#  81L, 86L, 103L, 104L, 115L, 116L, 117L, 120L, 132L, 133L, 134L,
#  135L, 136L, 137L, 138L, 148L, 152L, 153L, 154L, 155L, 156L, 173L,
#  174L, 175L, 192L, 193L, 194L, 195L, 212L, 213L, 214L, 215L, 233L,
#  234L, 235L, 236L, 237L, 508L, 524L, 525L, 552L, 553L, 554L, 574L,
#  577L, 578L, 580L, 591L, 592L, 593L, 594L, 595L, 596L, 603L, 604L,
#  605L, 617L, 618L, 619L, 620L, 621L, 622L, 629L, 630L, 631L, 632L,
#  633L, 634L, 635L, 642L, 643L, 644L, 645L, 646L, 647L, 648L, 649L,
#  655L, 656L, 657L, 658L, 659L, 667L, 668L, 669L, 670L, 671L, 672L,
#  673L, 675L, 676L, 680L, 681L, 682L, 683L, 685L, 692L, 693L, 694L,
#  695L, 696L, 697L, 698L, 700L, 701L, 704L, 705L, 706L, 707L, 708L,
#  709L, 710L, 722L, 723L, 724L, 725L, 726L, 727L, 730L, 731L, 732L,
#  733L, 734L, 735L), c(4L, 7L, 14L, 19L, 25L, 36L, 37L, 43L, 44L,
#  52L, 53L, 61L, 81L, 86L, 103L, 104L, 115L, 116L, 117L, 120L,
#  133L, 134L, 135L, 136L, 137L, 138L, 148L, 152L, 153L, 154L, 155L,
#  156L, 173L, 174L, 175L, 192L, 193L, 194L, 195L, 198L, 212L, 213L,
#  214L, 215L, 233L, 234L, 235L, 236L, 237L, 305L, 508L, 524L, 525L,
#  552L, 553L, 554L, 574L, 577L, 578L, 579L, 580L, 591L, 592L, 593L,
#  594L, 595L, 596L, 603L, 604L, 605L, 617L, 618L, 619L, 620L, 621L,
#  622L, 629L, 630L, 631L, 632L, 634L, 635L, 642L, 643L, 644L, 645L,
#  646L, 647L, 648L, 655L, 656L, 657L, 658L, 659L, 667L, 668L, 669L,
#  670L, 671L, 672L, 673L, 675L, 676L, 680L, 681L, 682L, 683L, 685L,
#  692L, 693L, 694L, 695L, 696L, 697L, 698L, 700L, 701L, 704L, 705L,
#  706L, 707L, 708L, 709L, 710L, 722L, 723L, 724L, 725L, 726L, 727L,
#  730L, 731L, 732L, 733L, 734L, 735L), c(4L, 7L, 14L, 19L, 25L,
#  37L, 43L, 44L, 52L, 53L, 61L, 81L, 86L, 103L, 104L, 115L, 116L,
#  117L, 120L, 132L, 133L, 134L, 135L, 136L, 137L, 138L, 148L, 152L,
#  153L, 154L, 155L, 156L, 173L, 174L, 175L, 192L, 193L, 194L, 195L,
#  198L, 212L, 213L, 214L, 215L, 233L, 234L, 235L, 236L, 237L, 305L,
#  508L, 524L, 525L, 552L, 553L, 554L, 574L, 577L, 578L, 579L, 580L,
#  591L, 593L, 594L, 595L, 596L, 603L, 604L, 605L, 617L, 618L, 619L,
#  620L, 621L, 622L, 629L, 630L, 631L, 632L, 633L, 634L, 635L, 642L,
#  643L, 644L, 645L, 646L, 647L, 648L, 655L, 656L, 657L, 658L, 659L,
#  667L, 668L, 669L, 670L, 671L, 672L, 673L, 675L, 676L, 680L, 681L,
#  682L, 683L, 685L, 692L, 693L, 694L, 695L, 696L, 697L, 698L, 700L,
#  701L, 704L, 705L, 706L, 707L, 708L, 709L, 710L, 722L, 723L, 724L,
#  725L, 726L, 727L, 730L, 731L, 732L, 733L, 734L, 735L), c(4L,
#  7L, 14L, 19L, 25L, 36L, 37L, 43L, 44L, 52L, 53L, 61L, 81L, 86L,
#  103L, 104L, 115L, 116L, 117L, 120L, 133L, 134L, 135L, 136L, 137L,
#  138L, 148L, 152L, 153L, 154L, 155L, 156L, 172L, 173L, 174L, 175L,
#  192L, 193L, 194L, 195L, 198L, 212L, 213L, 214L, 215L, 233L, 234L,
#  235L, 236L, 237L, 305L, 508L, 524L, 525L, 552L, 553L, 554L, 574L,
#  577L, 578L, 579L, 580L, 591L, 592L, 593L, 594L, 595L, 596L, 603L,
#  604L, 605L, 617L, 618L, 619L, 620L, 621L, 622L, 629L, 630L, 631L,
#  632L, 633L, 634L, 635L, 642L, 643L, 644L, 645L, 646L, 647L, 648L,
#  655L, 656L, 657L, 658L, 659L, 667L, 668L, 669L, 670L, 671L, 672L,
#  673L, 675L, 676L, 680L, 681L, 682L, 683L, 684L, 685L, 692L, 693L,
#  694L, 695L, 696L, 697L, 698L, 700L, 704L, 705L, 706L, 707L, 708L,
#  709L, 710L, 722L, 724L, 725L, 727L, 730L, 731L, 732L, 733L, 734L,
#  735L), c(7L, 14L, 19L, 25L, 36L, 39L, 44L, 52L, 53L, 81L, 86L,
#  103L, 104L, 116L, 117L, 120L, 133L, 135L, 136L, 137L, 138L, 148L,
#  152L, 153L, 155L, 156L, 163L, 166L, 172L, 173L, 174L, 175L, 192L,
#  193L, 194L, 195L, 196L, 198L, 201L, 209L, 212L, 213L, 214L, 215L,
#  233L, 234L, 235L, 236L, 237L, 265L, 305L, 463L, 479L, 508L, 518L,
#  524L, 525L, 526L, 552L, 553L, 554L, 574L, 577L, 578L, 580L, 591L,
#  593L, 594L, 603L, 604L, 617L, 618L, 619L, 620L, 621L, 622L, 626L,
#  629L, 630L, 631L, 632L, 633L, 635L, 642L, 643L, 644L, 645L, 646L,
#  647L, 648L, 655L, 656L, 657L, 658L, 659L, 667L, 668L, 669L, 670L,
#  671L, 672L, 673L, 675L, 676L, 680L, 681L, 682L, 683L, 685L, 692L,
#  693L, 694L, 695L, 696L, 697L, 698L, 700L, 701L, 704L, 705L, 706L,
#  707L, 708L, 709L, 710L, 722L, 723L, 724L, 725L, 726L, 727L, 730L,
#  731L, 732L, 733L, 734L, 735L), c(7L, 14L, 19L, 25L, 36L, 39L,
#  44L, 52L, 53L, 81L, 86L, 103L, 104L, 112L, 116L, 117L, 133L,
#  134L, 135L, 136L, 137L, 138L, 148L, 152L, 153L, 155L, 156L, 162L,
#  163L, 164L, 166L, 172L, 173L, 174L, 175L, 192L, 193L, 194L, 196L,
#  198L, 201L, 212L, 213L, 214L, 215L, 234L, 235L, 236L, 237L, 265L,
#  305L, 347L, 440L, 463L, 479L, 508L, 518L, 525L, 533L, 552L, 553L,
#  554L, 555L, 565L, 574L, 577L, 578L, 580L, 591L, 593L, 594L, 595L,
#  603L, 604L, 617L, 618L, 619L, 620L, 621L, 622L, 626L, 629L, 630L,
#  631L, 635L, 642L, 643L, 644L, 645L, 646L, 647L, 655L, 656L, 657L,
#  658L, 667L, 668L, 669L, 670L, 671L, 672L, 673L, 675L, 676L, 680L,
#  681L, 682L, 683L, 685L, 692L, 693L, 694L, 695L, 696L, 697L, 698L,
#  700L, 701L, 704L, 705L, 706L, 707L, 708L, 709L, 710L, 722L, 723L,
#  724L, 725L, 726L, 727L, 730L, 731L, 732L, 733L, 734L, 735L),
#      c(14L, 19L, 25L, 36L, 39L, 44L, 52L, 53L, 81L, 86L, 103L,
#      104L, 115L, 116L, 117L, 120L, 130L, 133L, 134L, 135L, 136L,
#      137L, 138L, 148L, 152L, 153L, 154L, 155L, 156L, 162L, 163L,
#      166L, 172L, 173L, 174L, 175L, 192L, 193L, 194L, 195L, 198L,
#      212L, 213L, 214L, 215L, 221L, 233L, 234L, 235L, 236L, 237L,
#      305L, 347L, 440L, 463L, 468L, 479L, 508L, 518L, 525L, 526L,
#      552L, 553L, 554L, 555L, 562L, 565L, 574L, 577L, 578L, 580L,
#      591L, 594L, 603L, 604L, 617L, 618L, 619L, 620L, 621L, 622L,
#      626L, 629L, 630L, 631L, 635L, 642L, 643L, 644L, 645L, 646L,
#      647L, 648L, 655L, 656L, 657L, 658L, 664L, 667L, 668L, 669L,
#      670L, 671L, 672L, 673L, 675L, 676L, 680L, 681L, 682L, 683L,
#      685L, 692L, 693L, 694L, 695L, 696L, 697L, 698L, 700L, 701L,
#      704L, 705L, 706L, 707L, 708L, 709L, 710L, 722L, 724L, 727L,
#      730L, 731L, 732L, 733L, 734L, 735L, 755L), c(14L, 19L, 25L,
#      36L, 39L, 52L, 53L, 81L, 86L, 103L, 104L, 112L, 116L, 117L,
#      120L, 130L, 133L, 134L, 135L, 136L, 137L, 138L, 148L, 152L,
#      153L, 155L, 156L, 162L, 163L, 164L, 166L, 172L, 173L, 174L,
#      175L, 192L, 193L, 194L, 196L, 198L, 201L, 212L, 213L, 214L,
#      215L, 221L, 234L, 235L, 236L, 237L, 265L, 305L, 319L, 347L,
#      410L, 440L, 463L, 468L, 495L, 501L, 504L, 508L, 518L, 526L,
#      533L, 540L, 553L, 555L, 562L, 565L, 574L, 577L, 578L, 591L,
#      593L, 594L, 603L, 604L, 617L, 618L, 619L, 620L, 621L, 622L,
#      624L, 626L, 629L, 630L, 631L, 642L, 643L, 644L, 645L, 646L,
#      647L, 648L, 655L, 656L, 657L, 658L, 664L, 667L, 668L, 669L,
#      670L, 671L, 672L, 673L, 675L, 676L, 680L, 681L, 682L, 683L,
#      687L, 692L, 693L, 694L, 695L, 696L, 697L, 698L, 700L, 704L,
#      705L, 706L, 707L, 708L, 710L, 722L, 724L, 727L, 730L, 731L,
#      732L, 733L, 734L, 735L, 755L), c(7L, 25L, 31L, 36L, 39L,
#      52L, 53L, 81L, 86L, 103L, 104L, 111L, 112L, 116L, 117L, 120L,
#      129L, 130L, 133L, 134L, 135L, 136L, 137L, 138L, 148L, 152L,
#      153L, 155L, 156L, 157L, 162L, 163L, 164L, 166L, 173L, 174L,
#      175L, 192L, 193L, 194L, 198L, 199L, 204L, 212L, 213L, 214L,
#      215L, 234L, 235L, 236L, 237L, 305L, 319L, 347L, 408L, 410L,
#      422L, 440L, 463L, 468L, 495L, 501L, 503L, 504L, 508L, 518L,
#      526L, 546L, 553L, 555L, 562L, 565L, 574L, 577L, 591L, 592L,
#      593L, 594L, 595L, 596L, 603L, 604L, 617L, 618L, 619L, 620L,
#      621L, 622L, 624L, 626L, 629L, 630L, 635L, 642L, 643L, 644L,
#      645L, 646L, 647L, 648L, 649L, 655L, 656L, 664L, 667L, 668L,
#      669L, 670L, 671L, 672L, 673L, 675L, 676L, 680L, 682L, 689L,
#      692L, 693L, 694L, 695L, 696L, 697L, 698L, 700L, 704L, 705L,
#      706L, 707L, 708L, 710L, 722L, 724L, 730L, 731L, 732L, 733L,
#      734L, 735L, 755L, 757L), c(4L, 7L, 10L, 16L, 19L, 26L, 31L,
#      39L, 52L, 53L, 55L, 67L, 70L, 80L, 81L, 109L, 111L, 112L,
#      122L, 127L, 129L, 130L, 136L, 137L, 143L, 148L, 153L, 161L,
#      162L, 163L, 164L, 166L, 167L, 170L, 172L, 174L, 186L, 189L,
#      193L, 196L, 198L, 199L, 201L, 204L, 209L, 220L, 234L, 235L,
#      237L, 245L, 251L, 255L, 256L, 265L, 276L, 282L, 288L, 291L,
#      297L, 305L, 319L, 320L, 327L, 364L, 386L, 389L, 392L, 394L,
#      408L, 422L, 430L, 440L, 448L, 463L, 468L, 471L, 495L, 497L,
#      501L, 502L, 504L, 507L, 508L, 509L, 511L, 518L, 524L, 526L,
#      527L, 533L, 541L, 546L, 548L, 555L, 562L, 565L, 574L, 577L,
#      586L, 594L, 603L, 604L, 608L, 610L, 617L, 619L, 620L, 621L,
#      626L, 631L, 642L, 643L, 644L, 645L, 646L, 647L, 654L, 655L,
#      664L, 667L, 668L, 669L, 670L, 671L, 672L, 673L, 676L, 689L,
#      692L, 693L, 694L, 695L, 696L, 697L, 698L, 700L, 704L, 707L,
#      710L, 722L, 724L, 732L, 746L, 751L, 753L, 756L, 757L, 758L
#      ))
#  
#  
#  # generate solutions
#  for (i in seq_along(space.targets)) {
#  	solutions[[i]] <- update(cs_rs_space, space.target=rep(space.targets[i],4), solve=FALSE)
#  	solutions[[i]] <- solve(solutions[[i]], b=selections[[i]])
#  }

