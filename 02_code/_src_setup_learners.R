# Set up learners ----
lrn_rpart = lrn("regr.rpartMod", id = "lrn_rpart")
lrn_ctree = lrn("regr.ctreeMod", id = "lrn_ctree")
lrn_glmertree_if = lrn("regr.glmertree", id = "lrn_glmertree_if")
lrn_glmertree_i = lrn("regr.glmertree", id = "lrn_glmertree_i")
lrn_glmertree_f = lrn("regr.glmertree", id = "lrn_glmertree_f")
lrn_reemtree_if = lrn("regr.reemtree", id = "lrn_reemtree_if")
lrn_reemtree_i = lrn("regr.reemtree",  id = "lrn_reemtree_i")
lrn_reemtree_f = lrn("regr.reemtree", id = "lrn_reemtree_f")
lrn_reemctree_if = lrn("regr.reemctree", id = "lrn_reemctree_if")
lrn_reemctree_i = lrn("regr.reemctree", id = "lrn_reemctree_i")
lrn_reemctree_f = lrn("regr.reemctree", id = "lrn_reemctree_f")
 
# Set up learner hyperparameters (fixed and tunable) ----
cp_lower = 0.001
cp_upper = 0.1
minbucket_lower = 10
minbucket_upper = 20
alpha_lower = 0.01 
alpha_upper = 0.1
mincriterion_upper = 1-alpha_lower
mincriterion_lower = 1-alpha_upper
exclude.from.partitioning.vars = c("companion_id", "team_id")
maxdepth = 4

tunable_hp_ps.cp_minbucket = paradox::ps(
  cp = p_dbl(cp_lower, cp_upper),
  minbucket = p_int(minbucket_lower, minbucket_upper)
)

tunable_hp_ps.alpha_minbucket = paradox::ps(
  alpha = p_dbl(alpha_lower, alpha_upper),
  minbucket = p_int(minbucket_lower, minbucket_upper)
)

tunable_hp_ps.mincriterion_minbucket = paradox::ps(
  mincriterion = p_dbl(mincriterion_lower, mincriterion_upper),
  minbucket = p_int(minbucket_lower, minbucket_upper)
)


## lrn_rpart
lrn_rpart.hp = list(
  fixed = list( 
    exclude.from.partitioning.vars = exclude.from.partitioning.vars,
    maxdepth = maxdepth,
    xval = 0),  
  tunable = tunable_hp_ps.cp_minbucket$clone()
)
## lrn_ctree
lrn_ctree.hp = list(
  fixed = list( 
    exclude.from.partitioning.vars = exclude.from.partitioning.vars,
    maxdepth = maxdepth),
  tunable = tunable_hp_ps.alpha_minbucket$clone()
)

## lrn_glmertree_if
lrn_glmertree_if.hp = list(
  fixed = list( 
    formula.random = "(1|team_id/companion_id)",
    exclude.from.partitioning.vars = exclude.from.partitioning.vars,
    re.form = NA,
    maxdepth = maxdepth),
  tunable = tunable_hp_ps.alpha_minbucket$clone()
)
## lrn_glmertree_i
lrn_glmertree_i.hp = lrn_glmertree_if.hp
lrn_glmertree_i.hp$fixed$formula.random = "(1|companion_id)"
lrn_glmertree_i.hp$tunable = tunable_hp_ps.alpha_minbucket$clone()

## lrn_glmertree_f
lrn_glmertree_f.hp = lrn_glmertree_if.hp
lrn_glmertree_f.hp$fixed$formula.random = "(1|team_id)"
lrn_glmertree_f.hp$tunable = tunable_hp_ps.alpha_minbucket$clone()


## lrn_reemtree_if
lrn_reemtree_if.hp = list(
  fixed = list( 
    exclude.from.partitioning.vars = exclude.from.partitioning.vars,
    random = ~1|team_id/companion_id,
    grouping = "", 
    EstimateRandomEffects = FALSE, 
    cv = FALSE,
    maxdepth = maxdepth),
  tunable = tunable_hp_ps.cp_minbucket$clone()
)

## lrn_reemtree_i
lrn_reemtree_i.hp = lrn_reemtree_if.hp
lrn_reemtree_i.hp$fixed$random =  ~1|companion_id
lrn_reemtree_i.hp$tunable = tunable_hp_ps.cp_minbucket$clone()

## lrn_reemtree_f
lrn_reemtree_f.hp = lrn_reemtree_if.hp
lrn_reemtree_f.hp$fixed$random =  ~1|team_id
lrn_reemtree_f.hp$tunable = tunable_hp_ps.cp_minbucket$clone()


## lrn_reemctree_if
lrn_reemctree_if.hp = list(
  fixed = list( 
    exclude.from.partitioning.vars = exclude.from.partitioning.vars,
    random = ~1|team_id/companion_id,
    maxdepth = maxdepth),
  tunable = tunable_hp_ps.mincriterion_minbucket$clone()
)
## lrn_reemctree_i
lrn_reemctree_i.hp = lrn_reemctree_if.hp
lrn_reemctree_i.hp$fixed$random =  ~1|companion_id
lrn_reemctree_i.hp$tunable = tunable_hp_ps.mincriterion_minbucket$clone()


## lrn_reemctree_f
lrn_reemctree_f.hp = lrn_reemctree_if.hp
lrn_reemctree_f.hp$fixed$random =  ~1|team_id
lrn_reemctree_f.hp$tunable = tunable_hp_ps.mincriterion_minbucket$clone()



# Assign fixed hyperparameter values to learners ----
lrn_rpart$param_set$values = lrn_rpart.hp$fixed
lrn_ctree$param_set$values = lrn_ctree.hp$fixed
lrn_glmertree_if$param_set$values = lrn_glmertree_if.hp$fixed
lrn_glmertree_i$param_set$values = lrn_glmertree_i.hp$fixed
lrn_glmertree_f$param_set$values = lrn_glmertree_f.hp$fixed
lrn_reemtree_if$param_set$values = lrn_reemtree_if.hp$fixed
lrn_reemtree_i$param_set$values = lrn_reemtree_i.hp$fixed
lrn_reemtree_f$param_set$values = lrn_reemtree_f.hp$fixed
lrn_reemctree_if$param_set$values = lrn_reemctree_if.hp$fixed
lrn_reemctree_i$param_set$values = lrn_reemctree_i.hp$fixed
lrn_reemctree_f$param_set$values = lrn_reemctree_f.hp$fixed


# Generate list of learners with assigned fixed hps ----
learners = list(lrn_rpart,lrn_ctree,lrn_glmertree_if,lrn_glmertree_i,lrn_glmertree_f, lrn_reemtree_if,
                     lrn_reemtree_i,lrn_reemtree_f,lrn_reemctree_if,lrn_reemctree_i,lrn_reemctree_f)
names(learners) = c("lrn_rpart","lrn_ctree","lrn_glmertree_if","lrn_glmertree_i","lrn_glmertree_f", "lrn_reemtree_if",
                         "lrn_reemtree_i","lrn_reemtree_f","lrn_reemctree_if","lrn_reemctree_i","lrn_reemctree_f")

# Generate list of search spaces for tunable hps ----
lrn_rpart.hp$tunable$set_id = "lrn_rpart"
lrn_ctree.hp$tunable$set_id = "lrn_ctree"
lrn_glmertree_if.hp$tunable$set_id = "lrn_glmertree_if"
lrn_glmertree_i.hp$tunable$set_id = "lrn_glmertree_i"
lrn_glmertree_f.hp$tunable$set_id = "lrn_glmertree_f"
lrn_reemtree_if.hp$tunable$set_id = "lrn_reemtree_if"
lrn_reemtree_i.hp$tunable$set_id = "lrn_reemtree_i"
lrn_reemtree_f.hp$tunable$set_id = "lrn_reemtree_f"
lrn_reemctree_if.hp$tunable$set_id = "lrn_reemctree_if"
lrn_reemctree_i.hp$tunable$set_id = "lrn_reemctree_i"
lrn_reemctree_f.hp$tunable$set_id = "lrn_reemctree_f"

learners_hp_searchspace = paste0(names(learners),".hp") %>% 
  purrr::map(~ ParamSetCollection$new(list(get(.x)$tunable)))


# Remove unnecessary objects ----
rm(# parameters 
  "tunable_hp_ps.alpha_minbucket","tunable_hp_ps.cp_minbucket","tunable_hp_ps.mincriterion_minbucket",
   "minbucket_lower","minbucket_upper","mincriterion_lower","mincriterion_upper",
   "alpha_lower","alpha_upper","cp_lower","cp_upper", "exclude.from.partitioning.vars","maxdepth",
   # hp lists
   "lrn_ctree.hp","lrn_glmertree_f.hp","lrn_glmertree_i.hp","lrn_glmertree_if.hp","lrn_reemctree_f.hp","lrn_reemctree_i.hp",
   "lrn_reemctree_if.hp","lrn_reemtree_f.hp","lrn_reemtree_i.hp","lrn_reemtree_if.hp","lrn_rpart.hp",
   # individual learners
   "lrn_ctree", "lrn_glmertree_f","lrn_glmertree_i","lrn_glmertree_if","lrn_reemctree_f","lrn_reemctree_i",
   "lrn_reemctree_if","lrn_reemtree_f","lrn_reemtree_i","lrn_reemtree_if","lrn_rpart")

#################################################################
search_space1 = ps(
  regr.rpart.cp = p_dbl(lower = 4, 
                        upper = 5)
)
search_space2 = ps(
  regr.rpart.minbucket = p_int(lower = 3, 
                               upper = 3)
)
search_space = ps(
  regr.rpart.minbucket = p_int(lower = 3, 
                               upper = 3),
  regr.rpart.cp = p_dbl(lower = 4, 
                        upper = 5)
)
search_space2$add(search_space1)

names(search_space1$lower)
search_space$lower
search_space$assert_values()


all.equal(tuner_params,
search_space)
