# Set up learners ----
lrn_cart = lrn("regr.rpartMod", id = "lrn_cart")
lrn_ctree = lrn("regr.ctreeMod", id = "lrn_ctree")
lrn_lmertree_it = lrn("regr.glmertree", id = "lrn_lmertree_it") # singular boundary warning
lrn_lmertree_i = lrn("regr.glmertree", id = "lrn_lmertree_i")
lrn_lmertree_t = lrn("regr.glmertree", id = "lrn_lmertree_t")
lrn_reemtree_it = lrn("regr.reemtree", id = "lrn_reemtree_it")
lrn_reemtree_i = lrn("regr.reemtree",  id = "lrn_reemtree_i")
lrn_reemtree_t = lrn("regr.reemtree", id = "lrn_reemtree_t")
 
# Set up learner hyperparameters (fixed and tunable) ----
cp_lower = 0.001
cp_upper = 0.1
minbucket_lower = 5
minbucket_upper = 20
alpha_lower = 0.01 
alpha_upper = 0.1
mincriterion_upper = 1-alpha_lower
mincriterion_lower = 1-alpha_upper
include.partitioning.vars_expr = "palliativephase|ipos|age|cogn|akps"
maxdepth = 4
minsplit = 20 

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


## lrn_cart
lrn_cart.hp = list(
  fixed = list( 
    include.partitioning.vars_expr = include.partitioning.vars_expr,
    maxdepth = maxdepth,
    minsplit = minsplit,
    xval = 0),  
  tunable = tunable_hp_ps.cp_minbucket$clone()
)
## lrn_ctree
lrn_ctree.hp = list(
  fixed = list( 
    include.partitioning.vars_expr = include.partitioning.vars_expr,
    maxdepth = maxdepth,
    minsplit = minsplit),
  tunable = tunable_hp_ps.alpha_minbucket$clone()
)

## lrn_lmertree_it
lrn_lmertree_it.hp = list(
  fixed = list( 
    formula.random = "(1|team_id/companion_id)",
    include.partitioning.vars_expr = include.partitioning.vars_expr,
    re.form = NA,
    maxdepth = maxdepth,
    minsplit = minsplit),
  tunable = tunable_hp_ps.alpha_minbucket$clone()
)
## lrn_lmertree_i
lrn_lmertree_i.hp = lrn_lmertree_it.hp
lrn_lmertree_i.hp$fixed$formula.random = "(1|companion_id)"
lrn_lmertree_i.hp$tunable = tunable_hp_ps.alpha_minbucket$clone()

## lrn_lmertree_t
lrn_lmertree_t.hp = lrn_lmertree_it.hp
lrn_lmertree_t.hp$fixed$formula.random = "(1|team_id)"
lrn_lmertree_t.hp$tunable = tunable_hp_ps.alpha_minbucket$clone()


## lrn_reemtree_it
lrn_reemtree_it.hp = list(
  fixed = list( 
    include.partitioning.vars_expr = include.partitioning.vars_expr,
    random = ~1|team_id/companion_id,
    grouping = "", 
    EstimateRandomEffects = FALSE, 
    cv = FALSE,
    maxdepth = maxdepth,
    minsplit = minsplit),
  tunable = tunable_hp_ps.cp_minbucket$clone()
)

## lrn_reemtree_i
lrn_reemtree_i.hp = lrn_reemtree_it.hp
lrn_reemtree_i.hp$fixed$random =  ~1|companion_id
lrn_reemtree_i.hp$tunable = tunable_hp_ps.cp_minbucket$clone()

## lrn_reemtree_t
lrn_reemtree_t.hp = lrn_reemtree_it.hp
lrn_reemtree_t.hp$fixed$random =  ~1|team_id
lrn_reemtree_t.hp$tunable = tunable_hp_ps.cp_minbucket$clone()



# Assign fixed hyperparameter values to learners ----
lrn_cart$param_set$values = lrn_cart.hp$fixed
lrn_ctree$param_set$values = lrn_ctree.hp$fixed
lrn_lmertree_it$param_set$values = lrn_lmertree_it.hp$fixed
lrn_lmertree_i$param_set$values = lrn_lmertree_i.hp$fixed
lrn_lmertree_t$param_set$values = lrn_lmertree_t.hp$fixed
lrn_reemtree_it$param_set$values = lrn_reemtree_it.hp$fixed
lrn_reemtree_i$param_set$values = lrn_reemtree_i.hp$fixed
lrn_reemtree_t$param_set$values = lrn_reemtree_t.hp$fixed


# Generate list of learners with assigned fixed hps ----
learners_default = list(lrn_cart,lrn_ctree,lrn_lmertree_it,lrn_lmertree_i,lrn_lmertree_t, lrn_reemtree_it,
                     lrn_reemtree_i,lrn_reemtree_t)
names(learners_default) = c("lrn_cart","lrn_ctree","lrn_lmertree_it","lrn_lmertree_i","lrn_lmertree_t", "lrn_reemtree_it",
                         "lrn_reemtree_i","lrn_reemtree_t")

# only use subset of all initially specified learners (not those with _it and _i) -> 4 learners in total
learners_default = learners_default[c("lrn_cart","lrn_ctree","lrn_lmertree_t","lrn_reemtree_t")]

# Generate list of search spaces for tunable hps ----
lrn_cart.hp$tunable$set_id = "lrn_cart"
lrn_ctree.hp$tunable$set_id = "lrn_ctree"
lrn_lmertree_it.hp$tunable$set_id = "lrn_lmertree_it"
lrn_lmertree_i.hp$tunable$set_id = "lrn_lmertree_i"
lrn_lmertree_t.hp$tunable$set_id = "lrn_lmertree_t"
lrn_reemtree_it.hp$tunable$set_id = "lrn_reemtree_it"
lrn_reemtree_i.hp$tunable$set_id = "lrn_reemtree_i"
lrn_reemtree_t.hp$tunable$set_id = "lrn_reemtree_t"

learners_hp_searchspace_default = paste0(names(learners_default),".hp") %>% 
  purrr::map(~ ParamSetCollection$new(list(get(.x)$tunable)))
names(learners_hp_searchspace_default) = names(learners_default)

# Remove unnecessary objects ----
rm(# parameters 
  "tunable_hp_ps.alpha_minbucket","tunable_hp_ps.cp_minbucket","tunable_hp_ps.mincriterion_minbucket",
   "minbucket_lower","minbucket_upper","mincriterion_lower","mincriterion_upper",
   "alpha_lower","alpha_upper","cp_lower","cp_upper", "include.partitioning.vars_expr","maxdepth","minsplit",
   # hp lists
   "lrn_ctree.hp","lrn_lmertree_t.hp","lrn_lmertree_i.hp","lrn_lmertree_it.hp",
  "lrn_reemtree_t.hp","lrn_reemtree_i.hp","lrn_reemtree_it.hp","lrn_cart.hp",
   # individual learners
   "lrn_ctree", "lrn_lmertree_t","lrn_lmertree_i","lrn_lmertree_it",
  "lrn_reemtree_t","lrn_reemtree_i","lrn_reemtree_it","lrn_cart")


