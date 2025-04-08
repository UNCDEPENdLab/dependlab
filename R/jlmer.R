#' Replacement for lmer function that uses Julia for fitting the model
#' @param formula a two-sided linear formula object describing both the fixed-effects and random-effects
#'   part of the model, with the response on the left of a ~ operator and the terms, 
#'   separated by + operators, on the right. Random-effects terms are distinguished by vertical bars (|)
#'   separating expressions for design matrices from grouping factors. Two vertical bars (||) can be used to
#'   specify multiple uncorrelated random effects for the same grouping variable.
#' @param data a data frame containing the variables named in formula.
#' @param REML logical scalar - Should the estimates be chosen to optimize the REML
#'   criterion (as opposed to the log-likelihood)?
#' @param JULIA_HOME the location of the Julia installation on your machine. If NULL, JuliaCall::julia_setup
#'   will search for the location, but may not find it.
#' @details
#'   The MixedModels package in Julia is extremely fast relative to lme4. Thus, this is a drop-in wrapper
#'   for lmer. It uses the JellyMe4 package in Julia to return an lmerMod object that can then be passed to
#'   all lme4-compatible helper functions, such as those in ggeffects or emmeans.
#'   
#'   This function requires that MixedModels, RCall, and JellyMe4 all be installed in your Julia environment.
#' @return An object of class merMod (more specifically, an object of subclass lmerMod), for which many
#'   methods are available (e.g. methods(class="merMod"))
#' @importFrom JuliaCall julia_setup julia_command julia_assign julia_eval
#' @importFrom glue glue
jlmer <- function(formula=NULL, data=NULL, REML=TRUE, JULIA_HOME=NULL) {
  julia <- julia_setup(JULIA_HOME=JULIA_HOME)
  julia_command("using MixedModels, RCall, JellyMe4")
  julia_assign("data", data) # put data into environment
  
  if (is.character(formula)) formula <- as.formula(formula) # force type conversion
  julia_assign("formula", formula) # put formula into environment
  
  REML <- ifelse(isTRUE(REML), "true", "false")
  
  julia_command(glue("m = fit(MixedModel, formula, data, REML={REML});"))
  julia_eval("robject(:lmerMod, (m, data));", need_return="R") # return the wrapped lmer R object
  
  #### Deprecated approach with character formula
  # ensure that we have a character string formula
  # if (inherits(formula, "formula")) formula <- paste(deparse(formula), collapse = " ")
  
  # version with character formula, but (1|id:session) syntax, with colon, doesn't work
  # to maximize inter-operability with R syntax, use formula assignment above, which will convert
  # : to & in random effects specification
  #julia_command(glue("m = fit(MixedModel, @formula({formula}), data, REML={REML});"))
  
}
