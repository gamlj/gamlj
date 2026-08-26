### Shared SmartTable initialization helpers

init_smart_emmeans <- function(objects, results, runner, options) {
    aSmartObj <- SmartArray$new(results$emmeans, runner)
    aSmartObj$activated <- is.something(options$emmeans)
    aSmartObj$expandOnInit <- TRUE
    if (options$.caller %in% c("glm", "glmer")) {
        aSmartObj$expandFrom <- 2
    }
    aSmartObj$combineBelow <- "new!"
    aSmartObj$spaceBy <- "new!"
    aSmartObj$ci("est", options$ci_width)
    if (identical(options$.caller, "lmer")) {
        aSmartObj$hideOn <- list(df = Inf)
    }
    ladd(objects) <- aSmartObj
    objects
}

init_smart_simple_effects <- function(objects, results, runner, options) {
    activated <- is.something(options$simple_x) & is.something(options$simple_mods)
    combine_below <- 1:(length(options$simple_mods) - 1)
    space_by <- length(options$simple_mods) - 1
    is_glm <- options$.caller %in% c("glm", "glmer")
    is_mixed <- options$.caller %in% c("lmer", "glmer")

    aSmartObj <- SmartTable$new(results$simpleEffects$anova, runner)
    aSmartObj$activated <- activated
    aSmartObj$expandOnInit <- TRUE
    aSmartObj$expandSuperTitle <- "Moderator"
    aSmartObj$key <- options$simple_x
    aSmartObj$combineBelow <- combine_below
    aSmartObj$spaceBy <- space_by
    if (is_mixed) {
        aSmartObj$hideOn <- list(df2 = Inf)
    }
    ladd(objects) <- aSmartObj

    aSmartObj <- SmartTable$new(results$simpleEffects$coefficients, runner)
    aSmartObj$activated <- activated
    aSmartObj$expandOnInit <- TRUE
    if (is_glm) {
        aSmartObj$expandFrom <- 2
    }
    aSmartObj$expandSuperTitle <- "Moderator"
    aSmartObj$key <- options$simple_x
    aSmartObj$ci("est", options$ci_width)
    if (is_glm) {
        aSmartObj$ci("expb", width = options$ci_width, format = "Exp(B) {}% Confidence Intervals")
    }
    aSmartObj$combineBelow <- combine_below
    aSmartObj$spaceBy <- space_by
    if (is_mixed) {
        aSmartObj$hideOn <- list(df = Inf)
    }
    ladd(objects) <- aSmartObj

    aSmartObj <- SmartArray$new(results$simpleInteractions, runner)
    aSmartObj$activated <- options$simple_interactions & is.something(options$simple_x) & length(options$simple_mods) > 1
    aSmartObj$expandOnInit <- TRUE
    aSmartObj$expandSuperTitle <- "Moderator"
    aSmartObj$ci("est", options$ci_width)
    aSmartObj$combineBelow <- "new!"
    aSmartObj$spaceBy <- "new!"
    ladd(objects) <- aSmartObj

    objects
}

init_smart_tables <- function(objects, mute) {
    for (tab in objects) {
        tab$initTable()
        tab$mutenotes <- mute
    }
    objects
}
