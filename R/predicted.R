predicted <- function(model, ...) UseMethod(".predicted")

.predicted.default <- function(model, obj) {
    root <- toupper(abbreviate(obj$options$model_type))
    preds <- stats::predict(model, type = obj$infomatic$predict)
    pdf <- data.frame(PRED = preds, row.names = rownames(insight::get_data(model, source = "frame")))

    for (p in seq_len(ncol(pdf))) {
        if (is.factor(pdf[[p]])) {
            pdf[[p]] <- fromb64(as.character(pdf[[p]]))
        }
    }

    names(pdf) <- paste0(paste0(root, "_", obj$options$dep), "_", fromb64(names(pdf)))
    pdf
}

.predicted.lmerModLmerTest <- function(model, obj) {
    root <- toupper(abbreviate(obj$options$model_type))
    rpreds <- stats::predict(model)
    fpreds <- stats::predict(model, re.form = NA)
    pdf <- data.frame(RPRED = rpreds, FPRED = fpreds, row.names = rownames(insight::get_data(model, source = "frame")))

    for (p in seq_len(ncol(pdf))) {
        if (is.factor(pdf[[p]])) {
            pdf[[p]] <- fromb64(as.character(pdf[[p]]))
        }
    }

    names(pdf) <- paste0(paste0("MIXED_", obj$options$dep), "_", fromb64(names(pdf)))
    pdf
}
