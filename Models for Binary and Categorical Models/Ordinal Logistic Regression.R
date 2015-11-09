

######################################################################
###########	Ordinal Logistic Regression model	##############
######################################################################

###	Ordered Response
###	yi=sum_j{1(yi*>=roj)}
###	where yi*=xi*b+ui, i=1,...,n


###	EX1

dat <- read.dta("http://www.ats.ucla.edu/stat/data/ologit.dta")
m <- polr(apply ~ pared + public + gpa, data = dat, Hess = TRUE)
summary(m)


###Source Code in R#####
function (formula, data, weights, start, ..., subset, na.action, 
    contrasts = NULL, Hess = FALSE, model = TRUE, method = c("logistic", 
        "probit", "loglog", "cloglog", "cauchit")) 
{
    m <- match.call(expand.dots = FALSE)
    method <- match.arg(method)
    if (is.matrix(eval.parent(m$data))) 
        m$data <- as.data.frame(data)
    m$start <- m$Hess <- m$method <- m$model <- m$... <- NULL
    m[[1L]] <- quote(stats::model.frame)
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    x <- model.matrix(Terms, m, contrasts)
    xint <- match("(Intercept)", colnames(x), nomatch = 0L)
    n <- nrow(x)
    pc <- ncol(x)
    cons <- attr(x, "contrasts")
    if (xint > 0L) {
        x <- x[, -xint, drop = FALSE]
        pc <- pc - 1L
    }
    else warning("an intercept is needed and assumed")
    wt <- model.weights(m)
    if (!length(wt)) 
        wt <- rep(1, n)
    offset <- model.offset(m)
    if (length(offset) <= 1L) 
        offset <- rep(0, n)
    y <- model.response(m)
    if (!is.factor(y)) 
        stop("response must be a factor")
    lev <- levels(y)
    llev <- length(lev)
    if (llev <= 2L) 
        stop("response must have 3 or more levels")
    y <- unclass(y)
    q <- llev - 1L
    Y <- matrix(0, n, q)
    if (missing(start)) {
        q1 <- llev%/%2L
        y1 <- (y > q1)
        X <- cbind(Intercept = rep(1, n), x)
        fit <- switch(method, logistic = glm.fit(X, y1, wt, family = binomial(), 
            offset = offset), probit = glm.fit(X, y1, wt, family = binomial("probit"), 
            offset = offset), loglog = glm.fit(X, y1, wt, family = binomial("probit"), 
            offset = offset), cloglog = glm.fit(X, y1, wt, family = binomial("probit"), 
            offset = offset), cauchit = glm.fit(X, y1, wt, family = binomial("cauchit"), 
            offset = offset))
        if (!fit$converged) 
            stop("attempt to find suitable starting values failed")
        coefs <- fit$coefficients
        if (any(is.na(coefs))) {
            warning("design appears to be rank-deficient, so dropping some coefs")
            keep <- names(coefs)[!is.na(coefs)]
            coefs <- coefs[keep]
            x <- x[, keep[-1L], drop = FALSE]
            pc <- ncol(x)
        }
        logit <- function(p) log(p/(1 - p))
        spacing <- logit((1L:q)/(q + 1L))
        if (method != "logistic") 
            spacing <- spacing/1.7
        gammas <- -coefs[1L] + spacing - spacing[q1]
        start <- c(coefs[-1L], gammas)
    }
    else if (length(start) != pc + q) 
        stop("'start' is not of the correct length")
    ans <- polr.fit(x, y, wt, start, offset, method, hessian = Hess, 
        ...)
    beta <- ans$coefficients
    zeta <- ans$zeta
    deviance <- ans$deviance
    res <- ans$res
    niter <- c(f.evals = res$counts[1L], g.evals = res$counts[2L])
    eta <- if (pc) 
        offset + drop(x %*% beta)
    else offset + rep(0, n)
    pfun <- switch(method, logistic = plogis, probit = pnorm, 
        loglog = pgumbel, cloglog = pGumbel, cauchit = pcauchy)
    cumpr <- matrix(pfun(matrix(zeta, n, q, byrow = TRUE) - eta), 
        , q)
    fitted <- t(apply(cumpr, 1L, function(x) diff(c(0, x, 1))))
    dimnames(fitted) <- list(row.names(m), lev)
    fit <- list(coefficients = beta, zeta = zeta, deviance = deviance, 
        fitted.values = fitted, lev = lev, terms = Terms, df.residual = sum(wt) - 
            pc - q, edf = pc + q, n = sum(wt), nobs = sum(wt), 
        call = match.call(), method = method, convergence = res$convergence, 
        niter = niter, lp = eta)
    if (Hess) {
        dn <- c(names(beta), names(zeta))
        H <- res$hessian
        dimnames(H) <- list(dn, dn)
        fit$Hessian <- H
    }
    if (model) 
        fit$model <- m
    fit$na.action <- attr(m, "na.action")
    fit$contrasts <- cons
    fit$xlevels <- .getXlevels(Terms, m)
    class(fit) <- "polr"
    fit
}
<bytecode: 0x000000000a4bfb40>
<environment: namespace:MASS>
