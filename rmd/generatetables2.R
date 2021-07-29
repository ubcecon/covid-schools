rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
source(paste(rootdir,"R/utils.R",sep="/")) # for panellag


library(lfe)
library(speedglm)
library(boot)
library(parallel)  
detectCores()

#' Wrapper for stargazer that saves latex output and displays html table
#'
regtable <- function(res, landscape=FALSE, texfile=NULL,
                     add.lines=NULL, ...) {
  if (sgtype=="latex" & !is.null(add.lines)) {
    tbl  <- capture.output(stargazer(res, type=sgtype, style=sgstyle,
                                     df=FALSE, header=FALSE,
                                     add.lines=add.lines,
                                     no.space=TRUE,
                                     p=c(0.1,0.05,0.01), ...))
  } else {
    # add.lines doesn't work for other output types ...
    tbl <- capture.output(stargazer(res, type=sgtype, style=sgstyle,
                                    df=FALSE, header=FALSE,
                                    p=c(0.1,0.05,0.01),
                                    ...))
  }
  if (landscape && knitr::is_latex_output()) {
    cat("\\afterpage{
    \\clearpage
    \\thispagestyle{empty}
    \\begin{landscape}
")
  }
  tbl <- relabel(tbl)
  tbl <- gsub("Model (\\d)", "\\(\\1\\)", tbl)
  cat(paste(relabel(tbl),collapse="\n"))
  if (landscape && knitr::is_latex_output()) {
    cat("    \\end{landscape}
    \\clearpage
}")
  }
  if (!is.null(texfile)) {
    tbl  <- capture.output(stargazer(res, type="latex", style=sgstyle,
                                     df=FALSE, header=FALSE,
                                     add.lines=add.lines,
                                     no.space=TRUE,
                                     #p=c(0.1,0.05,0.01),
                                     ...))
    tbl <- relabel(tbl)
    tbl <- gsub("Model (\\d)", "\\(\\1\\)", tbl)
    tbl <- gsub("No","Yes", tbl)
    cat(paste(tbl[c(-1,-2,-3,-4, -length(tbl))], collapse="\n"), file=texfile)
  }
  return(tbl)
};

#' Create formula for use with felm
#'
createfmla <- function(yvar, xvars, interactions, fe = "0", iv="0", cluster) { #+date") {
  rhs <- paste(xvars, collapse=" + ")
  if (!is.null(interactions)) {
    rhs <- paste(rhs, " + (", paste(interactions[[1]], collapse = " + "), ")*(",
               paste(interactions[[2]], collapse = " + "), ")")
  }
  if (!is.null(fe)) {
    rhs <- paste(rhs," | ", fe, " | ", iv , " | ", paste(cluster, collapse=" + "))
  }
  return(list(as.formula(paste(yvar, "~", rhs, sep=" ")), rhs))
}

policyreg <- function(df, # data
                      yvar, # outcome
                      pols, # policies
                      bvars, # behavior (NULL if not behavior)
                      x, # controls
                      interactions,
                      fe,
                      iv,
                      L=14,
                      cluster) { 
  if (L==0) {
    p <- pols
    b <- bvars
  } else {
    p <- sprintf("lag(%s, %d)", pols, L)
    b <- sprintf("lag(%s, %d)", bvars, L)
  }
  fmla <- createfmla(yvar, c(p, b, x), interactions, fe=fe, iv=iv, cluster)
  m <- felm(fmla[[1]], data=df, keepX=TRUE)
  w <- rep(1, length(p))
  peff <- c(sum(coef(m)[p]*w), sqrt(t(w) %*% (vcov(m)[p,p]) %*% w ))
  if (!is.null(bvars)) {
    w <- colMeans(subset(df, df$date>=as.Date("2020-04-01") &
                             df$date<=as.Date("2020-04-10"))[,bvars])
    beff <- c(sum(coef(m)[b]*w), sqrt(t(w) %*% (vcov(m)[b,b]) %*% w ))
  } else {
    beff <- NULL
  }
  return(list(reg=m, peff=peff, beff=beff))
}



##### DFE estimation based on cross-over jackknife
# Note: The output of this function facilitates calculation of bootstrap se
# source(paste(rootdir,"rmd/debiasedfe.R", sep="/"))
reg_bc <- function(df, # data
                   yvar, # dependent variable (1 of 4 bevahiors if pib)
                   pols, # policies
                   bvars, # behavior (NULL if not behavior)
                   x, # controls
                   interactions, 
                   iv, # iv for felm, if not needed just input "0"
                   L,
                   fe, 
                   cluster,
                   multiple = FALSE) {
  if (L == 0) {
    # This is for pib
    p <- pols
    b <- bvars
  } else {
    # This is for pbiy and piy
    p <- sprintf("lag(%s, %d)", pols, L)
    b <- sprintf("lag(%s, %d)", bvars, L)
  }
  # create regression formula
  xvars <- c(p, b, x)
  fmla <- createfmla(yvar, xvars,interactions, fe = fe, iv = iv, cluster=cluster)
  stopifnot(is.factor(df$fips)) # we need as.double(df$fips) to return values in 1:n
  unit <- as.double(df$fips)
  time <- as.double(df$date)
  n <- length(levels(df$fips))
  # Split by day. Data is linear so no bias coming from the time effects,
  # no cross section dependence as well.
  # Uncorrected and corrected estimator
  m <- felm(fmla[[1]], data = df) 
  coef_nobc <- coef(m)
  # Multiple split
  set.seed(123458) 
  s <- 5
  across <- 0 * coef(m)
  for (k in 1:s) {
    sample1 <- sample(n, ceiling(n/2), replace = FALSE)
    subsample1 <- (unit %in% sample1 & time <= median(time)) |
      (!(unit %in% sample1) & time >= median(time))
    subsample2 <- (unit %in% sample1 & time >= median(time)) |
      (!(unit %in% sample1) & time <= median(time)) 
    s1 <- df[subsample1,]
    s2 <- df[subsample2,]
    s1$fips <- as.character(s1$fips)
    s2$fips <- paste(as.character(s2$fips),"40",sep="")
    sdf <- rbind(s1, s2)
    sdf$fips <- as.factor(sdf$fips)
    cross <- felm(fmla[[1]], data = sdf)
    across <- across + coef(cross)/s 
  } 
  bc <- 2*coef(m) - across 
  w <- rep(1, length(p))
  peff <- c(sum(coef(m)[p]*w), sqrt(t(w) %*% (vcov(m)[p,p]) %*% w ))
  if (!is.null(bvars)) {c
    w <- colMeans(subset(df, df$date>=as.Date("2020-04-01") &
                           df$date<=as.Date("2020-04-10"))[,bvars])
    beff <- c(sum(coef(m)[b]*w), sqrt(t(w) %*% (vcov(m)[b,b]) %*% w ))
  } else {
    beff <- NULL
  }   
  return(list(reg=m, peff=peff, beff=beff, bc = bc))
} 

 

mainregressions <- function(df, # data
                       yvar,
                       plist, # policy variables
                       bvars, # behavior variables
                       infolist,
                       tvars,
                       xlist,
                       ilist,
                       ivlist,
                       fe,
                       L=14,
                       cluster = "fips",
                       debias = FALSE,
                       includepbiy=FALSE,
                       includepiy=TRUE,
                       includepib=TRUE,
                       includeip=FALSE
                       ) { 
    
  
  pols <- plist[[1]]
                
  stopifnot(length(xlist)==length(ilist))
  stopifnot(length(xlist)==length(ivlist))
  ijs <- expand.grid(1:length(plist), 1:length(xlist))
  
  if (debias) {
    reg <- reg_bc
  } else {
    reg <- policyreg
  } 
  
  if (includepbiy) {
    pbiy <- apply(ijs, 1, function(ij) { 
      # if (any(plist[[ij[1]]]=="pschoolfull")) {
      #   sdf <- subset(df,df$pschoolunknown>0)
      # } else {
      #   sdf <- df
      # }
      m <- reg(sdf, yvar, plist[[ij[1]]], bvars,
                c(sprintf("lag(%s, %d)", infolist[[ij[2]]], L),
                  tvars,
                  xlist[[ij[2]]]),
                ilist[[ij[2]]], fe=fe[[ij[2]]], iv[[ij[2]]], L=L, cluster=cluster)
      if (debias) { 
        k <- length(m$reg$beta)
        m$reg$beta[1:k] <- m$bc[1:k]
        m$reg$coefficients[1:k] <- m$bc[1:k]
        z <- m$reg$beta[1:k] /m$reg$cse[1:k]
        pval <- 2*pnorm(-abs(z))
        m$reg$ctval[1:k] <- z
        m$reg$cpval[1:k] <- pval
      }
      return(m)
    })
  } else {
    pbiy <- NULL
  } 
  
  if (includepiy) {
    piy <- apply(ijs, 1, function(ij) { 
      # if (any(plist[[ij[1]]]=="pschoolfull")) {
      #   sdf <- subset(df, df$portion.Unknown>0.5) 
      # } else {
      #   sdf <- df
      # }
      m <- reg(sdf, yvar, plist[[ij[1]]], NULL,
                c(sprintf("lag(%s, %d)", infolist[[ij[2]]], L),
                  tvars,
                  xlist[[ij[2]]]), 
             ilist[[ij[2]]], fe=fe[[ij[2]]], iv[[ij[2]]], L=L, cluster=cluster)
      if (debias) { 
        k <- length(m$reg$beta)
        m$reg$beta[1:k] <- m$bc[1:k]
        m$reg$coefficients[1:k] <- m$bc[1:k]
        z <- m$reg$beta[1:k] /m$reg$cse[1:k]
        pval <- 2*pnorm(-abs(z))
        m$reg$ctval[1:k] <- z
        m$reg$cpval[1:k] <- pval
      }
      return(m)
    }) 
  } else {
    piy <- NULL
  }

 
  if (includepib) {
    # make the timing the same as piy and piby
    sdf <- df
    sdf$month <- panellag(sdf$month,sdf$fips,sdf$date,-L) 
    sdf$week <- panellag(sdf$week,sdf$fips,sdf$date,-L)
    # adjust the unit so that regression coefficients are on the right orders
    vars <- c("college","school","gym","church","bar","restaurant","fullwork","partwork","home") 
    for(v in vars) {
      sdf[,v] <- sapply(sdf[,v], function(x) x*100) 
    }  
    # ijs <- expand.grid(1:length(bvars), 1)
    ijs <- expand.grid(1:length(plist),1:length(bvars))
    pib <- list()
    for (k in 1:length(xlist)) {  
      pib[[k]] <- apply(ijs, 1, function(ij) {
        m <- reg(sdf, bvars[ij[2]], plist[[ij[1]]], NULL,
                  c(infolist[[k]],
                    xlist[[k]]),
                  ilist[[k]], fe=fe[[k]], iv="0", L=0, cluster=cluster)
        if (debias) { 
          k <- length(m$reg$beta)
          m$reg$beta[1:k] <- m$bc[1:k]
          m$reg$coefficients[1:k] <- m$bc[1:k]
          z <- m$reg$beta[1:k] /m$reg$cse[1:k]
          pval <- 2*pnorm(-abs(z))
          m$reg$ctval[1:k] <- z
          m$reg$cpval[1:k] <- pval
        }
        return(m)
      })
    }
  } else {
    pib <- NULL
  }


  if (includeip) {
    ijs <- expand.grid(1:length(pols), 1:length(xlist))
    ip <- apply(ijs, 1, function(ij) {
      reg(df, pols[[ij[1]]],  NULL,  bvars,
                c(infolist[[ij[2]]],
                  xlist[[ij[2]]]),
                ilist[[ij[2]]], fe=fe[[ij[2]]], iv="0", L=L, cluster=cluster)
    }) 
  } else {
    ip <- NULL
  }

  return(list(pib=pib, pbiy=pbiy, piy=piy, ip=ip))
}



 

showhtmltables <- function(pib, pbiy, piy, ip=NULL) {

  omit <- c(countyvarregexp,
            "^month") #"testratedc:",)
  omit.labels <- c("county variables",
                   "Month : county variables")
   

  if (!(is.null(pib))) {
    cat("\n### Policies and Behavior\n")

    peff <- sapply(pib, function(x) x$peff[1])
    sep <- sapply(pib, function(x) x$peff[2])
    cnames <- sapply(pib, function(x) colnames(x$reg$response))
    stargazer(lapply(pib, function(x) x$reg),
              type="html",
              title="Policies and Behavior",
              #dep.var.labels=cnames,
              dep.var.labels.include=FALSE,
              column.labels=cnames,
              omit=omit,
              omit.labels=omit.labels,
              omit.stat=c("f", "ser"), model.names=FALSE,
              model.numbers=TRUE,
              add.lines = list("", "","",
                               "\\hline")
              # add.lines = list(c("sum Policy",sprintf("%.3f",peff)),
              #                  c("",sprintf("(%.3f)",sep)),
              #                  "\\hline")
              )

  }


  if (!(is.null(ip))) {
    cat("\n### Policies and Information\n")

    peff <- sapply(ip, function(x) x$peff[1])
    sep <- sapply(ip, function(x) x$peff[2])
    cnames <- sapply(ip, function(x) colnames(x$reg$response))
    stargazer(lapply(ip, function(x) x$reg),
              type="html",
              title="Policies and Information",
              #dep.var.labels=cnames,
              dep.var.labels.include=FALSE,
              column.labels=cnames,
              omit=omit,
              omit.labels=omit.labels,
              omit.stat=c("f", "ser"), model.names=FALSE,
              model.numbers=TRUE,
              # add.lines = list(c("sum Policy",sprintf("%.3f",peff)),
              #                  c("",sprintf("(%.3f)",sep)),
              #                  "\\hline")
              # add.lines=list(c("sum behavior",
              #                  sprintf("%.3f",sapply(ip, function(x) x$beff[1]))),
              #                c("",
              #                  sprintf("(%.3f)",sapply(ip, function(x) x$beff[2]))),
              #                "\\hline")
    )
  }

  if (!(is.null(pbiy))) {
    ylbl <- colnames(pbiy[[1]]$reg$response)
    if (ylbl=="dlogdd") ylbl <- "Death Growth"
    if (ylbl=="dlogdc") ylbl <- "Case Growth"

    cat(sprintf("\n### Policy, Behavior, and %s\n",ylbl))

    stargazer(lapply(pbiy, function(x) x$reg),
              type="html", dep.var.labels.include=FALSE,
              title=ylbl,
              omit=omit,
              omit.labels=omit.labels,
              #column.labels=c("OLS","IV"),
              #column.separate=c(2,2),
              # add.lines=list(c("sum policies",
              #                  sprintf("%.3f",sapply(pbiy, function(x) x$peff[1]))),
              #                c("",
              #                  sprintf("(%.3f)",sapply(pbiy, function(x) x$peff[2]))),
              #                c("sum behavior",
              #                  sprintf("%.3f",sapply(pbiy, function(x) x$beff[1]))),
              #                c("",
              #                  sprintf("(%.3f)",sapply(pbiy, function(x) x$beff[2])))
              #                ),
              omit.stat=c("f", "ser"), model.names=FALSE,
              model.numbers=TRUE)
  }

  if (!(is.null(piy))) {
    ylbl <- colnames(piy[[1]]$reg$response)
    if (ylbl=="dlogdd") ylbl <- "Death Growth"
    if (ylbl=="dlogdc") ylbl <- "Case Growth"

    cat(sprintf("\n### Policy and %s\n",ylbl))

    stargazer(lapply(piy, function(x) x$reg),
              type="html", dep.var.labels.include=FALSE,
              title=ylbl,
              omit=omit,
              omit.labels=omit.labels,
              #column.labels=c("OLS","IV"),
              #column.separate=c(2,2),
              add.lines=list(c("sum policies",
                               sprintf("%.3f",sapply(piy, function(x) x$peff[1]))),
                             c("",
                               sprintf("(%.3f)",sapply(piy, function(x) x$peff[2])))
                             ),
              omit.stat=c("f", "ser"), model.names=FALSE,
              model.numbers=TRUE)
  }




  return(NULL)
}



countyvarregexp <- gsub("\\)","\\\\)",gsub("\\(","\\\\(",countyvars))
countyvarregexp <- paste("^(",paste(c("Constant",countyvarregexp), collapse="|"),")",sep="")

printstars <- function(est, se, starp=c(0.1, 0.05, 0.01)) {
  stars <- paste(rep("*", sum(pnorm(-abs(est), sd=se)<starp/2)), collapse="")
  if (length(stars)==0 || stars=="") return(sprintf("%.3f",est))
  return(sprintf("%.3f$^{%s}$", est, stars))
}

showhtmltables <- function(pib, pbiy, piy, ip=NULL) {

  omit <- c(countyvarregexp,
            "^month") #"testratedc:",)
  omit.labels <- c("county variables",
                   "Month : county variables")
  
  omit <- c("^month","^state") #"testratedc:",)
  omit.labels <- c("Month",
                   "Month : State")

  if (!(is.null(pib))) {
    cat("\n### Policies and Behavior\n")

    peff <- sapply(pib, function(x) x$peff[1])
    sep <- sapply(pib, function(x) x$peff[2])
    cnames <- sapply(pib, function(x) colnames(x$reg$response))
    stargazer(lapply(pib, function(x) x$reg),
              type="html",
              title="Policies and Behavior",
              #dep.var.labels=cnames,
              dep.var.labels.include=FALSE,
              column.labels=cnames,
              omit=omit,
              omit.labels=omit.labels,
              omit.stat=c("f", "ser"), model.names=FALSE,
              model.numbers=TRUE,
              add.lines = list("", "","",
                               "\\hline")
              # add.lines = list(c("sum Policy",sprintf("%.3f",peff)),
              #                  c("",sprintf("(%.3f)",sep)),
              #                  "\\hline")
    )

  }


  if (!(is.null(ip))) {
    cat("\n### Policies and Information\n")

    peff <- sapply(ip, function(x) x$peff[1])
    sep <- sapply(ip, function(x) x$peff[2])
    cnames <- sapply(ip, function(x) colnames(x$reg$response))
    stargazer(lapply(ip, function(x) x$reg),
              type="html",
              title="Policies and Information",
              #dep.var.labels=cnames,
              dep.var.labels.include=FALSE,
              column.labels=cnames,
              omit=omit,
              omit.labels=omit.labels,
              omit.stat=c("f", "ser"), model.names=FALSE,
              model.numbers=TRUE,
              # add.lines = list(c("sum Policy",sprintf("%.3f",peff)),
              #                  c("",sprintf("(%.3f)",sep)),
              #                  "\\hline")
              add.lines=list(c("sum behavior",
                               sprintf("%.3f",sapply(ip, function(x) x$beff[1]))),
                             c("",
                               sprintf("(%.3f)",sapply(ip, function(x) x$beff[2]))),
                             "\\hline")
    )
  }

  if (!(is.null(pbiy))) {
    ylbl <- colnames(pbiy[[1]]$reg$response)
    if (ylbl=="dlogdd") ylbl <- "Death Growth"
    if (ylbl=="dlogdc") ylbl <- "Case Growth"

    cat(sprintf("\n### Policy, Behavior, and %s\n",ylbl))

    stargazer(lapply(pbiy, function(x) x$reg),
              type="html", dep.var.labels.include=FALSE,
              title=ylbl,
              omit=omit,
              omit.labels=omit.labels,
              #column.labels=c("OLS","IV"),
              #column.separate=c(2,2),
              add.lines=list(c("sum policies",
                               sprintf("%.3f",sapply(pbiy, function(x) x$peff[1]))),
                             c("",
                               sprintf("(%.3f)",sapply(pbiy, function(x) x$peff[2]))),
                             c("sum behavior",
                               sprintf("%.3f",sapply(pbiy, function(x) x$beff[1]))),
                             c("",
                               sprintf("(%.3f)",sapply(pbiy, function(x) x$beff[2])))
              ),
              omit.stat=c("f", "ser"), model.names=FALSE,
              model.numbers=TRUE)
  }

  if (!(is.null(piy))) {
    ylbl <- colnames(piy[[1]]$reg$response)
    if (ylbl=="dlogdd") ylbl <- "Death Growth"
    if (ylbl=="dlogdc") ylbl <- "Case Growth"

    cat(sprintf("\n### Policy and %s\n",ylbl))

    stargazer(lapply(piy, function(x) x$reg),
              type="html", dep.var.labels.include=FALSE,
              title=ylbl,
              omit=omit,
              omit.labels=omit.labels,
              #column.labels=c("OLS","IV"),
              #column.separate=c(2,2),
              add.lines=list(c("sum policies",
                               sprintf("%.3f",sapply(piy, function(x) x$peff[1]))),
                             c("",
                               sprintf("(%.3f)",sapply(piy, function(x) x$peff[2])))
              ),
              omit.stat=c("f", "ser"), model.names=FALSE,
              model.numbers=TRUE)

  }




  return(NULL)
}




savetextables <- function(pib, pbiy, piy, prefix, ip = NULL,
                          rootdir=system("git rev-parse --show-toplevel", intern=TRUE)[1])
{
  # omit <- c(countyvarregexp,
  #           "^week")
  # omit.labels <- c("county variables",
  #                  "Month $\\times$ county variables")
  # 
   
  omit <- c("week","state")
  omit.labels <- c("week",
                   "state")

  if (!(is.null(pib))) {
    peff <- sapply(pib, function(x) x$peff[1])
    sep <- sapply(pib, function(x) x$peff[2])
    cnames <- sapply(pib, function(x) colnames(x$reg$response))
    tbl <- capture.output(stargazer(
        lapply(pib, function(x) x$reg),
        type="latex",
        title="Policies and Behavior",
        #dep.var.labels=c("Trend Information","Lag Cases Information"), #c(bvars,bvars),
        dep.var.labels.include=FALSE,
        column.labels=cnames,
        omit=omit,
        omit.labels=omit.labels,
        omit.stat=c("f", "ser"), model.names=FALSE,
        model.numbers=TRUE,
        df=FALSE, header=FALSE,
        no.space=TRUE,
        column.sep.width="1pt" #,
        # add.lines = list(c("$\\sum_j \\mathrm{Policy}_j$",
        #                    sapply(1:length(peff), function(i) printstars(peff[i], sep[i]))),
        #                  c("",sprintf("(%.3f)",sep)))
        )
        )
    tbl <- relabel(tbl)
    tbl <- gsub("Model (\\d)", "\\(\\1\\)", tbl)
    #tbl <- gsub("No","Yes", tbl)
    texfile <- sprintf("%s/tex/tables_and_figures/%s-pib.tex",rootdir,prefix)
    cat(paste(tbl[c(-1,-2,-3,-4, -length(tbl))], collapse="\n"), file=texfile)
  }

  if (!(is.null(pbiy))) {
    yvar <- colnames(pbiy[[1]]$reg$response)
    if (yvar=="dlogdd") ylbl <- "Death Growth"
    if (yvar=="dlogdc") ylbl <- "Case Growth"


    tbl <- capture.output(stargazer(
        lapply(pbiy, function(x) x$reg),
        type="latex", dep.var.labels.include=FALSE,
        title=ylbl,
        omit=omit,
        omit.labels=omit.labels,
        column.labels=c(yvar),
        column.separate=c(length(pbiy)),
        # add.lines=list(c("$\\sum_j \\mathrm{Policy}_j$",
        #                  sapply(pbiy, function(x) printstars(x$peff[1], x$peff[2]))),
        #                c("",
        #                  sprintf("(%.3f)",sapply(pbiy, function(x) x$peff[2]))),
        #                c("$\\sum_k w_k \\mathrm{Behavior}_k$",
        #                  sapply(pbiy, function(x) printstars(x$beff[1], x$beff[2]))),
        #                c("",
        #                  sprintf("(%.3f)",sapply(pbiy, function(x) x$beff[2])))
        #                ),
        omit.stat=c("f", "ser"), model.names=FALSE,
        df=FALSE, header=FALSE,
        no.space=TRUE,
        column.sep.width="1pt",
        model.numbers=TRUE))
    tbl <- relabel(tbl)
    tbl <- gsub("Model (\\d)", "\\(\\1\\)", tbl)
    #tbl <- gsub("No","Yes", tbl)
    texfile <- sprintf("%s/tex/tables_and_figures/%s-pbiy.tex",rootdir,prefix)
    cat(paste(tbl[c(-1,-2,-3,-4, -length(tbl))], collapse="\n"), file=texfile)

  }

  if (!(is.null(piy))) { 
    yvar <- colnames(piy[[1]]$reg$response)
    ylbl <- colnames(piy[[1]]$reg$response)
    if (ylbl=="dlogdd") ylbl <- "Death Growth"
    if (ylbl=="dlogdc") ylbl <- "Case Growth" 
    tbl <- capture.output(stargazer(
        lapply(piy, function(x) x$reg),
        type="latex",
        dep.var.labels.include=FALSE,
        title=ylbl,
        omit=omit,
        omit.labels=omit.labels,
        column.labels=c(yvar),
        column.separate=c(length(piy)),
        # add.lines=list(c("$\\sum_j \\mathrm{Policy}_j$",
        #                  sapply(piy, function(x) printstars(x$peff[1], x$peff[2]))),
        #                c("",
        #                  sprintf("(%.3f)",sapply(piy, function(x) x$peff[2])))
        #                ),
        omit.stat=c("f", "ser"), model.names=FALSE,
        no.space=TRUE,
        column.sep.width="1pt",
        df=FALSE, header=FALSE,
        model.numbers=TRUE))
    tbl <- relabel(tbl)
    tbl <- gsub("Model (\\d)", "\\(\\1\\)", tbl)
    #tbl <- gsub("No","Yes", tbl)
    texfile <- sprintf("%s/tex/tables_and_figures/%s-piy.tex",rootdir,prefix)
    cat(paste(tbl[c(-1,-2,-3,-4, -length(tbl))], collapse="\n"), file=texfile)
  }


  if (!(is.null(ip))) {

  beff <- sapply(ip, function(x) x$beff[1])
  sep <- sapply(ip, function(x) x$beff[2])
  cnames <- sapply(ip, function(x) colnames(x$reg$response))
  tbl <- capture.output(stargazer(
    lapply(ip, function(x) x$reg),
    type="latex",
    title="Inofrmation and Policies",
    #dep.var.labels=c("Trend Information","Lag Cases Information"), #c(bvars,bvars),
    dep.var.labels.include=FALSE,
    column.labels=cnames,
    omit=omit,
    omit.labels=omit.labels,
    omit.stat=c("f", "ser"), model.names=FALSE,
    model.numbers=TRUE,
    df=FALSE, header=FALSE,
    no.space=TRUE,
    column.sep.width="1pt"#,
    # add.lines = list(c("$\\sum_j \\mathrm{Behavior}_j$",
    #                    sapply(1:length(beff), function(i) printstars(beff[i], sep[i]))),
    #                  c("",sprintf("(%.3f)",sep)))
    )
  )
  tbl <- relabel(tbl)
  tbl <- gsub("Model (\\d)", "\\(\\1\\)", tbl)
  #tbl <- gsub("No","Yes", tbl)
  texfile <- sprintf("%s/tex/tables_and_figures/%s-ip.tex",rootdir,prefix)
  cat(paste(tbl[c(-1,-2,-3,-4, -length(tbl))], collapse="\n"), file=texfile)

  }

  return(NULL)
}


figdev <- function(filename, width=8, height=6, pointsize=12) {
  cairo_pdf(filename, width=width, height=height, pointsize=pointsize,
            family="serif")
}

escapeforregexp <- function(string) {
  string <- gsub("\\(", "\\\\(",string)
  string <- gsub("\\)", "\\\\)",string)
  string <- gsub("\\.", "\\\\.",string)
  return(string)
}

#' Create table of direct, indirect, and total policy effects.
dieff_table <- function(pib, pbiy,  piy, policies=pols, behaviors=bvars, nsum=length(policies)) {
  stopifnot(length(pib)==length(behaviors))
  names(pib) <- behaviors
  pi <- rep(NA, length(policies))
  alpha <- rep(NA, length(behaviors))
  beta <- matrix(NA, nrow=length(policies), ncol=length(behaviors))
  names(pi) <- rownames(beta) <- policies
  colnames(beta) <- names(alpha) <- behaviors
  pi <- sapply(policies,
               function(p) pbiy[grep(sprintf("(^|lag\\()%s(, |$)",escapeforregexp(p)),names(pbiy))])
  alpha <- sapply(behaviors,
                  function(b) pbiy[grep(sprintf("(^|lag\\()%s(, |$)",escapeforregexp(b)),names(pbiy))])
  for (b in behaviors) {
    beta[,b] <- sapply(policies, function(p) {
      i <- grep(sprintf("(^|lag\\()%s(, |$)",escapeforregexp(p)),names(pib[[b]]))
      if (length(i)==0) return(0)
      else return( pib[[b]][i])
    })
  }

  tot <- sapply(policies,
                function(p) piy[grep(sprintf("(^|lag\\()%s(, |$)",escapeforregexp(p)),names(piy))])


  tbl <- cbind(pi, beta %*% alpha, pi + beta %*% alpha, tot,
  (pi + beta %*% alpha + tot)/2, pi + beta %*% alpha - tot)
  if (nsum==nrow(tbl)) {
    tbl <- rbind(tbl[1:nsum,], colSums(tbl[1:nsum,]))
    rownames(tbl) <- c(policies, "$\\sum_j \\mathrm{Policy}_j$")
  } else {
    tbl <- rbind(tbl[1:nsum,], colSums(tbl[1:nsum,]), tbl[(nsum+1):nrow(tbl),])
    rownames(tbl) <- c(policies[1:nsum], "$\\sum_j \\mathrm{Policy}_j$",
                       policies[(nsum+1):length(policies)])
  }
  colnames(tbl) <- c("Direct","Indirect", "Total", "PI$\\to$Y Coef.",
                     "Average", "Difference")
  return(tbl)
}
