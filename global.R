

library('shiny')
library('shinydashboard')
library('shinyWidgets')
library("shinyMatrix")
library('shinythemes')

library('tidyverse')
library('DT')


library('tidyverse')
library('rLQL')
library('smooth')
library('caret')

set.seed(316)

regiones <- c('Valle de México', 'Bajío', 'Sureste', 'Norte')
regionesVec <- rep(regiones, each = 104)


# Simulaciones ------------------------------------------------------------
# Ventas
ventas <- matrix(nrow = 104, ncol = 4)
ventas[1,1:4] <- rnorm(4, 100000, 50000)

for(i in 2:104) {
  ventas[i,] <- ventas[i-1,]*rnorm(4, 1, 0.1)
}

# Distribución
dist <- matrix(nrow = 104, ncol = 4)

dist[,1] <- 100 * ventas[,1]/max(ventas[,1])
dist[,2] <- 100 * ventas[,2]/max(ventas[,2])
dist[,3] <- 100 * ventas[,3]/max(ventas[,3])
dist[,4] <- 100 * ventas[,4]/max(ventas[,4])

for(i in 1:104) {
  dist[i,] <- (dist[i,] + rnorm(4, 10, 5))*rnorm(4, 1, 0.05)
}

dist <- apply(dist, 2, function(x) { 100*x/max(x) })

# Precio
precio <- matrix(nrow = 104, ncol = 4)

precio[,1] <- rnorm(1, 25, 5) * ventas[,1]/max(ventas[,1])
precio[,2] <- rnorm(1, 25, 5) * ventas[,2]/max(ventas[,2])
precio[,3] <- rnorm(1, 25, 5) * ventas[,3]/max(ventas[,3])
precio[,4] <- rnorm(1, 25, 5) * ventas[,4]/max(ventas[,4])

for(i in 1:104) {
  precio[i,] <- (precio[i,] + rnorm(4, 2, 2))*rnorm(4, 1, 0.05)
}

precio <- max(precio) + 5 - precio

df <- data.frame(fecha = rep(seq(as.Date('2019-01-07'), as.Date('2020-12-28'), by = 'week'), times = 4),
                 region = factor(regionesVec),
                 ventas = gather(data.frame(ventas))$value,
                 dist = gather(data.frame(dist))$value,
                 precio = gather(data.frame(precio))$value)


# CSS ---------------------------------------------------------------------

boxCSS <- '.box {
              position: relative;
              border-radius: 3px;
              background: #fff;
              border-top: 3px solid #fff;
              margin-bottom: 20px;
              width: 100%;
              box-shadow: 0 0px 0px rgb(0 0 0 / 10%);
          }'

valueBoxCSS <- '.bg-purple {
                  background-color: #172744!important;
               }

                .bg-blue {
                  background-color: #DF2A35!important;
                }
               
                .bg-yellow {
                  background-color: #666665!important;
               }'

boxBodyCSS <- '.box-body {
                  border-top-left-radius: 0;
                  border-top-right-radius: 0;
                  border-bottom-right-radius: 3px;
                  border-bottom-left-radius: 3px;
                  padding: 0px;
              }'

tradeoff.des.LQL <- function (items, shown, vers, tasks, fname = NULL, Rd = 20, 
                              Rc = NULL, print = TRUE) {
  cuentaWarnings <- 0
  ta <- Sys.time()
  if (print) {
    cat("tradeoff.des 0.9-2, ", date(), "\n")
    flush.console()
  }
  drows <- vers * tasks
  if (is.null(Rc)) {
    Rc <- max(1000, 10 * drows)
  }
  des.c <- tryCatch(optBlockC(factor(1:items), rep(shown, 
                                                   drows), nRepeats = Rd), error = function(e) return(NULL))
  if (is.null(des.c)) {
    stop("PROCEDURE STOPPED: Insufficient design space")
  }
  des.m <- t(matrix(des.c$rows, shown, drows))
  crit <- drows * shown
  crit.vec <- vector("numeric", Rc)
  des.p <- des.m
  for (d in 1:drows) {
    des.p[d, ] <- sample(des.p[d, ], shown)
  }
  cc <- 0
  for (i in 1:Rc) {
    ii <- sample(1:drows, 1)
    des.try <- des.p
    des.try[ii, ] <- sample(des.try[ii, ], shown)
    da <- as.vector(unlist(apply(des.try, 2, function(x) table(factor(x, 
                                                                      levels = 1:items)))))
    if (length(da) == items * shown) {
      crit.try <- abs(sqrt(items * shown) - sum(svd(da)$u))
      if (crit.try < crit) {
        des.p <- des.try
        crit <- crit.try
      }
    }
    crit.vec[i] <- crit
    if (i > 1) {
      cc <- cc + 1
      if (crit.vec[i] < crit.vec[i - 1]) {
        cc <- 0
      }
    }
  }
  des.d <- as.data.frame(des.p)
  for (g in 1:shown) {
    des.d[, g] <- as.factor(des.d[, g])
  }
  des.f <- tryCatch(optBlockC(des.d, rep(tasks, vers), nRepeats = Rd), 
                    error = function(e) return(NULL), warning = function(e) return(NULL))
  if (is.null(des.f)) {
    des.x <- des.p
    # warning("Insufficient design space to block into versions; using random assignment instead", 
    #         call. = FALSE, immediate. = FALSE)
    cuentaWarnings <- cuentaWarnings + 1
  }
  else {
    des.x <- des.p[des.f$rows, ]
  }
  bal.1w <- table(factor(des.x, levels = 1:items))
  bal.1w.mn <- mean(bal.1w)
  bal.1w.sd <- sd(bal.1w)
  bal.ps <- apply(des.x, 2, function(x) table(factor(x, levels = 1:items)))
  bal.ps.mn <- mean(bal.ps)
  bal.ps.sd <- sd(as.vector(bal.ps))
  bal.2w <- pw.eval(items, shown, drows, des.x)
  bal.final <- list(one.way = list(tbl = bal.1w, mean = bal.1w.mn, 
                                   stdv = bal.1w.sd), col.pos = list(tbl = bal.ps, mean = bal.ps.mn, 
                                                                     stdv = bal.ps.sd), two.way = bal.2w)
  Rc.crit = list(Rc = Rc, crit.stable = cc, crit.vec = crit.vec)
  card <- 1:drows
  ver <- rep(1:vers, rep(tasks, vers))
  tsk <- rep(1:tasks, vers)
  design <- cbind(card, ver, tsk, des.x)
  colnames(design) <- c("card", "version", "task", paste("item", 
                                                         1:shown, sep = ""))
  zvec <- which(bal.ps == 0)
  if (length(zvec) > 0) {
    # warning("SPARSE DESIGN: Not all items appear in all column positions", 
    #         call. = FALSE, immediate. = FALSE)
    cuentaWarnings <- cuentaWarnings + 1
  }
  tb <- Sys.time() - ta
  if (!is.null(fname)) {
    write.tab(design, fname)
  }
  if (print) {
    cat("total iterations: ", Rc, "\n", sep = "")
    cat("critical D: ", crit, ", stable: ", cc, " iterations\n", 
        sep = "")
    cat("time elapsed: ", tb, " secs\n", sep = "")
    cat("warnings: ", cuentaWarnings, sep = "")
  }
  return(list(design = design, balance = bal.final, Rc.crit = Rc.crit, 
              time.elapsed = tb, cuentaWarnings = cuentaWarnings))
}


optimizarDiseno <- function(items, shown, vers, tasks, opt) {
  des <- tradeoff.des.LQL(items = items, 
                          shown = shown,
                          vers = vers, 
                          tasks = tasks,
                          print = FALSE)
  
  if(opt == 'vers') {
    while(des$cuentaWarnings > 0) {
      vers <- vers + 1
      des <- tradeoff.des.LQL(items = items, 
                              shown = shown,
                              vers = vers, 
                              tasks = tasks,
                              print = FALSE)
    }
    return(des)
  }
  
  if(opt == 'tasks') {
    while(des$cuentaWarnings > 0) {
      tasks <- tasks + 1
      des <- tradeoff.des.LQL(items = items, 
                              shown = shown,
                              vers = vers, 
                              tasks = tasks,
                              print = FALSE)
    }
    return(des)
  }
  
  if(opt == 'both') {
    while(des$cuentaWarnings > 0) {
      vers <- vers + 1
      tasks <- tasks + 1
      des <- tradeoff.des.LQL(items = items, 
                              shown = shown,
                              vers = vers, 
                              tasks = tasks,
                              print = FALSE)
    }
    return(des)
  }
  
  return(des)
}