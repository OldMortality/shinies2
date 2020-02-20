# app1
library(shiny)
library(DT)

shinyServer <- function(input, output) {
  
  
  getPower <- function(n,sd,diff,alpha,bonf=F) {
    if (bonf==T) {
      alpha <- alpha / N
    }
    tp <- 0
    for (i in 1:1000) {
      y1 <- rnorm(n,diff,sd=sd)
      t <- t.test(y1)
      p <- t$p.value
      if (p<alpha) {
        tp <- tp + 1
      }
    }
    return(tp/1000)
    
  } 
  
  
  getSummary <- function() {
    # input$goButton 
    # t <- timenow$t
    # P = round(input$N * input$pperc)
    #  l0 <- paste('alpha=',input$alpha,'Bonferroni')
    #  l1 <- paste('True positives:',plain$TP,'---',bonf$TP)
    #  l2 <- paste('False positives:',plain$FP,'---',bonf$FP)
    #  l3 <- paste('True negatives:',plain$TN,'---',bonf$TN)
    #  l4 <- paste('False negatives:',plain$FN,'---',bonf$FN)
    # #l5 <- paste('Power:',round(TP/P,2),'---',round(TP.B/P,2))
    #l6 <- paste('FDR:',FP,'/(',FP,'+',TP,')=',   round(FP/(FP+TP),2))
    #l7 <- paste('FDR bonferroni:',FP.B,'/(',FP.B,'+',TP.B,')=',   round(FP.B/(FP.B+TP.B),2))
    result <- paste(l1,l2,l3,l4,sep="<BR>")
    return(result)  
  } 
  
  
   
  
  plain <- reactiveValues(FP = 0,
                          FN = 0,
                          TP = 0,
                          TN = 0)
  bonf <-  reactiveValues(FP = 0,
                          FN = 0,
                          TP = 0,
                          TN = 0)
  
  p <- reactiveValues(pvals=vector())
  l <- reactiveValues(labels=vector())
  
  
  timenow <- reactiveValues(t=Sys.time())
  
  
  getTable <- function() {
   
    N <- isolate(input$N)
    
    P = round(N * isolate(input$pperc))
    df <- data.frame(plain = c(plain$FP, plain$FN, plain$TP, plain$TN, 
                               getPower(n=input$S,
                                        sd = input$sd,
                                        diff = input$mu,
                                        alpha = input$alpha,
                                        bonf = F),
                               round(plain$FP/(plain$FP+plain$TP),2)),
                     bonf  = c(bonf$FP , bonf$FN , bonf$TP , bonf$TN , 
                               getPower(n=isolate(input$S),
                                        sd = isolate(input$sd),
                                        diff = isolate(input$mu),
                                        alpha = isolate(input$alpha),
                                        bonf = T),
                               round(bonf$FP/(bonf$FP+bonf$TP),2)))
    print('updated table')
    print(df)
    rownames(df) <- c('False +','False -','True +','True - ','Power','FDR')
    return(df)
  }
  
  output$table <- renderDT(getTable(),options=list(lengthChange=F))
  
  
  observeEvent(input$goButton, {
    simulation()
  })
  
  simulation <- function() {
    input$goButton
    print('running simulation')
    # N experiments, P true difference, N-P equal means
    N <- isolate(input$N)
    mu1 = isolate(input$mu)
    # number of experiments with a true difference
    P = round(N * isolate(input$pperc))
    # sample size
    alpha <- isolate(input$alpha)
    S <- isolate(input$S)
    p.vals <- vector()
    
    sd = isolate(input$sd)
    
    for (i in 1:N) {
      if (i <= P) {
        y1 <- rnorm(S,mu1,sd=sd)
      } else {
        y1 <- rnorm(S,0,sd=sd)
      }
      t <- t.test(y1)
      p.vals[i] <- t$p.value
      
    }
    
    labels <- rep(F,N)
    labels[1:P] <- T
    l$labels <- labels
    p$pvals <- p.vals
    #return(data.frame(labels=labels,p.vals = p.vals))
  }
                              
  
  
  output$plot1 <- renderPlot({ 
    
    print('plot1')
    
    df.sim <- data.frame(labels=l$labels,
                         p.vals = p$pvals)
    if ( length(df.sim$labels) > 0) {
      
    
    # experiments
    N <- isolate(input$N)
    # number of p-values in the distribution
    M <- 1000
    u <- matrix(runif(N*M),nrow=M)
    v <- apply(t(u),2,sort)
    u
    w <- t(v)
    plot('',xlim=c(1,min(N,input$xlim)),ylim=c(0,input$ylim))
    for (i in 1:N) {
      segments(x0=i,x1 = i,y0=quantile(probs=0.000025,x=w[,i]),y1=quantile(w[,i],0.9999975))
    }
    
    P = round(N * isolate(input$pperc))
    plain$TP <- length(which(df.sim$p.vals[1:P]<alpha))
    plain$FN <- length(which(df.sim$p.vals[1:P]>alpha))
    plain$TN <- length(which(df.sim$p.vals[P:N]>alpha))
    plain$FP <- length(which(df.sim$p.vals[P:N]<alpha))
    
    bonf$TP <- length(which(df.sim$p.vals[1:P] < (alpha/N) ))
    bonf$FN <- length(which(df.sim$p.vals[1:P] > (alpha/N)))
    bonf$TN <- length(which(df.sim$p.vals[P:N] > (alpha/N)))
    bonf$FP <- length(which(df.sim$p.vals[P:N] < (alpha/N)))
    # dp1 <- data.fra)me(p.vals = p.vals,
    #                   labels = labels)
    dp2 <- df.sim[order(df.sim$p.vals),]
    for (i in 1:N) {
      col <- 'red'
      if (dp2$labels[i]==T) {
        col <- 'blue'
      }
      points(i,dp2$p.vals[i],col=col)
    }
    abline(h=alpha,col='blue')
    abline(h=alpha/N,col='blue',lty='dashed')
    
    
    timenow$t <- Sys.time()
    } 
  }) # end plot1
  
  
  
  output$summary <- renderText(
    paste("<font size=4>",getSummary(),
          "</font>")
  ) 
  
  
}