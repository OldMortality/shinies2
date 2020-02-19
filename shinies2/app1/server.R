# app1
library(shiny)
shinyServer <- function(input, output) {
  
  
  
  getSummary <- function() {
    input$goButton 
    t <- timenow$t
    P = round(input$N * input$pperc)
    
    l1 <- paste('True positives:',TP)
    l2 <- paste('False positives:',FP)
    l3 <- paste('True negatives:',TN)
    l4 <- paste('False negatives:',FN)
    l5 <- paste('Power:',round(TP/P,2))
    l6 <- paste('FDR:',FP,'/(',FP,'+',TP,')=',   round(FP/(FP+TP),2))
    result <- paste(l1,l2,l3,l4,l5,l6,sep="<BR>")
    return(result)  
  } 
  
  
   
  FP <- 0
  FN <- 0
  TP <- 0
  TN <- 0
  
  timenow <- reactiveValues(t=Sys.time())
  
  output$plot1 <- renderPlot({ 
    input$goButton
    # experiments
    N <- input$N
    # number of p-values in the distribution
    M <- 10000
    u <- matrix(runif(N*M),nrow=M)
    v <- apply(t(u),2,sort)
    u
    w <- t(v)
    plot('',xlim=c(1,N),ylim=c(0,1))
    for (i in 1:N) {
      segments(x0=i,x1 = i,y0=quantile(probs=0.025,x=w[,i]),y1=quantile(w[,i],0.975))
    }
    
    # N experiments, P true difference, N-P equal means
    mu1 = input$mu
    # number of experiments with a true difference
    P = round(N * input$pperc)
    # sample size
    alpha <- input$alpha
    S <- input$S
    p.vals <- vector()
    labels <- vector()
    sd = input$sd
    true.pos <- 0
    true.neg <- 0
    false.pos <- 0
    false.neg <- 0
    for (i in 1:N) {
      if (i <= P) {
        y1 <- rnorm(S,mu1,sd=sd)
      } else {
        y1 <- rnorm(S,0,sd=sd)
      }
      t <- t.test(y1)
      
      # true difference
      if (i <= P) {
        if (t$p.value < alpha) {
          true.pos <- true.pos + 1
        } else {
          false.neg <- false.neg + 1
        }
      }
      if (i > P) {
        if (t$p.value < alpha) {
          false.pos <- false.pos + 1
        } else {
          true.neg <- true.neg + 1
        }
      }  
        
      FP <<- false.pos
      FN <<- false.neg
      TP <<- true.pos
      TN <<- true.neg
      
      
      p.vals[i] <- t$p.value
      if (i <= P) {
        labels[i] <- T
      } else {
        labels[i] <- F
      }
    }
    dp1 <- data.frame(p.vals = p.vals,
                      labels = labels)
    dp2 <- dp1[order(dp1$p.vals),]
    for (i in 1:N) {
      col <- 'red'
      if (dp2$labels[i]==T) col <- 'blue'
      points(i,dp2$p.vals[i],col=col)
    }
    abline(h=alpha,col='blue')
    
    timenow$t <- Sys.time()
     
  }) # end plot1
  
  
  
  output$summary <- renderText(
    paste("<font size=4>",getSummary(),
          "</font>")
  ) 
  
  
}