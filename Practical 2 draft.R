#### Practical 2 Draft  ####

### This function, Pone, aims to calculate the probability of a prisoner able to 
### get the card within n goes. Depending on the strategy, Pone generate a 
### sequence of the prisoner going through the boxes with the corresponding key
### card. 

### The input is n: the number of prisoners, k: the prison number 
### strategy: 3 strategies to choose, nreps.nreps: number of simulations, the 
### default value is set to 10,000

### The output is the probability of success.

Pone<-function (n, k, strategy, nreps.nreps=10000){
  ### We set the number of successes as 0
  success<-0
  prisseq<-seq(1,2*n) ### an ascending sequence of all prisoners' number
  
  ### Repeating the simulation for nreps.nreps times
  for (i in 1:nreps.nreps){
    cardseq<-rep(0,times=2*n) ### Generate an empty vector for the card numbers
    cardseq[] <- sample(prisseq,2*n) ### Randomly allocate card no. to boxes
    
   
     ### Enters if the strategy is 1 or 2
    if(strategy==1|strategy==2){
     
      
      
      if(strategy==1){ ### for strategy 1
        
        opencard<-cardseq[k] ### The first card to be opened
        
      } else { ### for strategy 2
        
        boxopen<-sample(prisseq,1) ### The box that the prisoner opens 
        opencard<-cardseq[boxopen] ### The card number in that box
      }
  
      
      i<-1 ### Set the number of boxes open to be 1
      
      ### When the card is still not found and boxes opened is less than n
      while (opencard!=k & i<n){
        ### Update the card opened by using the previous card number to find
        ### the corresponding box number
        opencard<-cardseq[opencard] 
        i<-i+1 ### Increase the count by 1
      }
      
      ### If the prisoner finds the card, then succeed!
      if(opencard==k){
        success<-success+1
      }
    }
    
    if (strategy==3){
      openseq<-sample(prisseq,n) ### Generate a sequence of boxes to be opened
      
      ### if the prison number is in one of the n boxes, it counts as success
      if (k %in% cardseq[openseq]){
        success<-success+1
        
      }
    }
    
  }
  
 success/nreps.nreps ### Output:the probability of success
  
}



Pall<-function(...){
  k<- sample(1,2)
  
  ### what to change in Pone
  ### 
  ### strategy 1: no more k
  ### cardseq[]<-sample(prisseq,2*n-1)  
  ### opencard<-cardseq[prisnum]      
  
  ### Apply function????
  
  
}



dloop<-function(n,nreps){
  
  
  
  
  
}




