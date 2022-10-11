#### Practical 2 Draft  ####

### This function, Pone, aims to calculate the probability of a prisoner able to 
### get the card within n goes. Depending on the strategy, Pone generate a 
### sequence of the prisoner going through the boxes with the corresponding key
### card. 

### The input is n: the number of prisoners, k: the first key number 
### strategy: 3 strategies to choose, nreps.nreps: number of simulations, the 
### default value is set to 10,000

### The output is the probability of success.

Pone<-function (n, k, strategy, nreps.nreps=10000){
  ### We set the number of successes as 0
  success<-0
  
  ### Repeating the simulation for nreps.nreps times
  for (i in 1:nreps.nreps){
    
    prisnum<-sample(2*n,1)  ### Randomly assign a number for the prisoner
    cardseq<-rep(0,times=2*n) ### Generate an empty vector for the card numbers
    prisseq<-seq(1,2*n) ### an ascending sequence of all prisoners' number
   
     ### Enters if the strategy is 1 or 2
    if(strategy==1|strategy==2){
     
      
      if(strategy==1){ ### for strategy 1
        prisseq<-prisseq[-k] ### We remove the value k in the prisoner seq 
        cardseq[prisnum]<-k ### Assign k to the box with the prisoner's num
        
        ### Randomly allocate other card number to other boxes
        cardseq[-prisnum]<-sample(prisseq,2*n-1)  
        opencard<-k ### The first card to be opened
        
      } else { ### for strategy 2
        cardseq[] <- sample(prisseq,2*n) ### Randomly allocate card no. to boxes
        boxopen<-sample(prisseq,1) ### The box that the prisoner opens 
        opencard<-cardseq[boxopen] ### The card number in that box
      }
  
      
      i<-1 ### Set the number of boxes open to be 1
      
      ### When the card is still not found and boxes opened is less than n
      while (opencard!=prisnum & i<n){
        ### Update the card opened by using the previous card number to find
        ### the corresponding box number
        opencard<-cardseq[opencard] 
        i<-i+1 ### Increase the count by 1
      }
      
      ### If the prisoner finds the card, then succeed!
      if(opencard==prisnum){
        success<-success+1
      }
    }
    
    if (strategy==3){
      cardseq[] <- sample(prisseq,2*n) ## Randomly allocate card no. to boxes
      openseq<-sample(prisseq,n) ### Generate a sequence of boxes to be opened
      
      ### if the card no is in one of  the n boxes, it counts as success
      if (prisnum %in% cardseq[openseq]){
        success<-success+1
        
      }
    }
    
    
    
  }
  
  
 success/nreps.nreps ### Output:the probability of success
  
}
