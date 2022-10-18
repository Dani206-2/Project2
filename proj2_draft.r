## Group34:Vangelis Antypas: s2449453, Jihan Li: s2322347, Daniel Kwok: s2308472
## Github repository address: 'https://github.com/Dani206-2/Project2'
## Contribution: To be completed

## ----------------------------------------------------------------------------

## The function Pone, aims to calculate the probability of an individual  
## prisoner finding their own number in n tries. Depending on the chosen 
## strategy, Pone generates a sequence of the boxes the prisoner is going 
## through, along with the corresponding card numbers.
 

## Inputs: n: the number of prisoners, k: the prison number 
## strategy: 3 strategies to choose, nreps.nreps: number of simulations, the 
## default value is set to 10,000.

## Output: probability of success for the chosen strategy for an individual
## prisoner.

## The function Pone is defined right after, the (sub)function Psub, which is 
## utilized in the definition of Pone

## ----------------------------------------------------------------------------

## The function Psub, aims to minimize the overlap between the function Pone
## and  the function Pall, which is defined later on.
## Inputs: n,k,strategy and a 2n-vector cardseq representing the sequence of
## cards in the corresponding boxes, i.e cardseq[k] is the number of the card
## contained in the box k.
## Output: True if the prisoners finds his number for the given inputs, False
## otherwise.


Psub<-function(n,k,strategy,cardseq){
  succeed=FALSE ## Assume the prisoner fails
  
  ## Checks if the strategy is 1 or 2
  if(strategy==1|strategy==2){
    
    
    
    if(strategy==1){ ## For strategy 1, pick box k
      
      opencard<-cardseq[k] ## The first card to be read
      
    } else { ## For strategy 2, random initial choice
      
      boxopen<-sample(seq(1,2*n,1),1) ## The box that the prisoner opens 
      opencard<-cardseq[boxopen] ## The card number in that box
    }
    
    
    i<-1 ## Set the number of boxes opened to be 1
    
    ## If the card is not found and boxes opened is less than n
    while (opencard!=k & i<n){
      ## Update the card read by using the previous card number to find
      ## the corresponding box number
      opencard<-cardseq[opencard] 
      i<-i+1 ## Increase the count by 1
    }
    
    ## If the prisoner finds the card, then succeed!
    if(opencard==k){
      succeed=TRUE
    }
  }
  
  if (strategy==3){## For strategy 3, random guessing 
    openseq<-sample(seq(1,2*n,1),n) ## Create a sequence of boxes to be opened
    
    ## If the prisoner number is in one of the n boxes, it counts as success
    if (k %in% cardseq[openseq]){
      succeed=TRUE
      
    }
  }
  succeed
}

## ----------------------------------------------------------------------------

Pone<-function (n,nreps=10000,...){
  ## We set the number of successes as 0
  success<-0
  
  
  ## Repeating the simulation for nreps.nreps times
  for (i in 1:nreps){
    cardseq<-c() ## Generate an empty vector for cardseq
    cardseq<- sample(seq(1,2*n,1),2*n) ## Randomly allocate card no. to boxes
    
    result<-Psub(n=n,cardseq=cardseq,...)
    
    if (result){
      success<-success+1
    }
  }
  success/nreps ## Output:the probability of success
  
}

## ----------------------------------------------------------------------------

## The function Pall aims to calculate the probability of success for all the
## prisoners, i.e. the probability that every single prisoner finds their 
## number, under a given strategy.
## Inputs: n: number of prisoners, nreps: number of simulations(default=10k)
## ++possible comment about '...' notation
## Output: Probability that all the prisoners find their number for the given
## inputs.  


 
Pall<-function(n,nreps=10000,...){
  ## We set the number of successes to 0
  success<-0
  prisseq<-seq(1,2*n) ## An ascending sequence of all prisoners' number
  
  ## Repeating the simulation for nreps times
  for (i in 1:nreps){
    cardseq<-c() ## Generate an empty vector for cardseq
    cardseq<- sample(prisseq,2*n) ## Randomly allocate card no. to boxes
    
    for (k in 1:(2*n)){ ## Test all prisoners from k=1 to 2n
      result<-Psub(n=n,k=k,cardseq=cardseq,...)
      if (result==FALSE){
        break ## If one prisoner fails we can stop the simulation
      }
      if (k==2*n){ ## If the last prisoner succeed, we are successful!
        success<-success+1
      }
    }
    
    
  }
  success/nreps.nreps ### Output:the probability of success
  
  
}

## ----------------------------------------------------------------------------
