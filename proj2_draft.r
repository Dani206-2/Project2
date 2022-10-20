## Group34:Vangelis Antypas: s2449453, Jihan Li: s2322347, Daniel Kwok: s2308472
## Github repository address: 'https://github.com/Dani206-2/Project2'
## Contribution: Daniel created the github repository and invited Vangelis and 
## Jihan. Q1: Daniel+Jihan, Q2: Daniel, Q5: Daniel+Jihan, Q3,Q4,Q6: Vangelis
## For the sake of keeping a record, we have reported who worked on
## every question. However the submissions are not a product of individual work 
## because we worked by getting together and talking about the questions and 
## this just cannot be tracked. As far as we are concerned the distribution of 
## work was as uniform as possible.

## ----------------------------------------------------------------------------

## The function Pone, aims to calculate the probability of an individual  
## prisoner finding their own number in n tries. Depending on the chosen 
## strategy, Pone generates a sequence of the boxes the prisoner is going 
## through, along with the corresponding card numbers.
 

## Inputs: n: the number of prisoners divided by 2, k: the prison number through 
## "..." notation, strategy: 3 strategies to choose, through "..." notation
## nreps: number of simulations, the default value is set to 10,000.

## Output: probability of success for the chosen strategy for an individual
## prisoner.

## The function Pone is defined right after, the (sub)function Psub, which is 
## utilized in the definition of Pone

## ----------------------------------------------------------------------------

## The function Psub, aims to minimize the overlap between the function Pone
## and the function Pall, which is defined later on.

## Inputs: n,k,strategy and a 2n-vector cardseq representing the sequence of
## cards in the corresponding boxes, i.e cardseq[k] is the number of the card
## contained in the box k 

## Output: True if the prisoners finds his number for the given inputs 
## successfully, False otherwise.


## We will use the Sys.time() command to measure the runtime of the entire code

start_time <- Sys.time()

## Control the randomness
set.seed(0)

Psub<-function(n,k,strategy,cardseq){
  succeed=FALSE ## Assume the prisoner fails
  
  ## Checks if the strategy is 1 or 2
  if(strategy==1|strategy==2){
    
    
    
    if(strategy==1){ ## For strategy 1, pick box k
      
      card<-cardseq[k] ## The first card to be read
      
    } else { ## For strategy 2, random initial choice
      
      boxopen<-sample(seq(1,2*n),1) ## The box that the prisoner opens 
      card<-cardseq[boxopen] ## The card number in that box
    }
    
    
    i<-1 ## Set the number of boxes opened to be 1
    
    ## If the card is not found and boxes opened is less than n
    while (card!=k & i<n){
      
      ## Update the card read by using the previous card number to find
      ## the corresponding box number
      card<-cardseq[card] 
      i<-i+1 ## Increase the count by 1
    }
    
    ## If the prisoner finds the card before opening n cards, then succeed!
    if(card==k){
      succeed=TRUE
    }
  }
  
  if (strategy==3){## For strategy 3, random guessing 
    
    openseq<-sample(seq(1,2*n),n) ## Create a sequence of boxes to be opened
    
    ## If the prisoner number is in one of the n boxes, it counts as success
    if (k %in% cardseq[openseq]){
      succeed=TRUE
      
    }
  }
  succeed ## Output: either TRUE or FALSE
}

## ----------------------------------------------------------------------------

Pone<-function (n,nreps=10000,...){
  ## We set the number of successes as 0
  success<-0
  
  
  ## Repeating the simulation for nreps times
  for (i in 1:nreps){
    cardseq<-c() ## Generate an empty vector for cardseq
    cardseq<- sample(seq(1,2*n),2*n) ## Randomly allocate cards to boxes
    
    ## We use the Psub function to run the simulation
    result<-Psub(n=n,cardseq=cardseq,...) 
    
    if (result){ ## Add 1 if Psub returns true, indicating success
      success<-success+1 
    }
  }
  success/nreps ## Output:the probability of success
  
}

## ----------------------------------------------------------------------------

## The function Pall aims to calculate the probability of success for all the
## prisoners, i.e. the probability that every single prisoner finds their 
## number, under a given strategy.
## Inputs: n, nreps (same meaning as above)
## plus strategy through '...' notation
## Output: Probability that all the prisoners find their number for the given
## inputs.  


 
Pall<-function(n,nreps=10000,...){
  ## We set the number of successes to 0
  success<-0
  
  ## Repeating the simulation for nreps times
  for (i in 1:nreps){
    cardseq<-c() ## Generate an empty vector for cardseq
    cardseq<- sample(seq(1,2*n),2*n) ## Randomly allocate card no. to boxes
    
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
  success/nreps ## Output:the probability of success
  
  
}

## ----------------------------------------------------------------------------


## Demonstration

## For an individual prisoner
## The probability of success is the same for every prisoner (under any 
## strategy), so without loss of generality we demonstrate for prisoner 1


## Probability of success for prisoner 1, for n=5, under strategy=1,2,3

r_ind5=c()

for(i in 1:3){
   
      r_ind5<-c(r_ind5,Pone(n=5,k=1,strategy=i))

}

## Probability of success for prisoner 1, for n=50, under strategy=1,2,3


r_ind50=c()

for(i in 1:3){
   
      r_ind50<-c(r_ind50,(Pone(n=50,k=1,strategy=i)))

}

## For all prisoners
## Probability of success for all prisoners simultaneously, under 
## strategy=1,2,3

r_all5=c()

for(i in 1:3){
   
      r_all5<-append(r_all5,(Pall(n=5,strategy=i)))

}


r_all50=c()

for(i in 1:3){
   
      r_all50=append(r_all50,(Pall(n=50,strategy=i)))

}


## ----------------------------------------------------------------------------

## results : r_ind5 = 0.4959 0.4028 0.4943, r_ind50 = 0.4955 0.3808 0.4947
## r_all5 = 0.3598 0.0001 0.0013, r_all50 = 0.3127 0.0000 0.0000

## ----------------------------------------------------------------------------

## We observe that for the individual cases stretagies 1,3 give better results
## than strategy 2, which matches our theoretical expectations. To understand
## why that is consider the fact that for strategy=1,3 the theoretical 
## probability is 1/2 while for strategy 2 the prisoner can choose a loop that 
## does not contain their number, so in that case they are stuck, which is why
## intuitively the probability for strategy 2 is <1/2. The really surprising 
## result comes when we consider the probability of success for all the 
## prisoners combined. For strategies 2,3 the probabilities of every prisoner
## are independent, because of the random choice of boxes, which makes the 
## combined probability fall off to zero really fast as the number of prisoners
## increases. For strategy 1, we get a very high probabilty compared to the 
## other strategies, because the individual probabilities of success are no
## longer independent. We have to take in to account the underlying 
## permutation of boxes, if for example prisoner 1 finds their
## number then every other prisoner whose number lies on the same loop is also
## going to succeed in finding their number, so the individual successes are 
## not independent. 
## References (for theoretical results): 
## 'https://en.wikipedia.org/wiki/100_prisoners_problem'.



## ----------------------------------------------------------------------------

##  The function dloop aims to estimate, by simulation, the probability of each 
##  loop length from 1 to 2n occurring at least once in a random shuffling of 
##  cards to boxes.

##  Inputs: n, nreps (same meaning as above)

##  Output: The probability of each loop length from 1 to 2n occurring at 
##  least once in a random shuffling of cards to boxes.



dloop<-function(n,nreps){
  
  
  v<-rep(0,2*n) ## Create an empty vector for total occurance for each loop len 
  
  ## Repeating the simulation for nreps times
  for (i in 1:nreps){
    cardseq<-c() 
    cardseq<- sample(seq(1,2*n),2*n) ## Randomly allocate card to boxes
    v.sim<-rep(0,2*n) ### vector used to find whether a loop length has occured 
    
    opencard.seq<-rep(0,2*n) ## assume all card are not opened
    for (k in 1:(2*n)){ ## Test prison number from 1 to 2k
      
      ## Only test cards that are NOT opened
      ## Cards opened will have the same loop length
      ## with one of the previous cards
      
      if(opencard.seq[k]==0){  
        
        loop.len<-1
        card<-cardseq[k] ## The first card to be opened
        
        while (card!=k){
          ## Update the card opened by using the previous card number to find
          ## the corresponding box number
          card<-cardseq[card]
          
          ## Record which card has been opened
          opencard.seq[card]<-1
          
          loop.len<-loop.len+1 ## Increase the loop by 1
        }
        v.sim[loop.len]<-1 ## Enter 1 for the loop length occured
      } 
    }
    
    
    
    ## Update the frequency in vector v by adding v.sim
    v<-v+v.sim
    
  }
  
  
  v/nreps ## Output: Vector with length 2n
}

## ----------------------------------------------------------------------------

## Calculating the probability that there does not exist a loop longer than 50
## for n=50,nreps=10000

u<-dloop(50,10000)
prob<-1 - sum(u[51:100])
## prob is really close to the theoretical probability for strat 1, n=100

## Plotting the probability that there exists a loop of lentgh L=1,...,100

## Plot title
title=c('Probability that there exists at least one loop up to length 100') 


plot(seq(1,100,1),u, main = title,xlab='Loop length',ylab='Probability',
         type='l')



## ----------------------------------------------------------------------------

end_time <- Sys.time()
runtime <- end_time - start_time
 
## ----------------------------------------------------------------------------


