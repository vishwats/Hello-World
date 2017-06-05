NPV<-function(cf, r = 0.1, t) {
  
  # cf -  cash flow at lease one negative number in the vector
  # r - discount rate
  # default discount rate 10%
  # t - project year
  
      npv<-cf/((1+r)^t)
      
      sum(npv)
      
}


between <- function(x,lower,upper,incbounds=TRUE)
{
  if(incbounds) x>=lower & x<=upper
  else x>lower & x<upper
}



# Find IRR
rate<- seq(0,1, length.out = 1000)
cash_flow<- c(-70000, 30000, 30000, 30000, 30000, 30000)
t = c(0,1,2,3,4,5)


for(r in rate) {
  
  
  
      if(!between(NPV(cash_flow, r, t), -100,100)) {
     
         next
      
                }
    
  
  print(paste("Discount Rate is ", round(r*100, 2), "%", "and NPV is ", NPV(cash_flow, r, t)))
  
  
  
}

  
  