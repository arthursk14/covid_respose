####################################################
## functions
####################################################
  
  # Packages 
  
      require(np)
  
  # Unconditional output oriented
  
      unconditional.output.oriented <- function(x,y,m)
        {
          
          if(is.null(dim(x))){t=length(x)} else{t=length(x[,1])}
          
          f <- function(lambda,x,y,i,m) # define a function, depending on the efficiency score lambda
            {
              nsum = 0
              dsum = 0
              
              for (j in (1:t))
                {
                
                  if(is.null(dim(x)) & is.null(dim(y)))
                    {
                      n = as.numeric((x[j] <= x[i]) & (y[j] >= (y[i] * lambda))) # indicator function to test whether it is lower or higher than a specific value
                      d = as.numeric(x[j] <= x[i]) # in theory we have y[j] >= 0, or simply the empirical CDF on X
                    } else if(is.null(dim(x)))
                      {
                        # n = as.numeric((x[j] <= x[i]) & all(y[j,] >= (y[i,] * lambda)))
                        a = 0
                        b = 1
                        while (all(a == 0 & b <= length(y[1,])))
                        {
                          a = a + as.numeric(y[j,b] < (y[i,b] * lambda))
                          b = b + 1
                        }
                        n = as.numeric((x[j] <= x[i]) & (a == 0))
                        d = as.numeric(x[j] <= x[i])
                      } else if(is.null(dim(y)))
                        {
                          # n = as.numeric(all(x[j,] <= x[i,]) & (y[j] >= (y[i] * lambda)))
                          # d = as.numeric(all(x[j,] <= x[i,]))
                          u = 0
                          v = 1
                          while (all(u == 0 & v <= length(x[1,])))
                          {
                            u = u + as.numeric(x[j,v] > x[i,v])
                            v = v + 1
                          }
                          n = as.numeric((u == 0) & (y[j] >= (y[i] * lambda)))
                          d = as.numeric(u == 0)
                        } else
                          {
                            # n = as.numeric(all(x[j,] <= x[i,]) & all(y[j,] >= (y[i,] * lambda)))
                            # d = as.numeric(all(x[j,] <= x[i,]))
                            a = 0
                            b = 1
                            while (all(a == 0 & b <= length(y[1,])))
                            {
                              a = a + as.numeric(y[j,b] < (y[i,b] * lambda))
                              b = b + 1
                            }
                            u = 0
                            v = 1
                            while (all(u == 0 & v <= length(x[1,])))
                            {
                              u = u + as.numeric(x[j,v] > x[i,v])
                              v = v + 1
                            }
                            n = as.numeric((u == 0) & (a == 0))
                            d = as.numeric(u == 0)
                          }
                
                  nsum = n+nsum
                  dsum = d+dsum
                }
              
              if(dsum==0){dsum=1}
              return(1-(1-(nsum/dsum))^m)
            }
          
          eff.uncond = matrix(nrow=t,ncol=1) # define a vector to store the results 
          
          for (i in (1:t))
            {
              # print(i)
              eff = integrate(f,0,Inf,x=x,y=y,i=i,m=m,stop.on.error=F) # integrate from O to infinity
              eff.uncond[i] = eff$value
            }
          
          return(eff.uncond)
          
        }
      
        # Example
        
          x = seq(10,200,1) + rnorm(191)
          y = x + rnorm(191,10,20)
        
          plot(x,y)
          
          m10 = unconditional.output.oriented(x,y,10)
          m100 = unconditional.output.oriented(x,y,100)
          
          points(x,y*m10,col="red")
          points(x,y*m100,col="blue")
        
  # Unconditional input oriented
      
      unconditional.input.oriented <- function(x,y,m)
        {
          
          if(is.null(dim(x))){t=length(x)} else{t=length(x[,1])}
          
          f <- function(theta,x,y,i,m) # define a function, depending on the efficiency score theta
            {
              nsum = 0
              dsum = 0
              
              for (j in (1:t))
                {
                  
                  if(is.null(dim(x)) & is.null(dim(y)))
                    {
                      n = as.numeric((y[j] >= y[i]) & (x[j] <= (x[i] * theta))) # indicator function to test whether it is lower or higher than a specific value
                      d = as.numeric(y[j] >= y[i]) # in theory we have x[j] <= 0, or simply the survival function Y
                    } else if(is.null(dim(y)))
                      {
                        # n = as.numeric((y[j] >= y[i]) & all(x[j,] <= (x[i,] * theta)))
                        a = 0
                        b = 1
                        while (all(a == 0 & b <= length(x[1,])))
                        {
                          a = a + as.numeric(x[j,b] > (x[i,b] * theta))
                          b = b + 1
                        }
                        n = as.numeric((y[j] >= y[i]) & (a == 0))
                        d = as.numeric(y[j] >= y[i])
                      } else if(is.null(dim(x)))
                        {
                          # n = as.numeric(all(y[j,] >= y[i,]) & (x[j] <= (x[i] * theta)))
                          # d = as.numeric(all(y[j,] >= y[i,]))
                          u = 0
                          v = 1
                          while (all(u == 0 & v <= length(y[1,])))
                          {
                            u = u + as.numeric(y[j,v] < y[i,v])
                            v = v + 1
                          }
                          n = as.numeric((u == 0) & (x[j] <= (x[i] * theta)))
                          d = as.numeric(u == 0)
                        } else
                          {
                            # n = as.numeric(all(y[j,] >= y[i,]) & all(x[j,] <= (x[i,] * theta)))
                            # d = as.numeric(all(y[j,] >= y[i,]))
                            a = 0
                            b = 1
                            while (all(a == 0 & b <= length(x[1,])))
                            {
                              a = a + as.numeric(x[j,b] > (x[i,b] * theta))
                              b = b + 1
                            }
                            u = 0
                            v = 1
                            while (all(u == 0 & v <= length(y[1,])))
                            {
                              u = u + as.numeric(y[j,v] < y[i,v])
                              v = v + 1
                            }
                            n = as.numeric((u == 0) & (a == 0))
                            d = as.numeric(u == 0)
                          }
                              
                  nsum = n+nsum
                  dsum = d+dsum
                }
              
              if(dsum==0){dsum=1}
              return((1-(nsum/dsum))^m)
            }
          
          eff.uncond = matrix(nrow=t,ncol=1)
          
          for (i in (1:t))
            {
              # print(i)
              eff = integrate(f,0,Inf,x=x,y=y,i=i,m=m,stop.on.error=F)
              eff.uncond[i] = eff$value
            }
          
          return(eff.uncond)
          
        }
  
        # Example
          
          x = seq(10,200,1) + rnorm(191)
          y = x + rnorm(191,10,20)
          
          plot(y,x)
          
          m10 = unconditional.input.oriented(x,y,10)
          m100 = unconditional.input.oriented(x,y,100)
          
          points(y,x*m10,col="red")
          points(y,x*m100,col="blue")
          
        # Error - not anymore (much slower though)
          
          x1 = seq(10,200,1) + rnorm(191); x2 = seq(10,200,1) + rnorm(191); x = cbind(x1,x2)
          n100 = unconditional.input.oriented(x,y,100)
          
          plot(y,x1)
          points(y,x1*n100,col="blue")
          
          plot(y,x2)
          points(y,x2*n100,col="blue")
          
  # Conditional output oriented
  
      conditional.output.oriented <- function(x,y,z,m)
        {
          
          if(is.null(dim(x))){t=length(x)} else{t=length(x[,1])}
        
          dat = data.frame(z)
          bw = np::npudensbw(dat=dat, 
                              bwmethod="cv.ls", ckertype="epanechnikov", 
                              nmulti=1000, bandwidth.compute=T) # least squares cross-validation method to define bandwidth
          # summary(bw)
          
          f <- function(lambda,x,y,i,m)
            {
              
              ker <- npudens(bws=bw,cykertype="epanechnikov",cxkertype="epanechnikov",oxkertype="liracine",tdat=dat[i,],edat=dat)
              K <- ker$dens
              
              nsum = 0
              dsum = 0
              
              for (j in (1:t))
                {
                  
                  if(is.null(dim(x)) & is.null(dim(y)))
                    {
                      n = as.numeric((x[j] <= x[i]) & (y[j] >= (y[i] * lambda))) * K[j]
                      d = as.numeric(x[j] <= x[i]) * K[j]
                    } else if(is.null(dim(x)))
                      {
                        a = 0
                        b = 1
                        while (all(a == 0 & b <= length(y[1,])))
                        {
                          a = a + as.numeric(y[j,b] < (y[i,b] * lambda))
                          b = b + 1
                        }
                        n = as.numeric((x[j] <= x[i]) & (a == 0)) * K[j]
                        d = as.numeric(x[j] <= x[i]) * K[j]
                      } else if(is.null(dim(y)))
                        {
                          u = 0
                          v = 1
                          while (all(u == 0 & v <= length(x[1,])))
                          {
                            u = u + as.numeric(x[j,v] > x[i,v])
                            v = v + 1
                          }
                          n = as.numeric((u == 0) & (y[j] >= (y[i] * lambda))) * K[j]
                          d = as.numeric(u == 0) * K[j]
                        } else
                          {
                            a = 0
                            b = 1
                            while (all(a == 0 & b <= length(y[1,])))
                            {
                              a = a + as.numeric(y[j,b] < (y[i,b] * lambda))
                              b = b + 1
                            }
                            u = 0
                            v = 1
                            while (all(u == 0 & v <= length(x[1,])))
                            {
                              u = u + as.numeric(x[j,v] > x[i,v])
                              v = v + 1
                            }
                            n = as.numeric((u == 0) & (a == 0)) * K[j]
                            d = as.numeric(u == 0) * K[j]
                          }
                  
                  nsum = n+nsum
                  dsum = d+dsum
                }
              
              if(dsum==0){dsum=1}
              return(1-(1-(nsum/dsum))^m)
            }
          
          eff.cond = matrix(nrow=t,ncol=1)
          
          for (i in (1:t))
            {
              # print(i)
              eff = integrate(f,0,Inf,x=x,y=y,i=i,m=m,stop.on.error=F)
              eff.cond[i] = eff$value
            }
          
          return(eff.cond)
          
        }
  
        # Example
    
          x = seq(10,200,1) + rnorm(191)
          z = rexp(191)
          y = x*exp(0.15*z) + rnorm(191,10,10)
          
          plot(x,y)
          
          m10 = conditional.output.oriented(x,y,z,10)
          m100 = conditional.output.oriented(x,y,z,100)
          
          points(x,y*m10,col="red")
          points(x,y*m100,col="blue")
  
  # Conditional input oriented
      
      conditional.input.oriented <- function(x,y,z,m)
        {
          
          if(is.null(dim(x))){t=length(x)} else{t=length(x[,1])}
        
          dat = data.frame(z)
          bw = np::npudensbw(dat=dat, 
                             bwmethod="cv.ls", ckertype="epanechnikov", 
                             nmulti=1000, bandwidth.compute=T)
          # summary(bw)
          
          f <- function(theta,x,y,i,m)
            {
            
              ker <- npudens(bws=bw,cykertype="epanechnikov",cxkertype="epanechnikov",oxkertype="liracine",tdat=dat[i,],edat=dat)
              K <- ker$dens  
            
              nsum = 0
              dsum = 0
              
              for (j in (1:t))
                {
                  
                  if(is.null(dim(x)) & is.null(dim(y)))
                    {
                      n = as.numeric((y[j] >= y[i]) & (x[j] <= (x[i] * theta))) * K[j]
                      d = as.numeric(y[j] >= y[i]) * K[j]
                    } else if(is.null(dim(y)))
                      {
                        a = 0
                        b = 1
                        while (all(a == 0 & b <= length(x[1,])))
                        {
                          a = a + as.numeric(x[j,b] > (x[i,b] * theta))
                          b = b + 1
                        }
                        n = as.numeric((y[j] >= y[i]) & (a == 0)) * K[j]
                        d = as.numeric(y[j] >= y[i]) * K[j]
                      } else if(is.null(dim(x)))
                        {
                          u = 0
                          v = 1
                          while (all(u == 0 & v <= length(y[1,])))
                          {
                            u = u + as.numeric(y[j,v] < y[i,v])
                            v = v + 1
                          }
                          n = as.numeric((u == 0) & (x[j] <= (x[i] * theta))) * K[j]
                          d = as.numeric(u == 0) * K[j]
                        } else
                          {
                            a = 0
                            b = 1
                            while (all(a == 0 & b <= length(x[1,])))
                            {
                              a = a + as.numeric(x[j,b] > (x[i,b] * theta))
                              b = b + 1
                            }
                            u = 0
                            v = 1
                            while (all(u == 0 & v <= length(y[1,])))
                            {
                              u = u + as.numeric(y[j,v] < y[i,v])
                              v = v + 1
                            }
                            n = as.numeric((u == 0) & (a == 0)) * K[j]
                            d = as.numeric(u == 0) * K[j]
                          }
                          
                  nsum = n+nsum
                  dsum = d+dsum
                }
              
              if(dsum==0){dsum=1}
              return((1-(nsum/dsum))^m)
            }
          
          eff.cond = matrix(nrow=t,ncol=1)
          
          for (i in (1:t))
            {
              # print(i)
              eff = integrate(f,0,Inf,x=x,y=y,i=i,m=m,stop.on.error=F)
              eff.cond[i] = eff$value
            }
          
          return(eff.cond)
          
        }
  
        # Example
        
          x = seq(10,200,1) + rnorm(191)
          z = rexp(191)
          y = x*exp(0.15*z) + rnorm(191,10,10)
          
          plot(y,x)
          
          m10 = conditional.input.oriented(x,y,z,10)
          m100 = conditional.input.oriented(x,y,z,100)
          
          points(y,x*m10,col="red")
          points(y,x*m100,col="blue")
          
        # Error - not anymore (much slower though)
          
          x1 = seq(10,200,1) + rnorm(191); x2 = seq(10,200,1) + rnorm(191); x = cbind(x1,x2)
          n100 = conditional.input.oriented(x,y,z,100)
          
          plot(y,x1)
          points(y,x1*n100,col="blue")
          
          plot(y,x2)
          points(y,x2*n100,col="blue")
    
