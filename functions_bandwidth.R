####################################################
## functions
####################################################
  
  # Packages 
  
      require(np)
  
  # Conditional output oriented
  
      conditional.output.oriented.bw <- function(x,y,z,m,bw)
        {
          
          if(is.null(dim(x))){t=length(x)} else{t=length(x[,1])}
        
          # summary(bw)
          
          f <- function(lambda,x,y,i,m)
            {
              
              ker <- npudens(bws=bw,cykertype="epanechnikov",cxkertype="epanechnikov",oxkertype="liracine",tdat=data.frame(z)[i,],edat=data.frame(z))
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
  
  # Conditional input oriented
      
      conditional.input.oriented.bw <- function(x,y,z,m,bw)
        {
          
          if(is.null(dim(x))){t=length(x)} else{t=length(x[,1])}
        
          # summary(bw)
          
          f <- function(theta,x,y,i,m)
            {
            
              ker <- npudens(bws=bw,cykertype="epanechnikov",cxkertype="epanechnikov",oxkertype="liracine",tdat=data.frame(z)[i,],edat=data.frame(z))
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
      