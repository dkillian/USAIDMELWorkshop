# potential outcomes errata

## ALGEBRA ALERT

$Y_i=D_iY_i^1+(1-D_i)Y_i^0$
  
  ::: columns
::: {.column width="50%"}
::: columns
$D_i=1$
  
  -   $Y_i=1*Y_i^1+1*Y_i^0-1*Y_i^0$
    
    -   $Y_i=Y_i^1+Y_i^0-Y_i^0$
      
      -   $Y_i=Y_i^1$
        :::
        :::
        
        ::: {.column width="50%"}
      <div>
        
        $D_i=0$
          
          -   $Y_i=0*Y_i^1+1*Y_i^0-0*Y_i^0$
            
            -   $Y_i=0+Y_i^0-0$
              
              -   $Y_i=Y_i^0$
                
                </div>
                :::
                :::
                
                
                
                
                
                
                
                
                
                
                *** ![](three body circles.png){fig-align="right", fig.height=2, fig.width=3}
              

              
              
              ```{r fig.align="right", fig.height=2, fig.width=3}
              
              include_graphics("three body circles.png")
              ```
              
              ![](three body Newton.png){.absolute left=10 top=100 height="300" width="400"}
              
              
              ![](luke impossible.jpg){.absolute right=0 top=0 height="200" width="250"}
              
              
              
              
              
              
              
              
              ### Selection bias
              
              Under random assignment of treatment, the simple difference-in-means design estimates the average treatment effect (it is unbiased)
              
              We are hoping to estimate the average treatment effect, but there are bias terms to worry about
              
              Which means we need the selection bias term to be zero
              
              The selection bias term says that the difference in the pre-treatment outcome in the treatment group $(E\bigr[Y^0|D=1])$ and the pre-treatment outcome among the control group $(E\bigr[Y^0|D=1])$ is zero 
              

              
              
              
              
              
              ### Randomization solves the selection problem
              
              Under randomization, the answer is yes! 
                
                If we have the opportunity to randomize the assignment of treatment, it means that any other characteristics, both observed and unobserved, will be approximately equal across the two groups
              
              So the selection bias term drops out (it equals zero), leaving us with only the average treatment effect
              
              Let's test this out
              
              
              # From differences-in-mean to differences-in-differences 


Under random assignment of treatment, the simple difference-in-means design estimates the average treatment effect (it is unbiased)

We must know the treatment assignment mechanism, in order to know if our estimates are unbiased, and if not how they may be biased away from the true estimate 

In this example, the treatment assignment mechanism is randomization