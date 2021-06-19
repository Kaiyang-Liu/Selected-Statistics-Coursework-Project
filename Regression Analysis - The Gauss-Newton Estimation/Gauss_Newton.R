GaussNewton <- function(fun,expression_dataFrame,parameter_string_wholeVersion,inital_point_string,max_iteration_time)  
{  
  
  num_independ_vars = dim(expression_dataFrame)[2] - 1  
  #first column is the dependent variable y; others are independent variables ...  
  for(i in 2:(num_independ_vars + 1))  
  {  
    eval(parse(text = paste(names(expression_dataFrame)[i]," = expression_dataFrame[[\"",names(expression_dataFrame)[i],"\"]]",sep="")))  
  }  
  
  #get parameter string  
  parameter_string=as.vector(unlist(strsplit(parameter_string_wholeVersion,",")))  
  num_parameter = length(parameter_string)  
  #get the string of initial point  
  parameter_point_string_wholeVersion = inital_point_string  
  #get parameter point(Vector version)  
  parameter_point_string = as.vector(unlist(strsplit(parameter_point_string_wholeVersion,",")))  
  parameter_point_numVector = as.numeric(parameter_point_string)  
  
  #inital SSE vector and parameter vector  
  ssres=c()  
  store_sigma=c()  
  
  # df:partial derivative function  
  df<-deriv(fun,parameter_string,function.arg = T)  
  
  #function to update the parameter string  
  updateParametersVector <- function(num_parameter,parameter_point_numVector){  
    n = 1  
    tempString = ''  
    while (n <= num_parameter)  
    {  
      tempString = paste(tempString,parameter_point_numVector[,n],sep = ',')  
      n = n + 1  
    }  
    parameter_point_string_wholeVersion = substring(tempString,2,nchar(tempString))  
    return(parameter_point_string_wholeVersion)  
  }  
  
  
  #Gauss-Newton Method part  
  i = 1  
  for (i in 1:max_iteration_time)  
  {  
    if(i==1){  
      partialDeriv_result = eval(parse(text = paste("df(",parameter_point_string_wholeVersion,")",sep="")))  
      #f_val : the function value at theta hat  
      f_val = as.vector(partialDeriv_result)  
      #J : jacobian matrix  
      J = attr(partialDeriv_result,"gradient")  
      #Store SSE into the vector(ssres[i+1] is the SSE for sigma i)  
      ssres=c(ssres,sum((y-f_val)^2))  
    }  
    
    #calculate the increment value  
    gamma = (solve(t(J)%*%J) %*% t(J)) %*% (y-f_val)  
    
    #Judge if we should terminate the iteration  
    judge<-all(abs(gamma)<stop_accuracy)  
    if (judge==TRUE)  
    {break}  
    
    #Update parameter Vector with the gamma increment  
    parameter_point_numVector = parameter_point_numVector + t(gamma)  
    #Update parameter String  
    parameter_point_string_wholeVersion = updateParametersVector(num_parameter,parameter_point_numVector)  
    
    
    partialDeriv_result = eval(parse(text = paste("df(",parameter_point_string_wholeVersion,")",sep="")))  
    #f_val : the function value at theta hat  
    f_val = as.vector(partialDeriv_result)  
    #J : jacobian matrix  
    J = attr(partialDeriv_result,"gradient")  
    
    #Store SSE into the vector(ssres[i+1] is the SSE for sigma i.)  
    ssres=c(ssres,sum((y-f_val)^2))  
    
    
    k = 1  
    while(ssres[i+1] > ssres[i])  
    {  
      
      #Back to previous parameter point  
      parameter_point_numVector = parameter_point_numVector - t(gamma)  
      
      #Use one half of previous increment to obtain the next parameter point  
      gamma = 0.5 * gamma  
      parameter_point_numVector = parameter_point_numVector + t(gamma)  
      
      #Update parameter String  
      parameter_point_string_wholeVersion = updateParametersVector(num_parameter,parameter_point_numVector)  
      
      
      partialDeriv_result = eval(parse(text = paste("df(",parameter_point_string_wholeVersion,")",sep="")))  
      #f_val : the function value at theta hat  
      f_val = as.vector(partialDeriv_result)  
      #J : jacobian matrix  
      J = attr(partialDeriv_result,"gradient")  
      
      #Update SSE vector  
      ssres[i+1]=sum((y-f_val)^2)  
      k = k + 1  
    }  
    
    #Update parameter String  
    parameter_point_string_wholeVersion = updateParametersVector(num_parameter,parameter_point_numVector)  
    
    #Store the parameter point  
    store_sigma = c(store_sigma,parameter_point_string_wholeVersion)  
    
  }  
  
  #Calculate the residual mean square  
  s_2=ssres[length(ssres+1)]/(length(y)-num_parameter)  
  
  #Calculate the covariance matrix  
  asyS=(s_2)*solve(t(J)%*%J)  
  #Calculate the t-value with the df:n-p  
  t_value=qt(0.975,df=length(y)-num_parameter)  
  
  #Upper bound for parameters' confidence intervals  
  uper=parameter_point_numVector+t_value*sqrt(diag(asyS))  
  #Lower bound for parameters' confidence intervals  
  lower=parameter_point_numVector-t_value*sqrt(diag(asyS))  
  
  #Output the result  
  list_return <- list(sqrt(diag(asyS)),uper,lower,ssres,store_sigma)  
  names(list_return) <- c('standError','uper','lower','SSres','Sigma')  
  
  return(list_return)  
  
}  



#Please input the data  
t=c(1,1,1,3,3,3,6,6,6,15,15,15,21,21,21)  
y=c(17,21,16,30,25,25,33,31,32,34,33,33,39,35,36)  


#Find the initial point  
s=exp(lm(log(y)~t)$coef[1])  
g=lm(log(y)~t)$coef[2]  


expression_dataFrame = data.frame(  
  # !!The  
  # The first column saves the data for dependent variable y  
  y = y,  
  # Other columns save the data for the other independent variables  
  t = t  
)  


#Please input the model in expression  
fun<-expression(s*(1-exp(-g*t)))  

#Please input the name of parameter in the ""(use ',' to separate)  
parameter_string_wholeVersion = "s,g"  


#Please input the initial point of parameter in the ""(use ',' to separate)  
inital_point_string="22.19713,0.0268686"  


#Please input the stop accuracy and max iteration times  
stop_accuracy=10^(-7)  
max_iteration_time = 30  


#Call the GaussNewton function  
GaussNewton(fun,expression_dataFrame,parameter_string_wholeVersion,inital_point_string,max_iteration_time)  
