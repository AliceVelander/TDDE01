#---- ASSIGNMENT 2--------

machines=read.csv2("machines.csv")

maxloglike=function(theta,x_vector){
  n=length(x_vector[,1])
  return(n*log(theta)-theta*sum(x_vector)) 
}
max_theta=function(x_vector){ 
    n=length(x_vector[,1])
    return(n/(sum(x_vector)))
}

theta_curve = curve(maxloglike(x, machines),
                    from=0, to=5, add=FALSE, ylim=c(0,-150),
                    xlab="theta", ylab="maxloglike")
max_theta(machines) 
maxloglike(1.126217, machines) 
max(theta_curve[["y"]]) 

#3
small_machines=t(machines[1:6,])
theta_curve_small = curve(maxloglike(x, small_machines),
                          from=0, to=5, add=TRUE, ylim=c(0,-150),
                          xlab="theta", ylab="maxloglike")
max_theta(small_machines) 

#4
bae=function(theta,x_vector, lambda){
  n=length(x_vector[,1])
  return(log(lambda) -theta*lambda + n*log(theta) - theta*sum(x_vector))
}

max_bae=function(x_vector, lambda){
    n=length(x_vector[,1])
    return(n/(lambda+sum(x_vector)))
}


theta_curve = curve(bae(x, machines, 10), from=0, to=5, add=TRUE, ylim=c(0,-150), xlab="theta", ylab="loglikelihood") 
max_bae(machines, lambda = 10) 
bae(0.9121907, machines, 10)

#5
maxloglike_randomX=function(theta){
  n=length(x_vector[,1])
  return(n*log(theta)-theta*sum(x_vector)) 
}
theta=max_theta(machines)
original = hist(machines$Length, xlim=c(0,max(machines)+1), breaks=10, col=rgb(0.5,1,1,0.5),
                xlab="Machines Values", ylab="Life Length", 
                main="Histogram on actual and generated values")
par(new=TRUE)
newHist = hist(rexp(50, rate=theta), 
               xlim=c(0,max(machines)+1), breaks=10,
               col=rgb(0.2,0.2,0.1,0.5),
               xlab="", ylab="", main="")