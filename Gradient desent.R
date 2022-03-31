x=c(0.8,1,1.2,1.4,1.6,1.8,2,2.2,2.4,2.6)
y=c(0.7,0.65,0.9,0.95,1.1,1.15,1.2,1.4,1.55,1.5)

## Standardization

stdx = ((x-min(x))/(max(x)-min(x)))
stdx

stdy = ((y-min(y))/(max(y)-min(y)))
stdy



## For iterations
iterations=1000

## To save thetha
his_thetha_list<-list(iterations)

X<-cbind(1,matrix(x))
X
alfa=0.01

thetha<-matrix(c(0.35,0.65 ),nrow = 2)
thetha

for(i in 1:iterations)
{


hthetha<-X%*%thetha
hthetha

difference<-(hthetha-y)
difference

## Partial derivative of thethe0
cost<-(t(X)%*%difference)/length(y)
cost

## Gradient Desent
thetha<-thetha-(alfa*cost)
thetha

his_thetha_list[[i]]<-thetha
}

print(thetha)
his_thetha_list


