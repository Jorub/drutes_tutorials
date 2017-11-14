t=seq(0.01,14,0.04166667)
heat=sin(t*pi*2-pi/2)
grades=rnorm(14,0.015,0.004)
id=sample(14,14)
for(i in 1:14){
  scaling[(24*(i-1)+1):(24*i)]=rep(grades[id[i]],24)

}

heat_new=scaling*heat+rnorm(length(t),0,0.001)#-0.0001*t^2
plot(t,heat_new,type="l")
write.table(cbind(t,heat_new),file="102.bc")
