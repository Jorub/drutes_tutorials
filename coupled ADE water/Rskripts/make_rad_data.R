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

heat_old=read.table("102.bc")
png('radiation_data.png',width=700,units='px')
plot(heat_old$t,heat_old$heat_new,type="l",xlab='',ylab='',col='darkred',cex.axis=1.3)
mtext(side=2,expression(paste('radiation in [W c',m^-3,']')),line=2,cex=1.5)
mtext(side=1,'time [days]',line=3,cex=1.5)
dev.off()


water=read.table("102_water.bc")
water_evap=water$V2[water$V2<=0]
water_prec=water$V2[water$V2>=-0.00001]
png('water_data.png',width=700,units='px')
plot(water$V1,water$V2,type="l",xlab='',ylab='',cex.axis=1.3,ylim=c(-0.8,4),xlim=c(0,14),col='darkblue')
par(new=T)
plot(water$V1[water$V2<0],water$V2[water$V2<0],lwd=2,type="l",xlab='',ylab='',col='darkred',cex.axis=1.3,ylim=c(-0.8,4),xlim=c(0,14))
mtext(side=2,expression(paste('water flux [cm',d^-1,']')),line=2,cex=1.5)
mtext(side=1,'time [days]',line=3,cex=1.5)
leg.text=c('precipitation','evaporation')
legend('top',leg.text,lty=1,col=c('darkblue','darkred'))
dev.off()