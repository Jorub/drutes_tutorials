#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
# read and plot output with SHP

ln_args=length(args)
isNAmin=T
isNAmax=T
isNAplot=T
isNAleg=T
isNApar=T
legopts=c("bottomright", "bottom", "bottomleft", 
          "left", "topleft", "top", "topright", "right", "center","none")
assign_args=T
if(ln_args>0){
  options(warn=-1)
  i=1
  while(assign_args){
    switch(args[i],
           "-name"={plotname=args[i+1];isNAplot=F},
           "-legpos"={legpos=args[i+1];isNAleg=F;if(!any(legopts==legpos)){stop(paste("-legpos only takes",legopts," as keyword. You entered:",args[i+1]))}},
           "-min"={if(is.na(as.numeric(args[i+1])))
           {stop(paste("-min only takes numeric values. You entered:",args[i+1]))}
             else
             {lims_min=as.numeric(args[i+1]);isNAmin=F};
             if(lims_min<0){stop(paste("-min only takes positive values. You entered:",args[i+1]))}
             if(lims_min%%1!=0){stop(paste("-min only takes integer values. You entered:",args[i+1]))}
           },
           "-max"={if(is.na(as.numeric(args[i+1])))
           {stop(paste("-max only takes numeric values. You entered:",args[i+1]))}
             else
             {lims_max=as.numeric(args[i+1]);isNAmax=F};
             if(lims_max<0){stop(paste("-max only takes positive values. You entered:",args[i+1]))}
             if(lims_max%%1!=0){stop(paste("-max only integer values. You entered:",args[i+1]))}
           },
           "-man"={readline("Manual:      -name - defines plotname 
                            -min - first index of data to be plotted
                            -max - last index of data to be plotted
                            -legpos - position of legend in the plot");i=i-1
           },
           "-parallel"={para=as.numeric(args[i+1]);isNApar=F},
           stop(paste("no valid argument, options are -name, -min, -legpos or -man. You entered:",args[i])))
    i=i+2
    if(i>ln_args){
      assign_args=F
    }
           }
  
  }else{
    plotname=""
    lims_min=1
    lims_max=100
    isNAmin=T
    isNAmax=T
    isNAplot=T
    isNApar=T
    legpos="topleft"
    para=100000
  }

if(isNAplot){
  plotname=""
}
if(isNAleg){
  legpos="topleft"
}

if(isNAmin){
  lims_min=1
}

if(isNApar){
  para=1000000
}


if(isNAmax){
  lims_max=100
}

lims=lims_min:lims_max

#mycol=colorRampPalette(c("darkred", "darkorange","goldenrod1","forestgreen","deepskyblue","royalblue2","darkblue"))
mycol=colorRampPalette(c("darkred", "goldenrod1","forestgreen","deepskyblue","darkblue"))

read_data_plot=function(filedr1,filedr2,ext,col1,col2,col3,xlabs,ylabs,whatisplotted,skip=0,k=0,legpos=legpos,idfix=0,lims=lims,explot=T){
  allread=F
  obs=list()
  n=1
  while(!allread){
    filedr1x=paste(k,'/',filedr1,ext,sep="")
    filedr2x=paste(k,'/',filedr2,ext,sep="")
    if(file.exists(filedr1x)){
      obs[[n]]=read.table(filedr1x,skip=skip)
    }else if(file.exists(filedr2x)){
      obs[[n]]=read.table(filedr2x,skip=skip)
    }else{
      allread=TRUE
    }
    if(!allread){
      if(n==1){
        mins=min(obs[[n]][,col2])
        maxs=max(obs[[n]][,col2])
        xmins=min(obs[[n]][,col1])
        xmaxs=max(obs[[n]][,col1])
      }else{
        if(min(obs[[n]][,col2]) < mins){
          mins=min(obs[[n]][,col2])
        }
        if(max(obs[[n]][,col2]) > maxs){
          maxs=max(obs[[n]][,col2])
        }
        if(min(obs[[n]][,col1]) < xmins){
          xmins=min(obs[[n]][,col1])
        }
        if(max(obs[[n]][,col1]) > xmaxs){
          xmaxs=max(obs[[n]][,col1])
        }
      }
    }
    if(k==para){
      allread=TRUE
    }
    
    k=k+1
    n=n+1
  }
  
  ln_obs=length(obs)
  if(ln_obs>0){
    if(isNAmax){
      if(lims_min>1){
        if(lims[length(lims)]!=length(obs[[1]][,col1])){
          lims=lims_min:length(obs[[1]][,col1])
        }
      }else{
        if(lims[length(lims)]!=length(obs[[1]][,col1])){
          lims=1:length(obs[[1]][,col1])
          print("Entire domain is plotted")
        }
      }
      
      
    }
    mycolors=mycol(ln_obs)
    pname=paste("obs_",whatisplotted,"_", plotname,".png",sep="")
    idname=2
    while(file.exists(pname)){
      pname2=paste("obs_",whatisplotted,"_", plotname,idname,".png",sep="")
      #print(paste(pname, " already exists"))
      pname=pname2
      idname=idname+1
    }
    png(pname,width = 800, height = 600, units = "px")
    print(paste("plotting", pname))
    par(cex=1.9,mar=c(5,4.5,4,2))
    leg.txt=c()
    for(i in 1:ln_obs){
      if(i == 1){
        plot(obs[[i]][lims,col1],obs[[i]][lims,col2],type="l",pch=i,col=mycolors[i],ylab=ylabs,xlab=xlabs,xlim=c(xmins,xmaxs),ylim=c(mins,maxs),lwd=1.5,main=plotname)
      }else{
        par(new=T)
        plot(obs[[i]][lims,col1],obs[[i]][lims,col2],type="l",pch=i,col=mycolors[i],ylab="",xlab="",xlim=c(xmins,xmaxs),ylim=c(mins,maxs),axes=F,lwd=1.5,lty=i)
      } 
      leg.txt[i]=paste("parallel ",i-idfix)
      
    }
    if(explot){
      par(new=T)
      plot(exp_dat$V1,exp_dat[,col3],type="l",xlim=c(xmins,xmaxs),ylim=c(mins,maxs),xlab="",ylab="",lwd=3,col="white")
      par(new=T)
      plot(exp_dat$V1,exp_dat[,col3],type="p",pch=4,xlim=c(xmins,xmaxs),ylim=c(mins,maxs),xlab="",ylab="",lwd=2)
    }
    ncols=1
    if(ln_obs>8){
      ncols=2
    }
    if(ln_obs>12){
      ncols=3
    }
    if(legpos!="none"){
      legend(legpos,leg.txt,ncol=ncols,col=mycolors,seg.len=0.5,lwd=2,bty="n",lty=1:6,cex=0.5)
    }
    invisible(dev.off())
    
    if(explot){
      #compare to this time
      time_end=max(obs[[1]]$V1)
      exp_comp=max(which(exp_dat$V1<time_end))
      make_lin=c()
      for(i in 1:ln_obs){
        approx_sim=approx(y=obs[[i]][lims,col2],x=obs[[i]][lims,col1],xout=exp_dat$V1[1:exp_comp])
        make_lin[i]=summary(lm(approx_sim$y~exp_dat[1:exp_comp,col3]))$r.squared
      }
      rname=paste("r_squared_",whatisplotted,"_", plotname,".out",sep="")
      
      idname=2
      while(file.exists(rname)){
        rname2=paste("r_squared_",whatisplotted,"_", plotname,idname,".out",sep="")
        #print(paste(pname, " already exists"))
        rname=rname2
        idname=idname+1
      }
      write.table(make_lin,file=rname)
    }
  }
  return(ln_obs)
}

exp_dat=read.table("drutemp/drutes.conf/inverse_modeling/soil.dat",skip=3)

ln_obs=read_data_plot('out/obspt_ADER_in_liquid-1','drutemp/out/obspt_ADER_in_liquid-1'
                      ,'.out',col1=1,col2=2,col3=2,ylabs=expression(paste("concentration [M ",V^-1,"]",sep=""))
                      ,xlabs="time [min]",whatisplotted='conc1',idfix=0,lims=lims,legpos = legpos ,skip=10,k=1)
if(ln_obs>0){
  print(paste("plot of",ln_obs,"parallel executions created: concentration vs. time"))
}

ln_obs=read_data_plot('out/obspt_ADER_in_liquid-2','drutemp/out/obspt_ADER_in_liquid-2'
                      ,'.out',col1=1,col2=2,col3=3,ylabs=expression(paste("concentration [M ",V^-1,"]",sep=""))
                      ,xlabs="time [min]",whatisplotted='conc2',idfix=0,lims=lims,legpos = legpos ,skip=10,k=1)
if(ln_obs>0){
  print(paste("plot of",ln_obs,"parallel executions created: concentration vs. time"))
}

ln_obs=read_data_plot('out/obspt_ADER_in_liquid-3','drutemp/out/obspt_ADER_in_liquid-3'
                      ,'.out',col1=1,col2=2,col3=4,ylabs=expression(paste("concentration [M ",V^-1,"]",sep=""))
                      ,xlabs="time [min]",whatisplotted='conc3',idfix=0,lims=lims,legpos = legpos ,skip=10,k=1)
if(ln_obs>0){
  print(paste("plot of",ln_obs,"parallel executions created: concentration vs. time"))
}

# ln_obs=read_data_plot('out/obspt_RE_matrix-1','drutes-dev/out/obspt_RE_matrix-1'
#                       ,'.out',col1=1,col2=4,col3=2,ylabs=expression(paste("flux [L",T^-1,"]",sep=""))
#                       ,xlabs="time [min]",'inv_press_1',idfix=0,lims=lims,legpos = legpos ,skip=10,k=1,explot=F)
# if(ln_obs>0){
#   print(paste("plot of",ln_obs,"parallel executions created: obs. point 1 flux vs. time"))
# }
# 
# ln_obs=read_data_plot('out/obspt_RE_matrix-2','drutes-dev/out/obspt_RE_matrix-2'
#                       ,'.out',col1=1,col2=4,col3=3,ylabs=expression(paste("flux [L",T^-1,"]",sep=""))
#                       ,xlabs="time [min]",'inv_press_2',idfix=0,lims=lims,legpos = legpos ,skip=10,k=1,explot=F)
# if(ln_obs>0){
#   print(paste("plot of",ln_obs,"parallel executions created: obs. point 2 flux vs. time"))
# }
# 
# ln_obs=read_data_plot('out/obspt_RE_matrix-3','drutes-dev/out/obspt_RE_matrix-3'
#                       ,'.out',col1=1,col2=4,col3=4,ylabs=expression(paste("flux [L",T^-1,"]",sep=""))
#                       ,xlabs="time [min]",'inv_press_3',idfix=0,lims=lims,legpos = legpos ,skip=10,k=1,explot=F)
# if(ln_obs>0){
#   print(paste("plot of",ln_obs,"parallel executions created: obs. point 3 flux vs. time"))
# }
