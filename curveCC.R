##===================================================================
## Function to draw Cartisian Coordenate Style with curve function
## --------------------------------------------------------------- 
# Autor: Diogo Augusto Ferrari
# 
#===================================================================

axisLimits = function(x,y, plot00, xmin, xmax, ymin, ymax){
  if(plot00){
    xmin = min(x)
    if(xmin>=0) xmin=0
    xmax = max(x)
    if(xmax<=0) xmax=0
    
    ymin = min(y)
    if(ymin>=0) ymin=0
    ymax = max(y)
    if(ymax<=0) ymax=0
  }else{
    xmin <- min(x)
    xmax <- max(x)
    ymin <- min(y)
    ymax <- max(y)
  }
  return(c(xmin,xmax,ymin,ymax))
}

drawAxis = function(xmin,xmax,ymin,ymax, hideXvalues, hideYvalues,
                    xmarks, ymarks, zmarks,labelXOrigPos, labelYOrigPos){
  arrows(xmin,   0   , xmax, 0, length=.1 )
  
  if (xmarks){
    if(hideXvalues){
      axis(1,pos=0, cex.axis=.8,tck=.015, labels=F)
      axis(1,pos=0, cex.axis=.8,tck=-.015, labels=F)
    }else{
      if (labelXOrigPos){
        axis(1,pos=0, cex.axis=.8,tck=.015)
        axis(1,pos=0, cex.axis=.8,tck=-.015, label=F)  
      }else{
        axis(1,pos=0, cex.axis=.8,tck=.015, padj=-1.5)
        axis(1,pos=0, cex.axis=.8,tck=-.015, label=F, padj=-1.5)  
      }
    }
  }
  if (ymarks){
    arrows(0,   ymin   , 0, ymax, length=.1 ) 
    if(hideYvalues){
      axis(2, pos=0,las=1, cex.axis=.8, tck=.015, labels=F)
      axis(2, pos=0,tck=-.015, labels=F)
    }else{
      if (labelYOrigPos){
        axis(2, pos=0,las=1, cex.axis=.8, tck=.015)
        axis(2, pos=0,label=F,tck=-.015)  
      }else{
        axis(2, pos=0,las=1, cex.axis=.8, tck=.015, hadj=0.3)
        axis(2, pos=0,label=F,tck=-.015, hadj=0.3)  
      }
    }
  }
}

drawVirtualAxis = function(xmin,xmax,ymin,ymax, hideXvalues, hideYvalues,
                           xmarks, ymarks, zmarks, ...){
  arrows(xmin,   ymin   , xmax, ymin, length=.1 )
  if (xmarks){
    if(hideXvalues){
      axis(1,pos=ymin, cex.axis=.8,tck=.015, labels=F)
      axis(1,pos=ymin, cex.axis=.8,tck=-.015, labels=F)
    }else{
      axis(1,pos=ymin, cex.axis=.8,tck=.015)
      axis(1,pos=ymin, cex.axis=.8,tck=-.015, label=F)  
    }
  }
  
  arrows(xmin,   ymin   , xmin, ymax, length=.1 ) 
  if (ymarks){
    if(hideYvalues){
      axis(2, pos=xmin,las=1, cex.axis=.8, tck=.015, labels=F)
      axis(2, pos=xmin,tck=-.015, labels=F)
    }else{
      axis(2, pos=xmin,las=1, cex.axis=.8, tck=.015)
      axis(2, pos=xmin,label=F,tck=-.015)  
    }
  }
  
}


addPoints = function(x,y,z=NA, pchStyle,...){
  if (is.na(z[1])){
    points(x,y, pch=pchStyle,...)
  }else{
    points(x,y,z, pch=pchStyle,...)
  }
}

curveCC = function(expression, z=F,xmin=NA, xmax=NA, ymin=NA, ymax=NA, zmin=F, zmax=F, type='l',
                  xmarks=T, ymarks=T, zmarks=T, label='', xlabelsLength=F,addGrid=F,
                  ylabelsLength=F, zlabelsLength=F, hideXvalues=F, hideYvalues=F,
                  plot00=T,addPoints=F, pch=20, labelXOrigPos=F, labelYOrigPos=F,
                  xlab='', ylab='', add=F, marb=0, marl=0, marr=0, maru=1,...){
    par.original <- par(no.readonly=TRUE);
    par(mar=c(marb,marl,maru,marr))
    if (add){
        curve(expression,add=T,...)
  }else{
    x <- curve(expression)$x
    y <- curve(expression)$y
    x <- x[!is.nan(y)]
    y <- y[!is.nan(y)]
    x <- x[!is.infinite(y)]
    y <- y[!is.infinite(y)]
    
    axisLimit <- axisLimits(x,y, plot00, xmin, xmax, ymin, ymax)
    if (is.na(xmin[1])) xmin <- axisLimit[1]
    if (is.na(xmax[1])) xmax <- axisLimit[2]
    if (is.na(ymin[1])) ymin <- axisLimit[3]
    if (is.na(ymax[1])) ymax <- axisLimit[4]
    
    curve(expression, type=type, axes=F, xlab=xlab, ylab=ylab, xlim=c(xmin,xmax), ylim=c(ymin,ymax), pch=pch,...)
  
  
    if(addGrid) grid()
    if(addPoints)addPoints(x,y,z, pch)
    if (plot00){
      drawAxis(xmin,xmax,ymin,ymax, hideXvalues, hideYvalues,xmarks, ymarks, zmarks,
               labelXOrigPos, labelYOrigPos)
    }else{
      drawVirtualAxis(xmin,xmax,ymin,ymax, hideXvalues, hideYvalues,xmarks, ymarks, zmarks,
                      labelXOrigPos, labelYOrigPos)
    }  
  }
    par(par.original)
}

#===================================================================
