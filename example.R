source('<path-to-file>/plotCC.R')
source('<path-to-file>/curveCC.R')

## =====================================================
## Standard R plot
## =====================================================
f <- quote(x^2-3)
g <- quote(x^3)

x <- seq(-3,3,length=50)
plot(x,eval(f), type='l', col='blue')
points(x,eval(g), type='l', col='red')
legend <- c(as.expression(f), as.expression(g) )
legend('bottomright', lty=1, col=c('blue','red'), legend=legend, bty='n')
title('ugly Standard better for functions')

par(las=1,cex.axis=.7,bty='l', pch=20, cex.main=.9, mar=c(4,5,3,2))
plot(x,eval(f), type='l', col='blue')
points(x,eval(g), type='l', col='red', add=T)
legend <- c(as.expression(f), as.expression(g) )
legend('bottomright', lty=1, col=c('blue','red'), legend=legend, bty='n')
title('Standard plot a little better')


## =====================================================
## Plot CC
## =====================================================
f <- quote(x^2-3)
g <- quote(x^3)

x <- seq(-3,3,length=50)
plotCC(x,eval(f), col='blue')
plotCC(x,eval(g), col='red', add=T)
legend <- c(as.expression(f), as.expression(g) )
legend('bottomright', lty=1, col=c('blue','red'), legend=legend, bty='n')
title('Using plotCC or curve CC')
dev.off()

## another method
f <- function(x) return(x^2-3)
g <- function(x) return(x^3)
x <- seq(-3,3,length=50)
plotCC(x,f(x), col='blue')
plotCC(x,g(x), col='red', add=T)
legend <- c(as.expression(bquote(f*(x)==x^2-3)), as.expression(bquote(f(x)==x^3)) )
legend('bottomright', lty=1, col=c('blue','red'), legend=legend, bty='n')
title('y=f(x)')

## =====================================================
## curve CC
## =====================================================
## not necessary to create x
f <- function(x) return(x^2-3)
g <- function(x) return(x^3)
curveCC(f,xmin=-3,xmax=3, ymax=6,  col='blue')
curveCC(g,xmin=-3,xmax=3, ymax=6,  col='red', add=T)
legend('bottomright', lty=1, col=c('blue','red'), legend=c('f','g'), bty='n')
