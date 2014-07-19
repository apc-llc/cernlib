*
* $Id: frsearc.F,v 1.1.1.1 1996/01/11 14:05:20 mclareni Exp $
*
* $Log: frsearc.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:20  mclareni
* Fritiof
*
*
C********************************* END FRNUCDF **************************
 
 
C********************************* FRSEARC *******************************
 
       SUBROUTINE FRSEARC(L,fmax,xmin)
 
C...this is a routine for finding maximum and 'sufficient minimum'
C...of wood-saxon or harmonic oscillator.  xm, fm are outputs.
C...Input L=1 for proj, L=2 for target.
 
       dimension xx(4)
       xx(1)=101.
       do 10 j=1,3
        fmax=0.
        do 20 i=1,200
         x=(.01**(j-1))*(i-1)+xx(j)-100*(.01**(j-1))
       F = FRROR(L,X)
        if(f.gt.fmax) then
         fmax=f
         xx(j+1)=x
        endif
  20   continue
  10   continue
 
        x1=xx(4)
        fmin=fmax
       do 30 i=int(x1),50
        x=float(i)
       F = FRROR(L,X)
           if(f.lt.fmin) then
      fmin=f
      xy=x
         endif
        if(fmin.lt.1.E-3*fmax) goto 35
  30    continue
 
  35    do 40 i=1,200
         x=.01*float(i-1)+xy-1.
       F = FRROR(L,X)
            if(f.lt.fmin) then
       fmin=f
         xmin=x
            endif
  40    continue
 
        return
       end
