*
* $Id: frord01.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frord01.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C************************************ FRORD01 **************************
 
      SUBROUTINE FRORD01(P,II,N,IQ)
 
C           Routine to arrange II so that
C           P(ii(1)) >= P(ii(2)) >= ... >= P(ii(n)), IF IQ>,=0
C           P(ii(1)) <= P(ii(2)) <= ... <= P(ii(n)), IF IQ<0
C
      real p(1)
      integer ii(1)
      logical done
 
      do 101 k=1,n
101   ii(k)=k
 
      do 110 nlim = n-1,1,-1
            done = .true.
              IF(IQ.GE.0) THEN
            do 120 k = 1,nlim
                  if ( p(ii(k)) .lt. p(ii(k+1)) ) then
                        done=.false.
                        itemp=ii(k)
                        ii(k)=ii(k+1)
                        ii(k+1)=itemp
                        end if
 120        continue
            if (done) return
              ELSE
            do 130 k = 1,nlim
                  if ( p(ii(k)) .gt. p(ii(k+1)) ) then
                        done=.false.
                        itemp=ii(k)
                        ii(k)=ii(k+1)
                        ii(k+1)=itemp
                        end if
 130        continue
            if (done) return
             ENDIF
110   continue
 
      RETURN
      END
