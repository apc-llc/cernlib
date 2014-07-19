*
* $Id: frfilhw.F,v 1.1.1.1 1996/01/11 14:05:21 mclareni Exp $
*
* $Log: frfilhw.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:21  mclareni
* Fritiof
*
*
C*************************** END FRSAVEN ********************************
 
C********************************* FRFILHW ******************************
 
      SUBROUTINE FRFILHW
 
C     TO ADD THOSE VECTOR BOSONS,HIGGS ETC (if they are produced from
C     parton subprocesses) TO LUJETS
 
      PARAMETER (KSZJ=4000)
      COMMON/LUJETS/N,K(KSZJ,5),P(KSZJ,5),V(KSZJ,5)
      COMMON/FRCNUT/NR,KR(10,5),PR(10,5),NR0
      SAVE /LUJETS/,/FRCNUT/
 
      IF(NR.LE.0) RETURN
      DO 10 I = 1, NR
       N= N+1
       DO 20 J=1, 5
       K(N, J) = KR(I,J)
20     P(N, J) = PR(I,J)
        K(N,1)=1
        K(N,3)=0
10    CONTINUE
 
      RETURN
      END
