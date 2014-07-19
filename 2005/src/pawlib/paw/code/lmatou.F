*
* $Id: lmatou.F,v 1.1.1.1 1996/03/01 11:38:37 mclareni Exp $
*
* $Log: lmatou.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:37  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/13 27/09/93  09.04.53  by  Rene Brun
*-- Author :
      SUBROUTINE LMATOU(IGO,NVAR,IFROM,ITO,NDIM,INORM)
C
C
C     ******************************************************************
C     *                                                                *
C     *                                                                *
C     *       prints results of linear transformation
C     *                                                                *
C     *                                                                *
C     ******************************************************************
C
#include "paw/pclint.inc"
#include "hbook/hcunit.inc"
#include "paw/pcwk.inc"
C
C     ------------------------------------------------------------------
C
C
C        if IGO=1 first  entry : normal output
C           IGO=2 second entry : plot of eigenvalues
C           IGO=3 third  entry : more results on the transformation
C
      GO TO(10 ,20 ,100),IGO
C
C        first entry in program
C
   10 WRITE(LOUT,10000)
      WRITE(LOUT,10100)
      WRITE(LOUT,10200) NDIM,NVAR
*
      IF(INORM.NE.0) WRITE(LOUT,10300)
*
      IF(ITO.GT.IFROM) WRITE(LOUT,10400) IFROM,ITO
*
      GO TO 120
*
*       second entry -- output-average-rms-eigenvalues
*
   20 CONTINUE
*
      WRITE(LOUT,10500)
*
*        THERE IS A MAXIMUM OF 3 NUMBERS PER LIGNE
*
      NLG=(NVAR+2)/3
      IF(NLG.NE.1)GO TO 30
      WRITE(LOUT,10600) (I,I=1,NVAR)
      WRITE(LOUT,10700) (XA(K),K=1,NVAR)
      WRITE(LOUT,10800) (S(K),K=1,NVAR)
      GO TO 50
   30 CONTINUE
C
      DO 40  I=1,NLG
         II=(I-1)*3+1
         III=I*3
         IF(I.EQ.NLG) III=NVAR
         WRITE(LOUT,10600) (K,K=II,III)
         WRITE(LOUT,10700) (XA(K),K=II,III)
         WRITE(LOUT,10800) (S(K),K=II,III)
   40 CONTINUE
   50 CONTINUE
      IF(NVAR.LE.10) WRITE(LOUT,10900)
      IF(NVAR.GT.10)WRITE(LOUT,11000)
      IF(NLG.NE.1)GO TO 60
      WRITE(LOUT,11100) (I,I=1,NVAR)
      WRITE(LOUT,11200) (EIGVAL(K),K=1,NVAR)
      GO TO 80
C
   60 CONTINUE
      DO 70  I=1,NLG
         II=(I-1)*3+1
         III=I*3
         IF(I.EQ.NLG) III=NVAR
         WRITE(LOUT,11100) (K,K=II,III)
         WRITE(LOUT,11200) (EIGVAL(K),K=II,III)
   70 CONTINUE
   80 CONTINUE
*
*     print of the trace of the dispersion matrix
*
      WRITE(LOUT,11300)EIGVAL(NVAR+1)
*
*     print eigenvectors
*
      WRITE(LOUT,11400)
      DO 90  K=1,NVAR
         I1=(K-1)*NVAR+1
         I2=K*NVAR
         WRITE(LOUT,11500) K,(EIGVEC(L),L=I1,I2)
   90 CONTINUE
*
      GO TO 120
*
*        third entry -- plot of eigenvalues
*
  100 CONTINUE
*
      XMI = 1.
      XMA = FLOAT(NVAR+1)
      VMX = 0.
*
*
*
      CALL HBOOK1(2000,'HISTOGRAM OF EIGENVALUES',NVAR,XMI,XMA,VMX)
      CALL HIDOPT(2000,'BLAC')
      CALL HIDOPT(2000,'LOGY')
      CALL HBIGBI(2000,10)

      DO 110 I=1,NVAR
         CALL HFILL(2000,FLOAT(I),0.,EIGVAL(I))
  110 CONTINUE
*
      IF(IWK.NE.0)THEN
         CALL HPLOT(2000,'B','HIST',0)
      ELSEIF(IWK.EQ.0.OR.IWK.EQ.-2)then
         CALL HPRINT(2000)
      ENDIF
*
      GO TO 120
C
C
10000 FORMAT('1',T20,41('-')/,1(T20,'I',T60,'I',/),T20,'I',T26,'PRINCIPA
     +L COMPONENTS ANALYSIS',T60,'I',/,1(T20,'I',T60,'I',/),
     +T20,41('-'))
10100 FORMAT('0',/T10,5('-'),'PARAMETER OF PRINCIPAL COMPONENTS ANALYSIS
     +',5('-'))
10200 FORMAT('0',T10,'NUMBER OF VARIABLES',T60,'=',T70,I5/
     +'0',T10,'MAXIMUM NUMBER OF COMPONENTS USED',T60,'=',T70,I5)
10300 FORMAT('0',/T10,'THE VARIABLE OF THE ANALYSIS ARE NORMALIZED')
10400 FORMAT('0',T10,'DEBUG OF EVENTS BETWEEN EVENT NUMBER ',I5,
     +' AND EVENT NUMBER ',I5)
10500 FORMAT('0'/T10,5('-'),'CARACTERISTICS OF ORIGINAL DATA',5('-'))
10600 FORMAT('0',T10,'VARIABLE NUMBER ',3(I10,7X))
10700 FORMAT(T10,'AVERAGE',T27,3(E14.6,2X))
10800 FORMAT(T10,'ROOT MEAN SQUARE',T27,3(E14.6,2X))
10900 FORMAT('0'/T10,5('-'),'CHARACTERISTICS OF DISPERSION MATRIX',
     +5('-'))
11000 FORMAT('1'/T10,5('-'),'CHARACTERISTICS OF DISPERSION MATRIX',
     +5('-'))
11100 FORMAT('0',T10,'PRINCIPAL COMPO.',3(I10,7X))
11200 FORMAT(T10,'EIGENVALUE',T27,3(E14.6,2X))
11300 FORMAT('0',T10,5('-'),'TRACE OF DISPERSION MATRIX',5('-'),
     + 5X,E14.6)
11400 FORMAT('0'/T10,5('-'),'EIGENVECTORS OF DISPERSION MATRIX',5('-'))
11500 FORMAT('0',T10,'EIGENVECTOR(',I3,')',(T27,E14.6,2(2X,E14.6)))
  120 RETURN
      END
