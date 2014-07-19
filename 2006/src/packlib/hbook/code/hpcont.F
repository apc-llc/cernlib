*
* $Id: hpcont.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hpcont.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 19/06/89  10.09.48  by  Rene Brun
*-- Author :
      SUBROUTINE HPCONT(LABEL,P,NS,ICASE,XMAXI,A,MST,B,ISIGNE,
     +            IEXP1,IEXP2)
*.==========>
*.              This routine prints array p in vertical direction
*.           the precision is limited to 5 lines. A decimal point
*.           is printed if any. negatives values are indicated by a -
*.           in the first line.
*.             a label dependant of ICASE is also printed.
*.           MSTEP = number of columns for a channel
*.           MST   = number of blanks before first character
*..=========> ( R.Brun )
      DIMENSION P(NS),A(NS),B(MST),FLAG(8),ID(4)
      CHARACTER*(*) LABEL
#include "hbook/hcunit.inc"
      INTEGER A,B,FLAG
#include "hbook/hcprin.inc"
*.___________________________________________
      IF(ICASE.EQ.7)MSTEP=4
      IEXP=0
*
*             NC is the effective number of channels
*
      IEDGE=0
      NC=NS/MSTEP
      NPZE=0
      CALL UCTOH(LABEL,FLAG,1,8)
      CALL VBLANK(ID,4)
      CALL VBLANK(A,NS)
      CALL VBLANK(B,MST)
      X=ABS(XMAXI)
      IF(X.LT.1.)IEDGE=1
      N=X
      NZERO=1
*
      X=N
      N=-1
      IF(X.GE.1.)N=LOG10(X+0.01)
      X=10.**N
      IVALUE=X
*
      NLINZE=0
      Z=X/20000.
      NOVAL=0
      M=1-MSTEP/2-MOD(MSTEP,2)
      IF(ICASE.EQ.5)M=1-MSTEP
      IF(ICASE.EQ.7)M=0
      DO 10 J=1,NC
         IF(P(J).GE.0.)GO TO 10
         NOVAL=1
         IEDGE=0
         KK=M+J*MSTEP
         A(KK)=IDG(39)
         NZERO=0
  10  CONTINUE
      IF(NZERO.NE.0)GO TO 40
      IF(NOVAL.EQ.0)GO TO 20
      IF(ICASE.NE.7)GO TO 15
      CALL VBLANK(ID,4)
      WRITE(LOUT,6000)B,ID,A
      GO TO 30
  15  CONTINUE
      WRITE(LOUT,1000)FLAG,B,A
      GO TO 30
  20  IF(ICASE.NE.7)GO TO 25
      CALL HBCDI(IVALUE,4,ID)
      IF(IVALUE.EQ.0)ID(4)=IDG(1)
      WRITE(LOUT,6000)B,ID,A
      GO TO 30
  25  CONTINUE
      WRITE(LOUT,2000)FLAG,IVALUE,B,A
  30  LL=1
      GO TO 160
  35  CONTINUE
*
*              PRINT maximum 5 lines
*
  40  I=0
  45  I=I+1
      JJ=0
*
*              Fill current line
*
      M=1-MSTEP/2-MOD(MSTEP,2)
      IF(ICASE.EQ.5)M=1-MSTEP
      IF(ICASE.EQ.7)M=0
      DO 80 J=1,NC
         M=M+MSTEP
         PDJ=ABS(P(J))+Z
         PJMOD=PDJ/X
         K=MOD(PJMOD,10.)+.00001
         IF(K.NE.0)GO TO 70
         IF(X.LT.1.)GO TO 50
         IF(PDJ.GE.X)GO TO 70
         NLINZE=NLINZE+1
         GO TO 60
  50     MODPJ=PDJ/X
         PJMOD=MODPJ
         DIF=ABS(P(J))/X-PJMOD-.1
         IF(DIF.GT.0.)GO TO 70
         JJ=JJ+1
         A(M)=IDG(1)
         GO TO 80
  60     JJ=JJ+1
         A(M)=IDG(41)
         GO TO 80
  70     A(M)=IDG(K+1)
  80  CONTINUE
*
      IF(NLINZE.EQ.NC)GO TO 120
      IF(JJ.EQ.NC.AND.IEXP.GT.1)GO TO 140
      IF(IEDGE.NE.1)GO TO 90
      IF(N-I+2.NE.0)GO TO 90
      IF(NPZE.EQ.1)GO TO 85
*
*              line where decimal point is printed
*
      IF(ICASE.NE.7)GO TO 83
      A(1)=IDG(38)
      CALL VBLANK(ID,4)
      WRITE(LOUT,6000)B,ID,A
      A(1)=IDG(41)
      GO TO 85
  83  CONTINUE
      WRITE(LOUT,3000)FLAG
  85  LL=2
      GO TO 160
  89  CONTINUE
      IF(ICASE.NE.7)GO TO 88
      CALL HBCDI(IVALUE,4,ID)
      IF(IVALUE.EQ.0)ID(4)=IDG(1)
      WRITE(LOUT,6000)B,ID,A
      GO TO 120
  88  CONTINUE
      WRITE(LOUT,2000)FLAG,IVALUE,B,A
      CALL VBLANK(FLAG,8)
      GO TO 120
  90  IF(N-I+2.EQ.1)GO TO 100
      IF(ICASE.NE.7)GO TO 95
      CALL HBCDI(IVALUE,4,ID)
      IF(IVALUE.EQ.0)ID(4)=IDG(1)
      WRITE(LOUT,6000)B,ID,A
      GO TO 110
  95  CONTINUE
      WRITE(LOUT,2000)FLAG,IVALUE,B,A
      CALL VBLANK(FLAG,8)
      GO TO 110
 100  IF(ICASE.NE.7)GO TO 105
      A(1)=IDG(38)
      CALL HBCDI(IVALUE,4,ID)
      IF(IVALUE.EQ.0)ID(4)=IDG(1)
      WRITE(LOUT,6000)B,ID,A
      A(1)=IDG(41)
      GO TO 106
 105  CONTINUE
      WRITE(LOUT,4000)FLAG,IVALUE,B,A
      CALL VBLANK(FLAG,8)
 106  NPZE=1
 110  LL=3
      GO TO 160
 120  X=X/10.
      NLINZE=NC+1
      IVALUE=X
      IF(I.LT.5)GO TO 45
*
 140  CALL HFORMA(2)
      GO TO 999
*
 160  IEXP=IEXP+1
      CALL UCTOH('*10**',FLAG,1,5)
      FLAG(6)=ISIGNE
      FLAG(7)=IEXP1
      FLAG(8)=IEXP2
      IF(IEXP2.EQ.IDG(41).OR.IEXP.GT.1)CALL VBLANK(FLAG,8)
*
      GO TO(35,89,120,999),LL
*
 1000 FORMAT(' ',8A1,5X,114A1)
 2000 FORMAT(' ',8A1,I4,1X,114A1)
 3000 FORMAT(' ',8A1,4X,1H.)
 4000 FORMAT(' ',8A1,I4,1H.,114A1)
 6000 FORMAT(' ',127A1)
 999  RETURN
      END
