*
* $Id: hoper1.F,v 1.2 1998/01/06 11:26:41 couet Exp $
*
* $Log: hoper1.F,v $
* Revision 1.2  1998/01/06 11:26:41  couet
* - option L in MZDROP was missing. Without this option the following PAW macro
*   doesn't work:
*
* v/cr p3(3) r 0 1 0
* v/cr a(10) r 1 3 2 5 4 8 6 10 7 9
* 1d 1 '' 10 0 10
* put/con 1 a
* h/fit 1 p2.f q 3 p3
* add 1 1 2
* h/pl 2
*
* with p2.f:
*
*       REAL FUNCTION P2(X)
*       REAL X, PAR(3)
*       COMMON /PAWPAR/PAR
*       P2 = PAR(1) + PAR(2)*X + PAR(3)*X*X
*       END
*
* Revision 1.1.1.1  1996/01/16 17:07:44  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :          26/09/95  11.16.41  by  Julian Bunn
*-- Author :
      SUBROUTINE HOPER1(KOP,NC,LC1,LC2,LC3,C1,C2,IOPTB)
*.==========>
*.           Auxiliary of HOPERA for one projection
*.
*.       Binomial errors (IOPTB=1) only used for divisions.
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
      DIMENSION STAT(2)
#if !defined(CERNLIB_DOUBLE)
      DIMENSION SWX(4)
#endif
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION SWX(4)
#endif
*.___________________________________________
      IF(LC1.EQ.0.OR.LC2.EQ.0.OR.LC3.EQ.0)GO TO 99
      LW1=LQ(LC1)
      LW2=LQ(LC2)
      LW3=LQ(LC3)
      IF(LW1.EQ.0.OR.LW2.EQ.0.OR.LW3.EQ.0)THEN
         LW123=0
      ELSE
         LW123=LW1
      ENDIF
      D1=C1*C1
      D2=C2*C2
*
*             Contents
*
      KN=IQ(LC1+KNOENT)+IQ(LC2+KNOENT)
      IQ(LC3+KNOENT)=-1
      IF(I7.NE.0)THEN
         STAT(1)=C1*Q(LC1+KSTAT1)+C2*Q(LC2+KSTAT1)
         STAT(2)=D1*Q(LC1+KSTAT1+1)+D2*Q(LC2+KSTAT1+1)
         CALL UCOPY(Q(LC1+KSTAT1+2),SWX(1),4)
         CALL UCOPY(Q(LC2+KSTAT1+2),SWX(3),4)
         SWX(1)=C1*SWX(1)+C2*SWX(3)
         SWX(2)=C1*SWX(2)+C2*SWX(4)
      ENDIF
      DO 10 I=0,NC+1
         LCONT=LC1
         NB=IQ(LCONT+KNBIT)
C
C CHECK ERROR BANKS DON'T EXIST ...
C
         IF(I8.EQ.0.OR.I1.EQ.0)THEN
            R1=HCX(I,1)
            S1=ABS(R1)
            IF(LW1.NE.0.AND.I.GT.0.AND.I.LE.NC)THEN
               S1=HCX(I,2)
               S1=S1*S1
            ENDIF
         ELSE
            R1=Q(LC1+KCON1+I)
            IF(I.GT.0.AND.I.LE.NC)S1=Q(LW1+I)
         ENDIF
         LCONT=LC2
         NB=IQ(LCONT+KNBIT)
C
C CHECK ERROR BANKS DON'T EXIST ...
C
         IF(I8.EQ.0.OR.I1.EQ.0)THEN
            R2=HCX(I,1)
            S2=ABS(R2)
            IF(LW2.NE.0.AND.I.GT.0.AND.I.LE.NC)THEN
               S2=HCX(I,2)
               S2=S2*S2
            ENDIF
         ELSE
            R2=Q(LC2+KCON1+I)
            IF(I.GT.0.AND.I.LE.NC)S2=Q(LW2+I)
         ENDIF
         LCONT=LC3
         IF(I.GT.0.AND.I.LE.NC)THEN
            KW=LW3
         ELSE
            KW=0
         ENDIF
         NB=IQ(LCONT+KNBIT)
         IF(KOP.EQ.1)THEN
            R3=C1*R1+C2*R2
            IF(KW.NE.0)THEN
               S3=D1*S1+D2*S2
            ENDIF
         ELSEIF(KOP.EQ.2)THEN
            R3=C1*R1-C2*R2
            IF(KW.NE.0)THEN
               S3=D1*S1+D2*S2
            ENDIF
         ELSEIF(KOP.EQ.3)THEN
            R3=C1*R1*C2*R2
            IF(KW.NE.0)THEN
               S3=D1*D2*(S1*R2*R2+S2*R1*R1)
            ENDIF
         ELSEIF(KOP.EQ.4)THEN
            DIV=C2*R2
            IF ( ABS(DIV) .GT. 1.0E-10 ) THEN
               R3=C1*R1/DIV
               IF(KW.NE.0)THEN
                  IF(IOPTB.EQ.0)THEN
                     S3=D1*D2*(S1*R2*R2+S2*R1*R1)/DIV**4
                  ELSE
                     S3=ABS(R3*(1.-R3)/DIV)
                  ENDIF
               ENDIF
            ELSE
               R3=0.
               S3=0.
            ENDIF
         ENDIF
         CALL HFCX(I,R3)
         IF(KW.NE.0)THEN
            Q(LW3+I)=S3
         ENDIF
  10  CONTINUE
      IQ(LC3+KNOENT)=KN
      IF(I7.NE.0)THEN
         CALL UCOPY(STAT,Q(LC3+KSTAT1  ),2)
         CALL UCOPY(SWX ,Q(LC3+KSTAT1+2),4)
      ENDIF
*
*              Profile histogram
*
      IF(I8.NE.0.AND.LW123.NE.0)THEN
         LF1=LQ(LW1)
         LF2=LQ(LW2)
         LF3=LQ(LW3)
         IF(LF1.EQ.0.OR.LF2.EQ.0.OR.LF3.EQ.0)GO TO 30
         DO 20 I=1,NC
            R1=Q(LF1+I)
            R2=Q(LF2+I)
            IF(KOP.EQ.1)THEN
               R3=C1*R1+C2*R2
            ELSEIF(KOP.EQ.2)THEN
               R3=C1*R1-C2*R2
            ELSEIF(KOP.EQ.3)THEN
               R3=C1*R1*C2*R2
            ELSEIF(KOP.EQ.4)THEN
               DIV=C2*R2
               IF(DIV.NE.0.)THEN
                  R3=C1*R1/DIV
               ELSE
                  R3=0.
               ENDIF
            ENDIF
            Q(LF3+I)=R3
  20     CONTINUE
      ENDIF
*
*              Function
*
  30  LF1=LQ(LC1-1)
      IF(LF1.EQ.0)GO TO 99
      LF2=LQ(LC2-1)
      IF(LF2.EQ.0)GO TO 99
      LF3=LQ(LC3-1)
      IF(LF3.EQ.0)GO TO 99
      NCF1=IQ(LF1+2)-IQ(LF1+1)+1
      NCF2=IQ(LF2+2)-IQ(LF2+1)+1
      NCF3=IQ(LF3+2)-IQ(LF3+1)+1
      IF(NCF1.NE.NCF2)GO TO 99
      IF(NCF1.NE.NCF3)GO TO 99
      IF(NCF1+2.GT.IQ(LF1-1))GO TO 99
*        If standard function, drop HFIT bank
      IF(IQ(LF3-2).NE.0)THEN
         IF(LQ(LF3-1).NE.0)CALL MZDROP(IHDIV,LQ(LF3-1),'L')
      ENDIF
      DO 40 I=1,NCF1
         R1=Q(LF1+I+2)
         R2=Q(LF2+I+2)
         IF(KOP.EQ.1)THEN
            R3=C1*R1+C2*R2
         ELSEIF(KOP.EQ.2)THEN
            R3=C1*R1-C2*R2
         ELSEIF(KOP.EQ.3)THEN
            R3=C1*R1*C2*R2
         ELSEIF(KOP.EQ.4)THEN
            DIV=C2*R2
            IF(DIV.NE.0.)THEN
               R3=C1*R1/DIV
            ELSE
               R3=0.
            ENDIF
         ENDIF
         Q(LF3+I+2)=R3
  40  CONTINUE
*
  99  RETURN
      END
