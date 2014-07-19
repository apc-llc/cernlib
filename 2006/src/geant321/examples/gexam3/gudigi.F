*
* $Id: gudigi.F,v 1.1.1.1 1995/10/24 10:22:14 cernlib Exp $
*
* $Log: gudigi.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:14  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GUDIGI
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       User routine to digitize one event                       *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcnum.inc"
C      DIMENSION NUMVS(1),NUMBV(1,30),HITS(7,30),ITRA(30)
C      DIMENSION XL(3),XR(3),PARL(3),PARR(3)
C      DIMENSION X(3),XNEW(3),XP(3),DX(3),XPNEW(3)
C      DIMENSION X1(6),X2(6),XINT(6),PZINT(4)
C      DIMENSION XSAV(2,6,30),YSAV(2,6,30)
C      DATA NUMVS/0/
C      DATA DX/0.,0.,0./
C      DATA NPL/6/
C      DATA S1,S2/0.,0./
C.
C.    ------------------------------------------------------------------
C.
C
C          READING OF TRANSFORMATION AND SHAPE PARAMETERS
C
C      IF(ISWIT(10).EQ.0)GO TO 99
C      CALL GLOOK('CAVE',IB(JVOLUM+1),NVOLUM,IVO)
C      JVO =IB(JVOLUM-IVO)
C      JINL=IB(JVO-6)
C      JINR=IB(JVO-7)
C      IROTL=B(JINL+4)
C      DO 3 I=1,3
C      XL(I)  =B(JINL+I+4)
C      PARL(I)=B(JINL+I+9)
C    3 CONTINUE
C      IROTR=B(JINR+4)
C      DO 4 I=1,3
C      XR(I)  =B(JINR+I+4)
C      PARR(I)=B(JINR+I+9)
C    4 CONTINUE
C      IF(IDEBUG.NE.0)THEN
C        IF(ISWIT(4).NE.0)THEN
C           WRITE(6,*)IROTL,XL(3),PARL(3)
C           WRITE(6,*)IROTR,XR(3),PARR(3)
C        ENDIF
C      ENDIF
C
C         TRANSFORMATION TO LOCAL REFERENCE FRAME
C
C      CALL GFHITS('DRFT','FDOU',1,7,30,0,NUMVS,ITRA,NUMBV,HITS,NHITS)
C
C      IF(NHITS.LE.0)GO TO 99
C      DO 10 IHIT=1,NHITS
C      DO 11 I=1,3
C      X(I) =HITS(I,IHIT)
C      XP(I)=HITS(I+3,IHIT)
C   11 CONTINUE
C      IF(NUMBV(1,IHIT).EQ.1)THEN
C            CALL GITRAN(X,XL,IROTL,XNEW)
C            CALL GITRAN(XP,XD,IROTL,XPNEW)
C            END IF
C      IF(NUMBV(1,IHIT).EQ.2)THEN
C            CALL GITRAN(X,XR,IROTR,XNEW)
C            CALL GITRAN(XP,XD,IROTR,XPNEW)
C            END IF
C      DO 12 I=1,3
C      HITS(I,IHIT)  =XNEW(I)
C      HITS(I+3,IHIT)=XPNEW(I)
C   12 CONTINUE
C   10 CONTINUE
C
C         INTERSECTION WITH SENSE PLANES
C
C      DZPL=2.*PARL(3)/FLOAT(NPL+1)
C      ZPLMIN=-PARL(3)+DZPL
C      DO 20 IHIT=1,NHITS
C      IF(ITRA(IHIT).NE.ITRA(IHIT+1))GO TO 20
C ********  BELOW CHANGE OF COORDINATES TO USE GIPLAN
C      DO 21 I=1,6,3
C      X1(I)  =-HITS(I,IHIT)
C      X1(I+1)= HITS(I+2,IHIT)
C      X1(I+2)= HITS(I+1,IHIT)
C      X2(I)  =-HITS(I,IHIT+1)
C      X2(I+1)= HITS(I+2,IHIT+1)
C      X2(I+2)= HITS(I+1,IHIT+1)
C   21 CONTINUE
C ********
C      ZPL=ZPLMIN
C      DO 22 IPL=1,NPL
C      ZMIN=AMIN1(X1(2),X2(2))
C      ZMAX=AMAX1(X1(2),X2(2))
C      IF((ZPL.LT.ZMIN).OR.(ZPL.GT.ZMAX))GO TO 22
C      CALL GIPLAN(ZPL,X1,X2,S1,S2,1,XINT,SINT,PZINT,IFLAG)
C ********  BACK TO LOCAL REFERENCE FRAME
C ********  AND SAVE INTERACTION COORDINATES
C      XSAV(NUMBV(1,IHIT),IPL,IHIT)=-XINT(1)
C      YSAV(NUMBV(1,IHIT),IPL,IHIT)= XINT(3)
C ********
C      ZPL=ZPL+DZPL
C   22 CONTINUE
C   20 CONTINUE
C
C           DIGITISATION
C
C
   99 RETURN
      END
