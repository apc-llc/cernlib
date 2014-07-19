*
* $Id: iearc.F,v 1.2 1996/09/25 14:56:58 couet Exp $
*
* $Log: iearc.F,v $
* Revision 1.2  1996/09/25 14:56:58  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:21  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.12/15 16/07/91  10.13.22  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IEARC(ICF,ICHOIO)
*.===========>
*.
*..==========> (O.Couet)
#include "higz/hiques.inc"
#include "higz/hiatt.inc"
#include "higz/higed.inc"
#include "higz/hiflag.inc"
#include "higz/hiloc.inc"
      CHARACTER*4 CHVAL(15)
*.______________________________________
*
      ICHOIO=0
   10 IPLACE=2
      CALL IGREQ(21,3,16,IPLACE,ICHOIC,CHVAL)
      IF(ICHOIC.EQ.-1000)GOTO 10
*
   20 X(1)=RQUEST(13)
      Y(1)=RQUEST(14)
      XN=RQUEST(11)
      YN=RQUEST(12)
      NTLOC=IQUEST(10)
*
      IF(IPLACE.EQ.3)GOTO 10
*
      IF(IPLACE.EQ.1)THEN
         ICF=1
         ICHOIO=ICHOIC
         RETURN
      ENDIF
*
      IF(ICHOIC.EQ.-1)THEN
         CALL IEMACA(IPLACE,ICHOIC)
         GOTO 20
      ENDIF
*
      IF(ICHOIC.EQ.-2)THEN
         CALL IGCLES
         CALL IZPICT(EDIPIC,'D')
         CALL IEGRID
         GOTO 10
      ENDIF
*
      IF(ICHOIC.EQ.-3)THEN
         CALL IZUNDO
         GOTO 10
      ENDIF
*
      IF(ICHOIC.GE.1)THEN
         ICF=2
         ICHOIO=ICHOIC
         RETURN
      ENDIF
*
      ZFLAG=.TRUE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      CALL ISELNT(NTLOC)
      CALL IGGRID(X(1),Y(1))
      CALL IGPLOT(X(1),Y(1))
#if defined(CERNLIB_GL)||defined(CERNLIB_X11)
   30 CALL IGLOC(31,NTLOC,IBN,XNDC,YNDC,X(2),Y(2))
#endif
#if (!defined(CERNLIB_GL))&&(!defined(CERNLIB_X11))
   30 CALL IGLOC(41,NTLOC,IBN,XNDC,YNDC,X(2),Y(2))
#endif
      IF(IBN.EQ.0)GOTO 10
      CALL IGGRID(X(2),Y(2))
      CALL IEALPT(X(1),Y(1))
      OLDXP=XN
      OLDYP=YN
      IF(NTLOC.NE.INTR)GOTO 30
      CALL IGPLOT(X(2),Y(2))
   40 CALL IGLOC(41,NTLOC,IBN,XNDC,YNDC,X(3),Y(3))
      IF(IBN.EQ.0)THEN
         XC=X(1)
         YC=Y(1)
         R1=SQRT((X(2)-X(1))**2+(Y(2)-Y(1))**2)
         R2=R1
         PHIMIN=0.
         PHIMAX=0.
         GOTO 60
      ELSE
         CALL IGGRID(X(3),Y(3))
         CALL IEALPT(X(2),Y(2))
         OLDXP=XN
         OLDYP=YN
      ENDIF
      IF(NTLOC.NE.INTR)GOTO 40
      CALL IGPLOT(X(3),Y(3))
*
   50 CALL IGLOC(31,NTLOC,IBN,XNDC,YNDC,X(4),Y(4))
      IF(IBN.EQ.0)THEN
         XC=X(1)
         YC=Y(1)
         R1=SQRT((X(2)-X(1))**2+(Y(2)-Y(1))**2)
         R2=R1
         R3=SQRT((X(3)-X(1))**2+(Y(3)-Y(1))**2)
         DELTA = Y(2) - Y(1)
         IF (DELTA .NE. 0) THEN
           DELTA = SIGN(MIN(1.0,ABS(DELTA/R1)),DELTA)
           PHIMIN=(ASIN(DELTA)*180.)/3.1415926
           IF(X(2).LT.X(1)) THEN
             PHIMIN=180-PHIMIN
           ELSE IF(Y(2).LT.Y(1)) THEN
             PHIMIN=PHIMIN+360
           END IF
         ELSE
           PHIMIN = 0.0
         END IF
*
         DELTA = Y(3) - Y(1)
         IF (DELTA .NE. 0) THEN
           DELTA = SIGN(MIN(1.0,ABS(DELTA/R3)),DELTA)
           PHIMAX=(ASIN(DELTA)*180.)/3.1415926
           IF(X(3).LT.X(1)) THEN
             PHIMAX=180-PHIMAX
           ELSE IF(Y(3).LT.Y(1)) THEN
             PHIMAX=PHIMAX+360
           END IF
         ELSE
           PHIMAX = 0.0
         END IF
         GOTO 60
      ELSE
         XC=X(1)
         YC=Y(1)
         R1=SQRT((X(2)-X(1))**2+(Y(2)-Y(1))**2)
         R2=SQRT((X(4)-X(1))**2+(Y(4)-Y(1))**2)
         R3=SQRT((X(3)-X(1))**2+(Y(3)-Y(1))**2)
         DELTA = Y(2) - Y(1)
         IF (DELTA .NE. 0) THEN
           DELTA = SIGN(MIN(1.0,ABS(DELTA/R1)),DELTA)
           PHIMIN=(ASIN(DELTA)*180.)/3.1415926
           IF(X(2).LT.X(1)) THEN
             PHIMIN=180-PHIMIN
           ELSE IF(Y(2).LT.Y(1)) THEN
             PHIMIN=PHIMIN+360
           END IF
         ELSE
           PHIMIN = 0.0
         END IF
*
         DELTA = Y(3) - Y(1)
         IF (DELTA .NE. 0) THEN
           DELTA = SIGN(MIN(1.0,ABS(DELTA/R3)),DELTA)
           PHIMAX=(ASIN(DELTA)*180.)/3.1415926
           IF(X(3).LT.X(1)) THEN
             PHIMAX=180-PHIMAX
           ELSE IF(Y(3).LT.Y(1)) THEN
             PHIMAX=PHIMAX+360
           END IF
         ELSE
           PHIMAX = 0.0
         END IF
         CALL IGGRID(X(4),Y(4))
      ENDIF
*
   60 CALL ISFAIS(ICACFI)
      CALL ISFASI(ICACFS)
      CALL ISFACI(ICACFC)
      CALL ISPLCI(ICACLC)
      CALL IGSET('BORD',FLOAT(ICACBO))
      CALL IGARC(XC,YC,R1,R2,PHIMIN,PHIMAX)
      GOTO 10
*
      END
