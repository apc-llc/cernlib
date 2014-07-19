*
* $Id: igtab5.F,v 1.1.1.1 1996/02/14 13:10:39 mclareni Exp $
*
* $Log: igtab5.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:39  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/07 29/06/94  15.16.15  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGTAB5(RMIN,RMAX,ANG,PAR)
*.===========>
*.
*.  Draw the axis for lego and surface plots.
*.
*. _Input parameters:
*.
*.   REAL RMIN(3) : MIN axis labels
*.   REAL RMAX(3) : MAX axis labels
*.   REAL ANG : Angle between X and Y axis
*.   REAL PAR(14) : Axis Parameters
*.                      PAR(1)  =  NDVX
*.                      PAR(2)  =  NDVY
*.                      PAR(3)  =  NDVZ
*.                      PAR(4)  =  XCOL
*.                      PAR(5)  =  YCOL
*.                      PAR(6)  =  ZCOL
*.                      PAR(7)  =  XTIC
*.                      PAR(8)  =  YTIC
*.                      PAR(9)  =  ZTIC
*.                      PAR(10) =  VSIZ
*.                      PAR(11) =  VFON
*.                      PAR(12) =  XVAL
*.                      PAR(13) =  YVAL
*.                      PAR(14) =  ZVAL
*.                      PAR(15) =  XLOG
*.                      PAR(16) =  YLOG
*.                      PAR(17) =  ZLOG
*.
*.   After a call to this routine, the axis position are returned
*.   in RQUEST(70-81).
*.
*..==========> (O.Couet)
#include "higz/hipack.inc"
#include "higz/hihid.inc"
#include "higz/hiatt.inc"
#include "higz/hiques.inc"
#include "higz/hiatnb.inc"
#include "higz/hisave.inc"
      DIMENSION PAR(*)
      CHARACTER*8       CHOPAX,CHOPAY,CHOPAZ
      REAL              RMIN(3),RMAX(3),AV(3,8),R(3,8)
      REAL              X1(3),X2(3),Y1(3),Y2(3),Z1(3),Z2(3)
      DOUBLE PRECISION  RAD,COSA,SINA
      DATA EPSIL /0.001/
*.______________________________________
*
      IF(ISYS.GT.1)RETURN
*
      RAD    = ATAN(1.D0)*4./180.
      COSA   = COS(ANG*RAD)
      SINA   = SIN(ANG*RAD)
      CALL IHAXIV(RMIN,RMAX,ANG,AV,IX1,IX2,IY1,IY2,IZ1,IZ2)
      DO 10 I=1,8
         R(1,I) = AV(1,I) + AV(2,I)*COSA
         R(2,I) = AV(2,I)*SINA
         R(3,I) = AV(3,I)
   10 CONTINUE
      CALL IHWTON(R(1,IX1),X1)
      CALL IHWTON(R(1,IX2),X2)
      CALL IHWTON(R(1,IY1),Y1)
      CALL IHWTON(R(1,IY2),Y2)
      CALL IHWTON(R(1,IZ1),Z1)
      CALL IHWTON(R(1,IZ2),Z2)
*
*              Initialize the axis options
*
      IF(X1(1).GT.X2(1))THEN
         CHOPAX = 'SDHV=+'
      ELSE
         CHOPAX = 'SDHV=-'
      ENDIF
      IF(Y1(1).GT.Y2(1))THEN
         CHOPAY = 'SDHV=+'
      ELSE
         CHOPAY = 'SDHV=-'
      ENDIF
      CHOPAZ = 'SDH+='
*
*              Option LOG is required ?
*
      IF(PAR(15).NE.0.)CHOPAX(8:8) = 'G'
      IF(PAR(16).NE.0.)CHOPAY(8:8) = 'G'
      IF(PAR(17).NE.0.)CHOPAZ(8:8) = 'G'
*
*              Initialize the number of division. If the
*              number of divisions is negative, option 'N'
*              is required.
*
      NDIVX = INT(PAR(1)+0.1)
      NDIVY = INT(PAR(2)+0.1)
      NDIVZ = INT(PAR(3)+0.1)
      IF(NDIVX.LT.0)THEN
         NDIVX       = ABS(NDIVX)
         CHOPAX(7:7) = 'N'
      ENDIF
      IF(NDIVY.LT.0)THEN
         NDIVY       = ABS(NDIVY)
         CHOPAY(7:7) = 'N'
      ENDIF
      IF(NDIVZ.LT.0)THEN
         NDIVZ       = ABS(NDIVZ)
         CHOPAZ(7:7) = 'N'
      ENDIF
*
*              Axis attributes setting
*              The variables SCALE allows to rescale the VSIZ
*              in order to have the same label size for all angles.
*
      SCALEX = (RWXMAX-RWXMIN)/2.
      SCALEY = (RWYMAX-RWYMIN)/2.
      CALL IGSET('LWID',1.)
      CALL IGSET('PLCI',1.)
      CALL IGSET('TXFP',PAR(11))
      CALL IGSET('LASI',PAR(10)*SCALEY)
*
*              X axis drawing
*
      IF(ABS(X1(1)-X2(1)).GE.EPSIL.OR.ABS(X1(2)-X2(2)).GT.EPSIL)THEN
         CALL IGSET('TXCI',PAR(4)+0.1)
         CALL IGSET('TMSI',PAR(7)*SCALEY)
         CALL IGSET('LAOF',PAR(13)*SCALEY)
         IF(PAR(15).NE.0.)THEN
            BMIN=10**RMIN(1)
            BMAX=10**RMAX(1)
         ELSE
            BMIN=RMIN(1)
            BMAX=RMAX(1)
         ENDIF
         CALL IGAXIS(X1(1),X2(1),X1(2),X2(2),BMIN,BMAX,NDIVX,CHOPAX)
         RQUEST(70) = ((WNVPSV(2)-WNVPSV(1))/(RWXMAX-RWXMIN))
     +                *(X1(1)-RWXMIN)+WNVPSV(1)
         RQUEST(71) = ((WNVPSV(2)-WNVPSV(1))/(RWXMAX-RWXMIN))
     +                *(X2(1)-RWXMIN)+WNVPSV(1)
         RQUEST(72) = ((WNVPSV(4)-WNVPSV(3))/(RWYMAX-RWYMIN))
     +                *(X1(2)-RWYMIN)+WNVPSV(3)
         RQUEST(73) = ((WNVPSV(4)-WNVPSV(3))/(RWYMAX-RWYMIN))
     +                *(X2(2)-RWYMIN)+WNVPSV(3)
      ENDIF
*
*              Y axis drawing
*
      IF(ABS(Y1(1)-Y2(1)).GE.EPSIL.OR.ABS(Y1(2)-Y2(2)).GT.EPSIL)THEN
         CALL IGSET('TXCI',PAR(5)+0.1)
         CALL IGSET('TMSI',PAR(8)*SCALEY)
         CALL IGSET('LAOF',PAR(12)*SCALEY)
         IF(ABS(Z1(1)-Z2(1)).LT.EPSIL.AND. ABS(Z1(2)-Z2(2)).LT.EPSIL)
     +   CHOPAY='SDH+=N'
         IF(IDIM1.NE.0)THEN
            CHOPAY='V=+UN'
            NDIVY=0
         ENDIF
         IF(ABS(Y1(1)-Y2(1)).LT.EPSIL)THEN
            Y2(1)=Y1(1)
            CALL IGSET('LAOF',PAR(14))
         ENDIF
         IF(PAR(16).NE.0.)THEN
            BMIN=10**RMIN(2)
            BMAX=10**RMAX(2)
         ELSE
            BMIN=RMIN(2)
            BMAX=RMAX(2)
         ENDIF
         CALL IGAXIS(Y1(1),Y2(1),Y1(2),Y2(2),BMIN,BMAX,NDIVY,CHOPAY)
         RQUEST(74) = ((WNVPSV(2)-WNVPSV(1))/(RWXMAX-RWXMIN))
     +                *(Y1(1)-RWXMIN)+WNVPSV(1)
         RQUEST(75) = ((WNVPSV(2)-WNVPSV(1))/(RWXMAX-RWXMIN))
     +                *(Y2(1)-RWXMIN)+WNVPSV(1)
         RQUEST(76) = ((WNVPSV(4)-WNVPSV(3))/(RWYMAX-RWYMIN))
     +                *(Y1(2)-RWYMIN)+WNVPSV(3)
         RQUEST(77) = ((WNVPSV(4)-WNVPSV(3))/(RWYMAX-RWYMIN))
     +                *(Y2(2)-RWYMIN)+WNVPSV(3)
      ENDIF
*
*              Z axis drawing
*
      IF(ABS(Z1(1)-Z2(1)).GE.EPSIL.OR.ABS(Z1(2)-Z2(2)).GT.EPSIL)THEN
         CALL IGSET('TXCI',PAR(5)+0.1)
         CALL IGSET('TMSI',PAR(9)*SCALEX)
         CALL IGSET('LAOF',PAR(14)*SCALEX)
         IF(PAR(17).NE.0.)THEN
            BMIN=10**RMIN(3)
            BMAX=10**RMAX(3)
         ELSE
            BMIN=RMIN(3)
            BMAX=RMAX(3)
         ENDIF
         CALL IGAXIS(Z1(1),Z2(1),Z1(2),Z2(2),BMIN,BMAX,NDIVZ,CHOPAZ)
         RQUEST(78) = ((WNVPSV(2)-WNVPSV(1))/(RWXMAX-RWXMIN))
     +                *(Z1(1)-RWXMIN)+WNVPSV(1)
         RQUEST(79) = ((WNVPSV(2)-WNVPSV(1))/(RWXMAX-RWXMIN))
     +                *(Z2(1)-RWXMIN)+WNVPSV(1)
         RQUEST(80) = ((WNVPSV(4)-WNVPSV(3))/(RWYMAX-RWYMIN))
     +                *(Z1(2)-RWYMIN)+WNVPSV(3)
         RQUEST(81) = ((WNVPSV(4)-WNVPSV(3))/(RWYMAX-RWYMIN))
     +                *(Z2(2)-RWYMIN)+WNVPSV(3)
      ENDIF
*
      CALL ISLN(1)
      END
