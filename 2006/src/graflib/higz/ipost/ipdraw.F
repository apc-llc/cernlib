*
* $Id: ipdraw.F,v 1.2 2005/10/28 12:38:13 couet Exp $
*
* $Log: ipdraw.F,v $
* Revision 1.2  2005/10/28 12:38:13  couet
* - SVG driver, workstation type -779
*
* Revision 1.1.1.1  1996/02/14 13:11:06  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/09 26/09/94  15.22.25  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IPDRAW(NN,XW,YW)
*.===========>
*.
*.   Draw a line with  the points  XW,YW. If NN=1
*. this routine do a move only. If NN=0 the XW(1)
*. and YW(1) are  written  in the PostScript file
*. according to the current NT.  If NN>0 the line
*. is clipped as a line, if NN<0 the line is clip-
*. -ped as a fill area.
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/hipost.inc"
#include "higz/hiques.inc"
      DIMENSION XW(*),YW(*)
      DIMENSION XC(2),YC(2)
      PARAMETER(MWORK=50)
      DIMENSION IXWORK(MWORK),IYWORK(MWORK)
      LOGICAL LOMOVE,LOLINE,LOCLIP
*.______________________________________
*
#include "higz/hiwcps.inc"
#include "higz/hiwcsv.inc"
      IF(NN.LT.0)THEN
         N      = ABS(NN)
         LOLINE = .FALSE.
      ELSE
         N      = NN
         LOLINE = .TRUE.
      ENDIF
*
      IF(N.LE.1)THEN
         IXD=IXWCPS(XW(1))
         IYD=IYWCPS(YW(1))
         IF(TFLAG) THEN
            CALL IPTLIN('m',IXD,IYD)
         ELSE
            CALL IPIOUT(IXD)
            CALL IPIOUT(IYD)
            IF(N.EQ.0)RETURN
            CALL IPPSTF(2,' m')
         ENDIF
         RETURN
      ENDIF
*
      IF (TFLAG) THEN
         NWORK = 0
         LOCLIP = .FALSE.
         LOMOVE = .TRUE.
         DO 10 I=2,N
            XC(1) = XW(I-1)
            XC(2) = XW(I)
            YC(1) = YW(I-1)
            YC(2) = YW(I)
            IF(IGCLIP(XC,YC,X1W,X2W,Y1W,Y2W).NE.0)THEN
               IF(LOMOVE)THEN
                  IF(NN.GT.0)CALL IPDLIN(NWORK,IXWORK,IYWORK)
                  IXD0 = IXWCPS(XC(1))
                  IYD0 = IYWCPS(YC(1))
                  CALL IPTLIN('m',IXD0,IYD0)
                  LOMOVE = .FALSE.
               ENDIF
               IXD = IXWCPS(XC(2))
               IYD = IYWCPS(YC(2))
               NWORK = NWORK+1
               IXWORK(NWORK) = IXD-IXD0
               IYWORK(NWORK) = IYD-IYD0
               IXD0 = IXD
               IYD0 = IYD
               IF(NWORK.EQ.MWORK)CALL IPDLIN(NWORK,IXWORK,IYWORK)
               IF(IQUEST(50).NE.0)THEN
                  IF(LOLINE)LOMOVE = .TRUE.
                  LOCLIP = .TRUE.
               ENDIF
            ELSE
               IF(LOLINE)LOMOVE = .TRUE.
               LOCLIP = .TRUE.
            ENDIF
   10    CONTINUE
         CALL IPDLIN(NWORK,IXWORK,IYWORK)
      ELSEIF(VFLAG)THEN
         IXDI = IXWCSV(XW(1))
         IYDI = IYWCSV(YW(1))
         CALL IPKOUT(IXDI)
         CALL IPPSTF(1,',')
         CALL IPKOUT(IYDI)
         DO 22 I=2,N
            IXDI = IXWCSV(XW(I))
            IYDI = IYWCSV(YW(I))
            CALL IPIOUT(IXDI)
            CALL IPPSTF(1,',')
            CALL IPKOUT(IYDI)
   22    CONTINUE
      ELSE
         IXD0 = IXWCPS(XW(1))
         IYD0 = IYWCPS(YW(1))
         CALL IPIOUT(IXD0)
         CALL IPIOUT(IYD0)
         CALL IPPSTF(2,' m')
         IDX = 0
         IDY = 0
         DO 20 I=2,N
            IXDI = IXWCPS(XW(I))
            IYDI = IYWCPS(YW(I))
            IX   = IXDI-IXD0
            IY   = IYDI-IYD0
            IXD0 = IXDI
            IYD0 = IYDI
            IF(IX.NE.0.AND.IY.NE.0)THEN
               IF(IDX.NE.0)THEN
                  CALL IPMOVE(IDX,0)
                  IDX = 0
               ENDIF
               IF(IDY.NE.0)THEN
                  CALL IPMOVE(0,IDY)
                  IDY = 0
               ENDIF
               CALL IPMOVE(IX,IY)
            ELSEIF(IX.NE.0)THEN
               IF(IDY.NE.0)THEN
                  CALL IPMOVE(0,IDY)
                  IDY = 0
               ENDIF
               IF(IDX.EQ.0)THEN
                  IDX = IX
               ELSEIF(ISIGN(IX,IDX).EQ.IX)THEN
                  IDX = IDX+IX
               ELSE
                  CALL IPMOVE(IDX,0)
                  IDX = IX
               ENDIF
            ELSEIF(IY.NE.0)THEN
               IF(IDX.NE.0)THEN
                  CALL IPMOVE(IDX,0)
                  IDX = 0
               ENDIF
               IF(IDY.EQ.0)THEN
                  IDY = IY
               ELSEIF(ISIGN(IY,IDY).EQ.IY)THEN
                  IDY = IDY+IY
               ELSE
                  CALL IPMOVE(0,IDY)
                  IDY = IY
               ENDIF
            ENDIF
   20    CONTINUE
         IF(IDX.NE.0)CALL IPMOVE(IDX,0)
         IF(IDY.NE.0)CALL IPMOVE(0,IDY)
      ENDIF
*
      END
