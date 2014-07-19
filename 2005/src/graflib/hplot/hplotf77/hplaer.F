*
* $Id: hplaer.F,v 1.4 2002/11/12 15:00:23 couet Exp $
*
* $Log: hplaer.F,v $
* Revision 1.4  2002/11/12 15:00:23  couet
* - EQUIVALENCE statement modified again. This version is more logical
*
* Revision 1.3  2002/11/12 14:56:52  couet
* - Mistake in the EQUIVALENCE statement (side effect on the previous mod).
*   The two Y errors vectors were pointing on the same adress.
*
* Revision 1.2  2001/12/07 15:19:55  couet
* - use higz include files from the higz include file directory.
*   option Z is now working in hplaer.F
*   option Z has been implemented in hplsym.F
*
* Revision 1.1.1.1  1996/01/19 10:49:57  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/06 01/11/95  10.05.56  by  O.Couet
*-- Author :
      SUBROUTINE HPLAER(XU,YU,DXU1,DXU2,DYU1,DYU2,N,CHOPT,ISYMB,USIZE)
*.==========>
*.
*.   Draws asymetric error bars defined by the user symbol ISYMB is
*. drawn at the centre of the errors according to CHOPT:
*.
*.   ' ' Coordinates are expressed in histogram coordinates
*.       (of the last drawn histogram). Error bars are drawn.
*.   'C' Coordinates are expressed in centimeters.
*.   '1' Small lines are drawn at the end of the error bars.
*.   '2' Error rectangles are drawn.
*.   '3' A filled area is drawn through the end points of the vertical
*.       error bars.
*.   '4' A smoothed filled area is drawn through the end points of the
*.       vertical error bars.
*.   '0' Turn off the symbols clipping.
*.   'W' The window and the axis are drawn.
*.   'H' The input data are ignored and the current histogram is used.
*.   'Z' The vectors XU,YU,DXU1,DXU2,DYU1 and DYU2 are taken into PAWC at the
*.       adresses IQUEST(81) and IQUEST(86)
*.
*. If ISYMB = 0 or SSIZE = 0. no symbol is drawn
*.
*..=========>
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl6.inc"
#include "hplot/hpl9.inc"
#include "hplot/hpl11.inc"
#include "hbook/hcbook.inc"
#include "hplot/quest.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
#include "higz/hilink.inc"
      EQUIVALENCE (LXU,LSTACK(1))  ,(LYU,LSTACK(2))
      EQUIVALENCE (LDXU1,LSTACK(3)),(LDXU2,LSTACK(4))
      EQUIVALENCE (LDYU1,LSTACK(5)),(LDYU2,LSTACK(6))
      CHARACTER*(*) CHOPT
      DIMENSION IOPT(8)
      EQUIVALENCE (IOPTC,IOPT(1)),(IOPT1,IOPT(2))
      EQUIVALENCE (IOPT2,IOPT(3)),(IOPT0,IOPT(4))
      EQUIVALENCE (IOPT3,IOPT(5)),(IOPT4,IOPT(6))
      EQUIVALENCE (IOPTW,IOPT(7)),(IOPTH,IOPT(8))
      LOGICAL LERR,DRMARK,LOSYMB
      DIMENSION XU(1),YU(1),DXU1(1),DYU1(1),DXU2(1),DYU2(1)
      DIMENSION XI(4),YI(4)
      DIMENSION XXI(4),YYI(4)
      DIMENSION RVAL(4)
*._____________________________
*
      CALL UOPTC(CHOPT,'C12034WH',IOPT)
      IF (IOPTH.NE.0)       IOPTW = 0
      IF (IOPT4.NE.0)       IOPT3 = 1
      IF (IOPT2+IOPT3.EQ.0) THEN
         IOPTE = 1
      ELSE
         IOPTE = 0
      ENDIF
      ISYM = ISYMB
      LERR = .FALSE.
*----
*      If necessary the adresses of the vectors are saved in
*      a link area.
*----
      IF(INDEX(CHOPT,'Z').NE.0)THEN
         LXU    = IQUEST(81)
         LYU    = IQUEST(82)
         LDXU1  = IQUEST(83)
         LDXU2  = IQUEST(84)
         LDYU1  = IQUEST(85)
         LDYU2  = IQUEST(86)
         IOPTZ = 1
      ELSE
         IOPTZ = 0
      ENDIF
*----
*      draw the frame if requested
*----
      IF (IOPTW.NE.0) THEN
         IF (IOPTZ.NE.0) THEN
            XMIN = Q(LXU)-Q(LDXU1)
            XMAX = Q(LXU)+Q(LDXU2)
            YMIN = Q(LYU)-Q(LDYU1)
            YMAX = Q(LYU)+Q(LDYU2)
            DO 10 I=1,N-1
               XMIN = MIN(Q(LXU+I)-Q(LDXU1+I),XMIN)
               XMAX = MAX(Q(LXU+I)+Q(LDXU2+I),XMAX)
               YMIN = MIN(Q(LYU+I)-Q(LDYU1+I),YMIN)
               YMAX = MAX(Q(LYU+I)+Q(LDYU2+I),YMAX)
   10       CONTINUE
         ELSE
            XMIN = XU(1)-DXU1(1)
            XMAX = XU(1)+DXU2(1)
            YMIN = YU(1)-DYU1(1)
            YMAX = YU(1)+DYU2(1)
            DO 20 I=2,N
               XMIN = MIN(XU(I)-DXU1(I),XMIN)
               XMAX = MAX(XU(I)+DXU2(I),XMAX)
               YMIN = MIN(YU(I)-DYU1(I),YMIN)
               YMAX = MAX(YU(I)+DYU2(I),YMAX)
   20       CONTINUE
         ENDIF
         DY   = 0.05*(YMAX-YMIN)
         DX   = 0.05*(XMAX-XMIN)
         CALL HPLFRA(XMIN-DX,XMAX+DX,YMIN-DY,YMAX+DY,' ')
      ENDIF
*----
*      LOCM must be set after the call to HPLFRA (if option W is
*      required) because HPLFRA changes the value of LOCM.
*----
      LOCM = IOPTC.NE.0
*----
*      set the graphics attributes
*----
      CALL IGQ('LWID',RLWID)
      CALL IGQ('FAIS',RFAIS)
      CALL IGQ('FASI',RFASI)
      CALL IGQ('FACI',RFACI)
      CALL IGQ('PLCI',RPLCI)
      CALL IGQ('BORD',RBORD)
      CALL HPLATT(1)
      IF (IOPTE.NE.0) CALL ISPLCI(IHCOL)
*----
*      initiate the first and last bin
*----
      IF (IOPTH.NE.0) THEN
         IF (LOZOOM) THEN
            IFIRST = NCMIN
            ILAST  = NCMAX
         ELSE
            IFIRST = 1
            ILAST  = ICMAX
         ENDIF
         CALL IGQ('MTYP',RMTYP)
         ISYM   = INT(RMTYP)
         SYMSIZ = TVSIZ(1)
         BXSIZ  = 0.5*SYMSIZ
         IF (ISYM.EQ.1) SYMSIZ = 0.01
      ELSE
         SYMSIZ = MAX(0.,USIZE)
         BXSIZ  = 0.5*SYMSIZ
         IFIRST = 1
         ILAST  = N
      ENDIF
*----
*      initiate the filled area drawing
*----
      IF (IOPT3.NE.0) THEN
         NP = ILAST-IFIRST+1
         IF (2*NP.GT.NMAX) THEN
            CALL HBUG('Too many points','HPLAER',0)
            GOTO 60
         ENDIF
         IF1 = 1
         IF2 = 2*NP
      ENDIF
*----
*      select the CM normalization transformation if requested
*----
      IF (LOCM) THEN
         IF (NTWIN.NE.1) CALL ISELNT(1)
         NTWIN = 1
      ELSE
         IF (NTWIN.NE.NTHIST) CALL ISELNT(NTHIST)
         NTWIN = NTHIST
      ENDIF
*----
*      define the offset of the error bars due to the symbol size
*----
      BYSIZ = BXSIZ
      S2X   = 0.5*SYMSIZ
      S2Y   = S2X
      IF (.NOT.LOCM) THEN
         CALL IGQWK(1,'NTWN',RVAL)
         RTX   = (RVAL(2)-RVAL(1))/(XHIGH-XLOW)
         S2X   = S2X*RTX
         BXSIZ = BXSIZ*RTX
         RTY   = (RVAL(4)-RVAL(3))/(YHIGH-YLOW)
         S2Y   = S2Y*RTY
         BYSIZ = BYSIZ*RTY
      ENDIF
*----
*      initiate the first XP in case a histogram is used
*----
      IF (IOPTH.NE.0) THEN
         IF (I6.EQ.0) THEN
            IF (LOGXFL) THEN
               XP = 10**XMINI+0.5*XXSIZE
            ELSE
               XP = XMINI+0.5*XXSIZE
            ENDIF
         ELSE
            LBINS = LQ(LCID-2)
            DELTA = Q(LBINS+IFIRST+1)-Q(LBINS+IFIRST)
            XP    = Q(LBINS+IFIRST)+0.5*DELTA
         ENDIF
      ENDIF
*----
*      if ISYM = 0 or SYMSIZ = 0. no symbol is drawn
*----
      LOSYMB = (SYMSIZ.NE.0..AND.ISYM.NE.0)
*----
*                      Loop over the points
*----
      DO 50 K=IFIRST,ILAST
*----
*      get the data
*
* XP      = X position of the current point
* YP      = Y position of the current point
* ABDX1   = Low X error
* ABDX2   = Up X error
* ABDY1   = Low Y error
* ABDY2   = Up Y error
* (XI,YI) = Error bars corrdinates
*
* If the option 'H' is on, these values are computed from the current
* histogram (1D or 2D). If the option 'H' is off, the input parameters
* are used.
*----
         IF (IOPTH.NE.0) THEN
            YP = HCX(K,1)*FACTOR
            IF (I6.EQ.0) THEN
               ABDX1 = RERRX*XXSIZE
            ELSE
               LBINS = LQ(LCID-2)
               DELTA = Q(LBINS+K+1)-Q(LBINS+K)
               ABDX1 = RERRX*DELTA
            ENDIF
            ABDY1 = HCX(K,2)*FACTOR
            ABDX2 = ABDX1
            ABDY2 = ABDY1
         ELSE
            IF (IOPTZ.NE.0) THEN
               XP    = Q(LXU+K-1)
               YP    = Q(LYU+K-1)
               ABDX1 = ABS(Q(LDXU1+K-1))
               ABDX2 = ABS(Q(LDXU2+K-1))
               ABDY1 = ABS(Q(LDYU1+K-1))
               ABDY2 = ABS(Q(LDYU2+K-1))
            ELSE
               XP    = XU(K)
               YP    = YU(K)
               ABDX1 = ABS(DXU1(K))
               ABDX2 = ABS(DXU2(K))
               ABDY1 = ABS(DYU1(K))
               ABDY2 = ABS(DYU2(K))
            ENDIF
         ENDIF
*
         IF (LOGYFL.AND.YP.LE.0.) THEN
            IF (IOPTH.EQ.0)LERR = .TRUE.
            GOTO 40
         ENDIF
         IF (LOGXFL.AND.XP.LE.0.) THEN
            IF (IOPTH.EQ.0) LERR = .TRUE.
            GOTO 40
         ENDIF
*
         XI(4) = XP
         XI(3) = XP
         XI(2) = XP+ABDX2
         XI(1) = XP-ABDX1
*
         YI(1) = YP
         YI(2) = YP
         YI(3) = YP-ABDY1
         YI(4) = YP+ABDY2
*----
*      take the LOG if necessary
*----
         IF (.NOT.LOCM) THEN
            DO 30 I=1,4
               IF (IOPTH.NE.0) THEN
                  IF (LOGXFL) XI(I) = LOG10(MAX(XI(I),10**XMINI))
                  IF (LOGYFL) YI(I) = LOG10(MAX(YI(I),10**YMINI))
               ELSE
                  IF (LOGXFL) THEN
                     XI(I) = LOG10(MAX(XI(I),10**XWMINI))
                  ENDIF
                  IF (.NOT.LOGYFL) THEN
                     YI(I) = YI(I)*FACTOR
                  ELSE
                     YI(I) = LOG10(MAX(YI(I),10**YWMINI))
                  ENDIF
               ENDIF
   30       CONTINUE
         ENDIF
*----
*      test if error bars are not outside the limits
*      otherwise they are truncated
*----
         IF (LOCM) THEN
            XI(1) = MAX(XI(1),XLOW)
            XI(2) = MIN(XI(2),XHIGH)
            YI(3) = MAX(YI(3),YLOW)
            YI(4) = MIN(YI(4),YHIGH)
         ELSE
            XI(1) = MAX(XI(1),XWMINI)
            XI(2) = MIN(XI(2),XWMAXI)
            YI(3) = MAX(YI(3),YWMINI)
            YI(4) = MIN(YI(4),YWMAXI)
         ENDIF
*----
*      If the option '0' is selected, test if the marker is on the frame
*      limits. If "Yes", the marker will be not drawn and the error bars
*      will be readjusted.
*----
         DRMARK = .TRUE.
         IF (IOPT0.EQ.0) THEN
            IF (((YI(1)-S2Y).LT.RVAL(3).AND.(YI(1)+S2Y).GT.RVAL(3))
     +     .OR.((YI(1)-S2Y).LT.RVAL(4).AND.(YI(1)+S2Y).GT.RVAL(4))
     +     .OR.((XI(3)-S2X).LT.RVAL(1).AND.(XI(3)+S2X).GT.RVAL(1))
     +     .OR.((XI(3)-S2X).LT.RVAL(2).AND.(XI(3)+S2X).GT.RVAL(2)))
     +      DRMARK = .FALSE.
         ENDIF
         DRMARK = DRMARK.AND.LOSYMB
*----
*      draw the error rectangles
*----
         IF (IOPT2.NE.0) THEN
            CALL IGBOX(XI(1),XI(2),YI(3),YI(4))
         ENDIF
*----
*      keep points for fill area drawing
*----
         IF (IOPT3.NE.0) THEN
            XLINE(IF1) = XI(3)
            XLINE(IF2) = XI(3)
            YLINE(IF1) = YI(4)
            YLINE(IF2) = YI(3)
            IF1        = IF1+1
            IF2        = IF2-1
         ENDIF
*----
*      draw the error bars
*----
         IF (IOPTE.NE.0) THEN
            YYI(3) = YI(3)
            YYI(4) = YI(1)-S2Y
            IF (.NOT.DRMARK) YYI(4) = YI(1)
            IF (YYI(3).LT.YYI(4)) CALL IPL(2,XI(3),YYI(3))
            YYI(3) = YI(1)+S2Y
            IF (.NOT.DRMARK) YYI(3) = YI(1)
            YYI(4) = YI(4)
            IF (YYI(3).LT.YYI(4)) CALL IPL(2,XI(3),YYI(3))
            XXI(1) = XI(1)
            XXI(2) = XI(3)-S2X
            IF (.NOT.DRMARK) XXI(2) = XI(3)
            IF (XXI(1).LT.XXI(2)) CALL IPL(2,XXI,YI)
            XXI(1) = XI(3)+S2X
            IF (.NOT.DRMARK) XXI(1) = XI(3)
            XXI(2) = XI(2)
            IF (XXI(1).LT.XXI(2)) CALL IPL(2,XXI,YI)
         ENDIF
*----
*      draw line at the end of the error bars
*----
         IF (IOPT1.NE.0) THEN
            XXI(1) = XI(3)-BXSIZ
            XXI(2) = XI(3)+BXSIZ
            YYI(1) = YI(3)
            YYI(2) = YI(3)
            IF (YI(3).LT.YI(1)-S2Y) CALL IPL(2,XXI,YYI)
            YYI(1) = YI(4)
            YYI(2) = YI(4)
            IF (YI(4).GT.YI(1)+S2Y) CALL IPL(2,XXI,YYI)
            YYI(1) = YI(1)-BYSIZ
            YYI(2) = YI(1)+BYSIZ
            XXI(1) = XI(1)
            XXI(2) = XI(1)
            IF (XI(1).LT.XI(3)-S2X) CALL IPL(2,XXI,YYI)
            XXI(1) = XI(2)
            XXI(2) = XI(2)
            IF (XI(2).GT.XI(3)+S2X) CALL IPL(2,XXI,YYI)
         ENDIF
*----
*      draw the marker
*----
         IF (DRMARK) CALL HPLSYM(XP,YP,1,ISYM,SYMSIZ,' ')
*----
*      increment XP in case of histogram
*----
   40    IF (IOPTH.NE.0) THEN
            IF (I6.EQ.0) THEN
               XP = XP+XXSIZE
            ELSE
               IF (K.LT.ILAST) THEN
                  LBINS = LQ(LCID-2)
                  DELTA = Q(LBINS+K+2)-Q(LBINS+K+1)
                  XP    = Q(LBINS+K+1)+0.5*DELTA
               ENDIF
            ENDIF
         ENDIF
   50 CONTINUE
*----
*      draw the filled area
*----
      IF (IOPT3.NE.0) THEN
         IF (IOPT4.NE.0) THEN
            CALL IGRAPH(2*NP,XLINE,YLINE,'FC')
         ELSE
            CALL IGRAPH(2*NP,XLINE,YLINE,'F')
         ENDIF
      ENDIF
*
      IF (LERR) CALL HBUG('Null or negative value with log scale'
     +,                   'HPLAER',0)
*
   60 CALL IGSET('LWID',RLWID)
      CALL IGSET('FAIS',RFAIS)
      CALL IGSET('FASI',RFASI)
      CALL IGSET('FACI',RFACI)
      CALL IGSET('PLCI',RPLCI)
      CALL IGSET('BORD',RBORD)
*
      END
