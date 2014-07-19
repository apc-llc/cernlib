*
* $Id: mnplot.F,v 1.1.1.1 1996/03/07 14:31:31 mclareni Exp $
*
* $Log: mnplot.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:31  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNPLOT(XPT,YPT,CHPT,NXYPT,NUNIT,NPAGWD,NPAGLN)
#include "minuit/d506dp.inc"
CC        plots points in array xypt onto one page with labelled axes
CC        NXYPT is the number of points to be plotted
CC        XPT(I) = x-coord. of ith point
CC        YPT(I) = y-coord. of ith point
CC        CHPT(I) = character to be plotted at this position
CC        the input point arrays XPT, YPT, CHPT are destroyed.
CC
      DIMENSION   XPT(*), YPT(*)
      CHARACTER*1 CHPT(*) ,  CHSAV,  CHBEST, CDOT, CSLASH, CBLANK
      PARAMETER (MAXWID=100)
      CHARACTER CLINE*100, CHMESS*30
      DIMENSION XVALUS(12)
      LOGICAL OVERPR
      DATA CDOT,CSLASH,CBLANK/ '.' , '/' , ' '/
      MAXNX = MIN(NPAGWD-20,MAXWID)
      IF (MAXNX .LT. 10)  MAXNX = 10
      MAXNY = NPAGLN
      IF (MAXNY .LT. 10)  MAXNY = 10
      IF (NXYPT .LE. 1)  RETURN
      XBEST = XPT(1)
      YBEST = YPT(1)
      CHBEST = CHPT(1)
C         order the points by decreasing y
      KM1 = NXYPT - 1
      DO 150 I= 1, KM1
      IQUIT = 0
      NI = NXYPT - I
      DO 140 J= 1, NI
      IF (YPT(J) .GT. YPT(J+1)) GO TO 140
        SAVX = XPT(J)
        XPT(J) = XPT(J+1)
        XPT(J+1) = SAVX
        SAVY = YPT(J)
        YPT(J) = YPT(J+1)
        YPT(J+1) = SAVY
        CHSAV = CHPT(J)
        CHPT(J) = CHPT(J+1)
        CHPT(J+1) = CHSAV
      IQUIT = 1
  140 CONTINUE
      IF (IQUIT .EQ. 0) GO TO 160
  150 CONTINUE
  160 CONTINUE
C         find extreme values
      XMAX = XPT(1)
      XMIN = XMAX
      DO 200 I= 1, NXYPT
        IF (XPT(I) .GT. XMAX)  XMAX = XPT(I)
        IF (XPT(I) .LT. XMIN)  XMIN = XPT(I)
  200 CONTINUE
      DXX = 0.001*(XMAX-XMIN)
      XMAX = XMAX + DXX
      XMIN = XMIN - DXX
      CALL MNBINS(XMIN,XMAX,MAXNX,XMIN,XMAX,NX,BWIDX)
      YMAX = YPT(1)
      YMIN = YPT(NXYPT)
      IF (YMAX .EQ. YMIN)  YMAX=YMIN+1.0
      DYY = 0.001*(YMAX-YMIN)
      YMAX = YMAX + DYY
      YMIN = YMIN - DYY
      CALL MNBINS(YMIN,YMAX,MAXNY,YMIN,YMAX,NY,BWIDY)
      ANY = NY
C         if first point is blank, it is an 'origin'
      IF (CHBEST .EQ. CBLANK)  GO TO 50
      XBEST = 0.5 * (XMAX+XMIN)
      YBEST = 0.5 * (YMAX+YMIN)
   50 CONTINUE
C         find scale constants
      AX = 1.0/BWIDX
      AY = 1.0/BWIDY
      BX = -AX*XMIN + 2.0
      BY = -AY*YMIN - 2.0
C         convert points to grid positions
      DO 300 I= 1, NXYPT
      XPT(I) = AX*XPT(I) + BX
  300 YPT(I) = ANY-AY*YPT(I) - BY
      NXBEST = AX*XBEST + BX
      NYBEST = ANY  - AY*YBEST - BY
C         print the points
      NY = NY + 2
      NX = NX + 2
      ISP1 = 1
      LINODD = 1
      OVERPR=.FALSE.
      DO 400 I= 1, NY
      DO 310 IBK= 1, NX
  310 CLINE (IBK:IBK) = CBLANK
      CLINE(1:1) = CDOT
      CLINE(NX:NX) = CDOT
      CLINE(NXBEST:NXBEST) = CDOT
      IF (I.NE.1 .AND. I.NE.NYBEST .AND. I.NE.NY)  GO TO 320
      DO 315 J= 1, NX
  315 CLINE(J:J) = CDOT
  320 CONTINUE
      YPRT = YMAX - FLOAT(I-1)*BWIDY
      IF (ISP1 .GT. NXYPT)  GO TO 350
C         find the points to be plotted on this line
        DO 341 K= ISP1,NXYPT
      KS = YPT(K)
      IF (KS .GT. I)  GO TO 345
      IX = XPT(K)
      IF (CLINE(IX:IX) .EQ.   CDOT)  GO TO 340
      IF (CLINE(IX:IX) .EQ. CBLANK)  GO TO 340
      IF (CLINE(IX:IX) .EQ.CHPT(K))  GO TO 341
      OVERPR = .TRUE.
C         OVERPR is true if one or more positions contains more than
C            one point
      CLINE(IX:IX) = '&'
      GO TO 341
  340 CLINE(IX:IX) = CHPT(K)
  341 CONTINUE
        ISP1 = NXYPT + 1
        GO TO 350
  345   ISP1 = K
  350 CONTINUE
      IF (LINODD .EQ. 1 .OR. I .EQ. NY)  GO TO 380
      LINODD = 1
      WRITE (NUNIT, '(18X,A)')       CLINE(:NX)
      GO TO 400
  380 WRITE (NUNIT,'(1X,G14.7,A,A)') YPRT, ' ..', CLINE(:NX)
      LINODD = 0
  400 CONTINUE
C         print labels on x-axis every ten columns
      DO 410 IBK= 1, NX
      CLINE(IBK:IBK) = CBLANK
      IF (MOD(IBK,10) .EQ. 1)  CLINE(IBK:IBK) = CSLASH
  410 CONTINUE
      WRITE (NUNIT, '(18X,A)')       CLINE(:NX)
C
      DO 430 IBK= 1, 12
  430 XVALUS(IBK) = XMIN + FLOAT(IBK-1)*10.*BWIDX
      ITEN = (NX+9) / 10
      WRITE (NUNIT,'(12X,12G10.4)')  (XVALUS(IBK), IBK=1,ITEN)
      CHMESS = ' '
      IF (OVERPR) CHMESS='   Overprint character is &'
      WRITE (NUNIT,'(25X,A,G13.7,A)') 'ONE COLUMN=',BWIDX, CHMESS
  500 RETURN
      END
