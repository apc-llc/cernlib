*
* $Id: hplfun.F,v 1.1.1.1 1996/01/19 10:50:08 mclareni Exp $
*
* $Log: hplfun.F,v $
* Revision 1.1.1.1  1996/01/19 10:50:08  mclareni
* Hplot
*
*
#include "hplot/pilot.h"
*CMZ :  5.20/00 13/04/95  11.06.15  by  O.Couet
*-- Author :
      SUBROUTINE HPLFUN(XU,YU,N,CHOPT)
*.==========>
*.            Draws a smooth curve on the picture. the curve will
*.            be smoothed to appear like a function (NN +ve) or like
*.            a contour (NN -ve)
*.            (Transformation NTHIST or 1 is selected
*.            according to CHOPT)
*..=========>
#include "hplot/quest.inc"
#include "hplot/hpl1.inc"
#include "hplot/hpl2.inc"
#include "hplot/hpl9.inc"
#include "hbook/hcbook.inc"
*
      CHARACTER*(*) CHOPT
      DIMENSION XU(N),YU(N)
*._____________________________
*
      CALL IGQ('LWID',RLWID)
      CALL IGQ('FAIS',RFAIS)
      CALL IGQ('FASI',RFASI)
      CALL IGQ('FACI',RFACI)
      CALL IGQ('PLCI',RPLCI)
      CALL IGQ('BORD',RBORD)
*
      IF (N.LE.2) THEN
         CALL HPLINE(XU,YU,N,CHOPT)
         GOTO 999
      ENDIF
*----
*     cm are required ?
*----
      IF (INDEX(CHOPT,'C').NE.0) THEN
         LOCM=.TRUE.
      ELSE
         LOCM=.FALSE.
      ENDIF
*----
*     Set the function Attributes
*----
      CALL HPLATT(2)
*
      IF (LOCM) THEN
         IF (NTWIN.NE.1) CALL ISELNT(1)
         NTWIN = 1
         IF(.NOT.LOASTK)THEN
            CALL IGRAPH(N,XU,YU,'C')
         ELSE
            CALL IGRAPH(N,XU,YU,'*C')
         ENDIF
         GOTO 999
      ELSE
         IF (NTWIN.NE.NTHIST) CALL ISELNT(NTHIST)
         NTWIN = NTHIST
      ENDIF
*----
*     Create temporary banks to store the points.
*----
      IF (LQ(LHPLOT-3).NE.0) CALL MZDROP(IHDIV,LQ(LHPLOT-3),' ')
      IF (LQ(LHPLOT-4).NE.0) CALL MZDROP(IHDIV,LQ(LHPLOT-4),' ')
      CALL MZNEED(IHDIV,2*(N+1)+50,'G')
      IF (IQUEST(11).LT.0) THEN
         CALL HBUG('Not enough space in memory','HPLFUN',0)
         GOTO 999
      ENDIF
      CALL MZBOOK(IHDIV,LX,LHPLOT,-3,'TMPX',0,0,N+1,3,0)
      CALL MZBOOK(IHDIV,LY,LHPLOT,-4,'TMPY',0,0,N+1,3,0)
*----
*     Loop round all user points
*----
      DO 10 I=1,N
         IF (.NOT.LOGXFL) THEN
            Q(LX+I) = XU(I)
         ELSE
            IF(XU(I).LE.0.)GO TO 20
            Q(LX+I) = LOG10(XU(I))
         ENDIF
         YY = FLOARG(YU(I))*FACTOR
         IF(.NOT.LOGYFL)THEN
            Q(LY+I) = YY
         ELSE
            IF(YY.LE.0.)GO TO 20
            Q(LY+I) = LOG10(YY)
         ENDIF
   10 CONTINUE
*----
*     Draw the function
*----
      IQUEST(81) = LX+1
      IQUEST(82) = LY+1
      IF(.NOT.LOASTK)THEN
         CALL IGRAPH(N,Q(LX),Q(LY),'ZC')
      ELSE
         CALL IGRAPH(N,Q(LX),Q(LY),'Z*C')
      ENDIF
      GOTO 30
*
   20 CALL HBUG('Routine called with zero or negative argument with
     +log scale','HPLFUN',0)
*
   30 CALL MZDROP(IHDIV,LQ(LHPLOT-3),' ')
      CALL MZDROP(IHDIV,LQ(LHPLOT-4),' ')
      LQ(LHPLOT-3) = 0
      LQ(LHPLOT-4) = 0
*
  999 CALL IGSET('LWID',RLWID)
      CALL IGSET('FAIS',RFAIS)
      CALL IGSET('FASI',RFASI)
      CALL IGSET('FACI',RFACI)
      CALL IGSET('PLCI',RPLCI)
      CALL IGSET('BORD',RBORD)
*
      END
