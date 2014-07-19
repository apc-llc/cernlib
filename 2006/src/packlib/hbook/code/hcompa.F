*
* $Id: hcompa.F,v 1.1.1.1 1996/01/16 17:07:33 mclareni Exp $
*
* $Log: hcompa.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:33  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 22/05/89  16.27.21  by  Rene Brun
*-- Author :
      SUBROUTINE HCOMPA(IDVECT,N)
*.==========>
*.           calculates the maximum and minimum value of all
*.           one-dim histograms referenced in IDVECT.
*.           if N.eq.0 or missing applies to all 1-dim
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcprin.inc"
      DIMENSION IDVECT(1)
*.___________________________________________
      J=0
      NARG=2
      CALL NOARG(NARG)
      XMIN=BIGP
      XMAX=-XMIN
      IF(NARG.EQ.1)GO TO 30
      IF(N.EQ.0)GO TO 30
*
*             USER GIVES AN ARRAY OF IDS
*
      DO 10 I=1,N
         ID=IDVECT(I)
         CALL HFIND(ID,'HCOMPA')
         IF(LCID.EQ.0)GO TO 10
         IF(JBIT(IQ(LCID+KBITS),1).NE.0)THEN
            XMA=HMAX(ID)
            XMI=HMIN(ID)
            XMAX=MAX(XMA,XMAX)
            XMIN=MIN(XMI,XMIN)
            J=1
         ENDIF
  10  CONTINUE
*
      IF(J.EQ.0)GO TO 90
      DO 20 I=1,N
         ID=IDVECT(I)
         CALL HFIND(ID,'HCOMPA')
         IF(LCID.EQ.0)GO TO 20
         IF(JBIT(IQ(LCID+KBITS),1).NE.0)THEN
            CALL HMAXIM(ID,XMAX)
            CALL HMINIM(ID,XMIN)
         ENDIF
  20  CONTINUE
      GO TO 99
*
*
*             APPLIES TO ALL 1-DIM HIST
*
  30  NRHIST=IQ(LCDIR+KNRH)
      DO 40 I=1,NRHIST
         ID=IQ(LTAB+I)
         LCID=LQ(LTAB-I)
         IF(JBIT(IQ(LCID+KBITS),1).NE.0)THEN
            XMA=HMAX(ID)
            XMI=HMIN(ID)
            XMAX=MAX(XMA,XMAX)
            XMIN=MIN(XMI,XMIN)
            J=1
         ENDIF
  40  CONTINUE
*
      IF(J.EQ.0)GO TO 90
      DO 50 I=1,NRHIST
         ID=IQ(LTAB+I)
         LCID=LQ(LTAB-I)
         IF(JBIT(IQ(LCID+KBITS),1).NE.0)THEN
            CALL HMAXIM(ID,XMAX)
            CALL HMINIM(ID,XMIN)
         ENDIF
  50  CONTINUE
      GO TO 99
*
  90  CALL HBUG('Histograms with different specifications','HCOMPA',0)
*
  99  RETURN
      END
