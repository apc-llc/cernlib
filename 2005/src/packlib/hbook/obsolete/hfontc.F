*
* $Id: hfontc.F,v 1.1.1.1 1996/01/16 17:08:11 mclareni Exp $
*
* $Log: hfontc.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:11  mclareni
* First import
*
*
#include "sys/CERNLIB_machine.h"
#include "_hbook/pilot.h"
*CMZ :  4.21/08 17/01/94  12.37.34  by  Rene Brun
*-- Author :
      SUBROUTINE HFONTC(ID,N,IC,CHOPT)
*.==========>
*.           To copy user array IC into /HCBONTC/ if CHOPT=' '
*.           To copy /HCBONTC/ in IC if CHOPT='R'
*..=========> ( R. Brun )
*
#include "hcbook.inc"
#include "hcbont.inc"
      CHARACTER*(*) IC(1)
      CHARACTER*(*) CHOPT
*___________________________________________________________________
*
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF(IDPOS.LE.0)THEN
         CALL HBUG('Unknown histogram','HFONTC',ID)
         RETURN
      ENDIF
      LC=LQ(LTAB-IDPOS)
      IOPTC=JBIT(IQ(LC+KBITS),8)
      IF(INDEX(CHOPT,'R').NE.0)GO TO 15
*
      IF(IOPTC.EQ.0)RETURN
      NCHONT=MIN(N,MAXNC)
      DO 10 I=1,NCHONT
         CHONT(I)=IC(I)
  10  CONTINUE
      RETURN
*
  15  N=NCHONT
      IF(IOPTC.EQ.0)N=0
      DO 20 I=1,NCHONT
         IC(I)=CHONT(I)
  20  CONTINUE
      END
