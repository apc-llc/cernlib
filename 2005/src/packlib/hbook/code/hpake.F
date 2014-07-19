*
* $Id: hpake.F,v 1.1.1.1 1996/01/16 17:07:45 mclareni Exp $
*
* $Log: hpake.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:45  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.22/05 20/06/94  10.39.31  by  Rene Brun
*-- Author :
      SUBROUTINE HPAKE(IDD,E)
*.==========>
*.           Fills histogram (1-D or 2-D) with error bars
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
      DIMENSION E(1)
*.___________________________________________
      CALL HFIND(IDD,'HPAKE ')
      IF(LCID.EQ.0)GO TO 99
      CALL HDCOFL
*
*   2-D histogram
*
      IF(I230.NE.0)THEN
         LW=LQ(LCONT)
         IF(LW.EQ.0)THEN
            CALL HBAR2(IDD)
            IF(IERR.NE.0)GO TO 99
            LW=LQ(LCONT)
         ENDIF
         DO 20 I=1,IQ(LW-1)
            Q(LW+I)=E(I)*E(I)
  20     CONTINUE
         CALL SBIT0(IQ(LCID),5)
         GO TO 99
      ENDIF
*
*   1-D histogram
*
      IF(I9.EQ.0)THEN
         CALL HBARX(IDD)
         IF(IERR.NE.0)GO TO 99
      ENDIF
*
      CALL SBIT0(IQ(LCID),5)
      LW=LQ(LCONT)
      NCX=IQ(LCID+KNCX)
      DO 30 I=1,NCX
         Q(LW+I)=E(I)*E(I)
  30  CONTINUE
*
  99  RETURN
      END
