*
* $Id: dzdsfk.F,v 1.1.1.1 1996/03/04 16:13:04 mclareni Exp $
*
* $Log: dzdsfk.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:04  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSFK
*      IMPLICIT NONE
#include "dzdzbrinc.inc"
      CHARACTER*4 CVALUE,CHYENO
      CHARACTER*8 CHOPT
      INTEGER NCH,NCH1, IVALUE, IKEYNR
      CALL KUGETI(IKEYNR)
      IF(IKEYNR.LE.0 .OR. IKEYNR.GT.9)THEN
         WRITE(*,*)'Illegal key, must be 0<key<10'
         GOTO 999
      ENDIF
      CALL KUGETI(IVALUE)
      CALL KUGETC(CVALUE,NCH)
      CALL KUGETC(CHOPT,NCH1)
      CALL KUGETC(CHYENO,NCH1)
      IF(CHYENO.EQ.'YES')THEN
         IAPPFL=1
      ELSE
         IAPPFL=0
      ENDIF
      IF     (CHOPT.EQ.'RESET')THEN
         KACMSK(IKEYNR)=0
      ELSE IF (CHOPT.EQ.'CHAR')THEN
         CALL UHTOC(CVALUE,KEYMSK(IKEYNR),4,4)
         KACMSK(IKEYNR)=1
      ELSE
         KEYMSK(IKEYNR)=IVALUE
         KACMSK(IKEYNR)=1
      ENDIF
      NCH=0
      DO 10 I=1,9
        IF(KACMSK(I).NE.0)NCH=NCH+1
10    CONTINUE
      NKYMSK=NCH
999   END
***********************************************************************
