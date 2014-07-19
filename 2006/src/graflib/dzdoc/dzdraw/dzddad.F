*
* $Id: dzddad.F,v 1.1.1.1 1996/03/04 16:13:01 mclareni Exp $
*
* $Log: dzddad.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:01  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDDAD
*     add dir to search path
*      IMPLICIT NONE
#include "dzdzbrinc.inc"
      INTEGER NCH
      CHARACTER*80 CHFILE
*
      CALL KUGETS(CHFILE,NCH)
      IF(NCH.LE.0)THEN
         WRITE(*,*)'No file name given'
         GOTO 999
      ENDIF
      IF(CHFILE(NCH:NCH).NE.'/')THEN
         CHFILE(NCH+1:NCH+1)='/'
      ENDIF
      IF(NDIADD.LT.MDIADD)THEN
          NDIADD=NDIADD+1
          CDIADD(NDIADD)=CHFILE
      ELSE
          WRITE(*,*)'No more adds allowed, max =',MDIADD
      ENDIF
999   END
 
*********************************************************************
