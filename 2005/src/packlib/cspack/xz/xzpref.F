*
* $Id: xzpref.F,v 1.1.1.1 1996/03/08 15:44:31 mclareni Exp $
*
* $Log: xzpref.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:31  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_IBMMVS)
      SUBROUTINE XZPREF(CPREFI,IRC)
*     ******************************
*
*     change MVS prefix
*
*
      CHARACTER*(*) CPREFI
      CHARACTER*(*) RPREFI
      CHARACTER*80  SPREFI
 
      SAVE           SPREFI
 
      IRC     = 0
      LCPREFI = LENOCC(CPREFI)
 
 
      IF (SPREFI(1:1).NE.'.') THEN
C         ----------------------------- set to default
          CALL KPREFI(SPREFI,LSPREFI)
          SPREFI(1:(LSPREFI+1))= '.'//SPREFI(:LSPREFI)
          LSPREFI = LSPREFI + 1
      ENDIF
 
      IF (LCPREFI.EQ.0)  GOTO  99
C         ----------------------------- nothing to change
*
*     create new prefix
*
      IF ( CPREFI(1:1) .EQ. '.' )         THEN
C         ----------------------------- new MVS high level qualifier
         IF (LCPREFI.LT.5)       GOTO  99
C         ----------------------------- no MVS high level qualifier
         CALL CFILL(' ',SPREFI,1,80)
         SPREFI  = CPREFI(1:LCPREFI)
      ELSE
C         ----------------------------- add sub structure
         SPREFI(1:(LSPREFI+LCPREFI))
     1           = SPREFI(1:LSPREFI)//CPREFI(1:LCPREFI)
      ENDIF
 
      LSPREFI = LENOCC(SPREFI)
      IF (SPREFI(LSPREFI:LSPREFI).NE.'.')  THEN
C         ----------------------------- add last dot
         LSPREFI=LSPREFI +1
         SPREFI(LSPREFI:LSPREFI) = '.'
      ENDIF
 
 99   RETURN
 
 
      ENTRY XZRPRE(RPREFI,LRPREFI)
*     ****************************
*
*     read prefix
*
      IF (SPREFI(1:1).NE.'.') THEN
C         ----------------------------- set to default
          CALL KPREFI(SPREFI,LSPREFI)
          SPREFI(1:(LSPREFI+1))= '.'//SPREFI(:LSPREFI)
          LSPREFI = LSPREFI + 1
      ENDIF
 
      RPREFI = SPREFI
      LRPREFI = LENOCC(SPREFI)
 
      RETURN
      END
#endif
