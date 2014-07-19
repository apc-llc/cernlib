*
* $Id: kaxini.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxini.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXINI(LUN)
*
*.....PERFORM CALL-DEPENDENT INITIALIZATION
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax030.inc"
#include "kapack/kax050.inc"
#include "kapack/kax060.inc"
#include "kapack/kax0a0.inc"
*
      LOGICAL DOFILE, NEWRUN
*
      DATA NEWRUN/.TRUE./
*
*-----------------------------------------------------------------------
*
      DOFILE = .TRUE.
*
*.....PERFORM RUN-DEPENDENT INITIALIZATION ON THE FIRST CALL
 1    IF ( NEWRUN )  THEN
         CALL KAXINR
         LUNKAF = 0
         NEWRUN = .FALSE.
      ENDIF
*
*.....CHECK LUN AND SEE WHETHER IT IS THE SAME AS IN THE PREVIOUS CALL
      IF ( LUN.LT.1 .OR. LUN.GT.MAXLUN )  GO TO 91
      NEWLUN = LUN .NE. LUNKAF
*
*.....SET THE VALUE OF THE KAFILE UNIT NUMBER FOR THIS CALL TO KAPACK
      LUNKAF = LUN
*
*.....INITIALIZE CALL-DEPENDENT PARAMETERS IF LUN HAS CHANGED
      IF ( NEWLUN )  THEN
         LUNERR = MOD(IOPTNS(LUNKAF)/10000,100)
         LUNLOG = MOD(IOPTNS(LUNKAF)/100,100)
         MODIFY = MOD(IOPTNS(LUNKAF)/10,10) .EQ. 1
         RETURN = MOD(IOPTNS(LUNKAF),10) .EQ. 1
         LBLK   = LBKS(LUNKAF)
         NBLKS  = NBKS(LUNKAF)
         NFREE  = NFBK(LUNKAF)
         MAXKEY = MAXK(LUNKAF)
         MAXPAK = MAXP(LUNKAF)
      ENDIF
*
*.....PERFORM FILE INITIALIZATION IF NECESSARY
      IF ( DOFILE .AND. LBKS(LUNKAF).LE.0 )  THEN
         CALL KAXOPN
         LBLK   = LBKS(LUNKAF)
         NBLKS  = NBKS(LUNKAF)
         NFREE  = NFBK(LUNKAF)
         MAXKEY = MAXK(LUNKAF)
         MAXPAK = MAXP(LUNKAF)
      ENDIF
*
*.....SUCCESSFUL COMPLETION
      RETURN
*
*.....ENTRY POINT FOR INITIALIZATION WITHOUT FILE
      ENTRY KAXINN(LUN)
      DOFILE = .FALSE.
      GO TO 1
*
*.....ERROR PROCESSING
 91   WRITE(MSG,191) LUN, MAXLUN
      LUNERR = ISYSWR
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 191  FORMAT('CRNKA221 KAXINI: ',I11,' IS NOT AN ACCEPTABLE UNIT NUMBER,
     +        IT MUST BE IN THE RANGE 1 - ',I3)
*
      END
