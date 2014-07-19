*
* $Id: karlse.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: karlse.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KARLSE(LUN,KEY,IRC)
*
*.....RELEASE ALL TREES LOCKED WITH THE SPECIFIED KEY
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0b0.inc"
*
      CHARACTER MAJOR*(MAXNAM)
*
      LOGICAL XRETRN
*
*-----------------------------------------------------------------------
*
      CALL KAXINI(LUN)
*
*.....SAVE THE CURRENT RETURN SETTING AND ENFORCE 'RETURN=YES'
      XRETRN = RETURN
      RETURN = .TRUE.
*
*.....LOOP FOR ALL MAJOR NAMES
      NRESET = 0
      MAJOR  = ' '
*
 1    CALL KASEQM(LUN,MAJOR,LDEF,LMAX,IRC)
      IF ( IRC .EQ. 0 )  THEN
         CALL KAXRD(KPROOT,IB,LBLK)
         IF ( IB(9) .EQ. KEY )  THEN
            IB(9) = 0
            CALL KAXWRT(IB(1),IB,IB(7)-1)
            NRESET = NRESET + 1
         ENDIF
         GO TO 1
      ELSE IF ( IRC .EQ. 1 )  THEN
         GO TO 11
      ENDIF
*
*.....BLOCK 1
      CALL KAXRD(1,IB,LBLK)
      IF ( IB(9) .EQ. KEY )  THEN
         IB(9) = 0
         CALL KAXWRT(IB(1),IB,IB(7)-1)
         NRESET = NRESET + 1
      ENDIF
*
*.....SUCCESSFUL COMPLETION
      IRC = 0
      WRITE(MSG,100) NRESET, LUN
      CALL KAXMSG(LUNLOG,MSG)
      RETURN = XRETRN
      RETURN
*
*.....ERROR PROCESSING
 11   IRC = 1
      RETURN = XRETRN
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 100  FORMAT('CRNKA370 KARLSE: ',I11,' LOCKS RELEASED IN THE KA-FILE
     +        ON UNIT ',I3)
*
      END
