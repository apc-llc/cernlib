*
* $Id: kaprik.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kaprik.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAPRIK(LUN,IRC)
*
*.....PRINT KEY VALUES
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0b0.inc"
*
      CHARACTER MAJNAM*(MAXNAM)
*
      LOGICAL LOCKED, XRETRN
*
*-----------------------------------------------------------------------
*
      CALL KAXINI(LUN)
*
*.....SAVE THE CURRENT RETURN SETTING AND ENFORCE 'RETURN=YES'
      XRETRN = RETURN
      RETURN = .TRUE.
*
      LOCKED = .FALSE.
*
*.....MAJOR TREE
      CALL KAXRD(1,IB,9)
      IF ( IB(9) .NE. 0 )  THEN
         LOCKED = .TRUE.
         WRITE(MSG,100) IB(9)
         CALL KAXMSG(LUNLOG,MSG)
      ENDIF
*
*.....LOOP FOR ALL MAJOR NAMES
      MAJNAM = ' '
*
 1    CALL KASEQM(LUN,MAJNAM,LDEF,LMAX,IRC)
      IF ( IRC .EQ. 0 )  THEN
         CALL KAXRD(KPROOT,IB,9)
         IF ( IB(9) .NE. 0 )  THEN
            LOCKED = .TRUE.
            NC     = INDEX(MAJNAM,' ') - 1
            WRITE(MSG,101) MAJNAM(1:NC), IB(9)
            CALL KAXMSG(LUNLOG,MSG)
         ENDIF
         GO TO 1
      ELSE IF ( IRC .EQ. 1 )  THEN
         GO TO 11
      ELSE IF ( IRC .EQ. 2 )  THEN
         IF ( .NOT.LOCKED )  THEN
            WRITE(MSG,102) LUN
            CALL KAXMSG(LUNLOG,MSG)
         ENDIF
      ENDIF
*
*.....SUCCESSFUL COMPLETION
      IRC = 0
      RETURN = XRETRN
      RETURN
*
*.....ERROR PROCESSING
 11   IRC = 1
      RETURN = XRETRN
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 100  FORMAT('CRNKA400 KAPRIK: THE MAJOR NAMES ARE LOCKED WITH A KEY
     +        VALUE OF ',I11)
 101  FORMAT('CRNKA401 KAPRIK: MAJOR NAME ',A,' IS LOCKED WITH A KEY
     +        VALUE OF ',I11)
 102  FORMAT('CRNKA402 KAPRIK: THERE ARE NO LOCKS SET IN THE KA-FILE ON
     +        UNIT ',I3)
*
      END
