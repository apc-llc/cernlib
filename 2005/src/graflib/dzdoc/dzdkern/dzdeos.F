*
* $Id: dzdeos.F,v 1.1.1.1 1996/03/04 16:12:54 mclareni Exp $
*
* $Log: dzdeos.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:54  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDEOS(*)
************************************************************************
*.                                                                     *
*...DZDEOS interprets the tag fields for the end of fields             *
*.                                                                     *
*. Characters 4 and 5 of the input line are tested against all possible*
*. tags. If a valid tag is found, consistency checks are made.         *
*. If no valid tag line interpretation is possible, the line is        *
*. considered to end the highest precedence tag field.                 *
*.                                                                     *
*. CALLS    : none                                                     *
*. CALLED   : DZDBKC                                                   *
*. COMMON   : DZDINC,DZDTYP                                            *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 1.02(9)   /  9 Jul 1986                                  *
*.                                                                     *
*.**********************************************************************
      SAVE
#include "dzdoc/bkstparq.inc"
#include "dzdoc/bktgparq.inc"
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
      CHARACTER CTAG*2
#include "dzdoc/bktgdatq.inc"
 
 1001 CONTINUE
      CTAG   = CARD(4:5)
 
      IF     (CARD(4:4).EQ.' ') THEN
*--         End present highest precedence field
C--    ===============================================================
C-     I   States              Precedence         Action             I
C-     I-------------------------------------------------------------I
C-     I   CHOICE                  4           close CHOICE bracket  I
C-     I   REPETITION              3           decrement REP level   I
C-     I   BIT/DATA/LINK           2           close BI/DA/LI field  I
C-     I   GENERAL                 1           end of bank descr.    I
C--    ===============================================================
          IF (IREPLV.GT.0) THEN
C-              Precedence 3 --> decrement repetition level
              IREPLV = IREPLV - 1
          ELSEIF (ISTATE.EQ.ISBITQ.OR.ISTATE.EQ.ISDATQ.OR.
     X            ISTATE.EQ.ISLINQ                        ) THEN
C-              Precedence 2 --> decrease to GEN level
              ISTATE = ISGENQ
          ELSEIF (ISTATE.EQ.ISGENQ) THEN
C-              Precedence 1 --> end of bank description
              ISTATE = 0
          ELSE
C-             Invalid state
              WRITE(LUNOUT,'(''0??  *B/ card indicating general EOS'',
     X         '' but invalid state : ISTATE = '',I10)') ISTATE
                                                           GO TO 998
          ENDIF
      ELSEIF (CTAG.EQ.CBKTAC(ITGBIQ)) THEN
*--         End of status bit field
          IF (ISTATE.NE.ISBITQ) THEN
*--            We were NOT inside a BIT description field -> ignore EOS
              WRITE(LUNOUT,'(''0??  *B/BI card indicating end of bit '',
     X         ''description field found and not inside bit field'')')
                                                           GO TO 998
          ELSE
              ISTATE = ISGENQ
          ENDIF
      ELSEIF (CTAG.EQ.CBKTAC(ITGDAQ)) THEN
*--         End of data field
          IF (ISTATE.NE.ISDATQ) THEN
*--            We were NOT inside a DATA description field -> ignore EOS
              WRITE(LUNOUT,'(''0??  *B/DA card indicating end of data'',
     X         '' description field found and not inside data field'')')
                                                           GO TO 998
          ELSE
              ISTATE = ISGENQ
          ENDIF
      ELSEIF (CTAG.EQ.CBKTAC(ITGLIQ)) THEN
*--         End of link field
          IF (ISTATE.NE.ISLINQ) THEN
*--           We were NOT inside a LINK description field -> ignore EOS
              WRITE(LUNOUT,'(''0??  *B/LI card indicating end of link'',
     X         '' description field found and not inside link field'')')
                                                           GO TO 998
          ELSE
              ISTATE = ISGENQ
          ENDIF
      ELSEIF (CTAG.EQ.CBKTAC(ITGRLQ)) THEN
*--         End of ref link field
          IF (ISTATE.NE.ISRLIQ) THEN
*--           We were NOT inside a RLINK description field -> ignore EOS
              WRITE(LUNOUT,'(''0??  *B/RL indicating end of Rlink'',
     X         '' description found and not inside Rlink field'')')
                                                           GO TO 998
          ELSE
              ISTATE = ISGENQ
          ENDIF
      ELSEIF (CTAG.EQ.CBKTAC(ITGREQ)) THEN
*--         End of repetition field
          IF (IREPLV.EQ.0) THEN
*--             We were NOT inside a REP field -> ignore EOS
              WRITE(LUNOUT,'(''0??  *B/RE card indicating end of '',
     X         ''REP field found and not inside REP field'')')
                                                           GO TO 998
          ELSE
              IREPLV = IREPLV - 1
          ENDIF
      ELSE
*--         No recognized tag -- IGNORE EOS
          WRITE(LUNOUT,'(''0??  '',A,'' card indicating end of '',
     X     ''sequence for unrecognized tag field'')') CARD(1:5)
                                                           GO TO 998
      ENDIF
 
                                                           GO TO 999
 
*--     Continuation card
  998 RETURN 1
 
  999 END
