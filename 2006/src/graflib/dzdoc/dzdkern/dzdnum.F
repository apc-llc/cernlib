*
* $Id: dzdnum.F,v 1.1.1.1 1996/03/04 16:12:54 mclareni Exp $
*
* $Log: dzdnum.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:54  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDNUM(*)
************************************************************************
*.                                                                     *
*...DZDNUM identifies and decodes numeric tag fields.                  *
*.                                                                     *
*. After a first search through the possible tags for the DZDOC bank   *
*. description, DZDNUM tries and interpret the tag as one or two       *
*. numbers. Possible values are a number, a * and a hyphen, separating *
*. the two fields (if present).                                        *
*. When the number interpretation is impossible, the alternate return  *
*. is taken, else the numbers are written to INUM1/2 in /DZDTYP/       *
*.                                                                     *
*. CALLS    : DZDNDC                                                   *
*. CALLED   : DZDCAR                                                   *
*. COMMON   : DZDINC                                                   *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 1.01(5)   /  9 Jul 1986                                  *
*.                                                                     *
*.**********************************************************************
      SAVE
#include "zebra/bkfoparq.inc"
#include "dzdoc/bknuparq.inc"
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
 
 1001 CONTINUE
*..     Find the length of the tag (delimited by the first blank)
 
      IBLANK = INDEX(CARD,' ')
 
C-      Tag too long
      IF (IBLANK.EQ.0)                                     GO TO 998
 
*--     Possible numeric tag --> start parsing
 
      IBLANK = IBLANK - 1
*--     Range interpretation
      IMINUS = INDEX(CARD(:IBLANK),'-')
      IF (IMINUS.EQ.0) THEN
*--         Only one number possible
          CALL DZDNDC(CARD(4:IBLANK),INUM1)
          INUM2  = 0
          IF (INUM1.EQ.INUTGQ) THEN
*--             Non-numeric range specification
              INUM1  = IBLANK/4
              CALL VBLANK(IHNUM1,INUM1)
              CALL UCTOH(CARD(4:),IHNUM1,4,IBLANK-3)
              INUM1  = -INUM1*JFOSEQ
          ENDIF
      ELSE
*--         Two numbers possible
C-          First field not filled in
          IF (IMINUS.EQ.4)                                 GO TO 998
C-          First number
          CALL DZDNDC(CARD(4:IMINUS-1),INUM1)
          IF (INUM1.EQ.INUTGQ) THEN
*--             Non-numeric range specification
              INUM1  = (IMINUS-1)/4
              CALL VBLANK(IHNUM1,INUM1)
              CALL UCTOH(CARD(4:),IHNUM1,4,IMINUS-4)
              INUM1  = -INUM1*JFOSEQ
          ENDIF
C-          Second number
          IF (IMINUS.EQ.IBLANK) THEN
C-              Indefinite second number
              INUM2 = INUINQ
          ELSE
C-              Get second number
              CALL DZDNDC(CARD(IMINUS+1:IBLANK),INUM2)
              IF (INUM2.EQ.0)                              GO TO 998
              IF (INUM2.EQ.INUTGQ) THEN
*--                 Non-numeric range specification
                  INUM2  = (IBLANK-IMINUS+3)/4
                  CALL VBLANK(IHNUM2,INUM2)
                  CALL UCTOH(CARD(IMINUS+1:),IHNUM2,4,IBLANK-IMINUS)
                  INUM2  = -INUM2*JFOSEQ
              ENDIF
          ENDIF
      ENDIF
 
*--     Success -- The number interpretation holds
                                                           GO TO 999
*--     Failure -- The number interpretation is impossible
  998 WRITE(LUNOUT,'(''0?? Entry descriptor has invalid structure'',
     X /,1X,A)') CARD
      RETURN 1
 
  999 END
