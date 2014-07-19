*
* $Id: dzd0tt.F,v 1.1.1.1 1996/03/04 16:12:56 mclareni Exp $
*
* $Log: dzd0tt.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:56  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZD0TT
************************************************************************
*.                                                                     *
*...DZD0TT returns tag and text information via /DZDTYP/               *
*.                                                                     *
*. DZD0TT assumes that the characters on the input line have to be     *
*. interpreted as tags and text.                                       *
*. The tag is supposed to be delimited by the first and second blank   *
*. field, while the text starts after the second blank field.          *
*. The absence of either of the tag/text field is indicated by a       *
*. character count equal to zero                                       *
*.                                                                     *
*. CALLS    : INDEXC,INDXBC,UCTOH                                      *
*. CALLED   : DZDCAR                                                   *
*. COMMON   : DZDINC,DZDTYP                                            *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 1.01(9)   /  9 Jul 1986                                  *
*.                                                                     *
*.**********************************************************************
      SAVE
#include "dzdoc/bkwrp.inc"
 
 1001 CONTINUE
 
*..     Find the first non-blank after the first blank series
 
      IFSBLK = INDEX(CARD,' ')
      ITGBEG = INDEXC(CARD(IFSBLK:),' ') + IFSBLK - 1
      IF (ITGBEG.LT.IFSBLK) THEN
*--         No tag and hence no text given
          NWTAG  = 0
          NWTXT  = 0
      ELSE
*--         Tag given
          ITGEND = INDEX(CARD(ITGBEG:),' ')
          IF (ITGEND.EQ.0) THEN
*--             No second blank field --> no explicit text
*--              use information both for text and tag fields
              NHTAG  = LNCARD - ITGBEG + 1
              NWTAG  = (NHTAG+3)/4
              CALL UCTOH(CARD(ITGBEG:),IHTAG,4,NHTAG)
              NHTXT  = NHTAG
              NWTXT  = NWTAG
              CALL UCTOH(CARD(ITGBEG:),IHTXT,4,NHTXT)
          ELSE
              NHTAG  = ITGEND - 1
              NWTAG  = (NHTAG+3)/4
              IF (NHTAG.EQ.1.AND.CARD(ITGBEG:ITGBEG).EQ.'*') THEN
*--                 If tag is * only --> ignore tag (none desired)
                  CONTINUE
              ELSE
                  CALL UCTOH(CARD(ITGBEG:),IHTAG,4,NHTAG)
              ENDIF
              ITGEND = ITGBEG + NHTAG
*--             Second blank field found -- Hunt for start of text
              ITXBEG = INDEXC(CARD(ITGEND+1:),' ') + ITGEND
              IF (ITXBEG.EQ.ITGEND) THEN
*--                 No text given
                  NWTXT  = 0
              ELSE
*--                 Text given
                  ITXEND = INDXBC(CARD,' ')
                  NHTXT  = ITXEND-ITXBEG+1
                  IF (NHTXT.EQ.1.AND.CARD(ITXBEG:ITXBEG).EQ.')') THEN
C-                      Same text as for previous entry
                      IHTXT(1) = IDEMTX
                      NWTXT    = 1
                  ELSE
                      NWTXT  = (NHTXT+3)/4
                      CALL UCTOH(CARD(ITXBEG:),IHTXT,4,NHTXT)
C-                          Flag lines ending with slashes
                      IF (CARD(ITXEND:ITXEND).EQ.'/') THEN
                          ISLASH = 1
                      ELSE
                          ISLASH = 0
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF
      ENDIF
 
  999 END
