*
* $Id: dzd0nt.F,v 1.1.1.1 1996/03/04 16:12:55 mclareni Exp $
*
* $Log: dzd0nt.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:55  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZD0NT
************************************************************************
*.                                                                     *
*...DZD0NT returns number and text information via /DZDTYP/            *
*.                                                                     *
*. DZD0NT assumes that the characters on the input line have to be     *
*. interpreted as a number (* accepted) and text.                      *
*. The nb. is supposed to be delimited by the first and second blank   *
*. field, while the text starts after the second blank field.          *
*. The absence of either of the tag/text field is indicated by a       *
*. character count equal to zero                                       *
*.                                                                     *
*. CALLS    : INDEXC,INDXBC,DZDNDC,UCTOH                               *
*. CALLED   : DZDCAR                                                   *
*. COMMON   : DZDINC,DZDTYP                                            *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 1.01(8)   /  9 Jul 1986                                  *
*.                                                                     *
*.**********************************************************************
      SAVE
#include "dzdoc/bkwrp.inc"
#include "dzdoc/bknuparq.inc"
 1001 CONTINUE
 
*..     Find the first non-blank after the first blank series
 
      IFSBLK = INDEX(CARD,' ')
      NUMBEG = INDEXC(CARD(IFSBLK:),' ') + IFSBLK - 1
      IF (NUMBEG.LT.IFSBLK) THEN
*--         No tag and hence no text given
          INUM1  = 0
          INUM2  = 0
          NWTXT  = 0
      ELSE
*--         Number given
          NUMEND = INDEX(CARD(NUMBEG:),' ')
          IF (NUMEND.EQ.0) THEN
*--             No second blank field --> only number?
*--             Is it a valid number
              CALL DZDNDC(CARD(NUMBEG:LNCARD),INUM1)
              IF (INUM1.NE.INUTGQ) THEN
C-                  Real number found
                  NWTAG  = 0
              ELSE
C-                  Tag instead of number
                  NHTAG  = LNCARD - NUMBEG + 1
                  NWTAG  = (NHTAG+3)/4
                  CALL UCTOH(CARD(NUMBEG:),IHTAG,4,NHTAG)
                  NWTXT  = 0
              ENDIF
          ELSE
              NUMEND = NUMBEG + NUMEND - 2
*--             Second blank field found
*--             Is it a valid number
              CALL DZDNDC(CARD(NUMBEG:NUMEND),INUM1)
              IF (INUM1.NE.INUTGQ) THEN
C-                  Real number found
                  NWTAG  = 0
              ELSE
C-                  Tag instead of number
                  NHTAG  = NUMEND - NUMBEG + 1
                  NWTAG  = (NHTAG+3)/4
                  CALL UCTOH(CARD(NUMBEG:),IHTAG,4,NHTAG)
              ENDIF
              ITXBEG = INDEXC(CARD(NUMEND+1:),' ') + NUMEND
              IF (ITXBEG.EQ.NUMEND) THEN
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
