*
* $Id: dzd00t.F,v 1.1.1.1 1996/03/04 16:12:55 mclareni Exp $
*
* $Log: dzd00t.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:55  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZD00T
************************************************************************
*.                                                                     *
*...DZD00T decodes the text part of a line of input                    *
*.                                                                     *
*. DZD00T assumes that the characters on the input line have to be     *
*. interpreted as text. Therefore the first and last non-blank chars.  *
*. are located. Then the text is copied to vector IHTXT.               *
*. The text is supposed to start after the first blank series.         *
*. If the line, apart from the beginning is all blank, NWTXT = 0       *
*. and NWTXT = 0.                                                      *
*.                                                                     *
*. CALLS    : INDEXC,INDXBC,UCTOH                                      *
*. CALLED   : DZDCAR                                                   *
*. COMMON   : DZDINC,DZDTYP                                            *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 1.01(8)   /  9 Jul 1986                                  *
*.                                                                     *
*.**********************************************************************
       SAVE
#include "dzdoc/bkwrp.inc"
 
 1001 CONTINUE
 
*..     Find the first non-blank after the first blank series
 
      IFSBLK = INDEX(CARD,' ')
      ITXBEG = INDEXC(CARD(IFSBLK:),' ') + IFSBLK - 1
      IF (ITXBEG.LT.IFSBLK) THEN
*--         No text given
          NWTXT  = 0
      ELSE
*--         Text given
          ITXEND = INDXBC(CARD,' ')
          NHTXT  = ITXEND-ITXBEG+1
          IF (NHTXT.EQ.1.AND.CARD(ITXBEG:ITXBEG).EQ.')') THEN
C-              Same text as for previous entry
              IHTXT(1) = IDEMTX
              NWTXT    = 1
          ELSE
              NWTXT  = (NHTXT+3)/4
              CALL UCTOH(CARD(ITXBEG:),IHTXT,4,NHTXT)
C-                  Flag lines ending with slashes
              IF (CARD(ITXEND:ITXEND).EQ.'/') THEN
                  ISLASH = 1
              ELSE
                  ISLASH = 0
              ENDIF
          ENDIF
      ENDIF
 
  999 END
