*
* $Id: dzdndc.F,v 1.2 1996/04/02 22:24:30 thakulin Exp $
*
* $Log: dzdndc.F,v $
* Revision 1.2  1996/04/02 22:24:30  thakulin
* Remove extra SAVE statement (all variables already under generic
* SAVE).
*
* Revision 1.1.1.1  1996/03/04 16:12:54  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDNDC(CHAR,NUMBER)
************************************************************************
*.                                                                     *
*...DZDNDC decodes a character variable as a number                    *
*.                                                                     *
*. The value NUMBER returned is an integer corresponding to the        *
*. character representation of CHAR.                                   *
*. The detection of a non-numeric character gives an alternate return  *
*.                                                                     *
*. INPUT:                                                              *
*. ------                                                              *
*.   CHAR   : CHARACTER variable containing the text to be decoded     *
*. OUTPUT:                                                             *
*. -------                                                             *
*.   NUMBER : Integer corresponding to number interpretation of CHAR   *
*.                                                                     *
*. CALLS    : none                                                     *
*. CALLED   : DZD0NT,DZDNUM                                            *
*. COMMON   : none                                                     *
*.                                                                     *
*. AUTHOR   : M. Goossens DD/US                                        *
*. VERSION  : 1.01(4)   /  9 Jul 1986                                  *
*.                                                                     *
*.**********************************************************************
      SAVE
#include "dzdoc/bknuparq.inc"
      CHARACTER *(*) CHAR
      DATA INIT/0/
 
      IF (INIT.EQ.0) THEN
          IZERO  = ICHAR('0')
          ININE  = ICHAR('9')
          INIT   = 1
      ENDIF
 
 1001 CONTINUE
      LCHAR  = LEN(CHAR)
*--     Is the only character a star (indicating any number)?
      IF (LCHAR.EQ.1) THEN
          IF (CHAR(1:1).EQ.'*') THEN
              NUMBER = INUINQ
                                                           GO TO 999
          ENDIF
      ENDIF
 
*..     Start of decoding
      NUMBER = 0
*--         Beginning of loop over characters
      DO 100 I = 1,LCHAR
          IICHAR = ICHAR(CHAR(I:I))
          IF (IICHAR.LT.IZERO.OR.IICHAR.GT.ININE)          GO TO 998
          NUMBER = 10*NUMBER + IICHAR - IZERO
  100 CONTINUE
*--         End       of loop over characters
                                                           GO TO 999
*--     Non-numeric field
  998 NUMBER = INUTGQ
 
  999 END
