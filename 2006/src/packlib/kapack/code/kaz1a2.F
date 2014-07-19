*
* $Id: kaz1a2.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaz1a2.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAZ1A2
*
*.....AUXILIARY ROUTINE TO KA1TO2: PROCESS THE RECORDS IN A GIVEN BLOCK
*
#include "kapack/kax000.inc"
#include "kapack/kax0a0.inc"
*
*-----------------------------------------------------------------------
*
      LOC = IA(6)
 1    IF ( LOC .LT. IA(7) )  THEN
         CALL KAZ1B2(IA(LOC+NRCW))
         LOC = LOC + IA(LOC)
         GO TO 1
      ENDIF
*
      END
