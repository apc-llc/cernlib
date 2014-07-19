*
* $Id: kaxnam.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxnam.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXNAM(NAME,KEY,*)
*
*.....VALIDATE A NAME AND CONVERT IT TO INTERNAL REPRESENTATION
*
#include "kapack/kax000.inc"
*
      CHARACTER NAME*(*)
*
      INTEGER KEY(*)
*
*-----------------------------------------------------------------------
*
*.....CHECK THE NAME
      CALL KAXWRD(NAME,LOC1,NCHARS,*11)
      IF ( NCHARS .GT. MAXNAM )  GO TO 11
      LOC2 = LOC1 + NCHARS - 1
      CALL KAXWRD(NAME(LOC2+1:),JUNK1,JUNK2,*1)
      GO TO 11
*
*.....CONVERT IT
 1    CALL KAXE2I(NAME(LOC1:LOC2),KEY,*11)
*
*.....SUCCESSFUL COMPLETION
      RETURN
*
*.....ERROR PROCESSING
 11   RETURN 1
*
      END
