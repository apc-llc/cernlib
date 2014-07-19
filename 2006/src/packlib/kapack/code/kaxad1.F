*
* $Id: kaxad1.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxad1.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXAD1(LOC,KEY,IDATA,LDATA)
*
*.....ADD A RECORD TO A TREE: METHOD 1, INSERTION IN CURRENT BLOCK
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax0a0.inc"
*
      INTEGER IDATA(*), KEY(*)
*
*-----------------------------------------------------------------------
*
*.....CALCULATE THE TOTAL LENGTH OF THE NEW RECORD
      LREC = NRCW + KEY(1) + LDATA
*
*.....INSERT THE RECORD IF THERE IS SUFFICIENT SPACE, ELSE CALL KAXAD2
      IF ( LREC .LE. LBLK+1-IA(7) )  THEN
         CALL KAXINS(IA,LOC,KEY,IDATA,LDATA)
         CALL KAXWRT(IA(1),IA,IA(7)-1)
      ELSE
         CALL KAXAD2(LOC,KEY,IDATA,LDATA)
      ENDIF
*
      END
