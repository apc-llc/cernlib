*
* $Id: kaxdl1.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxdl1.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXDL1(LOC)
*
*.....DELETE A BLOCKED RECORD
*
#include "kapack/kax000.inc"
#include "kapack/kax0a0.inc"
*
*-----------------------------------------------------------------------
*
*.....CHECK FOR UNBLOCKED OR SEGMENTED RECORDS
      IF ( LOC.EQ.IA(6) .AND. LOC+IA(LOC).GE.IA(7) )  THEN
         CALL KAXDL2
*
      ELSE
*
*........BLOCKED RECORDS
         IF ( LOC+IA(LOC) .LT. IA(7) )  THEN
*
*...........NOT THE HIGHEST RECORD IN THE BLOCK
            IA(7)  = IA(7) - IA(LOC)
            CALL UCOPY( IA(LOC+IA(LOC)), IA(LOC), IA(7)-LOC )
            CALL KAXWRT(IA(1),IA,IA(7)-1)
            CALL KAXCON
*
         ELSE
*
*...........THE HIGHEST RECORD IN THE BLOCK
            CALL KAXGXD(IA)
            IA(7)  = IA(7) - IA(LOC)
            CALL KAXWRT(IA(1),IA,IA(7)-1)
            CALL KAXGXA(IA)
*
         ENDIF
      ENDIF
*
      END
