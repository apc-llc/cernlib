*
* $Id: kaxadd.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxadd.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXADD(IROOT,KEY,IDATA,LDATA,*,*)
*
*.....ADD A RECORD TO A TREE: STEERING ROUTINE
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax050.inc"
#include "kapack/kax080.inc"
#include "kapack/kax0a0.inc"
*
      INTEGER IDATA(*), KEY(*)
*
*-----------------------------------------------------------------------
*
*.....ENSURE THERE IS ENOUGH SPACE ASSUMING THE WORST CASE
      NEED = 1 + LEVELS + (NRCW+KEY(1)+LDATA-1)/(LBLK-NBCW) + 1
      IF ( NEED .GT. NFREE )  RETURN 2
*
*.....LOCK THE TREE
      CALL KAXOLK(IROOT)
*
*.....DETERMINE THE POTENTIAL LOCATION OF THE RECORD
      CALL KAXGET(IROOT,KEY,1,LOC,*1)
      GO TO 11
*
*.....INITIALIZE THE INDEX ADDITION QUEUE POINTERS
 1    IXAF = 0
      IXAL = 0
*
*.....ADD THE NEW RECORD
      CALL KAXAD1(LOC,KEY,IDATA,LDATA)
*
*.....PERFORM ANY INDEX ADDITIONS WHICH HAVE BEEN GENERATED
 2    IF ( IXAF .NE. IXAL )  THEN
         NEXT = MOD(IXAF+1,LXAQ+1)
         CALL KAXGET(IROOT,IXAKEY(1,NEXT),IXALEV(NEXT),IXLOC,*3)
         GO TO 91
 3       CALL KAXAD1(IXLOC,IXAKEY(1,NEXT),IXAPTR(NEXT),1)
         IXAF = NEXT
         GO TO 2
      ENDIF
*
*.....SUCCESSFUL COMPLETION
      CALL KAXRLK(IROOT)
      RETURN
*
*.....ENTRY POINT TO ADD INDEX ENTRIES ONLY
      ENTRY KAXADX(IROOT)
      CALL KAXOLK(IROOT)
      GO TO 2
*
*.....ERROR PROCESSING
 11   CALL KAXRLK(IROOT)
      RETURN 1
*
 91   WRITE(MSG,191) LUNKAF
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 191  FORMAT('CRNKA141 KAXADD: KAPACK SYSTEM ERROR OR CORRUPT FILE ON
     +        UNIT ',I3)
*
      END
