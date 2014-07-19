*
* $Id: kaxgxd.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxgxd.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXGXD(IZ)
*
*.....GENERATE AN ENTRY IN THE INDEX DELETION QUEUE
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax050.inc"
#include "kapack/kax090.inc"
*
      INTEGER IZ(*)
*
*-----------------------------------------------------------------------
*
*.....SEARCH FOR THE LAST RECORD, (I.E. THE HIGHEST KEY), IN THE BLOCK
      I = IZ(6)
 1    IF ( I .LT. IZ(7) )  THEN
         LOC = I
         I = LOC + IZ(LOC)
         GO TO 1
      ENDIF
*
*.....QUEUE THE ENTRY
      IXDL = MOD(IXDL+1,LXDQ+1)
      IF ( IXDL .EQ. IXDF )  GO TO 91
      CALL UCOPY( IZ(LOC+NRCW), IXDKEY(1,IXDL), IZ(LOC+NRCW) )
      IXDLEV(IXDL) = IZ(5) + 1
*
*.....SUCCESSFUL COMPLETION
      RETURN
*
*.....ERROR PROCESSING
 91   WRITE(MSG,191)
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 191  FORMAT('CRNKA211 KAXGXD: KAPACK SYSTEM ERROR, INDEX DELETION
     +        QUEUE FULL')
*
      END
