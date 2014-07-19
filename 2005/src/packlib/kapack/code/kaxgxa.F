*
* $Id: kaxgxa.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxgxa.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXGXA(IZ)
*
*.....GENERATE AN ENTRY IN THE INDEX ADDITION QUEUE
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax050.inc"
#include "kapack/kax080.inc"
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
      IXAL = MOD(IXAL+1,LXAQ+1)
      IF ( IXAL .EQ. IXAF )  GO TO 91
      CALL UCOPY( IZ(LOC+NRCW), IXAKEY(1,IXAL), IZ(LOC+NRCW) )
      IXALEV(IXAL) = IZ(5) + 1
      IXAPTR(IXAL) = IZ(1)
*
*.....SUCCESSFUL COMPLETION
      RETURN
*
*.....ERROR PROCESSING
 91   WRITE(MSG,191)
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 191  FORMAT('CRNKA201 KAXGXA: KAPACK SYSTEM ERROR, INDEX ADDITION
     +        QUEUE FULL')
*
      END
