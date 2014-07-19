*
* $Id: kaxdl2.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxdl2.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXDL2
*
*.....DELETE AN UNBLOCKED OR SEGMENTED RECORD
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0a0.inc"
*
*-----------------------------------------------------------------------
*
*.....CHECK THE TYPE OF THE FIRST BLOCK
      IF ( IA(8).NE.0 .AND. IA(8).NE.1 )  GO TO 91
      NLEFT = IA(2)
*
*.....STORE INDEX DELETION DATA
      CALL KAXGXD(IA)
*
*.....LOOP TO FREE ALL BLOCKS IN THE CHAIN
 1    CALL KAXFBK(IA(1))
      IF ( IA(8).EQ.1 .OR. IA(8).EQ.3 )  THEN
         CALL KAXRD(IA(3),IA,NBCW)
         IF ( IA(8).NE.2 .AND. IA(8).NE.3 )  GO TO 91
         GO TO 1
      ENDIF
      NRIGHT = IA(3)
*
*.....LINK THE REMAINING BLOCKS TOGETHER
      IF ( NLEFT .NE. 0 )  THEN
         CALL KAXRD(NLEFT,IA,LBLK)
         IA(3) = NRIGHT
         CALL KAXWRT(IA(1),IA,IA(7)-1)
      ENDIF
      CALL KAXRD(NRIGHT,IA,LBLK)
      IA(2) = NLEFT
      CALL KAXWRT(IA(1),IA,IA(7)-1)
*
*.....SUCCESSFUL COMPLETION
      RETURN
*
*.....ERROR PROCESSING
 91   WRITE(MSG,191) IA(8), IA(1), LUNKAF
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 191  FORMAT('CRNKA171 KAXDL2: INVALID BLOCK TYPE ''',I11,
     +       ''' FOUND IN BLOCK ',I11,' ON UNIT ',I3)
*
      END
