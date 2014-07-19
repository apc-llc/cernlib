*
* $Id: kaxget.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxget.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXGET(IROOT,KEY,LEVEL,LOC,*)
*
*.....GET THE ACTUAL OR POTENTIAL LOCATION OF A KEY IN A TREE
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0a0.inc"
*
      INTEGER KEY(*)
*
*-----------------------------------------------------------------------
*
*.....READ THE INITIAL BLOCK
      CALL KAXRD(IROOT,IA,LBLK)
*
*.....SEARCH THE BLOCK
 1    LOC = IA(6)
*
 2    IF ( LOC .GE. IA(7) )  GO TO 91
      ICMP = KAXCMP(KEY,IA(LOC+NRCW))
      IF ( ICMP .GT. 0 )  THEN
         LOC = LOC + IA(LOC)
         GO TO 2
      ENDIF
*
*.....MATCH FOUND, CHECK WHETHER IT IS AT THE REQUIRED LEVEL
      IF ( IA(5) .NE. LEVEL )  THEN
         IPTR   = LOC + NRCW + IA(LOC+NRCW)
         CALL KAXRD(IA(IPTR),IA,LBLK)
         GO TO 1
      ENDIF
*
*.....MATCH FOUND AT THE REQUIRED LEVEL
      IF ( ICMP .EQ. 0 )  RETURN
      RETURN 1
*
*.....ERROR PROCESSING
 91   WRITE(MSG,191) IA(1), LUNKAF
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 191  FORMAT('CRNKA231 KAXGET: KAPACK SYSTEM ERROR OR CORRUPT FILE, KEY
     +        SEARCH ERROR IN BLOCK ',I11,' ON UNIT ',I3)
*
      END
