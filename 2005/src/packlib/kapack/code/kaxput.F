*
* $Id: kaxput.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxput.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXPUT(IROOT,KEY,IDATA,*,*)
*
*.....REPLACE A RECORD IN A BLOCK
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0a0.inc"
*
      INTEGER IDATA(*), KEY(*)
*
*-----------------------------------------------------------------------
*
*.....LOCATE THE RECORD
      CALL KAXGET(IROOT,KEY,1,LOC,*11)
*
*.....ENSURE THE TREE IS LOCKED
      CALL KAXTLK(IROOT,*12)
*
*.....REPLACE IT
      IF ( IA(8) .EQ. 0 )  THEN
*
*........NON-SEGMENTED RECORD
         LDATA = IA(LOC) - NRCW - IA(LOC+NRCW)
         IPTR  = LOC + NRCW + IA(LOC+NRCW)
         CALL UCOPY(IDATA,IA(IPTR),LDATA)
         CALL KAXWRT(IA(1),IA,IA(7)-1)
*
      ELSE
*
*........SEGMENTED RECORD
         IF ( IA(8) .NE. 1 )  GO TO 91
         LSEG = IA(7) - IA(6) - NRCW - IA(LOC+NRCW)
         IPTR  = LOC + NRCW + IA(LOC+NRCW)
         CALL UCOPY(IDATA,IA(IPTR),LSEG)
         CALL KAXWRT(IA(1),IA,IA(7)-1)
         LDONE = LSEG
*
 1       CALL KAXRD(IA(3),IA,LBLK)
         IF ( IA(8).NE.2 .AND. IA(8).NE.3 )  GO TO 91
         LSEG = IA(7) - IA(6)
         CALL UCOPY (IDATA(LDONE+1),IA(IA(6)),LSEG)
         CALL KAXWRT(IA(1),IA,IA(7)-1)
         LDONE = LDONE + LSEG
         IF ( IA(8) .NE. 2 )  GO TO 1
*
      ENDIF
*
*.....SUCCESSFUL COMPLETION
      CALL KAXRLK(IROOT)
      RETURN
*
*.....ERROR PROCESSING
 11   RETURN 1
*
 12   RETURN 2
*
 91   WRITE(MSG,191) IA(8), IA(1), LUNKAF
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 191  FORMAT('CRNKA251 KAXPUT: INVALID BLOCK TYPE ''',I11,
     +       ''' FOUND IN BLOCK ',I11,' ON UNIT ',I3)
*
      END
