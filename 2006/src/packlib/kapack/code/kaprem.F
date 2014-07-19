*
* $Id: kaprem.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kaprem.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAPREM(LUN,MAJNAM,LDEF,LMAX,IRC)
*
*.....RETRIEVE THE MAJOR NAME PRECEDING THE ONE SUPPLIED BY THE CALLER
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax02c.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0a0.inc"
*
      CHARACTER MAJNAM*(*)
      CHARACTER MAJOLD*(MAXNAM)
      SAVE MAJOLD
*
*-----------------------------------------------------------------------
*
      CALL KAXINI(LUN)
*
*.....CHECK WHETHER THIS IS AN IMMEDIATE RECALL
      IF (.NOT.NEWLUN .AND. MAJNAM.EQ.MAJOLD .AND. IA(10).EQ.2) GO TO 2
*
*.....NOT AN IMMEDIATE RECALL SO CONVERT THE SUPPLIED MAJOR NAME
 1    IF ( MAJNAM .EQ. ' ' )  THEN
         MAJKEY(1) = 2
         MAJKEY(2) = MAXKEY
      ELSE
         CALL KAXNAM(MAJNAM,MAJKEY,*11)
      ENDIF
*
*.....DO A KEY SEARCH FROM THE ROOT
      CALL KAXGET(1,MAJKEY,1,LOC,*2)
*
*.....THE SUPPLIED NAME HAS BEEN LOCATED SO GET THE PREVIOUS ONE
 2    IF ( LOC .EQ. IA(6) )  THEN
         IF ( IA(2).EQ.0 .OR. IA(1).EQ.1 )  GO TO 21
         NBLK = IA(1)
         CALL KAXRD(IA(2),IA,LBLK)
         IF ( IA(3) .NE. NBLK )  GO TO 1
         LOC = IA(7)
      ENDIF
*
      LOCP = IA(6)
 3    IF ( LOCP+IA(LOCP) .LT. LOC )  THEN
         LOCP = LOCP + IA(LOCP)
         GO TO 3
      ENDIF
      LOC = LOCP
*
*.....CONVERT THE LOCATED KEY TO AN EXTERNAL NAME
      CALL KAXI2E(IA(LOC+NRCW),MAJNAM,*12)
*
*.....STORE THE DEFAULT AND MAXIMUM LENGTHS FOR THE CALLER
      IPTR = LOC + NRCW + IA(LOC+NRCW)
      LDEF = IA(IPTR+1)
      LMAX = IA(IPTR+2)
*
*.....SUCCESSFUL COMPLETION
      MAJOLD = MAJNAM
      IA(10) = 2
      IRC    = 0
      RETURN
*
*.....ERROR PROCESSING
 11   IRC = 1
      WRITE(MSG,111) MAJNAM
      GO TO 99
*
 12   IRC = 1
      WRITE(MSG,112) LUN, LEN(MAJNAM)
      GO TO 99
*
 21   IRC = 2
      IF ( RETURN )  RETURN
      WRITE(MSG,121) LUN
      GO TO 99
*
 99   CALL KAXMSG(LUNERR,MSG)
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 111  FORMAT('CRNKA391 KAPREM: ''',A,''' IS NOT AN ACCEPTABLE MAJOR
     +        NAME')
 112  FORMAT('CRNKA392 KAPREM: RETRIEVAL OF MAJOR NAMES FAILED',
     +       ' ON UNIT ',I3,', THE LENGTH OF ',I11,
     +       ' IS INSUFFICIENT FOR THE SECOND ARGUMENT')
 121  FORMAT('CRNKA393 KAPREM: BEGINNING OF FILE DETECTED DURING
     +        REVERSE SEQUENTIAL RETRIEVAL OF MAJOR NAMES ON UNIT ',I3)
*
      END
