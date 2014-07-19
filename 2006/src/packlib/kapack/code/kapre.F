*
* $Id: kapre.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kapre.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAPRE(LUN,MAJNAM,MINNAM,IDATA,LDATA,IRC)
*
*.....RETRIEVE THE RECORD PRECEDING THE ONE SPECIFIED BY THE CALLER
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0a0.inc"
#include "kapack/kax0b0.inc"
*
      CHARACTER MAJNAM*(*), MINNAM*(*)
      CHARACTER MINOLD*(MAXNAM)
*
      INTEGER IDATA(*)
*
      LOGICAL DODATA
*
*-----------------------------------------------------------------------
*
      DODATA = .TRUE.
*
 1    CALL KAXINI(LUN)
*
*.....PROCESS THE MAJOR NAME
      CALL KAXMAJ(MAJNAM,*11,*21)
*
*.....CHECK WHETHER THIS IS AN IMMEDIATE RECALL
      IF ( MINNAM.EQ.MINOLD .AND. IA(10).EQ.1 )  GO TO 3
*
*.....NOT AN IMMEDIATE RECALL SO CONVERT THE SUPPLIED MINOR NAME
 2    IF ( MINNAM .EQ. ' ' )  THEN
         MINKEY(1) = 2
         MINKEY(2) = MAXKEY
      ELSE
         CALL KAXNAM(MINNAM,MINKEY,*12)
      ENDIF
*
*.....DO A KEY SEARCH FROM THE ROOT
      CALL KAXGET(KROOT(0),MINKEY,1,LOC,*3)
*
*.....THE SUPPLIED NAME HAS BEEN LOCATED SO GET THE PREVIOUS ONE
 3    IF ( LOC .EQ. IA(6) )  THEN
         IF ( IA(2).EQ.0 .OR. IA(1).EQ.1 )  GO TO 31
 4       NBLK = IA(1)
         CALL KAXRD(IA(2),IA,LBLK)
         IF ( IA(3) .NE. NBLK )  GO TO 2
         IF ( IA(8) .GT. 1 )  GO TO 4
         LOC = IA(7)
      ENDIF
*
      LOCP = IA(6)
 5    IF ( LOCP+IA(LOCP) .LT. LOC )  THEN
         LOCP = LOCP + IA(LOCP)
         GO TO 5
      ENDIF
      LOC = LOCP
*
*.....CONVERT THE LOCATED KEY TO AN EXTERNAL NAME
      CALL KAXI2E(IA(LOC+NRCW),MINNAM,*13)
*
*.....COPY THE RECORD TO THE USER'S STORAGE
      LDATA = IA(LOC) - NRCW - IA(LOC+NRCW)
      IF ( DODATA )  THEN
         IF ( IA(8) .EQ. 0 )  THEN
*
*...........NON-SEGMENTED RECORD
            IPTR  = LOC + NRCW + IA(LOC+NRCW)
            CALL UCOPY( IA(IPTR), IDATA, LDATA )
*
         ELSE
*
*...........SEGMENTED RECORD
            IF ( IA(8) .NE. 1 )  GO TO 91
            LDATA = IA(7) - IA(6) - NRCW - IA(LOC+NRCW)
            IPTR  = LOC + NRCW + IA(LOC+NRCW)
            CALL UCOPY( IA(IPTR), IDATA, LDATA )
            IB(3) = IA(3)
 6          CALL KAXRD(IB(3),IB,LBLK)
            IF ( IB(8).NE.2 .AND. IB(8).NE.3 )  GO TO 91
            LSEG = IB(7) - IB(6)
            CALL UCOPY( IB(IB(6)), IDATA(LDATA+1), LSEG )
            LDATA = LDATA + LSEG
            IF ( IB(8) .NE. 2 )  GO TO 6
         ENDIF
      ENDIF
*
*.....SUCCESSFUL COMPLETION
      MINOLD = MINNAM
      IA(10) = 1
      IRC    = 0
      RETURN
*
*.....ENTRY POINT FOR NO DATA RETURN
      ENTRY KAPRE1(LUN,MAJNAM,MINNAM,LDATA,IRC)
      DODATA = .FALSE.
      GO TO 1
*
*.....ERROR PROCESSING
 11   IRC = 1
      WRITE(MSG,111) MAJNAM
      GO TO 99
*
 12   IRC = 1
      WRITE(MSG,112) MINNAM
      GO TO 99
*
 13   IRC = 1
      WRITE(MSG,113) MAJNAM, LUN, LEN(MINNAM)
      GO TO 99
*
 21   IRC = 2
      WRITE(MSG,121) MAJNAM, LUN
      GO TO 99
*
 31   IRC = 3
      IF ( RETURN )  RETURN
      WRITE(MSG,131) MAJNAM, LUN
      GO TO 99
*
 91   WRITE(MSG,191) IA(8), IA(1), LUNKAF
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 99   CALL KAXMSG(LUNERR,MSG)
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 111  FORMAT('CRNKA381 KAPRE: ''',A,''' IS NOT AN ACCEPTABLE MAJOR
     +        NAME')
 112  FORMAT('CRNKA382 KAPRE: ''',A,''' IS NOT AN ACCEPTABLE MINOR
     +        NAME')
 113  FORMAT('CRNKA383 KAPRE: RETRIEVAL FAILED FOR MAJOR ',
     +       ' NAME ''',A,''' ON UNIT ',I3,', THE LENGTH OF ',I11,
     +       ' IS INSUFFICIENT FOR THE THIRD ARGUMENT')
 121  FORMAT('CRNKA384 KAPRE: RETRIEVAL FAILED FOR MAJOR ',
     +       ' NAME ''',A,''' ON UNIT ',I3,
     +       ', THIS MAJOR NAME DOES NOT EXIST IN THE KA-FILE')
 131  FORMAT('CRNKA385 KAPRE: BEGINNING OF FILE DETECTED DURING
     +        REVERSE SEQUENTIAL RETRIEVAL FOR MAJOR NAME ''',A,
     +        ''' ON UNIT ',I3)
 191  FORMAT('CRNKA386 KAPRE: INVALID BLOCK TYPE ''',I11,
     +       ''' FOUND IN BLOCK ',I11,' ON UNIT ',I3)
*
      END
