*
* $Id: kaget.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kaget.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAGET(LUN,MAJNAM,MINNAM,MODE,IDATA,LDATA,IRC)
*
*.....RETRIEVE A RECORD FROM A KA-FILE
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0a0.inc"
*
      CHARACTER MAJNAM*(*), MINNAM*(*), MODE*(*)
*
      INTEGER IDATA(*)
*
*-----------------------------------------------------------------------
*
      CALL KAXINI(LUN)
*
*.....PROCESS THE MAJOR NAME
      CALL KAXMAJ(MAJNAM,*12,*21)
*
*.....CONVERT THE MINOR NAME TO A KEY
      CALL KAXNAM(MINNAM,MINKEY,*13)
*
*.....LOCK THE MINOR TREE IF THE MODE IS UPDATE
      IF ( MODE .EQ. 'UPDATE')  THEN
         CALL KAXOLK(KROOT(0))
      ELSE IF ( MODE .NE. 'READ' )  THEN
         GO TO 11
      ENDIF
*
*.....LOCATE THE RECORD
      CALL KAXGET(KROOT(0),MINKEY,1,LOC,*31)
*
*.....COPY THE RECORD TO THE USER'S STORAGE
      IF ( IA(8) .EQ. 0 )  THEN
*
*........NON-SEGMENTED RECORD
         LDATA = IA(LOC) - NRCW - IA(LOC+NRCW)
         IPTR  = LOC + NRCW + IA(LOC+NRCW)
         CALL UCOPY( IA(IPTR), IDATA, LDATA )
*
      ELSE
*
*........SEGMENTED RECORD
         IF ( IA(8) .NE. 1 )  GO TO 91
         LDATA = IA(7) - IA(6) - NRCW - IA(LOC+NRCW)
         IPTR  = LOC + NRCW + IA(LOC+NRCW)
         CALL UCOPY( IA(IPTR), IDATA, LDATA )
 1       CALL KAXRD(IA(3),IA,LBLK)
         IF ( IA(8).NE.2 .AND. IA(8).NE.3 )  GO TO 91
         LSEG = IA(7) - IA(6)
         CALL UCOPY( IA(IA(6)), IDATA(LDATA+1), LSEG )
         LDATA = LDATA + LSEG
         IF ( IA(8) .NE. 2 )  GO TO 1
      ENDIF
*
*.....SUCCESSFUL COMPLETION
      IRC = 0
      RETURN
*
*.....ERROR PROCESSING
 11   IRC = 1
      WRITE(MSG,111) MODE
      GO TO 99
*
 12   IRC = 1
      WRITE(MSG,112) MAJNAM
      GO TO 99
*
 13   IRC = 1
      WRITE(MSG,113) MINNAM
      GO TO 99
*
 21   IRC = 2
      WRITE(MSG,121) MAJNAM, MINNAM, LUN
      GO TO 99
*
 31   IRC = 3
      WRITE(MSG,131) MAJNAM, MINNAM, LUN
      IF ( MODE .EQ. 'UPDATE' )  CALL KAXRLK(KROOT(0))
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
 111  FORMAT('CRNKA051 KAGET: ''',A,''' IS NOT AN ACCEPTABLE VALUE FOR
     +        THE FOURTH ARGUMENT, (MODE), IT MUST BE EITHER ''READ'' OR
     +        ''UPDATE''')
 112  FORMAT('CRNKA052 KAGET: ''',A,''' IS NOT AN ACCEPTABLE MAJOR
     +        NAME')
 113  FORMAT('CRNKA053 KAGET: ''',A,''' IS NOT AN ACCEPTABLE MINOR
     +        NAME')
 121  FORMAT('CRNKA054 KAGET: RETRIEVAL OF ''',A,' ',A,''' FAILED
     +        ON UNIT ',I3,', THE MAJOR NAME DOES NOT EXIST IN THE
     +        KA-FILE')
 131  FORMAT('CRNKA055 KAGET: RETRIEVAL OF ''',A,' ',A,''' FAILED
     +        ON UNIT ',I3,', THIS RECORD DOES NOT EXIST IN THE
     +        KA-FILE')
 191  FORMAT('CRNKA056 KAGET: INVALID BLOCK TYPE ''',I11,
     +       ''' FOUND IN BLOCK ',I11,' ON UNIT ',I3)
*
      END
