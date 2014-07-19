*
* $Id: kalist.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kalist.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KALIST(LUN,MAJNAM,MODE,IRC)
*
*.....LIST THE CONTENTS OF A KA-FILE
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax050.inc"
*
      CHARACTER MAJNAM*(*), MAJOR*(MAXNAM)
*
      LOGICAL XRETRN
*
*-----------------------------------------------------------------------
*
      CALL KAXINI(LUN)
*
*.....CHECK THE VALUE OF MODE
      IF ( MODE .NE. 1 )  GO TO 11
*
*.....SAVE THE CURRENT RETURN SETTING AND ENFORCE 'RETURN=YES'
      XRETRN = RETURN
      RETURN = .TRUE.
*
*.....CHECK WHETHER A SPECIFIC MAJOR NAME HAS BEEN REQUESTED
      IF ( MAJNAM .NE. ' ' )  THEN
*
*........PROCESS A SPECIFIC MAJOR NAME
         CALL KAGETM(LUN,MAJNAM,'READ',LDEF,LMAX,IRC)
         IF ( IRC .EQ. 0 )  THEN
            WRITE(MSG,100) LUN, MAJNAM, LDEF, LMAX
            CALL KAXMSG(LUNLOG,MSG)
            CALL KAXLS1(MAJNAM)
         ELSE IF ( IRC .EQ. 1 )  THEN
            GO TO 12
         ELSE IF ( IRC .EQ. 2 )  THEN
            GO TO 21
         ENDIF
*
      ELSE
*
*........PROCESS ALL MAJOR NAMES
         WRITE(MSG,101) LUN
         CALL KAXMSG(LUNLOG,MSG)
         NMAJOR = 0
         MAJOR  = ' '
*
 1       CALL KASEQM(LUN,MAJOR,LDEF,LMAX,IRC)
         IF ( IRC .EQ. 0 )  THEN
            NMAJOR = NMAJOR + 1
            NC     = INDEX(MAJOR,' ') - 1
            WRITE(LUNLOG,103) NMAJOR, LDEF, LMAX, MAJOR(1:NC)
            CALL KAXLS1(MAJOR)
            GO TO 1
         ELSE IF ( IRC .EQ. 1 )  THEN
            GO TO 13
         ELSE IF ( IRC .EQ. 2 )  THEN
            IF ( NMAJOR .EQ. 0 )  THEN
               WRITE(MSG,102) LUN
               CALL KAXMSG(LUNLOG,MSG)
            ENDIF
         ENDIF
      ENDIF
*
*.....SUCCESSFUL COMPLETION
      IRC = 0
      RETURN = XRETRN
      RETURN
*
*.....ERROR PROCESSING
 11   IRC = 1
      WRITE(MSG,111) MODE
      GO TO 99
*
 12   IRC = 1
      RETURN = XRETRN
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 13   IRC = 1
      RETURN = XRETRN
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 21   IRC = 2
      RETURN = XRETRN
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 99   CALL KAXMSG(LUNERR,MSG)
      RETURN = XRETRN
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 100  FORMAT('CRNKA290 KALIST: SEQUENTIAL LISTING OF THE KA-FILE ON
     +        UNIT ',I3,' FOR MAJOR NAME ''',A,'''',
     +        ', LDEF = ',I11,', LMAX = ',I11,':')
 101  FORMAT('CRNKA290 KALIST: SEQUENTIAL LISTING OF THE KA-FILE ON
     +        UNIT ',I3,':')
 102  FORMAT('CRNKA291 KALIST: THE KA-FILE ON UNIT ',I3,' IS EMPTY')
 103  FORMAT('      ',I5,'. LDEF ',I5,', LMAX ',I5,
     +       ', MAJOR NAME: ',A)
 111  FORMAT('CRNKA292 KALIST: ',I11,' IS NOT AN ACCEPTABLE MODE, IT
     +        MUST HAVE THE VALUE 1')
*
      END
