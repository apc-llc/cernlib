*
* $Id: golwri.F,v 1.1.1.1 1996/02/14 13:10:48 mclareni Exp $
*
* $Log: golwri.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:48  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.14/11 29/06/92  13.58.00  by  Unknown
*-- Author : V.Fine
      SUBROUTINE GOLWRI(JOB,PAR,CHARS)
*.
*...  GOLWRI(JOB,PAR,CHAR)  - WRITE TO META-FILE ONE PRIMITIVE COMMAND
*.
*.    JOB  - CODE OF COMMAND
*.    PAR  - ARRAY CONTANED PARAMETERS OF COMMAND
*.    CHAR - STRING PARAMETER
*
*. 06/11/91 03:47pm by Fine V.E. from JINR LCTA
*.
#include "higz/golden.inc"
#include "higz/himeta.inc"
      DIMENSION PAR(4)
      CHARACTER*(*) CHARS
      EQUIVALENCE (PEN,IPEN)

      IF (JOB .GT. NJOB .OR. JOB .LE. 0 .OR. .NOT. FILOPN ) RETURN
*
      PEN = PAR(1)
      IF (JOB .EQ. SP) THEN
         IF (IPEN .EQ. 0) THEN
            LOCK = .TRUE.
         ELSE
            LOCK = .FALSE.
*--
*--   CHANGE BLACK AND WHITE
*
            IF (IPEN .EQ. 7) IPEN = 0
         END IF
      END IF
      IF (.NOT. LOCK .OR. JOB .EQ. SC .OR. JOB .EQ. SS) THEN
         IF (FRADEL .AND. (JOB .EQ. PS .OR. JOB .EQ. PA)) THEN
            FRADEL = .FALSE.
         END IF
*--
*--   DO
*
         IF (BIMODE) THEN
            CALL MFOTIB(JOB)
            GO TO (10,10,10,20,10,40,50), JOB
*
   10       CONTINUE
            CALL MFOTRB(PAR(1))
            CALL MFOTRB(PAR(2))
            GO TO 140
*
   20       CONTINUE
            DO 30 I=1,4
               CALL MFOTRB(PAR(I))
   30       CONTINUE
   40       CONTINUE
            LCH = LENOCC(CHARS)
            CALL MFOTIB(LCH)
            CALL MPUTBF(CHARS,LCH)
            GO TO 140
*
   50       CONTINUE
            CALL MFOTIB(IPEN)
         ELSE
            GO TO (60 ,70 ,80 ,90 ,100,110,120), JOB
   60       WRITE (LUNG(1),10000) 'PA',PAR(1),PAR(2)
            GO TO 140
   70       WRITE (LUNG(1),10000) 'MA',PAR(1),PAR(2)
            GO TO 140
   80       WRITE (LUNG(1),10000) 'TR',PAR(1),PAR(2)
            GO TO 140
   90       CONTINUE
            WRITE(LUNG(1),10100) 'PS',(PAR(i),i=1,4),CHARS
            GO TO 140
  100       CONTINUE
            WRITE(LUNG(1),10200) 'SC',PAR(1),PAR(2)
            GO TO 140
  110       CONTINUE
            WRITE(LUNG(1),10300) 'SS ',CHARS
            GO TO 140
  120       WRITE(LUNG(1),10400) 'SP ',IPEN
  130       CONTINUE
         END IF
      END IF
*
  140 CONTINUE
10000 FORMAT(A,1X,F6.0,1x,f6.0)
10100 FORMAT(A,1X,3f7.0,G12.3,' "',A,'"')
10200 FORMAT(A,1X,F10.6,1x,f10.6)
10300 format(a,'"',A,'"')
10400 FORMAT(A,I3)
      END
#endif
