*
* $Id: kxunit.F,v 1.2 1998/02/11 15:17:20 couet Exp $
*
* $Log: kxunit.F,v $
* Revision 1.2  1998/02/11 15:17:20  couet
* - do not print the bottom line of the table if no files have been found.
*
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.01/00 25/02/93  14.26.32  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE KXUNIT
*
********************************************************************************
*
* Execution routine for command '/KUIP/UNITS'
*
* (by the courtesy of ZCEDEX)
*
********************************************************************************
*
#include "kuip/kcunit.inc"
#include "kuip/kcques.inc"
      CHARACTER*80 FILE
      CHARACTER*11 FRM
      CHARACTER*10 ACC
      CHARACTER*5  CHRECL
*
#if defined(CERNLIB_NEWLIB)
      CHARACTER*70 RTAREA
      CHARACTER*8  FILENAME
#endif
*
      CALL KUALFA
      WRITE(LUOUT,2000)
2000  FORMAT
     +  (' +',4('-'),'+',39('-'),'+',12('-'),'+',11('-'),'+',6('-'),'+')
      WRITE(LUOUT,2100)
2100  FORMAT
     +  (' | Lu | File name',T47,'| Format     | Access    | Recl |')
      WRITE(LUOUT,2000)
      IFNP = 0
      DO 200 I=1,99
#if defined(CERNLIB_NEWLIB)
        CALL ITOFT(I,FILENAME,IERR)
        RTAREA=' '
        CALL DYNAM(0,'INFO',RTAREA,'DDN='//FILENAME//';')
#endif
        FILE=' '
        CALL KUINQF(FILE,I)
        IF (FILE.NE.' ') THEN
          IFNP = 1
          IF (IQUEST(11).EQ.1) THEN
            FRM='FORMATTED'
          ELSE IF (IQUEST(11).EQ.2) THEN
            FRM='UNFORMATTED'
          ELSE
            FRM=' '
          ENDIF
          IF (IQUEST(12).EQ.1) THEN
            ACC='SEQUENTIAL'
          ELSE IF (IQUEST(12).EQ.2) THEN
            ACC='DIRECT'
          ELSE
            ACC=' '
          ENDIF
          LRECL=IQUEST(13)
          IF (LRECL.EQ.0) THEN
            CHRECL=' '
          ELSE
            WRITE (CHRECL,'(I5)') LRECL
          ENDIF
#if defined(CERNLIB_NEWLIB)
          IF (RTAREA(9:16).NE.'NULLFILE' .AND. RTAREA(9:12).NE.'SYS9')
     +    FILE=RTAREA(9:52)
#endif
          L=LENOCC(FILE)
          IF(L.LE.38) THEN
            WRITE(LUOUT,2200)I,FILE(1:L),FRM,ACC,CHRECL
2200        FORMAT
     +      (1X,'|',I3,' | ',A,T47,'| ',A,T60,'| ',A,T72,'|',A,' |')
          ELSE
            WRITE(LUOUT,2300)I,FILE(1:L),FRM,ACC,CHRECL
2300        FORMAT
     +      (1X,'|',I3,' | ',A/T47,'| ',A,T60,'| ',A,T72,'|',A,' |')
          ENDIF
#if defined(CERNLIB_NEWLIB)
        ELSE
          IF (RTAREA.NE.' ') THEN
             FILE=RTAREA(9:52)
             IF (FILE(1:8).EQ.'NULLFILE') FILE='TERMINAL'
             IF (FILE(1:4).EQ.'SYS9') FILE='TEMPORARY FILE'
             FRM='FORMATTED'
             ACC='SEQUENTIAL'
             CHRECL=' '
             WRITE(LUOUT,2200)I,FILE(1:LENOCC(FILE)),FRM,ACC,CHRECL
          ENDIF
#endif
        ENDIF
200   CONTINUE
      IF (IFNP.NE.0) WRITE(LUOUT,2000)
      END
