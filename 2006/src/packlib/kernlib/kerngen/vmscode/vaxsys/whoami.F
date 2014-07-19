*
* $Id: whoami.F,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
*
* $Log: whoami.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
* Kernlib
*
*
#include "kerngen/pilot.h"
      SUBROUTINE WHOAMI (NAME)
C
C CERN PROGLIB# Z266    WHOAMI          .VERSION KERNVAX  2.42  930609
C ORIG. 08/06/93  FCA + JZ
C
      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,SLDUMMY(37)

      CHARACTER    NAME*(*)

      PARAMETER   (JPI$_IMAGNAME = '207'X)
      INTEGER*4    NCHALL
      INTEGER*4    LISTF(4)
      INTEGER*2    LISTH(8)
      EQUIVALENCE (LISTH(1),LISTF(1))
      INTEGER      SYS$GETJPIW

C--    System service item list

      DATA LISTH(1),LISTH(2) / 99, JPI$_IMAGNAME /
      DATA LISTF(3),LISTF(4) / 0 , 0 /


      LISTH(1) = LEN(NAME)
      LISTF(2) = %LOC(NAME)
      LISTF(3) = %LOC(NCHALL)
      ISTAT    = SYS$GETJPIW(,,,LISTF,,,)
      IF (JBIT(ISTAT,1).EQ.0)  THEN
          NDSLAT = 0
          RETURN
        ENDIF

      NCHSEM = ICFIND (';', NAME,1,NCHALL) - 1

      IF (NAME(NCHSEM-3:NCHSEM).EQ.'.EXE')  THEN
          NDSLAT = NCHSEM - 4
        ELSE
          NDSLAT = NCHSEM
        ENDIF

      NESLAT = NCHSEM
      NFSLAT = NCHALL
      RETURN
      END
