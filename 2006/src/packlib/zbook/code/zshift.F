*
* $Id: zshift.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: zshift.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZSHIFT(IZ,IOLD,IMAX,NPUSH,LABEL)
C
C
C     ******************************************************************
C     *                                                                *
C     *                                                                *
C     *       INTERNAL ZBOOK ROUTINE                                   *
C     *       MODIFICATION TO ALLOW MOTHER BANKS TO FOLLOW DAUGHTERS   *
C     *                                                                *
C     *    MODS BY        DANNY LANSKE + FRANZ-JOSEF HASERT            *
C     *       SHIFT MEMORY FROM ADDRESS I1 BY NPUSH WORDS              *
C     *                                                                *
C     *                                                                *
C     ******************************************************************
C
      DIMENSION IZ(1),LABEL(1)
C
C
C     ------------------------------------------------------------------
C
C
      LOGICAL INSIDE
C
      INSIDE (IPOINT) = IPOINT .GE. IOLD .AND. IPOINT .LT. IMAX
C
      JZ     = IZ(1)
      IFIRST = IOLD + NPUSH
      CALL UCOPY2(IZ(IOLD),IZ(IFIRST),IMAX-IOLD+1)
C
      IF (IFIRST.GE.IMAX + NPUSH)                GO TO 30
C
  10  NLONG  = IZ(IFIRST)
      IFIRST = IFIRST + IABS(NLONG)
      IF (IFIRST.GT.IZ(JZ + 14))                 GO TO 90
      IF (NLONG.LT.0)                            GO TO 25
C
C             UPDATE POINTERS AREA (IF ANY)
C
      NLID = IZ(IFIRST - 1)
      IF(NLID .LT. IZ(JZ+12))                    GO TO 90
      IF(NLID .GT. IZ(JZ+13))                    GO TO 90
C
      IF(INSIDE(NLID))                           GO TO 12
C
      IF(NLID .NE. 1)  IZ(NLID) = IZ(NLID) + NPUSH
      GO TO 14
C
  12  CONTINUE
      NLID = NLID + NPUSH
      IF(NLID .LT. IZ(JZ+12))  IZ(JZ+12) = NLID
      IF(NLID .GT. IZ(JZ+13))  IZ(JZ+13) = NLID
      IZ(IFIRST - 1) = NLID
C
  14  CONTINUE
      ILINK = IFIRST - 2
      NLINK = IZ(ILINK)
      IF(NLINK .LE. 0)                           GO TO 25
C
      IDATA = IFIRST - NLONG + NLINK + 1
C
      DO 20 I = 1, NLINK
      JP = IZ(IDATA - I)
      IF(JP .EQ. 0)                              GO TO 20
C
      IF(INSIDE(JP))                             GO TO 16
C
      IF(JP .GT. IZ(JZ+14))                      GO TO 90
C
      ND = IZ(JP)
      IL = JP + ND + 2
      IF(IL .LE. 0)                              GO TO 90
      IF(IL .GT. IZ(JZ+14))                      GO TO 90
C
      NID = IZ(IL) + NPUSH
      IF(NID .LT. IZ(JZ+12))  IZ(JZ+12) = NID
      IF(NID .GT. IZ(JZ+13))  IZ(JZ+13) = NID
      IZ(IL) = NID
      GO TO 20
C
  16  CONTINUE
      JP = JP + NPUSH
      IF(JP .GT. IZ(JZ+14))                      GO TO 90
      IZ(IDATA - I) = JP
C
  20  CONTINUE
  25  IF (IZ(IFIRST).EQ.0)                       GO TO 30
      IF (IFIRST.LE.IMAX + NPUSH)                GO TO 10
C
C             UPDATE USER LOCAL POINTERS
C
  30  IF (IZ(JZ - 1).EQ.0)                       GO TO 95
      IF (IOLD.GE.IMAX)                          GO TO 95
      CALL ZUPLOC(IZ,IOLD,IMAX,NPUSH)
      GO TO 95
  90  CALL ZERROR(IZ,400,LABEL ,0)
      RETURN
C
  95  IF (IMAX.LT.IZ(JZ + 15))                   GO TO 99
      IZ(JZ + 15) = IZ(JZ + 15) + NPUSH
      IZ(JZ + 1) = IZ(JZ + 14) - IZ(JZ + 15)
  99  RETURN
      END
