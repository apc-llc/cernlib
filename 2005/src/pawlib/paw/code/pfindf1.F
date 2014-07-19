*
* $Id: pfindf1.F,v 1.1.1.1 1996/03/01 11:38:46 mclareni Exp $
*
* $Log: pfindf1.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.06/16 22/02/95  15.14.34  by  Fons Rademakers
*-- Author :    Fons Rademakers   24/01/94
      SUBROUTINE PFINDF1(CADR, IERROR)
*.==========>
*.
*.     Scan the COMIS memory for the occurance of Ntuple variables.
*.     Add all found variables to the VAR list.
*.
*..=========> ( A.A.Rademakers )
*
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
#include "paw/pntold.inc"
*
      PARAMETER    (NBLK = 4)
      CHARACTER*32  VAR
      CHARACTER*6   CMBLK(NBLK)
      INTEGER       CADR, ICS(NBLK)
*
      SAVE          CMBLK, ICS
*
      DATA          CMBLK /'PAWIDN', 'PAWCR4', 'PAWCR8', 'PAWC32'/
      DATA          ICS   /4, 4, 8, 32/
*
      IERROR = 0
*
*-- scan the common blocks PAWIDN, PAWCR4, PAWCR8, PAWC32
*
      DO 10 I = 1, NBLK
*
         IF (NTOLD .AND. I.GT.1) RETURN
         IF (.NOT.NTOLD .AND. I.EQ.1) GOTO 10
*
         IIOFF = -1
  20     CALL CSLISTX(CMBLK(I), CADR, IIOFF, NEL)
  25     IF (NEL .GT. 0) THEN
            IF (ICS(I) .EQ. 4)  IOFF = IIOFF
            IF (ICS(I) .EQ. 8)  IOFF = IIOFF/2
            IF (ICS(I) .EQ. 32) IOFF = IIOFF/8
            IF (NTOLD) THEN
*-- 13 is the offset in the PAWIDN common where X starts
               J = IOFF - 13
               IF (J .LT. 1) GOTO 20
               ITAG1 = IQ(LCID+10)
               VAR = ' '
               CALL UHTOC(IQ(LCID+ITAG1+2*(J-1)),4,VAR,8)
               ISTA = 1
    5          IF (VAR(ISTA:ISTA) .EQ. ' ') THEN
                  ISTA = ISTA+1
                  GOTO 5
               ENDIF
               VAR = VAR(ISTA:)
               IELEM = 1
            ELSE
               CALL HNTGETI(ID,ICS(I),IOFF+1,VAR,ITYPE,ISIZE,IELEM,IER)
               IF (IER .NE. 0) THEN
                  CALL HBUG('Array dimension does not match Ntuple'//
     +                      ' dimension','PFINDF1',ID)
                  GOTO 99
               ENDIF
            ENDIF
*
*-- accept variable
*
            CALL PADVAR(VAR, IDUM, IER)
            IF (IER .NE. 0) GOTO 99
*
*-- next array element (in case of EQUIVALENCE)
*
            IF (ICS(I) .EQ. 4) THEN
               NEL   = NEL - IELEM
               IIOFF = IIOFF + IELEM
            ENDIF
            IF (ICS(I) .EQ. 8) THEN
               NEL   = NEL - IELEM
               IIOFF = IIOFF + (IELEM*2)
            ENDIF
            IF (ICS(I) .EQ. 32) THEN
               NEL   = NEL - IELEM
               IIOFF = IIOFF + (IELEM*8)
            ENDIF
*
            IF (NEL .GT. 0) GOTO 25
*
            GOTO 20
*
***         ELSE
***            IOFF = -2
***            CALL CSLISTX(CMBLK(I), CADR, IOFF, NEL)
         ENDIF
*
  10  CONTINUE
*
      RETURN
*
  99  IERROR = 1
*
      END
