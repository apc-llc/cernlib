*
* $Id: mncntr.F,v 1.1.1.1 1996/03/07 14:31:28 mclareni Exp $
*
* $Log: mncntr.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:28  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNCNTR(FCN,KE1,KE2,IERRF,FUTIL)
#include "minuit/d506dp.inc"
CC       to print function contours in two variables, on line printer
CC
#include "minuit/d506cm.inc"
      EXTERNAL FCN,FUTIL
      PARAMETER (NUMBCS=20,NXMAX=115)
      DIMENSION CONTUR(NUMBCS), FCNA(NXMAX),FCNB(NXMAX)
      CHARACTER CLABEL*(NUMBCS)
      CHARACTER CHLN*(NXMAX),CHMID*(NXMAX),CHZERO*(NXMAX)
      DATA CLABEL/'0123456789ABCDEFGHIJ'/
C                 input arguments: parx, pary, devs, ngrid
      IF (KE1.LE.0 .OR. KE2.LE.0)  GO TO 1350
      IF (KE1.GT.NU .OR. KE2.GT.NU)  GO TO 1350
      KI1 = NIOFEX(KE1)
      KI2 = NIOFEX(KE2)
      IF (KI1.LE.0 .OR. KI2.LE.0)  GO TO 1350
      IF (KI1 .EQ. KI2)  GO TO 1350
C
      IF (ISW(2) .LT. 1)  THEN
          CALL MNHESS(FCN,FUTIL)
          CALL MNWERR
          ENDIF
      NPARX = NPAR
      XSAV = U(KE1)
      YSAV = U(KE2)
      DEVS = WORD7(3)
      IF (DEVS .LE. ZERO)  DEVS=2.
      XLO = U(KE1) - DEVS*WERR(KI1)
      XUP = U(KE1) + DEVS*WERR(KI1)
      YLO = U(KE2) - DEVS*WERR(KI2)
      YUP = U(KE2) + DEVS*WERR(KI2)
      NGRID = WORD7(4)
      IF (NGRID .LE. 0)  THEN
          NGRID=25
          NX = MIN(NPAGWD-15,NGRID)
          NY = MIN(NPAGLN-7, NGRID)
      ELSE
          NX = NGRID
          NY = NGRID
      ENDIF
      IF (NX .LT. 11) NX=11
      IF (NY .LT. 11) NY=11
      IF (NX .GE. NXMAX)  NX=NXMAX-1
C         ask if parameter outside limits
      IF (NVARL(KE1) .GT. 1)  THEN
         IF (XLO .LT. ALIM(KE1))  XLO = ALIM(KE1)
         IF (XUP .GT. BLIM(KE1))  XUP = BLIM(KE1)
      ENDIF
      IF (NVARL(KE2) .GT. 1)   THEN
         IF (YLO .LT. ALIM(KE2))  YLO = ALIM(KE2)
         IF (YUP .GT. BLIM(KE2))  YUP = BLIM(KE2)
      ENDIF
      BWIDX = (XUP-XLO)/REAL(NX)
      BWIDY = (YUP-YLO)/REAL(NY)
      IXMID = INT((XSAV-XLO)*REAL(NX)/(XUP-XLO)) + 1
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN(FCN,FUTIL)
      DO 185 I= 1, NUMBCS
      CONTUR(I) = AMIN + UP*FLOAT(I-1)**2
  185 CONTINUE
      CONTUR(1) = CONTUR(1) + 0.01*UP
C                fill FCNB to prepare first row, and find column zero
      U(KE2) = YUP
      IXZERO = 0
      XB4 = ONE
      DO 200 IX= 1, NX+1
      U(KE1) = XLO + REAL(IX-1)*BWIDX
      CALL FCN(NPARX,GIN,FF,U,4,FUTIL)
      FCNB(IX) = FF
      IF (XB4.LT.ZERO .AND. U(KE1).GT.ZERO)  IXZERO = IX-1
      XB4 = U(KE1)
      CHMID(IX:IX) = '*'
      CHZERO(IX:IX)= '-'
  200 CONTINUE
      WRITE (ISYSWR,'(A,I3,A,A)') ' Y-AXIS: PARAMETER ',
     +      KE2,': ',CPNAM(KE2)
      IF (IXZERO .GT. 0)  THEN
         CHZERO(IXZERO:IXZERO) = '+'
         CHLN = ' '
         WRITE (ISYSWR,'(12X,A,A)') CHLN(1:IXZERO),'X=0'
      ENDIF
C                 loop over rows
      DO 280 IY= 1, NY
      UNEXT = U(KE2) - BWIDY
C                 prepare this line's background pattern for contour
      CHLN = ' '
      CHLN(IXMID:IXMID) = '*'
      IF (IXZERO .NE. 0) CHLN(IXZERO:IXZERO) = ':'
      IF (U(KE2).GT.YSAV .AND. UNEXT.LT.YSAV) CHLN=CHMID
      IF (U(KE2).GT.ZERO .AND. UNEXT.LT.ZERO) CHLN=CHZERO
      U(KE2) = UNEXT
      YLABEL = U(KE2) + 0.5*BWIDY
C                 move FCNB to FCNA and fill FCNB with next row
      DO 220 IX= 1, NX+1
      FCNA(IX) = FCNB(IX)
      U(KE1) = XLO + REAL(IX-1)*BWIDX
      CALL FCN(NPARX,GIN,FF,U,4,FUTIL)
      FCNB(IX) = FF
  220 CONTINUE
C                 look for contours crossing the FCNxy squares
      DO 250 IX= 1, NX
      FMX = MAX(FCNA(IX),FCNB(IX),FCNA(IX+1),FCNB(IX+1))
      FMN = MIN(FCNA(IX),FCNB(IX),FCNA(IX+1),FCNB(IX+1))
      DO 230 ICS= 1, NUMBCS
      IF (CONTUR(ICS) .GT. FMN)  GO TO 240
  230 CONTINUE
      GO TO 250
  240 IF (CONTUR(ICS) .LT. FMX) CHLN(IX:IX)=CLABEL(ICS:ICS)
  250 CONTINUE
C                 print a row of the contour plot
      WRITE (ISYSWR,'(1X,G12.4,1X,A)') YLABEL,CHLN(1:NX)
  280 CONTINUE
C                 contours printed, label x-axis
      CHLN = ' '
      CHLN( 1: 1) = 'I'
      CHLN(IXMID:IXMID) = 'I'
      CHLN(NX:NX) = 'I'
      WRITE (ISYSWR,'(14X,A)') CHLN(1:NX)
C                the hardest of all: print x-axis scale!
      CHLN = ' '
      IF (NX .LE. 26) THEN
          NL = MAX(NX-12,2)
          NL2 = NL/2
          WRITE (ISYSWR,'(8X,G12.4,A,G12.4)') XLO,CHLN(1:NL),XUP
          WRITE (ISYSWR,'(14X,A,G12.4)')   CHLN(1:NL2),XSAV
      ELSE
          NL = MAX(NX-24,2)/2
          NL2 = NL
          IF (NL .GT. 10) NL2=NL-6
          WRITE (ISYSWR,'(8X,G12.4,A,G12.4,A,G12.4)')  XLO,
     +      CHLN(1:NL),XSAV,CHLN(1:NL2),XUP
      ENDIF
      WRITE (ISYSWR,'(6X,A,I3,A,A,A,G12.4)') ' X-AXIS: PARAMETER',
     +    KE1,': ',CPNAM(KE1),'  ONE COLUMN=',BWIDX
      WRITE (ISYSWR,'(A,G12.4,A,G12.4,A)') ' FUNCTION VALUES: F(I)=',
     +    AMIN,' +',UP,' *I**2'
C                 finished.  reset input values
      U(KE1) = XSAV
      U(KE2) = YSAV
      IERRF = 0
      RETURN
 1350 WRITE (ISYSWR,1351)
 1351 FORMAT (' INVALID PARAMETER NUMBER(S) REQUESTED.  IGNORED.' /)
      IERRF = 1
      RETURN
      END
