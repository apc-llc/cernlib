*
* $Id: mnlims.F,v 1.2 1996/03/15 18:02:48 james Exp $
*
* $Log: mnlims.F,v $
* Revision 1.2  1996/03/15 18:02:48  james
*     Modified Files:
* mnderi.F eliminate possible division by zero
* mnexcm.F suppress print on STOP when print flag=-1
*          set FVAL3 to flag if FCN already called with IFLAG=3
* mninit.F set version 96.03
* mnlims.F remove arguments, not needed
* mnmigr.F VLEN -> LENV in debug print statement
* mnparm.F move call to MNRSET to after NPAR redefined, to zero all
* mnpsdf.F eliminate possible division by zero
* mnscan.F suppress printout when print flag =-1
* mnset.F  remove arguments in call to MNLIMS
* mnsimp.F fix CSTATU so status is PROGRESS only if new minimum
* mnvert.F eliminate possible division by zero
*
* Revision 1.1.1.1  1996/03/07 14:31:30  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNLIMS
#include "minuit/d506dp.inc"
CC       Called from MNSET
CC       Interprets the SET LIM command, to reset the parameter limits
CC
#include "minuit/d506cm.inc"
C
      CFROM = 'SET LIM '
      NFCNFR = NFCN
      CSTATU= 'NO CHANGE '
      I2 = WORD7(1)
      IF (I2 .GT. MAXEXT .OR. I2 .LT. 0)  GO TO 900
      IF (I2 .GT. 0)  GO TO 30
C                                     set limits on all parameters
      NEWCOD = 4
      IF (WORD7(2) .EQ. WORD7(3))  NEWCOD = 1
      DO 20 INU= 1, NU
      IF (NVARL(INU) .LE. 0)  GO TO 20
      IF (NVARL(INU).EQ.1 .AND. NEWCOD.EQ.1)  GO TO 20
      KINT = NIOFEX(INU)
C             see if parameter has been fixed
      IF (KINT .LE. 0)  THEN
         IF (ISW(5) .GE. 0)  WRITE (ISYSWR,'(11X,A,I3)')
     +      ' LIMITS NOT CHANGED FOR FIXED PARAMETER:',INU
         GO TO 20
      ENDIF
      IF (NEWCOD .EQ. 1)  THEN
C            remove limits from parameter
         IF (ISW(5) .GT. 0)     WRITE (ISYSWR,134)  INU
         CSTATU = 'NEW LIMITS'
         CALL MNDXDI(X(KINT),KINT,DXDI)
         SNEW = GSTEP(KINT)*DXDI
         GSTEP(KINT) = ABS(SNEW)
         NVARL(INU) = 1
      ELSE
C             put limits on parameter
         ALIM(INU) = MIN(WORD7(2),WORD7(3))
         BLIM(INU) = MAX(WORD7(2),WORD7(3))
         IF (ISW(5) .GT. 0) WRITE (ISYSWR,237)  INU,ALIM(INU),BLIM(INU)
         NVARL(INU) = 4
         CSTATU = 'NEW LIMITS'
         GSTEP(KINT) = -0.1
      ENDIF
   20 CONTINUE
      GO TO 900
C                                       set limits on one parameter
   30 IF (NVARL(I2) .LE. 0)  THEN
        WRITE (ISYSWR,'(A,I3,A)') ' PARAMETER ',I2,' IS NOT VARIABLE.'
        GO TO 900
      ENDIF
      KINT = NIOFEX(I2)
C                                       see if parameter was fixed
      IF (KINT .EQ. 0)  THEN
         WRITE (ISYSWR,'(A,I3)')
     +     ' REQUEST TO CHANGE LIMITS ON FIXED PARAMETER:',I2
         DO 82 IFX= 1, NPFIX
         IF (I2 .EQ. IPFIX(IFX)) GO TO 92
   82    CONTINUE
         WRITE (ISYSWR,'(A)') ' MINUIT BUG IN MNLIMS. SEE F. JAMES'
   92    CONTINUE
      ENDIF
      IF (WORD7(2) .NE. WORD7(3))  GO TO 235
C                                       remove limits
      IF (NVARL(I2) .NE. 1)  THEN
         IF (ISW(5) .GT. 0)  WRITE (ISYSWR,134)  I2
  134    FORMAT (30H LIMITS REMOVED FROM PARAMETER  ,I4)
         CSTATU = 'NEW LIMITS'
         IF (KINT .LE. 0)  THEN
            GSTEPS(IFX) = ABS(GSTEPS(IFX))
         ELSE
            CALL MNDXDI(X(KINT),KINT,DXDI)
            IF (ABS(DXDI) .LT. 0.01)  DXDI=0.01
            GSTEP(KINT) = ABS(GSTEP(KINT)*DXDI)
            GRD(KINT) = GRD(KINT)*DXDI
         ENDIF
         NVARL(I2) = 1
      ELSE
         WRITE (ISYSWR,'(A,I3)') ' NO LIMITS SPECIFIED.  PARAMETER ',
     +        I2,' IS ALREADY UNLIMITED.  NO CHANGE.'
      ENDIF
      GO TO 900
C                                        put on limits
  235 ALIM(I2) = MIN(WORD7(2),WORD7(3))
      BLIM(I2) = MAX(WORD7(2),WORD7(3))
      NVARL(I2) = 4
      IF (ISW(5) .GT. 0)   WRITE (ISYSWR,237)  I2,ALIM(I2),BLIM(I2)
  237 FORMAT (10H PARAMETER ,I3, 14H LIMITS SET TO  ,2G15.5)
      CSTATU = 'NEW LIMITS'
      IF (KINT .LE. 0)  THEN
         GSTEPS(IFX) = -0.1
      ELSE
         GSTEP(KINT) = -0.1
      ENDIF
C
  900 CONTINUE
      IF (CSTATU .NE. 'NO CHANGE ')  THEN
        CALL MNEXIN(X)
        CALL MNRSET(1)
      ENDIF
      RETURN
      END
