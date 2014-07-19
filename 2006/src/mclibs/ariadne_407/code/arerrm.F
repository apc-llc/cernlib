*
* $Id: arerrm.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
*
* $Log: arerrm.F,v $
* Revision 1.1.1.1  1996/03/08 16:51:01  mclareni
* Ariadne
*
*
#include "ariadne/pilot.h"
C***********************************************************************
C $Id: arerrm.F,v 1.1.1.1 1996/03/08 16:51:01 mclareni Exp $
 
      SUBROUTINE ARERRM(SUB,IERR,ILINE)
 
C...ARiadne subroutine ERRor Message
 
C...Writes out an error message and optionally terminates the program
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,PT2MAX,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
      COMMON /ARDAT3/ IWRN(40)
      SAVE /ARDAT3/
 
      CHARACTER SUB*(*)
 
 
C...Write out common message
      IF (IWRN(IERR).LT.MSTA(10)) WRITE(MSTA(8),1000) SUB,IERR,MSTA(4)
      MSTA(13)=IERR
      IWRN(IERR)=IWRN(IERR)+1
      IFATAL=0
      IDUMP=0
 
C...Check error code and write appropriate message
      IF (IERR.EQ.1) THEN
        WRITE(MSTA(8),1010)
        WRITE(MSTA(8),1001) ILINE
        IFATAL=1
        IDUMP=1
      ELSEIF (IERR.EQ.2) THEN
        WRITE(MSTA(8),1020)
        WRITE(MSTA(8),1001) ILINE
        IFATAL=1
        IDUMP=1
      ELSEIF (IERR.EQ.3) THEN
        IF (IWRN(3).GT.MSTA(10)) RETURN
        IWRN(3)=IWRN(3)+1
        WRITE(MSTA(8),1030)
        IF (IWRN(3).EQ.MSTA(10)) THEN
          WRITE(MSTA(8),1001) ILINE
          IDUMP=1
        ENDIF
      ELSEIF (IERR.EQ.4) THEN
        WRITE(MSTA(8),1040)
        WRITE(MSTA(8),1001) ILINE
        IFATAL=1
        IDUMP=1
      ELSEIF (IERR.EQ.5) THEN
        WRITE(MSTA(8),1050)
        WRITE(MSTA(8),1001) ILINE
        IFATAL=1
        IDUMP=1
      ELSEIF (IERR.EQ.6) THEN
        WRITE(MSTA(8),1060) MAXPAR
        IFATAL=1
      ELSEIF (IERR.EQ.7) THEN
        WRITE(MSTA(8),1070) MAXDIP
        IFATAL=1
      ELSEIF (IERR.EQ.8) THEN
        WRITE(MSTA(8),1080) MAXSTR
        IFATAL=1
      ELSEIF (IERR.EQ.9) THEN
        IF (IWRN(9).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1090)
        IF (IWRN(9).EQ.MSTA(10)) IDUMP=1
      ELSEIF (IERR.EQ.10) THEN
        IF (IWRN(10).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1100)
      ELSEIF (IERR.EQ.11) THEN
        WRITE(MSTA(8),1110)
        IFATAL=1
        IDUMP=1
      ELSEIF (IERR.EQ.12) THEN
        WRITE(MSTA(8),1120)
        IFATAL=1
      ELSEIF (IERR.EQ.13) THEN
        IF (IWRN(13).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1130)
      ELSEIF (IERR.EQ.14) THEN
        WRITE(MSTA(8),1140)
        IFATAL=1
      ELSEIF (IERR.EQ.20) THEN
        IF (IWRN(20).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1200)
      ELSEIF (IERR.EQ.21) THEN
        IF (IWRN(21).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1210)
      ELSEIF (IERR.EQ.22) THEN
        IF (IWRN(22).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1220)
      ELSEIF (IERR.EQ.23) THEN
        WRITE(MSTA(8),1230)
        IFATAL=1
      ELSEIF (IERR.EQ.24) THEN
        WRITE(MSTA(8),1240)
        IFATAL=1
      ELSEIF (IERR.EQ.25) THEN
        IF (IWRN(25).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1250)
        IF (IWRN(25).EQ.MSTA(10)) IDUMP=1
      ELSEIF (IERR.EQ.26) THEN
        IF (IWRN(26).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1260)
      ELSEIF (IERR.EQ.27) THEN
        WRITE(MSTA(8),1270)
        IFATAL=1
      ELSEIF (IERR.EQ.28) THEN
        WRITE(MSTA(8),1280)
        IFATAL=1
      ELSEIF (IERR.EQ.29) THEN
        IF (IWRN(29).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1290)
      ELSEIF (IERR.EQ.30) THEN
        WRITE(MSTA(8),1300)
        IFATAL=1
      ELSEIF (IERR.EQ.31) THEN
        WRITE(MSTA(8),1310)
        IFATAL=1
      ELSEIF (IERR.EQ.32) THEN
        WRITE(MSTA(8),1320)
        IFATAL=1
      ELSEIF (IERR.EQ.33) THEN
        WRITE(MSTA(8),1330) ILINE
        IFATAL=1
      ELSEIF (IERR.EQ.34) THEN
        WRITE(MSTA(8),1340) ILINE
        IFATAL=1
      ELSEIF (IERR.EQ.35) THEN
        WRITE(MSTA(8),1350) ILINE
        IFATAL=1
      ELSEIF (IERR.EQ.36) THEN
        WRITE(MSTA(8),1360) ILINE
        IFATAL=1
      ELSEIF (IERR.EQ.37) THEN
        WRITE(MSTA(8),1370) ILINE
        IFATAL=1
      ELSEIF (IERR.EQ.38) THEN
        WRITE(MSTA(8),1380) ILINE,ILINE+1
        IFATAL=1
      ENDIF
 
C...Dump ariadne dipole record and list the event if necessary
      IF (IDUMP.GT.0.OR.IFATAL.GT.0) THEN
        IF (.NOT.QDUMP) CALL ARDUMP
        WRITE(MSTA(8),1002)
        CALL LULIST(2)
      ENDIF
 
C...Stop execution if necessary
      IF (IFATAL.GT.0) THEN
        CALL ARPRDA
        WRITE(MSTA(8),1003)
        STOP 0
      ENDIF
 
 1000 FORMAT('*** ERROR Found by Ariadne ***'/'In routine ',A6,
     $     '. Error type =',I3,'. Ariadne call number:',I7)
 1001 FORMAT('Line number:',I4)
 1002 FORMAT('Dump of event follows:')
 1003 FORMAT('Error is fatal. Execution stopped.')
 
 1010 FORMAT('Found colour-singlet particle in string.')
 1020 FORMAT('Found colour-triplet particle in string.')
 1030 FORMAT('Found colour-singlet particle in string.',
     $       ' Will try to cope...')
 1040 FORMAT('Found colour-triplet particle in purely gluonic string.')
 1050 FORMAT('Inconsistent colour flow in string.')
 1060 FORMAT('Maximum number of partons (',I5,') exceeded. See manual.')
 1070 FORMAT('Maximum number of dipoles (',I5,') exceeded. See manual.')
 1080 FORMAT('Maximum number of strings (',I5,') exceeded. See manual.')
 1090 FORMAT('Four-momentum was not conserved.')
 1100 FORMAT('Particle has inconsistent four-momentum. ',
     $     'Will try to cope...')
 1110 FORMAT('Recoil transfer for Drell-Yan process was not',
     $       ' kinematically allowed.')
 1120 FORMAT('Ariadne not properly initialized before call to AREXEC.')
 1130 FORMAT('Dipole has inconsistent mass. Will try to cope...')
 1140 FORMAT('Unphysical boost vector.',/,
     $     'Try switching to double precision - see manual')
 1200 FORMAT('Selected sub-process in PYTHIA is not suported by',
     $  ' Ariadne.',/,
     $  '(only processes 11,12,13,28,53,68 are currently supported)',/,
     $  'Will try to continue but results may not be meaningful.')
 1210 FORMAT('Too many jet-initiators. ARCLUS was not performed.')
 1220 FORMAT('Caught in an infinite loop. Please disregard this event.')
 1230 FORMAT('Cannot handle non-baryon targets (yet).')
 1240 FORMAT('This routine should not have been called.',/,
     $     ' See installation instructions for further information')
 1250 FORMAT('Four-momentum was not conserved.',/,
     $     ' Please disregard this event')
 1260 FORMAT('Probability in BGF greater than 1')
 1270 FORMAT('Could not find Drell-Yan particle.')
 1280 FORMAT('Uphysical parameters in /ARPOPA/')
 1290 FORMAT('Pomeron structure function greater than the total.',/,
     $     ' Rescaling Pomeron structure function.')
 1300 FORMAT('Inconsistent remnants')
 1310 FORMAT('Tried to access event record outside of the stack')
 1320 FORMAT('No colour information available for rearrangement.')
 1330 FORMAT('Too few particles (',i2,') in event record.')
 1340 FORMAT('Particle in line ',i2,' is not a fermion.')
 1350 FORMAT('Particle in line ',i2,' does not obey sign')
 1360 FORMAT('Particle in line ',i2,' is not a photon.')
 1370 FORMAT('Particle in line ',i2,
     $       ' has incinsistent momentum vector.')
 1380 FORMAT('Fermion pair in lines ',i2,' and',i2,' is inconsistent.')
 
      RETURN
 
C**** END OF ARERRM ****************************************************
      END
