*
* $Id: arerrm.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
*
* $Log: arerrm.F,v $
* Revision 1.1.1.1  1996/01/11 14:05:17  mclareni
* Fritiof
*
*
C***********************************************************************
C $Id: arerrm.F,v 1.1.1.1 1996/01/11 14:05:17 mclareni Exp $
 
      SUBROUTINE ARERRM(SUB,IERR,ILINE)
 
C...ARiadne subroutine ERRor Message
 
C...Writes out an error message and optionally terminates the program
 
 
      PARAMETER(MAXDIP=500,MAXPAR=500,MAXSTR=100)
 
      IMPLICIT DOUBLE PRECISION (D)
      IMPLICIT DOUBLE PRECISION (B)
      IMPLICIT LOGICAL (Q)
 
      COMMON /ARSTRS/ IPF(MAXSTR),IPL(MAXSTR),IFLOW(MAXSTR),
     $                PT2LST,IMF,IML,IO,QDUMP,ISTRS
      SAVE /ARSTRS/
 
      COMMON /ARDAT1/ PARA(40),MSTA(40)
      SAVE /ARDAT1/
 
      COMMON /ARDAT3/ IWRN(40)
      SAVE /ARDAT3/
 
      COMMON /LUDAT1/ MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
      CHARACTER SUB*(*)
 
C...Write out common message
      IF(IWRN(IERR).LT.MSTA(10)) WRITE(MSTA(8),1000) SUB,IERR,MSTA(4)
      MSTA(13)=IERR
      IWRN(IERR)=IWRN(IERR)+1
      IFATAL=0
      IDUMP=0
 
C...Check error code and write appropriate message
      IF(IERR.EQ.1) THEN
        WRITE(MSTA(8),1010)
        WRITE(MSTA(8),1001) ILINE
        IFATAL=1
        IDUMP=1
      ELSEIF(IERR.EQ.2) THEN
        WRITE(MSTA(8),1020)
        WRITE(MSTA(8),1001) ILINE
        IFATAL=1
        IDUMP=1
      ELSEIF(IERR.EQ.3) THEN
        IF(IWRN(3).GT.MSTA(10)) RETURN
        IWRN(3)=IWRN(3)+1
        WRITE(MSTA(8),1030)
        IF(IWRN(3).EQ.MSTA(10)) THEN
          WRITE(MSTA(8),1001) ILINE
          IDUMP=1
        ENDIF
      ELSEIF(IERR.EQ.4) THEN
        WRITE(MSTA(8),1040)
        WRITE(MSTA(8),1001) ILINE
        IFATAL=1
        IDUMP=1
      ELSEIF(IERR.EQ.5) THEN
        WRITE(MSTA(8),1050)
        WRITE(MSTA(8),1001) ILINE
        IFATAL=1
        IDUMP=1
      ELSEIF(IERR.EQ.6) THEN
        WRITE(MSTA(8),1060) MAXPAR
        IFATAL=1
      ELSEIF(IERR.EQ.7) THEN
        WRITE(MSTA(8),1070) MAXDIP
        IFATAL=1
      ELSEIF(IERR.EQ.8) THEN
        WRITE(MSTA(8),1080) MAXSTR
        IFATAL=1
      ELSEIF(IERR.EQ.9) THEN
        IF(IWRN(9).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1090)
        IF(IWRN(9).EQ.MSTA(10)) IDUMP=1
      ELSEIF(IERR.EQ.10) THEN
        IF(IWRN(10).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1100)
      ELSEIF(IERR.EQ.11) THEN
        WRITE(MSTA(8),1110)
        IFATAL=1
        IDUMP=1
      ELSEIF(IERR.EQ.12) THEN
        WRITE(MSTA(8),1120)
        IFATAL=1
      ELSEIF(IERR.EQ.13) THEN
        IF(IWRN(13).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1130)
      ELSEIF(IERR.EQ.14) THEN
        WRITE(MSTA(8),1140)
        IFATAL=1
      ELSEIF(IERR.EQ.20) THEN
        IF(IWRN(20).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1200)
      ELSEIF(IERR.EQ.21) THEN
        IF(IWRN(21).GT.MSTA(10)) RETURN
        WRITE(MSTA(8),1210)
      ENDIF
 
C...Dump ariadne dipole record and list the event if necessary
      IF(IDUMP.GT.0) THEN
        IF(.NOT.QDUMP) CALL ARDUMP
        WRITE(MSTA(8),1002)
        CALL LULIST(2)
      ENDIF
 
C...Stop execution if necessary
      IF(IFATAL.GT.0) THEN
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
 1120 FORMAT('AREXEC called before initialization with ARINIT.')
 1130 FORMAT('Dipole has inconsistent mass. Will try to cope...')
 1140 FORMAT('Unphysical boost vector.',/,
     $     'Try switching to double precision - see manual')
 1200 FORMAT('Selected sub-process in PYTHIA is not suported by',
     $  ' Ariadne.',/,
     $  '(only processes 11,12,13,28,53,68 are currently supported)',/,
     $  'Will try to continue but results may not be meaningful.')
 1210 FORMAT('Too many jets. ARCLUS not able to order jets in energy.')
 
      RETURN
 
C**** END OF ARERRM ****************************************************
      END
