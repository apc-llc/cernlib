C.----------------------------------------------------------------------
C.
C.    PHOTOS:   PHOton radiation in decays TeST program
C.
C.    Purpose:  Example of application of PHOTOS.
C.
C.    Input Parameters:   None
C.
C.    Output Parameters:  None
C.
C.    Author(s):  B. van Eijk, E. Barberio        Created at:  31/05/90
C.                                                Last Update: 05/06/90
C.
C.----------------------------------------------------------------------
      PROGRAM PHOTST
C      IMPLICIT NONE
      INTEGER EVENT,NHEP0
      INTEGER NMXHEP
      PARAMETER (NMXHEP=4000)
      INTEGER IDHEP,ISTHEP,JDAHEP,JMOHEP,NEVHEP,NHEP
      REAL*8 PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      INTEGER PHLUN
      COMMON/PHOLUN/PHLUN
C--
C--   Initialise PHOTOS
      CALL PHOINI
C--
C--   Loop over JETSET event until PHOTOS has generated one or more pho-
C--   tons.  Do this  for 10 JETSET events.  The event record is printed
C--   before and after photon emission.
      DO 20 EVENT=1,1 
        CALL LUEEVT(4,91.)
C--
C--   Conversion to /HEPEVT/ standard
        CALL LUHEPC(1)
C--
C--   Write event record before emission...
        NEVHEP=EVENT
        CALL PHODMP
        NHEP0=NHEP
C--
C--   Generate photon(s)...
   10   CALL PHOTOS(1)
        IF (NHEP.EQ.NHEP0) GOTO 10
C--
C--   Write event record...
      WRITE(PHLUN,9050)
      WRITE(PHLUN,9040)
        CALL PHODMP
   20 CONTINUE
      WRITE(PHLUN,9000)
      WRITE(PHLUN,9010)
      WRITE(PHLUN,9020)
      WRITE(PHLUN,9030)
      WRITE(PHLUN,9020)
      WRITE(PHLUN,9010)
      STOP
 9000 FORMAT(1H1)
 9010 FORMAT(1H ,80('*'))
 9020 FORMAT(1H ,'*',78X,'*')
 9030 FORMAT(1H ,'**** PHOTOS Test Run has successfully ended',32X,
     &' ****')
 9040 FORMAT(1H ,26X,'=== after PHOTOS: ===')
 9050 FORMAT(1H0,80('='))
      END
      SUBROUTINE PHODMP
C.----------------------------------------------------------------------
C.
C.    PHOTOS:   PHOton radiation in decays event DuMP routine
C.
C.    Purpose:  Print event record.
C.
C.    Input Parameters:   Common /HEPEVT/
C.
C.    Output Parameters:  None
C.
C.    Author(s):  B. van Eijk                     Created at:  05/06/90
C.                                                Last Update: 05/06/90
C.
C.----------------------------------------------------------------------
C      IMPLICIT NONE
      DOUBLE PRECISION  SUMVEC(5)
      INTEGER I,J
      INTEGER NMXHEP
      PARAMETER (NMXHEP=4000)
      INTEGER IDHEP,ISTHEP,JDAHEP,JMOHEP,NEVHEP,NHEP
      REAL*8 PHEP,VHEP
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      INTEGER PHLUN
      COMMON/PHOLUN/PHLUN
      DO 10 I=1,5
   10 SUMVEC(I)=0.
C--
C--   Print event number...
      WRITE(PHLUN,9000)
      WRITE(PHLUN,9010) NEVHEP
      WRITE(PHLUN,9080)
      WRITE(PHLUN,9020)
      DO 30 I=1,NHEP
C--
C--   For 'stable particle' calculate vector momentum sum
        IF (JDAHEP(1,I).EQ.0) THEN
          DO 20 J=1,4
   20     SUMVEC(J)=SUMVEC(J)+PHEP(J,I)
          IF (JMOHEP(2,I).EQ.0) THEN
            WRITE(PHLUN,9030) I,IDHEP(I),JMOHEP(1,I),(PHEP(J,I),J=1,5)
          ELSE
            WRITE(PHLUN,9040) I,IDHEP(I),JMOHEP(1,I),JMOHEP(2,I),(PHEP
     &      (J,I),J=1,5)
          ENDIF
        ELSE
          IF (JMOHEP(2,I).EQ.0) THEN
            WRITE(PHLUN,9050) I,IDHEP(I),JMOHEP(1,I),JDAHEP(1,I),
     &      JDAHEP(2,I),(PHEP(J,I),J=1,5)
          ELSE
            WRITE(PHLUN,9060) I,IDHEP(I),JMOHEP(1,I),JMOHEP(2,I),
     &      JDAHEP(1,I),JDAHEP(2,I),(PHEP(J,I),J=1,5)
          ENDIF
        ENDIF
   30 CONTINUE
      SUMVEC(5)=SQRT(SUMVEC(4)**2-SUMVEC(1)**2-SUMVEC(2)**2-
     &SUMVEC(3)**2)
      WRITE(PHLUN,9070) (SUMVEC(J),J=1,5)
      RETURN
 9000 FORMAT(1H0,80('='))
 9010 FORMAT(1H ,29X,'Event No.:',I10)
 9020 FORMAT(1H0,1X,'Nr',3X,'Type',3X,'Parent(s)',2X,'Daughter(s)',6X,
     &'Px ',7X,'Py ',7X,'Pz ',7X,'E ',4X,'Inv. M.')
 9030 FORMAT(1H ,I4,I7,3X,I4,9X,'Stable',2X,5F10.4)
 9040 FORMAT(1H ,I4,I7,I4,' - ',I4,5X,'Stable',2X,5F10.4)
 9050 FORMAT(1H ,I4,I7,3X,I4,6X,I4,' - ',I4,5F10.4)
 9060 FORMAT(1H ,I4,I7,I4,' - ',I4,2X,I4,' - ',I4,5F10.4)
 9070 FORMAT(1H0,23X,'Vector Sum: ', 5F10.4)
 9080 FORMAT(1H0,6X,'Particle Parameters')
      END
