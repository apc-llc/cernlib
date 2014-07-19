#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/itapes.inc"
#include "isajet/idrun.inc"
#if defined(CERNLIB_ZEBINIT)
#include "isajet/isabnk.inc"
#include "isajet/isaunt.inc"
#endif
C
      CHARACTER*80 FILDKY,FILEVT,FILCOM,FILLIS,FILEX
      CHARACTER*40 V,VISAJE
      CHARACTER*1 YN
      LOGICAL NEWCOM,QPART,QCAL,QLEP
      INTEGER IDKY
C
C
      V=VISAJE()
      PRINT 1000,V
1000  FORMAT(/,10X,A40,/,5X,' (interactive version)',//,
     $15X,'All input should be UPPER CASE.'/
     $15X,'If in trouble, try HELP.'/)
C
C          SPECIFY DECAY TABLE -- OPENED ONLY FOR EXECUTION
      PRINT*,' Enter file name for the decay table.'
      READ 1010,FILDKY
1010  FORMAT(A80)
      PRINT*,' Do you want to print the decay table? (Y/N)'
      READ 1020,YN
1020  FORMAT(A1)
      IF(YN.EQ.'N') THEN
        IDKY=-11
      ELSE
        IDKY=+11
      ENDIF
C
C          SPECIFY OUTPUT FILE -- OPENED ONLY FOR EXECUTION
      PRINT*,' Enter file name for the output data.'
      READ 1010,FILEVT
      PRINT*,
     $' Do you want both resonances and stable particles? (Y/N)'
      READ 1020,YN
      IF(YN.EQ.'N') THEN
        ITEVT=-12
      ELSE
        ITEVT=+12
      ENDIF
C
C          SPECIFY COMMAND FILE
      PRINT* ,' Do you wish to use an OLD command file? (Y/N)'
      READ 1020,YN
      IF(YN.EQ.'Y') THEN
        PRINT* ,' Enter file name for the old command file.'
        READ 1010,FILCOM
        FILEX=FILCOM
        OPEN(UNIT=13,ERR=100,FILE=FILCOM,STATUS='OLD',FORM='FORMATTED')
        NEWCOM=.FALSE.
      ELSE
        PRINT*, ' Enter file name for the new command file.'
        PRINT*, ' If the name is SCRATCH, the file will not be saved.'
        READ 1010,FILCOM
        FILEX=FILCOM
        OPEN(UNIT=13,ERR=100,FILE=FILCOM,STATUS='NEW',FORM='FORMATTED')
        NEWCOM=.TRUE.
      ENDIF
C
C          SPECIFY LISTING FILE
      PRINT*,' Enter file name for the listing.'
      READ 1010, FILLIS
      FILEX=FILLIS
      OPEN(UNIT=14,ERR=100,FILE=FILLIS,STATUS='NEW',FORM='FORMATTED')
C
C          PREPARE COMMAND FILE
      IF(NEWCOM) CALL ISASET(IDKY,ITEVT,13,14)
C
C          EXECUTE COMMAND FILE
      PRINT*,' Do you wish to run this job? (Y/N)'
      READ 1020,YN
      IF(YN.EQ.'Y') THEN
        FILEX=FILDKY
        OPEN(UNIT=11,ERR=100,FILE=FILDKY,STATUS='OLD',FORM='FORMATTED')
        FILEX=FILEVT
        OPEN(UNIT=12,ERR=100,FILE=FILEVT,STATUS='NEW',
     &    FORM='UNFORMATTED')
#if defined(CERNLIB_ZEBINIT)
C
C          Setup for ZEBRA only
C
      ISUNIT=IABS(ITEVT)
      FILISA=FILEVT
      PRINT *,' Select ZEBRA banks by typing:ISAP or ISAC or ISAL'
      PRINT *,' ISAP: all particles banks will be written'
      PRINT *,' ISAC: pseudo calorimeter banks will be written'
      PRINT *,' ISAL: leptons banks will be written'
      PRINT *,' Any combination is allowed, i.e.',
     $' ISAPISACISAL will write all 3 banks'
      READ 1011,BANK
 1011 FORMAT(A12)
C
C  Set up flags to select type of output
C
      QPART=BANK(1:4).EQ.'ISAP'.OR.BANK(5:8).EQ.'ISAP'.OR.
     $        BANK(9:12).EQ.'ISAP'
      QCAL=BANK(1:4).EQ.'ISAC'.OR.BANK(5:8).EQ.'ISAC'.OR.
     $       BANK(9:12).EQ.'ISAC'
      QLEP=BANK(1:4).EQ.'ISAL'.OR.BANK(5:8).EQ.'ISAL'.OR.
     $     BANK(9:12).EQ.'ISAL'
      CALL ISBKST(QPART,QCAL,QLEP)
C
      CALL MZEBRA(0)
      CALL ISAZEB('O')           ! initialize Zebra for ISAJET
#endif
        CALL ISAJET(IDKY,ITEVT,13,14)
        PRINT*,' Events have been generated on file'
        PRINT*,FILEVT
      ENDIF
C
C          TERMINATE
      IF(FILCOM(1:7).EQ.'SCRATCH') THEN
        FILEX=FILCOM
        CLOSE(13,ERR=100,STATUS='DELETE')
      ENDIF
      PRINT*,' Job terminated normally.'
      STOP
C
100   PRINT 1030,FILEX
1030  FORMAT(/' Unable to open or close file '/1X,A80/
     $' Job terminated.')
      STOP 99
      END
