*
* $Id: mngrad.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mngrad.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNGRAD(FCN,FUTIL)
#include "minuit/d506dp.inc"
CC       Called from MNSET
CC       Interprets the SET GRAD command, which informs MINUIT whether
CC       the first derivatives of FCN will be calculated by the user
CC       inside FCN.  It can check the user's derivative calculation
CC       by comparing it with a finite difference approximation.
CC
#include "minuit/d506cm.inc"
C
      EXTERNAL FCN,FUTIL
      CHARACTER*4 CGOOD,CBAD,CNONE,CWD
      LOGICAL LNONE
      DIMENSION GF(MNI)
      PARAMETER (CGOOD='GOOD',CBAD=' BAD',CNONE='NONE')
C
      ISW(3) = 1
      NPARX = NPAR
      IF (WORD7(1) .GT. ZERO)  GO TO 2000
C                  get user-calculated first derivatives from FCN
      DO 30 I= 1, NU
   30 GIN(I) = UNDEFI
      CALL MNINEX(X)
      CALL FCN(NPARX,GIN,FZERO,U,2,FUTIL)
      NFCN = NFCN + 1
      CALL MNDERI(FCN,FUTIL)
      DO 40 I= 1, NPAR
   40 GF(I) = GRD(I)
C                    get MINUIT-calculated first derivatives
      ISW(3) = 0
      ISTSAV = ISTRAT
      ISTRAT = 2
      CALL MNHES1(FCN,FUTIL)
      ISTRAT = ISTSAV
      WRITE (ISYSWR,51)
   51 FORMAT(/' CHECK OF GRADIENT CALCULATION IN FCN'/12X,'PARAMETER',
     + 6X,9HG(IN FCN) ,3X,9HG(MINUIT) ,2X,'DG(MINUIT)',3X,9HAGREEMENT)
      ISW(3) = 1
      LNONE = .FALSE.
      DO 100 LC = 1, NPAR
      I = NEXOFI(LC)
      CWD = CGOOD
      ERR = DGRD(LC)
      IF (ABS(GF(LC)-GRD(LC)) .GT. ERR)  CWD = CBAD
      IF (GIN(I) .EQ. UNDEFI)  THEN
          CWD = CNONE
          LNONE = .TRUE.
          GF(LC) = 0.
          ENDIF
      IF (CWD .NE. CGOOD)  ISW(3) = 0
      WRITE (ISYSWR,99) I,CPNAM(I),GF(LC),GRD(LC),ERR,CWD
   99 FORMAT (7X,I5,2X ,A10,3E12.4,4X ,A4)
  100 CONTINUE
      IF (LNONE) WRITE (ISYSWR,'(A)')
     +  '  AGREEMENT=NONE  MEANS FCN DID NOT CALCULATE THE DERIVATIVE'
      IF (ISW(3) .EQ. 0)  WRITE (ISYSWR,1003)
 1003 FORMAT(/' MINUIT DOES NOT ACCEPT DERIVATIVE CALCULATIONS BY FCN'/
     + ' TO FORCE ACCEPTANCE, ENTER "SET GRAD    1"'/)
C
 2000 CONTINUE
      RETURN
      END
