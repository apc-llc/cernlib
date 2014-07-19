*
* $Id: mnmigr.F,v 1.2 1996/03/15 18:02:49 james Exp $
*
* $Log: mnmigr.F,v $
* Revision 1.2  1996/03/15 18:02:49  james
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
      SUBROUTINE MNMIGR(FCN,FUTIL)
#include "minuit/d506dp.inc"
CC        Performs a local function minimization using basically the
CC        method of Davidon-Fletcher-Powell as modified by Fletcher
CC        ref. -- Fletcher, Comp.J. 13,317 (1970)   "switching method"
CC
#include "minuit/d506cm.inc"
      EXTERNAL FCN,FUTIL
      DIMENSION GS(MNI), STEP(MNI),  XXS(MNI), FLNU(MNI), VG(MNI)
      LOGICAL LDEBUG
      PARAMETER (TOLER=0.05)
      IF (NPAR .LE. 0)  RETURN
      IF (AMIN .EQ. UNDEFI)  CALL MNAMIN(FCN,FUTIL)
      LDEBUG = (IDBG(4) .GE. 1)
      CFROM = 'MIGRAD  '
      NFCNFR = NFCN
      NFCNMG = NFCN
      CSTATU= 'INITIATE  '
      ISWTR = ISW(5) - 2*ITAUR
      NPFN = NFCN
      NPARX = NPAR
      LENV = NPAR*(NPAR+1)/2
      NRSTRT = 0
      NPSDF = 0
      LINED2 = 0
      ISW(4) = -1
      RHOTOL = 1.0E-3*APSI
      IF (ISWTR .GE. 1)  WRITE (ISYSWR,470) ISTRAT,RHOTOL
  470 FORMAT (' START MIGRAD MINIMIZATION.  STRATEGY',I2,
     +'.  CONVERGENCE WHEN EDM .LT.',E9.2)
C                                           initialization strategy
      IF (ISTRAT.LT.2 .OR. ISW(2).GE.3)  GO TO 2
C                                come (back) here to restart completely
    1 CONTINUE
      IF (NRSTRT .GT. ISTRAT)  THEN
         CSTATU= 'FAILED    '
         ISW(4) = -1
         GO TO 230
         ENDIF
C                                      . get full covariance and gradient
      CALL MNHESS(FCN,FUTIL)
      CALL MNWERR
      NPSDF = 0
      IF (ISW(2) .GE. 1)  GO TO 10
C                                        . get gradient at start point
    2 CONTINUE
      CALL MNINEX(X)
      IF (ISW(3) .EQ. 1) THEN
          CALL FCN(NPARX,GIN,FZERO,U,2,FUTIL)
          NFCN = NFCN + 1
      ENDIF
      CALL MNDERI(FCN,FUTIL)
      IF (ISW(2) .GE. 1)  GO TO 10
C                                   sometimes start with diagonal matrix
      DO 3 I= 1, NPAR
         XXS(I) = X(I)
         STEP(I) = ZERO
    3 CONTINUE
C                           do line search if second derivative negative
      LINED2 = LINED2 + 1
      IF (LINED2 .LT. (ISTRAT+1)*NPAR) THEN
      DO 5 I= 1, NPAR
         IF (G2(I) .GT. ZERO)  GO TO 5
         STEP(I) = -SIGN(GSTEP(I),GRD(I))
         GDEL = STEP(I)*GRD(I)
         FS = AMIN
         CALL MNLINE(FCN,XXS,FS,STEP,GDEL,TOLER,FUTIL)
         CALL MNWARN('D','MNMIGR','Negative G2 line search')
         IEXT = NEXOFI(I)
         IF (LDEBUG) WRITE (ISYSWR,'(A,I3,2G13.3)')
     +    ' Negative G2 line search, param ',IEXT,FS,AMIN
         GO TO 2
    5 CONTINUE
      ENDIF
C                           make diagonal error matrix
      DO 8 I=1,NPAR
         NDEX = I*(I-1)/2
           DO 7 J=1,I-1
           NDEX = NDEX + 1
    7      VHMAT(NDEX) = 0.
         NDEX = NDEX + 1
         IF (G2(I) .LE. ZERO)  G2(I) = 1.
         VHMAT(NDEX) = 2./G2(I)
    8 CONTINUE
      DCOVAR = 1.
      IF (LDEBUG) WRITE (ISYSWR,'(A,A/(1X,10G10.2))') ' DEBUG MNMIGR,',
     +  ' STARTING MATRIX DIAGONAL,  VHMAT=', (VHMAT(KK),KK=1,LENV)
C                                         ready to start first iteration
   10 CONTINUE
      NRSTRT = NRSTRT + 1
      IF (NRSTRT .GT. ISTRAT+1)  THEN
         CSTATU= 'FAILED    '
         GO TO 230
         ENDIF
      FS = AMIN
C                                        . . . get EDM and set up loop
      EDM = 0.
         DO 18 I= 1, NPAR
         GS(I) = GRD(I)
         XXS(I) = X(I)
         NDEX = I*(I-1)/2
           DO 17 J= 1, I-1
           NDEX = NDEX + 1
   17      EDM = EDM + GS(I)*VHMAT(NDEX)*GS(J)
         NDEX = NDEX + 1
   18    EDM = EDM + 0.5 * GS(I)**2 *VHMAT(NDEX)
      EDM = EDM * 0.5 * (1.0+3.0*DCOVAR)
        IF (EDM .LT. ZERO)  THEN
        CALL MNWARN('W','MIGRAD','STARTING MATRIX NOT POS-DEFINITE.')
        ISW(2) = 0
        DCOVAR = 1.
        GO TO 2
        ENDIF
      IF (ISW(2) .EQ. 0)  EDM=BIGEDM
      ITER = 0
      CALL MNINEX(X)
      CALL MNWERR
      IF (ISWTR .GE. 1)  CALL MNPRIN(3,AMIN)
      IF (ISWTR .GE. 2)  CALL MNMATU(0)
C                                        . . . . .  start main loop
   24 CONTINUE
      IF (NFCN-NPFN .GE. NFCNMX)  GO TO 190
      GDEL = 0.
      GSSQ = 0.
         DO 30  I=1,NPAR
         RI = 0.
         GSSQ = GSSQ + GS(I)**2
           DO 25 J=1,NPAR
           M = MAX(I,J)
           N = MIN(I,J)
           NDEX = M*(M-1)/2 + N
   25      RI = RI + VHMAT(NDEX) *GS(J)
         STEP(I) = -0.5*RI
   30    GDEL = GDEL + STEP(I)*GS(I)
      IF (GSSQ .EQ. ZERO)  THEN
          CALL MNWARN('D','MIGRAD',
     +             ' FIRST DERIVATIVES OF FCN ARE ALL ZERO')
          GO TO 300
      ENDIF
C                 if gdel positive, V not posdef
      IF (GDEL .GE. ZERO)  THEN
         CALL MNWARN('D','MIGRAD',' NEWTON STEP NOT DESCENT.')
         IF (NPSDF .EQ. 1)  GO TO 1
         CALL MNPSDF
         NPSDF = 1
         GO TO 24
         ENDIF
C                                        . . . . do line search
      CALL MNLINE(FCN,XXS,FS,STEP,GDEL,TOLER,FUTIL)
      IF (AMIN .EQ. FS) GO TO 200
      CFROM  = 'MIGRAD  '
      NFCNFR = NFCNMG
      CSTATU= 'PROGRESS  '
C                                        . get gradient at new point
      CALL MNINEX(X)
      IF (ISW(3) .EQ. 1) THEN
          CALL FCN(NPARX,GIN,FZERO,U,2,FUTIL)
          NFCN = NFCN + 1
      ENDIF
      CALL MNDERI(FCN,FUTIL)
C                                         . calculate new EDM
      NPSDF = 0
   81 EDM = 0.
      GVG = 0.
      DELGAM = 0.
      GDGSSQ = 0.
         DO 100 I= 1, NPAR
         RI = 0.
         VGI = 0.
           DO 90 J= 1, NPAR
           M = MAX(I,J)
           N = MIN(I,J)
           NDEX = M*(M-1)/2 + N
           VGI = VGI + VHMAT(NDEX)*(GRD(J)-GS(J))
   90      RI  =  RI + VHMAT(NDEX)* GRD(J)
      VG(I) = VGI*0.5
      GAMI = GRD(I) - GS(I)
      GDGSSQ = GDGSSQ + GAMI**2
      GVG = GVG + GAMI*VG(I)
      DELGAM = DELGAM + DIRIN(I)*GAMI
  100 EDM = EDM + GRD(I)*RI*0.5
      EDM = EDM * 0.5 * (1.0 + 3.0*DCOVAR)
C                          . if EDM negative,  not positive-definite
      IF (EDM .LT. ZERO .OR. GVG .LE. ZERO)  THEN
         CALL MNWARN('D','MIGRAD','NOT POS-DEF. EDM OR GVG NEGATIVE.')
         CSTATU = 'NOT POSDEF'
         IF (NPSDF .EQ. 1)  GO TO 230
         CALL MNPSDF
         NPSDF = 1
         GO TO 81
      ENDIF
C                            print information about this iteration
      ITER = ITER + 1
      IF (ISWTR.GE.3 .OR. (ISWTR.EQ.2.AND.MOD(ITER,10).EQ.1)) THEN
         CALL MNWERR
         CALL MNPRIN(3,AMIN)
      ENDIF
      IF (GDGSSQ .EQ. ZERO)  CALL MNWARN('D','MIGRAD',
     +           'NO CHANGE IN FIRST DERIVATIVES OVER LAST STEP')
      IF (DELGAM .LT. ZERO) CALL MNWARN('D','MIGRAD',
     +          'FIRST DERIVATIVES INCREASING ALONG SEARCH LINE')
C                                        .  update covariance matrix
      CSTATU = 'IMPROVEMNT'
        IF (LDEBUG) WRITE (ISYSWR,'(A,(1X,10G10.3))') ' VHMAT 1 =',
     +             (VHMAT(KK),KK=1,10)
      DSUM = 0.
      VSUM = 0.
         DO  120  I=1, NPAR
           DO  120  J=1, I
           D = DIRIN(I)*DIRIN(J)/DELGAM - VG(I)*VG(J)/GVG
           DSUM = DSUM + ABS(D)
           NDEX = I*(I-1)/2 + J
           VHMAT(NDEX) = VHMAT(NDEX) + 2.0*D
           VSUM = VSUM + ABS(VHMAT(NDEX))
  120      CONTINUE
C                smooth local fluctuations by averaging DCOVAR
      DCOVAR = 0.5*(DCOVAR + DSUM/VSUM)
      IF (ISWTR.GE.3 .OR. LDEBUG) WRITE (ISYSWR,'(A,F5.1,A)')
     +      ' RELATIVE CHANGE IN COV. MATRIX=',DCOVAR*100.,'%'
      IF (LDEBUG) WRITE (ISYSWR,'(A,(1X,10G10.3))') ' VHMAT 2 =',
     +             (VHMAT(KK),KK=1,10)
      IF (DELGAM .LE. GVG)  GO TO 135
      DO 125 I= 1, NPAR
  125 FLNU(I) = DIRIN(I)/DELGAM - VG(I)/GVG
      DO 130 I= 1, NPAR
      DO 130 J= 1, I
      NDEX = I*(I-1)/2 + J
  130 VHMAT(NDEX) = VHMAT(NDEX) + 2.0*GVG*FLNU(I)*FLNU(J)
  135 CONTINUE
C                                              and see if converged
      IF (EDM .LT. 0.1*RHOTOL)  GO TO 300
C                                    if not, prepare next iteration
      DO 140 I= 1, NPAR
      XXS(I) = X(I)
      GS(I) = GRD(I)
  140 CONTINUE
      FS = AMIN
      IF (ISW(2) .EQ. 0  .AND. DCOVAR.LT. 0.5 )  ISW(2) = 1
      IF (ISW(2) .EQ. 3  .AND. DCOVAR.GT. 0.1 )  ISW(2) = 1
      IF (ISW(2) .EQ. 1  .AND. DCOVAR.LT. 0.05)  ISW(2) = 3
      GO TO 24
C                                        . . . . .  end main loop
C                                         . . call limit in MNMIGR
  190 ISW(1) = 1
      IF (ISW(5) .GE. 0)
     +     WRITE (ISYSWR,'(A)')  ' CALL LIMIT EXCEEDED IN MIGRAD.'
      CSTATU = 'CALL LIMIT'
      GO TO 230
C                                         . . fails to improve . .
  200 IF (ISWTR .GE. 1)  WRITE (ISYSWR,'(A)')
     +           ' MIGRAD FAILS TO FIND IMPROVEMENT'
      DO 210 I= 1, NPAR
  210 X(I) = XXS(I)
      IF (EDM .LT. RHOTOL)  GO TO 300
      IF (EDM .LT. ABS(EPSMA2*AMIN))  THEN
         IF (ISWTR .GE. 0)  WRITE (ISYSWR, '(A)')
     +      ' MACHINE ACCURACY LIMITS FURTHER IMPROVEMENT.'
         GO TO 300
         ENDIF
      IF (ISTRAT .LT. 1)  THEN
         IF (ISW(5) .GE. 0) WRITE (ISYSWR, '(A)')
     +    ' MIGRAD FAILS WITH STRATEGY=0.   WILL TRY WITH STRATEGY=1.'
         ISTRAT = 1
      ENDIF
         GO TO 1
C                                         . . fails to converge
  230 IF (ISWTR .GE. 0)  WRITE (ISYSWR,'(A)')
     +    ' MIGRAD TERMINATED WITHOUT CONVERGENCE.'
      IF (ISW(2) .EQ. 3)  ISW(2) = 1
      ISW(4) = -1
      GO TO 400
C                                         . . apparent convergence
  300 IF (ISWTR .GE. 0) WRITE(ISYSWR,'(/A)')
     +   ' MIGRAD MINIMIZATION HAS CONVERGED.'
      IF (ITAUR .EQ. 0) THEN
        IF (ISTRAT .GE. 2 .OR. (ISTRAT.EQ.1.AND.ISW(2).LT.3)) THEN
           IF (ISW(5) .GE. 0)  WRITE (ISYSWR, '(/A)')
     +      ' MIGRAD WILL VERIFY CONVERGENCE AND ERROR MATRIX.'
           CALL MNHESS(FCN,FUTIL)
           CALL MNWERR
           NPSDF = 0
           IF (EDM .GT. RHOTOL) GO TO 10
        ENDIF
      ENDIF
      CSTATU='CONVERGED '
      ISW(4) = 1
C                                           come here in any case
  400 CONTINUE
      CFROM = 'MIGRAD  '
      NFCNFR = NFCNMG
      CALL  MNINEX(X)
      CALL MNWERR
      IF (ISWTR .GE. 0)  CALL MNPRIN (3,AMIN)
      IF (ISWTR .GE. 1)  CALL MNMATU(1)
      RETURN
      END
