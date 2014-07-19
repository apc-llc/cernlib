C     All external names are of the form SUxxxx.
C     Must link with block data ALDATA.
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/sslun.inc"
#include "isajet/sspar.inc"
#include "isajet/sstype.inc"
#include "isajet/sugmg.inc"
#include "isajet/sugxin.inc"
#include "isajet/sugpas.inc"
#include "isajet/sugnu.inc"
#include "isajet/isapw.inc"
      CHARACTER*80 FNAME
      REAL M0,MHF,A0,TANB,SGNMU,MT,XLAMGM,XMESGM,XN5GM,AMPL,XCMGV
      INTEGER NSTEP,IMODEL,INUSUG,IMODIN
      INTEGER K,NOUT,IALLOW,IITEST,J
      CHARACTER*40 VERSN,VISAJE
      PARAMETER (NOUT=33)
      INTEGER IDOUT(NOUT)
      CHARACTER*30 ISAPW2
      SAVE ISAPW2
C
      DATA IDOUT/
     $IDTP,ISGL,ISUPL,ISDNL,ISSTL,ISCHL,ISBT1,ISTP1,ISUPR,ISDNR,
     $ISSTR,ISCHR,ISBT2,ISTP2,ISEL,ISMUL,ISTAU1,ISNEL,ISNML,ISNTL,
     $ISER,ISMUR,ISTAU2,ISZ1,ISZ2,ISZ3,ISZ4,ISW1,ISW2,
     $ISHL,ISHH,ISHA,ISHC/
      DATA AMPL/2.4E18/
C          ISAPW2 is used to check whether ALDATA is loaded
      DATA ISAPW2/'ALDATA REQUIRED BY FORTRAN G,H'/
C
C          Initialize
C
      IF(ISAPW1.NE.ISAPW2) THEN
        PRINT*, ' ERROR: BLOCK DATA ALDATA HAS NOT BEEN LOADED.'
        PRINT*, ' ISAJET CANNOT RUN WITHOUT IT.'
        PRINT*, ' PLEASE READ THE FINE MANUAL FOR ISAJET.'
        STOP99
      ENDIF
C
      LOUT=1
      NSTEP=1000
      XNRIN(2)=1.E20
C
      PRINT*,'ENTER output file name (in single quotes):'
      READ*,FNAME
      OPEN(1,FILE=FNAME,STATUS='NEW',FORM='FORMATTED')
      PRINT*,'ENTER 1 for mSUGRA:'
      PRINT*,'ENTER 2 for mGMSB:'
      PRINT*,'ENTER 3 for non-universal SUGRA:'
      PRINT*,'ENTER 4 for SUGRA with truly unified gauge couplings:'
      PRINT*,'ENTER 5 for non-minimal GMSB:'
      PRINT*,'ENTER 6 for SUGRA+right-handed neutrino:'
      PRINT*,'ENTER 7 for anomaly-mediated SUSY breaking:'
      READ*,IMODIN
      IMODEL=IMODIN
      IF (IMODEL.EQ.4) THEN
        IAL3UN=1
        IMODEL=1
      END IF
      IF (IMODEL.EQ.1.OR.IMODEL.EQ.3.OR.IMODEL.EQ.6) THEN
        PRINT*,'ENTER M_0, M_(1/2), A_0, tan(beta), sgn(mu), M_t:'
        READ*,M0,MHF,A0,TANB,SGNMU,MT
        IF (IMODEL.EQ.6) THEN
          IMODEL=1
          PRINT*,' ENTER M(nu_3)[=0], M_Majorana, A_N, M(NRSS)'
          READ*,XNRIN(1),XNRIN(2),XNRIN(3),XNRIN(4)
          GO TO 15
        END IF
        IF (IMODEL.EQ.3) THEN
          IMODEL=1
10        PRINT*,' ENTER 1,...,5 for NUSUGx keyword; 0 to continue:'
          PRINT*,' NUSUG1 = GUT scale gaugino masses'
          PRINT*,' NUSUG2 = GUT scale A terms'
          PRINT*,' NUSUG3 = GUT scale Higgs masses'
          PRINT*,' NUSUG4 = GUT scale 1st/2nd generation masses'
          PRINT*,' NUSUG5 = GUT scale 3rd generation masses'
          PRINT*,' ENTER 6 to activate right-hand neutrino'
          PRINT*,' ENTER 7 to enter alternate high scale Q_max.ne.M_GUT'
          READ*,INUSUG
          IF (INUSUG.EQ.0) THEN
            GO TO 15
          ELSE IF (INUSUG.EQ.1) THEN
            PRINT*,'Enter GUT scale M_1, M_2, M_3:'
            READ*,XNUSUG(1),XNUSUG(2),XNUSUG(3)
            IF (XNUSUG(3).LE.0.) THEN
            PRINT*, ' NEGATIVE M_3 IS NOT ALLOWED'
            STOP 99
            END IF
          ELSE IF (INUSUG.EQ.2) THEN
            PRINT*,'Enter GUT scale A_t, A_b, A_tau:'
            READ*,XNUSUG(6),XNUSUG(5),XNUSUG(4)
          ELSE IF (INUSUG.EQ.3) THEN
            PRINT*,'Enter GUT scale m_Hd, m_Hu:'
            READ*,XNUSUG(7),XNUSUG(8)
          ELSE IF (INUSUG.EQ.4) THEN
            PRINT*,'Enter GUT scale M(ul), M(dr), M(ur), M(el), M(er):'
            READ*,XNUSUG(13),XNUSUG(11),XNUSUG(12),XNUSUG(10),XNUSUG(9)
          ELSE IF (INUSUG.EQ.5) THEN
            PRINT*,'Enter GUT scale M(tl), M(br), M(tr), M(Ll), M(Lr):'
            READ*,XNUSUG(18),XNUSUG(16),XNUSUG(17),XNUSUG(15),XNUSUG(14)
          ELSE IF (INUSUG.EQ.6) THEN
            PRINT*,' ENTER M(nu_3), M_Majorana, A_N, M(NRSS)'
            READ*,XNRIN(1),XNRIN(2),XNRIN(3),XNRIN(4)
          ELSE IF (INUSUG.EQ.7) THEN
            PRINT*,' ENTER Q_max high scale for SUSY BCs'
            READ*,XSUGIN(7)
          END IF
          GO TO 10
        END IF
      ELSE IF (IMODEL.EQ.2.OR.IMODEL.EQ.5) THEN
          PRINT*,'ENTER Lambda, M_mes, N_5, tan(beta), sgn(mu), ',
     $    'M_t, C_gv:'
          READ*,M0,MHF,A0,TANB,SGNMU,MT,XCMGV
          XGMIN(7)=XCMGV
          XGMIN(8)=1.
          AMGVSS=M0*MHF*XCMGV/SQRT(3.)/AMPL
          IF (IMODEL.EQ.5) THEN
            IMODEL=2
            PRINT*,'Rsl = factor multiplying gaugino masses at M_mes'
            PRINT*,'dmH_d^2, dmH_u^2 = Higgs mass**2 shifts at M_mes'
            PRINT*,'d_Y = mass**2 shifts proportional to Y at M_mes'
            PRINT*,'n5_1,n5_2,n5_3 = n5 values for U(1),SU(2),SU(3)'
            PRINT*,'ENTER Rsl, dmH_d^2, dmH_u^2, d_Y, n5_1, n5_2, n5_3'
            READ*,XGMIN(8),XGMIN(9),XGMIN(10),XGMIN(11),XGMIN(12),
     $      XGMIN(13),XGMIN(14)
            END IF
      ELSE IF (IMODEL.EQ.7) THEN
        PRINT*,'ENTER M_0, M_(3/2), tan(beta), sgn(mu), M_t:'
        READ*,M0,MHF,TANB,SGNMU,MT
        A0=0.
      ELSE
        PRINT*,'Invalid model choice.'
        STOP99
      END IF
C
C          Solve RG equations
C
15    CALL SUGRA(M0,MHF,A0,TANB,SGNMU,MT,IMODEL)
C
C          Print results
C
      VERSN=VISAJE()
      WRITE(LOUT,20) VERSN
20    FORMAT(' ',44('*')/' *',42X,'*'/
     $  ' * ',A40,' *'/
     $  ' *',42X,'*'/' ',44('*')/)
      IF (NOGOOD.EQ.1) THEN
        PRINT*, 'BAD POINT: TACHYONIC PARTICLES!'
        WRITE(LOUT,*) 'BAD POINT: TACHYONIC PARTICLES!'
      ELSE IF (NOGOOD.EQ.2) THEN
        PRINT*, 'BAD POINT: NO EW SYMMETRY BREAKING!'
        WRITE(LOUT,*) 'BAD POINT: NO EW SYMMETRY BREAKING!'
      ELSE IF (NOGOOD.EQ.3) THEN
        PRINT*, 'BAD POINT: M(H_P)^2<0!'
        WRITE(LOUT,*) 'BAD POINT: M(H_P)^2<0!'
      ELSE IF (NOGOOD.EQ.4) THEN
        PRINT*, 'BAD POINT: YUKAWA>10!'
        WRITE(LOUT,*) 'BAD POINT: YUKAWA>10!'
      ELSE IF (NOGOOD.EQ.5.AND.IMODEL.EQ.1) THEN
        PRINT*, 'SUGRA BAD POINT: Z1SS NOT LSP!'
        WRITE(LOUT,*) 'SUGRA BAD POINT: Z1SS NOT LSP!'
      ELSE IF (NOGOOD.EQ.7) THEN
        PRINT*, 'BAD POINT: XT EWSB BAD!'
        WRITE(LOUT,*) 'BAD POINT: XT EWSB BAD!'
      ELSE IF (NOGOOD.EQ.8) THEN
        PRINT*, 'BAD POINT: MHL^2<0!'
        WRITE(LOUT,*) 'BAD POINT: MHL^2<0!'
      END IF
      IF(NOGOOD.NE.0) STOP99
      IF(ITACHY.NE.0) THEN
        WRITE(LOUT,*) 'WARNING: TACHYONIC SLEPTONS AT GUT SCALE'
        WRITE(LOUT,*) '         POINT MAY BE INVALID'
      ENDIF
C
C          Print selected model and results
C
      IF(IMODIN.EQ.1) WRITE(LOUT,1001)
1001  FORMAT(//' Minimal supergravity (mSUGRA) model:'/)
      IF(IMODIN.EQ.2) WRITE(LOUT,1002)
1002  FORMAT(//' Minimal gauge mediated (GMSB) model:'/)
      IF(IMODIN.EQ.3) WRITE(LOUT,1003)
1003  FORMAT(//' Non-universal supergravity model:'/)
      IF(IMODIN.EQ.4) WRITE(LOUT,1004)
1004  FORMAT(//' Supergravity model with truly unified couplings:'/)
      IF(IMODIN.EQ.5) WRITE(LOUT,1005)
1005  FORMAT(//' Non-minimal gauge mediated (GMSB) model:'/)
      IF(IMODIN.EQ.6) WRITE(LOUT,1006)
1006  FORMAT(//' Supergravity model with right-handed neutrinos:'/)
      IF(IMODIN.EQ.7) WRITE(LOUT,1006)
1007  FORMAT(//' Anomaly-mediated SUSY breaking model:'/)
      CALL SUGPRT(IMODEL,IMODIN)
C
C          Calculate all masses and decay modes
C
        CALL SSMSSM(XISAIN(1),XISAIN(2),XISAIN(3),
     $ XISAIN(4),XISAIN(5),XISAIN(6),XISAIN(7),XISAIN(8),XISAIN(9),
     $ XISAIN(10),XISAIN(11),XISAIN(12),XISAIN(13),XISAIN(14),
     $ XISAIN(15),XISAIN(16),XISAIN(17),XISAIN(18),XISAIN(19),
     $ XISAIN(20),XISAIN(21),XISAIN(22),XISAIN(23),XISAIN(24),
     $ MT,IALLOW,IMODEL)
C
C          Test parameters
C
      IF(IALLOW.NE.0) THEN
        WRITE(LOUT,2001)
2001    FORMAT(//' MSSM WARNING: Z1SS IS NOT LSP')
      ENDIF
C
      CALL SSTEST(IALLOW)
      IITEST=IALLOW/2
      IF(MOD(IITEST,2).NE.0) THEN
        WRITE(LOUT,2002)
2002    FORMAT(' MSSM WARNING: Z -> Z1SS Z1SS EXCEEDS BOUND')
      ENDIF
      IITEST=IITEST/2
      IF(MOD(IITEST,2).NE.0) THEN
        WRITE(LOUT,2004)
2004    FORMAT(' MSSM WARNING: Z -> CHARGINOS ALLOWED')
      ENDIF
      IITEST=IITEST/2
      IF(MOD(IITEST,2).NE.0) THEN
        WRITE(LOUT,2008)
2008    FORMAT(' MSSM WARNING: Z -> Z1SS Z2SS TOO BIG')
      ENDIF
      IITEST=IITEST/2
      IF(MOD(IITEST,2).NE.0) THEN
        WRITE(LOUT,2016)
2016    FORMAT(' MSSM WARNING: Z -> SQUARKS, SLEPTONS ALLOWED')
      ENDIF
      IITEST=IITEST/2
      IF(MOD(IITEST,2).NE.0) THEN
        WRITE(LOUT,2032)
2032    FORMAT(' MSSM WARNING: Z -> Z* HL0 EXCEEDS BOUND')
      ENDIF
      IITEST=IITEST/2
      IF(MOD(IITEST,2).NE.0) THEN
        WRITE(LOUT,2064)
2064    FORMAT(' MSSM WARNING: Z -> HL0 HA0 ALLOWED')
      ENDIF
C
      WRITE(LOUT,3600)
3600  FORMAT(//' ISASUSY decay modes:'/
     $' Parent --> daughters',18X,'Width',10X,'Branching ratio'/)
C          Write all modes
      DO 200 J=1,NOUT
        CALL SSPRT(IDOUT(J))
200   CONTINUE
C
      STOP
      END
