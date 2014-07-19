*
* $Id: pjcone.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: pjcone.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE PJCONE(LPJHDI,DRCUT,ETCUT,MXPASS,IR,MUON,NPJ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : COMBINES PARTONS INTO PARTON JETS
C-
C-   Inputs  : LPJHD, DRCUT, ETCUT, MXPASS,IR,MUON
C-   Outputs : NPJ
C-   Controls: None
C-
C-             LPJHD  - Address of PJHD bank to hang PJET from.
C-             DRCUT  - dR=sqrt(dETA**2+dPHI**2) cut around Leading Partons.
C-             ETCUT  - Transverse Energy cut ( minimum for defining a JET ).
C-             MXPASS - Maximum number of iterations.
C-             IR     - Initial Radiation switch: throw out if IR.NE.0.
C-             MUON   - MUON switch: throw out if MUON.EQ.0.
C-             NPJ    - No. of Parton Jets found.
C-
C-   Created  27-OCT-1989   Boaz Klima
C-   Updated  15-DEC-1989   Boaz Klima   :  FORTRAN77 compatible
C-   Updated  15-JAN-1990   Harrison B. Prosper
C-      Removed declarations of LISAE etc. (already in LKPJET.INC)
C-   Updated  19-JAN-1990   Boaz Klima
C-      Add call to PJPTFL ( reference link to ISAQ )
C-   Updated  25-JAN-1990   Harrison B. Prosper
C-      Increased NPMAX from 50 to 100
C-   Updated  16-MAY-1990   Chip Stewart, Boaz Klima
C-      Added MUON switch
C-   Updated  22-MAY-1990   Boaz Klima
C-      NOT using Phi and Theta from ISAQ
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "pi.inc"
#include "zebcom.inc"
#include "lkpjet.inc"
C
      INTEGER IER
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      CHARACTER*8 NAME,LABEL
      INTEGER NPMAX, MXPASS, IR, MUON
      PARAMETER (NPMAX=100)
      REAL    SUM, DIF
      INTEGER NP, JP, J, JO, JOP1, JOP2, JP1, JP2, ISKP, IP, IPN, IJ
      REAL    PTN, PTO, PTNO, PTNOR, X1, Y1, PHI1, PHI2
      REAL    DETA, DPHI, DR, DRCUT, ETCUT
      INTEGER JIORD(NPMAX), JDORD(NPMAX), JCNN(NPMAX,NPMAX)
      INTEGER JSKP(NPMAX)
      INTEGER NJET, LJETS, I, JJ, INFI, IDABS
      INTEGER GZJETS
      INTEGER GZISAJ, GZISAQ
      REAL    P_PX(NPMAX),P_PY(NPMAX),P_PZ(NPMAX), P_E(NPMAX)
      REAL    P_MS(NPMAX), P_PHI(NPMAX), P_THE(NPMAX), P_ETA(NPMAX)
      REAL    P_P(NPMAX), P_PT(NPMAX), PDM_PT(NPMAX)
      INTEGER NPJ
      REAL    PJ_PX(NPMAX), PJ_PY(NPMAX), PJ_PZ(NPMAX), PJ_E(NPMAX)
      REAL    PJ_MS(NPMAX), PJ_PHI(NPMAX), PJ_THE(NPMAX)
      REAL    PJ_ETA(NPMAX), PJ_P(NPMAX), PJ_PT(NPMAX)
      INTEGER PJ_MPI(NPMAX,NPMAX)    ! PARTON JET MAPING  PJET --> ISAQ
      INTEGER PJ_ID(NPMAX)          ! PARTON LINKS TO ISAQ
      INTEGER PJ_NP(NPMAX)               ! NUMBER OF PARTONS IN PARTON JET
      REAL    EPS
      PARAMETER( EPS = 1.0E-5 )
C ZEBRA POINTERS
      INTEGER GZPJHD,LPJHDI
C----------------------------------------------------------------------
C
      LPJHD = LPJHDI
      IF(LPJHD.EQ.0) LPJHD = GZPJHD()    ! GET LINK to PJHD header bank
C
      IF(LPJHD.EQ.0) CALL BKPJHD(LPJHD)
C
C ***  BOOK THE BANK IF ARGUMENT = 0.
C
C ... GET ISAJET BANKS INFO
C
      LISAJ=GZISAJ()
   10 CONTINUE
      LISAJ = LQ( LISAJ )
      IF( LISAJ .NE. 0 ) GOTO 10
      LISAQ=GZISAQ()
      NP = 0
   20 CONTINUE
C
C ****  NO NEUTRINOS ( OR MUONS if muon switch is set to 0) !
C
      IDABS=IABS(IQ(LISAQ+1))
      IF(IDABS.EQ.11.OR.IDABS.EQ.13.OR.IDABS.EQ.15)
     &  GOTO 30
      IF(MUON.EQ.0  .AND. IDABS.EQ.14) GOTO 30
C
      NP = NP + 1
      INFI = 2
C
C ****  WANT TO INCLUDE INITIAL RADIATION PARTONS ?
C
      IF ( IR.NE.0 ) INFI = LQ(LISAQ-1)
C
      IF ( INFI.NE.0 ) THEN
        P_PX(NP)  = Q(LISAQ+ 2)
        P_PY(NP)  = Q(LISAQ+ 3)
        P_PZ(NP)  = Q(LISAQ+ 4)
        P_E(NP)   = Q(LISAQ+ 5)
        P_MS(NP)  = Q(LISAQ+ 6)
        P_PT(NP)  = SQRT( P_PX(NP)**2+P_PY(NP)**2 )
        P_P(NP)   = SQRT( P_PT(NP)**2+P_PZ(NP)**2 )
        P_PHI(NP) = ATAN2 (P_PY(NP),P_PX(NP)+EPS)
        IF(P_PHI(NP).LT.0.)P_PHI(NP)=P_PHI(NP)+TWOPI
        P_THE(NP) = ATAN2 (P_PT(NP),P_PZ(NP)+EPS)
        P_ETA(NP) = -ALOG ( TAN(P_THE(NP)/2.) + EPS )
      ELSE
        NP = NP - 1
      ENDIF
   30 LISAQ = LQ( LISAQ )
      IF( LISAQ .NE. 0 ) GOTO 20
C
C ... ORDER PARTONS IN PT
C
      DO 100 JP = 1 , NP
        JIORD(JP) = JP
  100 PDM_PT(JP)=P_PT(JP)
      CALL ISASRT(PDM_PT(1),NP,JIORD)
      DO 105 JP = 1 , NP
  105 JDORD(JP) = JIORD(NP-JP+1)
C
C ... COMBINE PARTONS CLOSE IN R SPACE
C
      DO 110 J = 1 , NP
        JO=JDORD(J)
  110 JCNN(JO,1)=0
      ISKP=0
      DO 120 JP1 = 1 , NP-1
        JOP1=JDORD(JP1)
C ... CHECK IF PARTON ALREADY CONNECTED TO OTHER ONE
        IF ( JCNN(JOP1,1).EQ.-1 ) GOTO 120
        DO 130 JP2 = JP1+1 , NP
          JOP2=JDORD(JP2)
C ... CHECK IF PARTON ALREADY CONNECTED TO OTHER ONE
          IF ( JCNN(JOP2,1).EQ.-1 ) GOTO 130
          DETA = P_ETA(JOP1) - P_ETA(JOP2)
          PHI1 = P_PHI(JOP1)
          PHI2 = P_PHI(JOP2)
          X1 = COS(PHI2-PHI1)
          Y1 = SIN(PHI2-PHI1)
          IF(X1.EQ.0.0) THEN
            DPHI = HALFPI
          ELSE
            DPHI = ATAN2(Y1,X1)
          END IF
          DR = SQRT(DETA**2+DPHI**2)
C --- CRITERION FOR COMBINING PARTONS
          IF ( DR.LT.DRCUT ) THEN
            JCNN(JOP1,1)=JCNN(JOP1,1)+1
            JCNN(JOP2,1)=-1
            JCNN(JOP1,JCNN(JOP1,1)+1)=JOP2
            ISKP=ISKP+JCNN(JOP1,1)
            JSKP(ISKP)=JOP2
          ELSE
            GOTO 130
          ENDIF
  130   CONTINUE
  120 CONTINUE
C
C ... BOOKKEEPING FOR PARTON JETS
C
      DO 150 IP = 1 , NPJ
        PJ_PX(IP) =0.
        PJ_PY(IP) =0.
        PJ_PZ(IP) =0.
        PJ_E(IP)  =0.
        PJ_MS(IP) =0.
        PJ_PHI(IP)=0.
        PJ_THE(IP)=0.
        PJ_ETA(IP)=0.
        PJ_P(IP)  =0.
        PJ_PT(IP) =0.
  150 CONTINUE
      NPJ=0
      DO 200 JP1 = 1 , NP
        JOP1=JDORD(JP1)
C ... ALREADY CONNECTED, SINGLE PARTON JET, OR HAS OTHERS TO CONNECT TO
        IF ( JCNN(JOP1,1).GE.0 ) THEN
          NPJ=NPJ+1
          PJ_NP(NPJ)=1
          PJ_MPI(PJ_NP(NPJ),NPJ)=JOP1
          PJ_PX(NPJ)  = P_PX(JOP1)
          PJ_PY(NPJ)  = P_PY(JOP1)
          PJ_PZ(NPJ)  = P_PZ(JOP1)
          PJ_E(NPJ)   = P_E(JOP1)
          PJ_MS(NPJ)  = P_MS(JOP1)
          PJ_PHI(NPJ) = P_PHI(JOP1)
          PJ_THE(NPJ) = P_THE(JOP1)
          PJ_ETA(NPJ) = P_ETA(JOP1)
          PJ_P(NPJ)   = P_P(JOP1)
          PJ_PT(NPJ) = P_PT(JOP1)
          IF ( JCNN(JOP1,1).EQ.0 ) GOTO 205
          DO 210 JJ = 1 , JCNN(JOP1,1)
            PJ_NP(NPJ)=PJ_NP(NPJ)+1
            PJ_MPI(PJ_NP(NPJ),NPJ)=JCNN(JOP1,JJ+1)
            PJ_E(NPJ)  = PJ_E(NPJ)  + P_E(JCNN(JOP1,JJ+1))
            PJ_PX(NPJ) = PJ_PX(NPJ) + P_PX(JCNN(JOP1,JJ+1))
            PJ_PY(NPJ) = PJ_PY(NPJ) + P_PY(JCNN(JOP1,JJ+1))
            PJ_PZ(NPJ) = PJ_PZ(NPJ) + P_PZ(JCNN(JOP1,JJ+1))
  210     CONTINUE
          PJ_PT(NPJ) = SQRT( PJ_PX(NPJ)**2 + PJ_PY(NPJ)**2 )
          PJ_P(NPJ) = SQRT( PJ_PT(NPJ)**2 + PJ_PZ(NPJ)**2 )
          PJ_PHI(NPJ) = ATAN2 (PJ_PY(NPJ),PJ_PX(NPJ)+EPS)
          IF(PJ_PHI(NPJ).LT.0.)PJ_PHI(NPJ)=PJ_PHI(NPJ)+TWOPI
          PJ_THE(NPJ) = ATAN2 (PJ_PT(NPJ),PJ_PZ(NPJ)+EPS)
          PJ_ETA(NPJ) = -ALOG ( TAN(PJ_THE(NPJ)/2.) + EPS )
          SUM = PJ_E(NPJ) + PJ_P(NPJ)
          DIF = PJ_E(NPJ) - PJ_P(NPJ)
          IF ( DIF.LT.0. ) DIF = 0.
          PJ_MS(NPJ) = SQRT( SUM*DIF )
C --- CRITERION FOR DROPPING A PARTON JET ( ET < ETCUT )
  205     IF ( PJ_PT(NPJ).GT.ETCUT ) GOTO 200
          NPJ=NPJ-1
        ENDIF
  200 CONTINUE
C
C ****  FILL PJ_ID WITH ISAQ LINKS
C
      LISAQ=GZISAQ()
      NP = 0
  300 CONTINUE
      IDABS=IABS(IQ(LISAQ+1))
      IF(IDABS.EQ.11.OR.IDABS.EQ.13.OR.IDABS.EQ.15)
     &  GOTO 330
      IF (MUON.EQ.0 .AND. IDABS.EQ.14) GOTO 330
C
      NP = NP + 1
      INFI = 2
      IF ( IR.NE.0 ) INFI = LQ(LISAQ-1)
      IF ( INFI.NE.0 ) THEN
        PJ_ID(NP) = IQ(LISAQ-5)        ! Save numeric bank Id
      ELSE
        NP = NP - 1
      ENDIF
  330 LISAQ = LQ( LISAQ )
      IF( LISAQ .NE. 0 ) GOTO 300
      DO IJ = 1, NPJ
C
C ****  BOOK AND FILL JET BANK FOR THIS JET
C
        CALL BKPJET(LPJHD,LPJET)
        Q(LPJET+2) = PJ_PT(IJ)
        Q(LPJET+3) = PJ_PX(IJ)
        Q(LPJET+4) = PJ_PY(IJ)
        Q(LPJET+5) = PJ_PZ(IJ)
        Q(LPJET+6) = PJ_E(IJ)
        Q(LPJET+7) = PJ_MS(IJ)
        Q(LPJET+8) = PJ_PHI(IJ)
        Q(LPJET+9)  = PJ_THE(IJ)
        Q(LPJET+10) = PJ_ETA(IJ)
C
C ****  BOOK AND FILL JET POINTER BANK FOR THIS  PJET
C
        CALL PJPTFL (PJ_ID,PJ_MPI(1,IJ),NPMAX,PJ_NP(IJ))
      ENDDO

C-
  999 RETURN
      END
