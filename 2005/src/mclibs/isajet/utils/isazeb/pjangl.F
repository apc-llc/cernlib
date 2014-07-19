*
* $Id: pjangl.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: pjangl.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE PJANGL(LPJHDI,OP_CUT,ET_CUT,MAX_IT,IR,MUON,NJET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determine Jets from Partons using opening
C-     angle cone algorithm. Boost into parton rest frame - then sort jets.
C-
C-   Inputs  : LPJHD          ...  Address of PJHD bank to hang PJET from
C-             OP_CUT         ...  opening angle in Radians to look for partons
C-             ET_CUT         ...  Et at which a jet doesn't count
C-             MAX_IT         ...  Maximum number of iteration around jet axis
C-             IR             ...  DONT use Initial Radiation then set IR.NE.0
C-             MUON           ...  MUON switch  - 0=no MUONS  - 1=use MUONS
C-   Outputs : NJET - number of PJETs found - PJET bank
C-   Controls: none
C-
C-   Created   5-NOV-1989   Chip Stewart
C-   Updated  15-JAN-1990   Harrison B. Prosper
C-      Removed declarations of LISAE etc.
C-   Updated  25-JAN-1990   Harrison B. Prosper
C-      Increase NPMAX from 50 t0 100
C-   Updated  16-MAY-1990   Chip Stewart  Boaz Klima
C-      Added MUON switch
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
C INPUT
      REAL    OP_CUT,ET_CUT
      REAL    OPANGL
      INTEGER MAX_IT,IR,MUON
C INPUT BANK POINTERS
      INTEGER LSUP,GZISAQ
C PARTON MOMENTA
      INTEGER NP,NPMAX
      PARAMETER (NPMAX=100)
      REAL    P(4),EM,PM
      REAL    P_LAB(4,NPMAX),P_CM(4,NPMAX),P_TMP(4,NPMAX)
      REAL    PBOOST(4),P_CHK(4)
      REAL    PT(NPMAX),P_AXIS(4),P_JET(4,NPMAX),PT_JET(NPMAX)
C PJET MOMENTA AND MAPPING
      REAL PJET(4,NPMAX)                 ! PARTON JET 4 MOMENTA
      INTEGER MAPINV(NPMAX,NPMAX)    ! PARTON JET MAPING  PJET --> ISAQ
      INTEGER NQ(NPMAX)               ! PARTON JET NUMBER OF PARTNONS /JET
C BOOKEEPING ARRAYS, POINTERS
      INTEGER ORDER(NPMAX),JETMAP(NPMAX),SEEDMP(NPMAX)
      LOGICAL SKPMAP(NPMAX)
      INTEGER PJ_ID(NPMAX)
      INTEGER ID,I,IP,IJ,JP,IDABS
      INTEGER NJET,IPASS
      LOGICAL LX,LY,LZ
C OBLIGATORY ETA PHI ETC...
      REAL    ETA,PHI,TH
      REAL    ETMAX
      CHARACTER NAME*8,LABEL*8
C ZEBRA INCLUDES
#include "zebcom.inc"
#include "lkpjet.inc"
#include "izisaq.inc"
#include "itapes.inc"
C ZEBRA POINTERS
      INTEGER GZPJHD,LPJHDI
C----------------------------------------------------------------------
C
      LPJHD = LPJHDI
      IF(LPJHD.EQ.0) LPJHD = GZPJHD()    ! GET LINK to 1st PJHD if LPJHD zero
C
      IF(LPJHD.EQ.0) CALL BKPJHD(LPJHD)  ! If no PJHD bank then Book 1st PJHD
C
C ****  add up parton momenta from ISAQ
C
      LSUP = LISAE-IZISAQ   ! find pointer to first bank
      NP = 0
      CALL VZERO(PBOOST(1),4)
      CALL VZERO(SKPMAP(1),NPMAX)
   10 CALL GTISAQ(LSUP,LISAQ,ID,P,PHI,TH,ETA)
      IF( LISAQ .GT. 0 ) THEN
        NP = NP + 1
C
C ****  NO NEUTRINOS !
C
        IDABS=IABS(IQ(LISAQ+1))
        IF(IDABS.EQ.11.OR.IDABS.EQ.13.OR.IDABS.EQ.15)SKPMAP(NP)=.TRUE.
        IF(IDABS.EQ.14.AND.MUON.EQ.0) SKPMAP(NP) = .TRUE.
C
        IF (IR.NE.0 .AND. LQ(LISAQ-1).EQ.0) SKPMAP(NP) = .TRUE.
        IF (.NOT. SKPMAP(NP) ) THEN
          CALL UCOPY(P(1),P_LAB(1,NP),4)
          DO I = 1, 4
            PBOOST(I) = PBOOST(I) + P(I)
          END DO
        END IF
        LSUP = LISAQ
        GOTO 10
      END IF
      CALL VZERO(SEEDMP(1),NP)
      CALL VZERO(JETMAP,NP)            ! zero any jet pointers
C
C ****  BOOST TO PARTON REST FRAME
C
      DO 90 IP = 1, NP
        ORDER(IP) = IP
        IF ( SKPMAP(IP) ) THEN
          PT(IP) = 0
          GOTO 90
        END IF
C        CALL LBOOST (PBOOST ,1 ,P_LAB(1,IP),P_CM(1,IP) )
        CALL LOREN4 (PBOOST ,P_LAB(1,IP),P_CM(1,IP) )
        PT(IP) = -1.0 * SQRT ( P_CM(1,IP)*P_CM(1,IP)
     &    + P_CM(2,IP)*P_CM(2,IP) )
C ****** CHECK BOOST - WHAT IS PT AFTER BOOST ?
        DO I = 1, 4
          P_CHK(I) = P_CHK(I) + P_CM(I,IP)
        END DO
C ******
   90 CONTINUE
C
C ****   ORDER PARTONS IN PT
C
      CALL ISASRT(PT,NP,ORDER)
      DO IP = 1, NP
        PT(IP) = -1*PT(IP)
        DO I = 1, 4
          P_TMP(I,IP) = P_CM(I,ORDER(IP))
        END DO
        IF ( SKPMAP( ORDER(IP) ) ) THEN
          CALL VZERO(P_TMP(1,IP),4)
          SEEDMP(IP) = 999
          JETMAP(IP) = 999
        END IF
      END DO
C
C ****  INITIALIZE JET ALG
C
      NJET=0
    1 ETMAX=0.
      IPASS=0
      IP = 0
C
C ****  FIND SEED PARTON WITH HIGHEST UNUSED PT
C
  200 IP = IP + 1                       ! loop over unused cells
      IF( IP.LE.NP ) THEN
        IF (SEEDMP(IP).EQ.0 .AND.  JETMAP(IP).EQ.0) THEN
          IF(PT(IP) .GT. ETMAX) THEN
            ETMAX= PT (IP)
            SEEDMP(IP)  = NJET + 1    ! DON'T USE SAME PARTON AS SEED TWICE
            DO I = 1, 4
              P_AXIS(I) = P_TMP(I,IP)
            END DO
          ENDIF
        ENDIF
C
        GOTO 200
      ENDIF
C
C ****  EXIT FOR JET ALGORITHM
C
      IF(ETMAX.EQ. 0.0 ) GOTO 400       !  no more jets to be found
C
      NJET=NJET+1
      IF(NJET.GT.NPMAX) THEN
        WRITE(ITLIS,99) NJET
   99   FORMAT(' ERROR IN CJANGL...NJET > ',I5)
        RETURN
      ENDIF
C
C
C ****  LOOP OVER ITERATIONS THROUGH ALGORITHM
C
    2 IPASS=IPASS+1
C
      IF(IPASS.GT. NP)  THEN
        WRITE(ITLIS,88) IPASS
   88   FORMAT(' ERROR IN CJANGL...IPASS > ',I6)
        RETURN
      ENDIF
C
C **** START A NEW JET
C
      CALL VZERO(P_JET(1,NJET),4)
      IP = 0
  300 IP = IP + 1                       ! loop over unused cells
      IF( IP.LE.NP ) THEN
        IF( JETMAP( IP ) .EQ. 0) THEN
C
          IF( OPANGL ( P_AXIS, P_TMP(1,IP) ) .LT. OP_CUT ) THEN  ! add cell
            DO I = 1, 4
              P_JET(I,NJET)= P_JET(I,NJET) + P_TMP(I,IP)
            END DO
            JETMAP(IP) = NJET
          ENDIF
C
        ENDIF
C
        GOTO 300
      ENDIF
C
C ****  CHECK IF NEED FURTHER ITERATIONS
C
      LX = P_JET(1,NJET).EQ. P_AXIS(1)
      LY = P_JET(2,NJET).EQ. P_AXIS(2)
      LZ = P_JET(3,NJET).EQ. P_AXIS(3)
      IF ( LX .AND. LY. AND. LZ .OR. IPASS.GE.MAX_IT) GOTO 3
C
C ****  SET AXIS TO JET MOMENTA, RESET MAP AND GO THROUGH ANOTHER ITERATION
C
      CALL UCOPY(P_JET(1,NJET),P_AXIS(1),4)
      DO IP = 1, NP
        IF ( JETMAP(IP) .EQ. NJET) JETMAP(IP) = 0
      END DO
      GOTO 2
C
C **** DISCARD JET IF Et < ETCUT, FILL COMMON OTHERWISE.
C
    3 PT_JET (NJET) =SQRT(P_JET(1,NJET)**2+P_JET(2,NJET)**2)
      IF(PT_JET(NJET) .LT. ET_CUT) THEN
        DO IP = 1, NP
          IF ( JETMAP(IP) .EQ. NJET) JETMAP(IP) = 0
        END DO
        DO I = 1, 4
          P_JET(I,NJET) = 0.0
        END DO
        NJET=NJET-1
      ENDIF
      GO TO 1
C
C ****  FOUND ALL JETS - FILL PJET BANK
C
  400 CALL VZERO(PJET(1,1),4*NJET)
      CALL VZERO(NQ(1),NP)
C
      DO IP = 1, NP
        JP = JETMAP( IP )
        IF( JP .GT. 0  .AND. JP.LT.NPMAX) THEN
          DO I = 1, 4
            PJET(I,JP) =
     &        PJET(I,JP) + P_LAB(I,ORDER(IP))
          END DO
          NQ( JP) =  NQ( JP ) + 1
          MAPINV( NQ(JP), JP ) = ORDER(IP)
        END IF
      END DO
C
C ****  FILL PJ_ID WITH ISAQ NUMERIC ID'S
C
      LISAQ=GZISAQ()
      NP = 0
  195 CONTINUE
      NP = NP + 1
      PJ_ID(NP) = IQ(LISAQ-5)
      LISAQ = LQ( LISAQ )
      IF( LISAQ .NE. 0 ) GOTO 195
      DO 295 IJ = 1, NJET
        PT_JET(IJ) = SQRT( PJET(1,IJ)*PJET(1,IJ) +
     &    PJET(2,IJ)*PJET(2,IJ) )
C
C ****  FINAL CHECK OF JET PT AFTER BOOST BACK TO PP CMS FRAME
C
        IF ( PT_JET(IJ).LT.ET_CUT ) GOTO 295
C
C ****  BOOK AND FILL JET BANK FOR THIS JET
C
        CALL BKPJET(LPJHD,LPJET)
        Q(LPJET+2) = PT_JET(IJ)
        Q(LPJET+3) = PJET(1,IJ)
        Q(LPJET+4) = PJET(2,IJ)
        Q(LPJET+5) = PJET(3,IJ)
        Q(LPJET+6) = PJET(4,IJ)
        CALL ISPETA(PJET(1,IJ),TH,PHI,ETA)
        Q(LPJET+8) = PHI
        Q(LPJET+9)  = TH
        Q(LPJET+10) = ETA
        EM = PJET(4,IJ)
        PM = SQRT ( PJET(1,IJ)*PJET(1,IJ)
     &    + PJET(2,IJ)*PJET(2,IJ)
     &    + PJET(3,IJ)*PJET(3,IJ) )
        Q(LPJET+7) = SQRT ( (EM+PM)* ABS( (EM-PM) ) )  ! SMALL MASS PROBLEM
        IQ(LPJET+11) = 0
        IQ(LPJET+12) = 0
C
C ****  BOOK AND FILL JET POINTER BANK FOR ALL PJETS
C
        CALL PJPTFL (PJ_ID,MAPINV(1,IJ),NPMAX,NQ(IJ))
  295 CONTINUE
C
  999 RETURN
      END
