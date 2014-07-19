*
* $Id: pjetfl.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: pjetfl.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE PJETFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank PJET
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   7-NOV-1989 18:10:09.84  Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "zebcom.inc"
#include "lkpjet.inc"
C
      INTEGER I,J,NJET,GZISAQ
      LOGICAL FIRST,OPEN_ANGLE,DR_CONE
      INTEGER MAXALG
      PARAMETER( MAXALG = 20 )
      REAL ETCUT(MAXALG),DRCUT(MAXALG)
      INTEGER MAXIT(MAXALG), IR(MAXALG), MUON(MAXALG)
      CHARACTER ALG(MAXALG)*20
      INTEGER N
C----------------------------------------------------------------------
C
C ****  Activate PJET temporary link area
C
      CALL PJLINK (PJLON)
C
C
C ****  GET ALGORITHM PARAMETERS FROM PJPGET
C
      CALL PJPGET(N,ALG,ETCUT,DRCUT,MAXIT,IR,MUON)
C
C ****  DON'T DO PJET IF NO ISAQ BANKS OR NO PJET PARAMETERS
C
      LISAQ = GZISAQ()
      IF (N.EQ.0 .OR. LISAQ.LE.0) GOTO 999
C
C ****  LOOP OVER ALGORITHMS
C
      DO I = 1, N
C
C ****  Book next PJHD bank to hang this PJET from
C
        CALL BKPJHD(LPJHD)
        IF (INDEX(ALG(I),'CONE').GT.0) THEN
          CALL PJCONE(LPJHD,DRCUT(I),ETCUT(I),MAXIT(I),
     &      IR(I),MUON(I),NJET)
          IQ(LPJHD+2) = 1                 ! Delta R CONE ALGORITHM
        ELSE IF (INDEX(ALG(I),'ANGL').GT.0) THEN
          CALL PJANGL(LPJHD,DRCUT(I),ETCUT(I),MAXIT(I),
     &      IR(I),MUON(I),NJET)
          IQ(LPJHD+2) = 2                 ! CMS OPENING ANGLE ALGORITHM
        END IF
        IQ(LPJHD+3) = NJET                 ! NUMBER OF JETS FROM CONE ALGORITHM
        Q(LPJHD+4) = DRCUT(I)              ! Delta R /Opening Angle CUT
        Q(LPJHD+5) = ETCUT(I)              ! JET ET CUT
        IQ(LPJHD+6) = MAXIT(I)             ! MAX_PASS = 1 FOR DR CONE
        IQ(LPJHD+7) = IR(I)                ! INIT RAD SWITCH  = NOT 0 FOR NO IR
        IQ(LPJHD+8) = MUON(I)              ! MUON SWITCH  = 0 for NO MUONS
      END DO
      CALL PJISAQ(0)
C
C ****  De-activate temporary link area
C
      CALL PJLINK (PJLOFF)
C-
C----------------------------------------------------------------------
  999 RETURN
      END
