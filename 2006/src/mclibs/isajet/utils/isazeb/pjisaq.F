*
* $Id: pjisaq.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: pjisaq.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE PJISAQ(LPJHDI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO FILL LINK FROM ISAQ (-2) TO
C-   A REFERENCE LINK TO PJET JET NUMBER.
C-
C-   Inputs  :   LPJHDI - ADDRESS OF PJET HEADER BANK LPJHDI 
C-               = 0 - first PJHD bank
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  29-NOV-1989   Chip Stewart
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER LPJHD,LPJET,LPJPT,LPJHDI
      INTEGER LISAQ,J
      INTEGER GZPJHD
#include "zebcom.inc"
#include "izpjet.inc"
#include "izpjpt.inc"
C----------------------------------------------------------------------
C
C ****  DETERMINE WHICH ISAQ PARTONS GOTO WHICH PJET FROM PJPT
C
      LPJHD = LPJHDI
      LPJET=0
      IF (LPJHD .EQ. 0 ) LPJHD = GZPJHD ()           ! PJET HEADER BANK
      IF(LPJHD.GT.0) LPJET = LQ ( LPJHD - IZPJET)    ! PJET BANK FROM PJHD
      IF(LPJET.LE.0) GOTO 999
    1 LPJPT = LQ ( LPJET - IZPJPT)              ! PJET POINTER BANK
      DO 2, J = 2, IQ(LPJPT-3)                  ! LOOP OVER POINTERS TO ISAQ
C
C ****  OVERWRITE ISAQ LINK WITH REFERNCE TO PJET BANKS
C
       LISAQ = LQ(LPJPT- J)
       LQ (LISAQ - 2) = LPJET
    2 CONTINUE
      LPJET = LQ ( LPJET  )
      IF ( LPJET .GT. 0 ) GOTO 1
  999 RETURN
      END
