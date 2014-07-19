*
* $Id: ibad.F,v 1.1.1.1 1996/02/15 17:53:26 mclareni Exp $
*
* $Log: ibad.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:26  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION IBAD (MC)
C
C
      IMC= MC
      M= 0
      DO 10 I=1,4
      IF (MOD(IMC,4).EQ.2)  M=M+1
   10 IMC= IMC/4
      GO TO 99
C
         ENTRY IMEA                                                      CDC
C        ENTRY IMEA    (MC)                                             -CDC
C
      IMC= MC
      M= 0
      DO 20 I=1,4
      IF (MOD(IMC,4).GE.2)  M=M+1
   20 IMC= IMC/4
      GO TO 99
C
         ENTRY IUNME                                                     CDC
C        ENTRY IUNME   (MC)                                             -CDC
C
      IMC= MC
      M= 0
      DO 30 I=1,4
      IF (MOD(IMC,4).EQ.1)  M=M+1
   30 IMC= IMC/4
C
   99 IBAD= M
C     IMEA= IBAD                                                        IBM
C     IUNME= IBAD                                                       IBM
      RETURN
      END
