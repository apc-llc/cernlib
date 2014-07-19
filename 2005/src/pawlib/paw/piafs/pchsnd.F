*
* $Id: pchsnd.F,v 1.1.1.1 1996/03/01 11:39:26 mclareni Exp $
*
* $Log: pchsnd.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:26  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/16 04/10/93  18.32.05  by  Fons Rademakers
*-- Author :    Fons Rademakers   23/08/93
      SUBROUTINE PCHSND(ISLAV, ISTAT)
******************************************************************
*                                                                *
*  Send the current chain defintion from the master to the slave *
*  server ISLAV.                                                 *
*                                                                *
******************************************************************
*
#include "hbook/hcpiaf.inc"
*
      CHARACTER*80   CCHAIN
      CHARACTER*128  MEMBER
*
      ISTAT = 0
*
*-- Get the top of the chain tree
*
      CALL PCHTOP(CCHAIN, LC)
      IF (LC .LT. 0) GOTO 20
*
      CALL PCHSET(CCHAIN, LC, MEMBER, LM)
      IF (LM .LT. 0) GOTO 20
*
      CHLMPF = 'nt/chain -;nt/chain '//CCHAIN(1:LC)//' '//MEMBER(1:LM)
*
   10 CALL PCHNXT(CHSMPF, LD, MEMBER, LM)
      IF (LM .GT. 0) THEN
         LL = LENOCC(CHLMPF)
         IF (LL+LM+2 .GT. LEN(CHLMPF)) THEN
            CALL PFKUPN(CHLMPF, ISLAV, ISTAT)
            IF (ISTAT.NE.0) GOTO 20
            CHLMPF = 'nt/chain '//CCHAIN(1:LC)
            LL = LENOCC(CHLMPF)
         ENDIF
         CHLMPF(LL+2:) = MEMBER(1:LM)
         GOTO 10
      ENDIF
*
      CALL PFKUPN(CHLMPF, ISLAV, ISTAT)
*
   20 END
