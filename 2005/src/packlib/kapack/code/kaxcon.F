*
* $Id: kaxcon.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxcon.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXCON
*
*.....PERFORM CONCATENATION WITH THE LEFT OR RIGHT BROTHER
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax0a0.inc"
#include "kapack/kax0b0.inc"
*
*-----------------------------------------------------------------------
*
*.....SPECIAL CASE - CHECK FOR A ROOT BLOCK, THEN
*        IF IT IS NOT A LEAF AND HAS ONLY ONE ENTRY, DELETE THE LEVEL
      IF ( IA(4) .EQ. IA(1) )  THEN
         IF ( IA(5).NE.1 .AND. IA(6)+IA(IA(6)).GE.IA(7) )  CALL KAXDLV
         RETURN
      ENDIF
*
*.....DON'T ATTEMPT CONCATENATION IF OCCUPANCY IS ABOVE THRESHOLD
      NSPACE = LBLK + 1 - IA(6)
      NUSEDA = IA(7) - IA(6)
      USED = REAL(NUSEDA) / REAL(NSPACE)
      IF ( USED .GT. THRESH )  RETURN
*
*.....TRY TO CONCATENATE WITH THE LEFT BROTHER
      IF ( IA(2) .EQ. 0 )  GO TO 1
      CALL KAXRD(IA(2),IB,LBLK)
      IF ( IB(8) .NE. 0 )  GO TO 1
      NUSEDB = IB(7) - IB(6)
      IF ( NUSEDA+NUSEDB .GT. NSPACE )  GO TO 1
      CALL KAXGXD(IB)
      CALL KAXSHR(IB,IA,NUSEDB)
      IA(2) = IB(2)
      CALL KAXWRT(IA(1),IA,IA(7)-1)
      CALL KAXFBK(IB(1))
      IF ( IA(2) .NE. 0 )  THEN
         CALL KAXRD(IA(2),IB,LBLK)
         IB(3) = IA(1)
         CALL KAXWRT(IB(1),IB,IB(7)-1)
      ENDIF
      RETURN
*
*.....TRY TO CONCATENATE WITH THE RIGHT BROTHER
 1    IF ( IA(3) .EQ. 0 )  GO TO 2
      CALL KAXRD(IA(3),IB,LBLK)
      IF ( IB(8) .NE. 0 )  GO TO 2
      NUSEDB = IB(7) - IB(6)
      IF ( NUSEDA+NUSEDB .GT. NSPACE )  GO TO 2
      CALL KAXGXD(IA)
      CALL KAXSHR(IA,IB,NUSEDA)
      IB(2) = IA(2)
      CALL KAXWRT(IB(1),IB,IB(7)-1)
      CALL KAXFBK(IA(1))
      IF ( IA(2) .NE. 0 )  THEN
         CALL KAXRD(IA(2),IB,LBLK)
         IB(3) = IA(3)
         CALL KAXWRT(IB(1),IB,IB(7)-1)
      ENDIF
*
 2    RETURN
*
      END
