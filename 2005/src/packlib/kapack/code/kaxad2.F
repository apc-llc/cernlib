*
* $Id: kaxad2.F,v 1.1.1.1 1996/03/08 11:40:52 mclareni Exp $
*
* $Log: kaxad2.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:52  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXAD2(LOC,KEY,IDATA,LDATA)
*
*.....ADD A RECORD TO A TREE: METHOD 2, SPLIT THE CURRENT BLOCK
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax0a0.inc"
#include "kapack/kax0b0.inc"
*
      INTEGER IDATA(*), KEY(*)
*
      LOGICAL LONG
*
*-----------------------------------------------------------------------
*
*.....GENERATE A NEW TREE LEVEL TO AVOID SPLITTING A ROOT BLOCK
      IF ( IA(1) .EQ. IA(4) )  CALL KAXGLV(LOC)
*
      NLEFT  = IA(2)
      NRIGHT = IA(1)
*
*.....THE BLOCK CANNOT BE SPLIT IF INSERTION IS AT THE BEGINNING
      IF ( LOC .EQ. IA(6) )  THEN
         CALL KAXAD3(KEY,IDATA,LDATA,NBLOCK)
*
      ELSE
*
*.....GET A BLOCK TO BECOME THE NEW LEFT BROTHER
         CALL KAXGBK(NBLOCK)
         IA(2) = NBLOCK
*
*........DETERMINE SPACE AVAILABLE AFTER THE SPLIT
         LA   = LBLK+1 - IA(7)  +  LOC - IA(6)
         LB   = LBLK+1 - LOC
         LONG = (LREC.GT.LA) .AND. (LREC.GT.LB)
         IF ( LONG )  CALL KAXAD3(KEY,IDATA,LDATA,NRIGHT)
*
*........SPLIT THE CURRENT BLOCK
         IB( 1) = NBLOCK
         IB( 2) = NLEFT
         IB( 3) = NRIGHT
         IB( 4) = IA(4)
         IB( 5) = IA(5)
         IB( 6) = NBCW + 1
         IB( 7) = NBCW + 1
         IB( 8) = 0
         IB( 9) = 0
         IB(10) = 0
         CALL KAXSHL(IA,IB,LOC-IA(6))
*
*........INSERT THE NEW RECORD
         IF ( .NOT. LONG )  THEN
            IF ( LA .GT. LB )  THEN
               CALL KAXINS(IA,IA(6),KEY,IDATA,LDATA)
            ELSE
               CALL KAXINS(IB,LOC,KEY,IDATA,LDATA)
            ENDIF
         ENDIF
*
*........WRITE THE NEW BLOCK
         CALL KAXWRT(IB(1),IB,IB(7)-1)
         CALL KAXGXA(IB)
      ENDIF
*
*.....UPDATE THE RIGHT POINTER IN THE OLD LEFT BROTHER
      IF ( NLEFT .NE. 0 )  THEN
         CALL KAXRD(NLEFT,IB,LBLK)
         IB(3) = NBLOCK
         CALL KAXWRT(IB(1),IB,IB(7)-1)
      ENDIF
*
*.....REPLACE THE CURRENT BLOCK
      CALL KAXWRT(IA(1),IA,IA(7)-1)
*
      END
