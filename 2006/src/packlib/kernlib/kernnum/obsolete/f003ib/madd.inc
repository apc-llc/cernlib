*
* $Id: madd.inc,v 1.1.1.1 1996/02/15 17:48:21 mclareni Exp $
*
* $Log: madd.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:21  mclareni
* Kernlib
*
*
*
* madd.inc
*
#include "basic.inc"
#include "add.inc"
           MACRO
           MAIN      &F
#include "global.inc"
 &TWORDS   SETA      10
           SETYPE
           HEADER    ENTRY          (M,N,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
 M         EQU       2
 N         EQU       3
 X         EQU       4
 JX        EQU       5
 IX        EQU       6
 Y         EQU       7
 JY        EQU       8
 IY        EQU       9
 Z         EQU       10
 JZ        EQU       11
 IZ        EQU       12
 &REGLIM   SETA      12
 ENTRY     PROLOG
           LM        2,12,0(1)
 *                                       R2 = (M)
 *                                       R3 = (N)
 *                                       R4 = (X)
 *                                       R5 = (X12)
 *                                       R6 = (X21)
 *                                       R7 = (Y)
 *                                       R8 = (Y12)
 *                                       R9 = (Y21)
 *                                       R10= (Z)
 *                                       R11= (Z12)
 *                                       R12= (Z21) + 8000 0000
           LA        12,0(12)            R12= (Z21)
           L         M,0(2)
           L         N,0(3)
           LTR       0,M                 TEST R0 = M
           BNH       RETURN              IF(M .LE. 0) RETURN
           LTR       0,N                 TEST R0 = N
           BNH       RETURN              IF(N .LE. 0) RETURN
           SR        5,4                 R5 = (X12) - (X) = JX
           SR        6,4                 R6 = (X21) - (X) = IX
           SR        8,7                 R8 = (Y12) - (Y) = JY
           SR        9,7                 R9 = (Y21) - (Y) = IY
           SR        11,10               R11= (Z12) - (Z) = JZ
           SR        12,10               R12= (Z21) - (Z) = IZ
           CR        M,N
           BNH       ALABEL              IF(M .LE. N)
           SWAP      M,N
           SWAP      IX,JX
           SWAP      IY,JY
           SWAP      IZ,JZ
 ALABEL    TMADD     &F,M,N,X,IX,JX,Y,IY,JY,Z,IZ,JZ
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
