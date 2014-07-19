*
* $Id: mscl.inc,v 1.1.1.1 1996/02/15 17:48:22 mclareni Exp $
*
* $Log: mscl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:22  mclareni
* Kernlib
*
*
*
* mscl.inc
*
#include "basic.inc"
#include "scl.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      5
           SETYPE
           HEADER    ENTRY               (M,N,S,X,X12,X21,Z,Z12,Z21)
 M         EQU       2
 N         EQU       3
 X         EQU       5
 JX        EQU       6
 IX        EQU       7
 Z         EQU       8
 JZ        EQU       9
 IZ        EQU       10
 &REGLIM   SETA      10
 ENTRY     PROLOG
           LM        2,10,0(1)
 *                                       R2 = (M)
 *                                       R3 = (N)
 *                                       R4 = (S)
 *                                       R5 = (X)
 *                                       R6 = (X12)
 *                                       R7 = (X21)
 *                                       R8 = (Z)
 *                                       R9 = (Z12)
 *                                       R10= (Z21) + 8000 0000
           LA        10,0(10)            R10= (Z21)
           L         M,0(2)              R2 = M
           L         N,0(3)              R3 = N
           LOAD      4,4                 F4 = S
           LTR       0,M                 TEST R0 = M
           BNH       RETURN              IF(M .LE. 0) RETURN
           LTR       0,N                 TEST R0 = N
           BNH       RETURN              IF(N .LE. 0) RETURN
           SR        6,5                 R6 = (X12) - (X) = JX
           SR        7,5                 R7 = (X21) - (X) = IX
           SR        9,8                 R9 = (Z12) - (Z) = JZ
           SR        10,8                R10= (Z21) - (Z) = IZ
           CR        M,N
           BNH       ALABEL              IF(M .LE. N)
           SWAP      M,N
           SWAP      IX,JX
           SWAP      IZ,JZ
 ALABEL    TMSCL     M,N,X,IX,JX,Z,IZ,JZ
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
