*
* $Id: mcpy.inc,v 1.1.1.1 1996/02/15 17:48:22 mclareni Exp $
*
* $Log: mcpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:22  mclareni
* Kernlib
*
*
*
* mcpy.inc
*
#include "basic.inc"
#include "cpy.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      5
           SETYPE
           HEADER    ENTRY               (M,N,X,X12,X21,Z,Z12,Z21)
 M         EQU       2
 N         EQU       3
 X         EQU       4
 JX        EQU       5
 IX        EQU       6
 Z         EQU       7
 JZ        EQU       8
 IZ        EQU       9
 &REGLIM   SETA      9
 ENTRY     PROLOG
           LM        2,9,0(1)
 *                                       R2 = (M)
 *                                       R3 = (N)
 *                                       R4 = (X)
 *                                       R5 = (X12)
 *                                       R6 = (X21)
 *                                       R7 = (Z)
 *                                       R8 = (Z12)
 *                                       R9 = (Z21) + 8000 0000
           LA        9,0(9)              R9 = (Z21)
           L         M,0(2)
           L         N,0(3)
           LTR       0,M                 TEST R0 = M
           BNH       RETURN              IF(M .LE. 0) RETURN
           LTR       0,N                 TEST R0 = N
           BNH       RETURN              IF(N .LE. 0) RETURN
           SR        5,4                 R5 = (X12) - (X) = JX
           SR        6,4                 R6 = (X21) - (X) = IX
           SR        8,7                 R8 = (Z12) - (Z) = JZ
           SR        9,7                 R9 = (Z21) - (Z) = IZ
           CR        M,N
           BNH       ALABEL              IF(M .LE. N)
           SWAP      M,N
           SWAP      IX,JX
           SWAP      IZ,JZ
 ALABEL    TMCPY     M,N,X,IX,JX,Z,IZ,JZ
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
