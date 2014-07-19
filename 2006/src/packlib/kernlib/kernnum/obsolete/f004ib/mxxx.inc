*
* $Id: mxxx.inc,v 1.1.1.1 1996/02/15 17:48:28 mclareni Exp $
*
* $Log: mxxx.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:28  mclareni
* Kernlib
*
*
*
* mxxx.inc
*
#include "basic.inc"
#include "mpy.inc"
#include "mlt.inc"
           MACRO
           MAIN      &F
#include "global.inc"
 &TWORDS   SETA      6
           SETYPE
           HEADER    ENTRY     (M,N,K,X,X12,X21,Y,Y12,Y21,Z,Z12,Z21)
 X         EQU       2
 JX        EQU       3
 IX        EQU       4
 Y         EQU       5
 LY        EQU       6
 JY        EQU       7
 Z         EQU       8
 LZ        EQU       9
 IZ        EQU       10
 M         EQU       11
 N         EQU       12
 &REGLIM   SETA      12
 ENTRY     PROLOG
           L         11,0(1)             R11= (M)
           L         12,4(1)             R12= (N)
           LM        1,10,8(1)           R1 = (K)
 *                                       R2 = (X)
 *                                       R3 = (X12)
 *                                       R4 = (X21)
 *                                       R5 = (Y)
 *                                       R6 = (Y12)
 *                                       R7 = (Y21)
 *                                       R8 = (Z)
 *                                       R9 = (Z12)
 *                                       R10= (Z21)+8000 0000
           N         10,=X'7FFFFFFF'
           L         11,0(11)            R11= M
           L         12,0(12)            R12= N
           L         1,0(1)              R1 = K
           C         M,=F'0'
           BNH       RETURN              IF(M .LE. 0) RETURN
           C         N,=F'0'
           BNH       RETURN              IF(N .LE. 0) RETURN
           C         1,=F'0'
           BNH       RETURN              IF(K .LE. 0) RETURN
           SR        3,2                 R3 = (X12) - (X) = JX
           SR        4,2                 R4 = (X21) - (X) = IX
           SR        6,5                 R6 = (Y12) - (Y) = LY
           SR        7,5                 R7 = (Y21) - (Y) = JY
           SR        9,8                 R9 = (Z12) - (Z) = LZ
           SR        10,8                R10= (Z21) - (Z) = IZ
 A&SYSNDX  PUSH      1                   SAVE K-I+1
           TXMPY     &F,M,N,X,IX,JX,Y,JY,Z,IZ
           AR        Y,LY
           AR        Z,LZ
           POP       1                   R1=K-I+1
           S         1,=F'1'             R1=K-I
           BNZ       A&SYSNDX
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
