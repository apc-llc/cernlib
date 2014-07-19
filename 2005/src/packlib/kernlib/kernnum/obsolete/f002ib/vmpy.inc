*
* $Id: vmpy.inc,v 1.1.1.1 1996/02/15 17:48:18 mclareni Exp $
*
* $Log: vmpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:18  mclareni
* Kernlib
*
*
*
* vmpy.inc
*
#include "basic.inc"
#include "mpy.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      0
           SETYPE
           HEADER    ENTRY
 JY2       EQU       2
 N         EQU       3
 X         EQU       4
 JX        EQU       5
 Y         EQU       6
 JY        EQU       7
 JX2       EQU       8
 &REGLIM   SETA      8
 ENTRY     PROLOG
           AIF       ('&NAME' EQ 'VMPA ').BETA
           AIF       ('&NAME' EQ 'VMPAC').BETA
           LM        3,7,0(1)
 *                                       R3 = (N)
 *                                       R4 = (X)
 *                                       R5 = (X2)
 *                                       R6 = (Y)
 *                                       R7 = (Y2)
           N         7,=X'7FFFFFFF'
           CLEAR     0
           AGO       .GAMMA
 .BETA     ANOP
           LM        3,8,0(1)
 *                                       R3 = (N)
 *                                       R4 = (X)
 *                                       R5 = (X2)
 *                                       R6 = (Y)
 *                                       R7 = (Y2)
 *                                       R8 = (Z)
           N         8,=X'7FFFFFFF'
           LOAD      0,8
 .GAMMA    ANOP
           L         N,0(3)
           C         N,=F'0'
           BNH       RETURN              IF(N .LE. 0) RETURN
           SR        5,4                 R5 = (X2) - (X1) = JX
           SR        7,6                 R7 = (Y2) - (Y1) = JY
           LR        JX2,JX
           LR        JY2,JY
           AR        JX2,JX              JX2 = 2 * JX
           AR        JY2,JY              JY2 = 2 * JY
           VMPY      N,X,JX,Y,JY,JX2,JY2
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
