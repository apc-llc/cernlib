*
* $Id: vsca.inc,v 1.1.1.1 1996/02/15 17:48:19 mclareni Exp $
*
* $Log: vsca.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:19  mclareni
* Kernlib
*
*
*
* vsca.inc
*
#include "basic.inc"
#include "sca.inc"
           MACRO
           MAIN      &F
#include "global.inc"
 &TWORDS   SETA      0
           SETYPE
           HEADER    ENTRY               (N,S,X,X2,Y,Y2,Z,Z2)
 J         EQU       1
 NSTEP     EQU       2
 N         EQU       3
 JX2       EQU       4
 X         EQU       5
 JX        EQU       6
 Y         EQU       7
 JY        EQU       8
 Z         EQU       9
 JZ        EQU       10
 JY2       EQU       11
 JZ2       EQU       12
 &REGLIM   SETA      12
 ENTRY     PROLOG
           LM        3,10,0(1)
 *                                       R3 = (N)
 *                                       R4 = (S)
 *                                       R5 = (X)
 *                                       R6 = (X2)
 *                                       R7 = (Y)
 *                                       R8 = (Y2)
 *                                       R9 = (Z)
 *                                       R10= (Z2)
           N         10,=X'7FFFFFFF'
           L         N,0(3)
           LA        NSTEP,2             NSTEP = 2
           C         N,=F'0'
           BC        LE,RETURN           IF(N .LE. 0) RETURN
           LOAD      4,4
           SR        6,5                 R6 = (X2) - (X) = JX
           SR        8,7                 R8 = (Y2) - (Y) = JY
           SR        10,9                R10= (Z2) - (Z) = JZ
           LR        JX2,JX
           LR        JY2,JY
           LR        JZ2,JZ
           AR        JX2,JX              JX2 = 2 * JX
           AR        JY2,JY              JY2 = 2 * JY
           AR        JZ2,JZ              JZ2 = 2 * JZ
           VSCA      &F,N,X,JX,Y,JY,Z,JZ,JX2,JY2,JZ2,J
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
