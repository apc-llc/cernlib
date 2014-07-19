*
* $Id: vscl.inc,v 1.1.1.1 1996/02/15 17:48:19 mclareni Exp $
*
* $Log: vscl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:19  mclareni
* Kernlib
*
*
*
* vscl.inc
*
#include "basic.inc"
#include "scl.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      0
           SETYPE
           HEADER    ENTRY               (N,S,X,X2,Y,Y2)
 J         EQU       1
 NSTEP     EQU       2
 N         EQU       3
 JX2       EQU       4
 X         EQU       5
 JX        EQU       6
 Y         EQU       7
 JY        EQU       8
 JY2       EQU       9
 &REGLIM   SETA      9
 ENTRY     PROLOG
           LM        3,8,0(1)
 *                                       R3 = (N)
 *                                       R4 = (S)
 *                                       R5 = (X)
 *                                       R6 = (X2)
 *                                       R7 = (Y)
 *                                       R8 = (Y2)
           N         8,=X'7FFFFFFF'
           L         N,0(3)
           LA        NSTEP,2             NSTEP = 2
           C         N,=F'0'
           BC        LE,RETURN           IF(N .LE. 0) RETURN
           LOAD      4,4
           SR        6,5                 R6 = (X2) - (X) = JX
           SR        8,7                 R8 = (Y2) - (Y) = JY
           LR        JX2,JX
           LR        JY2,JY
           AR        JX2,JX              JX2 = 2 * JX
           AR        JY2,JY              JY2 = 2 * JY
           VSCL      N,X,JX,Y,JY,JX2,JY2,J
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
