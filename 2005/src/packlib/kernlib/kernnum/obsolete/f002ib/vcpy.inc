*
* $Id: vcpy.inc,v 1.1.1.1 1996/02/15 17:48:18 mclareni Exp $
*
* $Log: vcpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:18  mclareni
* Kernlib
*
*
*
* vcpy.inc
*
#include "basic.inc"
#include "cpy.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      0
           SETYPE
           HEADER    ENTRY               (N,X,X2,Y,Y2)
 J         EQU       1
 NSTEP     EQU       2
 N         EQU       3
 X         EQU       4
 JX        EQU       5
 Y         EQU       6
 JY        EQU       7
 JX2       EQU       8
 JY2       EQU       9
 &REGLIM   SETA      9
 ENTRY     PROLOG
           LM        3,7,0(1)
 *                                       R3 = (N)
 *                                       R4 = (X)
 *                                       R5 = (X2)
 *                                       R6 = (Y)
 *                                       R7 = (Y2)
           N         7,=X'7FFFFFFF'
           L         N,0(3)
           LA        NSTEP,2             NSTEP = 2
           C         N,=F'0'
           BC        LE,RETURN           IF(N .LE. 0) RETURN
           SR        5,4                 R5 = (X2) - (X) = JX
           SR        7,6                 R7 = (Y2) - (Y) = JY
           LR        JX2,JX
           LR        JY2,JY
           AR        JX2,JX              JX2 = 2 * JX
           AR        JY2,JY              JY2 = 2 * JY
           VCPY      N,X,JX,Y,JY,JX2,JY2,J
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
