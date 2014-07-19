*
* $Id: vxch.inc,v 1.1.1.1 1996/02/15 17:48:19 mclareni Exp $
*
* $Log: vxch.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:19  mclareni
* Kernlib
*
*
*
* vxch.inc
*
#include "basic.inc"
#include "xch.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      0
           SETYPE
           HEADER    ENTRY               (N,X,X2,Y,Y2)
 N         EQU       2
 X         EQU       3
 JX        EQU       4
 Y         EQU       5
 JY        EQU       6
 JX2       EQU       7
 JY2       EQU       8
 &REGLIM   SETA      8
 ENTRY     PROLOG
           LM        2,6,0(1)
 *                                       R2 = (N)
 *                                       R3 = (X)
 *                                       R4 = (X2)
 *                                       R5 = (Y)
 *                                       R6 = (Y2)
           N         7,=X'7FFFFFFF'
           L         N,0(2)
           C         N,=F'0'
           BC        LE,RETURN           IF(N .LE. 0) RETURN
           SR        4,3                 R4 = (X2) - (X1) = JX
           SR        6,5                 R6 = (Y2) - (Y1) = JY
           LR        JX2,JX
           LR        JY2,JY
           AR        JX2,JX              JX2 = 2 * JX
           AR        JY2,JY              JY2 = 2 * JY
           VXCH2     N,X,JX,Y,JY,JX2,JY2
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
