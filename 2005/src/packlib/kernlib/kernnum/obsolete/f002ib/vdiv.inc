*
* $Id: vdiv.inc,v 1.1.1.1 1996/02/15 17:48:18 mclareni Exp $
*
* $Log: vdiv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:18  mclareni
* Kernlib
*
*
*
* vdiv.inc
*
#include "basic.inc"
#include "div.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      0
           SETYPE
           HEADER    ENTRY               (N,X,X2,Y,Y2,Z,Z2,IFAIL)
 J         EQU       1
 NSTEP     EQU       2
 N         EQU       3
 X         EQU       4
 JX        EQU       5
 Y         EQU       6
 JY        EQU       7
 Z         EQU       8
 JZ        EQU       9
 IFAIL     EQU       10
 &REGLIM   SETA      10
 ENTRY     PROLOG
           LM        3,10,0(1)
 *                                       R3 = (N)
 *                                       R4 = (X)
 *                                       R5 = (X2)
 *                                       R6 = (Y)
 *                                       R7 = (Y2)
 *                                       R8 = (Z)
 *                                       R9 = (Z2)
 *                                       R10= (IFAIL)
           N         10,=X'7FFFFFFF'
           LA        J,0                 IFAIL=0
           L         N,0(3)
           LA        NSTEP,1             NSTEP = 1
           C         N,=F'0'
           BC        LE,RETURN           IF(N .LE. 0) RETURN
           SR        5,4                 R5 = (X2) - (X) = JX
           SR        7,6                 R7 = (Y2) - (Y) = JY
           SR        9,8                 R9 = (Z2) - (Z) = JZ
           VDIV      N,X,JX,Y,JY,Z,JZ,J
 RETURN    ST        J,0(IFAIL)          IFAIL = J
           EPILOG
           MEND
#include "tglobal.inc"
