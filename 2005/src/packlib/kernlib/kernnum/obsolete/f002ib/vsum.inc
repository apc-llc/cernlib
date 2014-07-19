*
* $Id: vsum.inc,v 1.1.1.1 1996/02/15 17:48:19 mclareni Exp $
*
* $Log: vsum.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:19  mclareni
* Kernlib
*
*
*
* vsum.inc
*
#include "basic.inc"
#include "sum.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      0
           SETYPE
           HEADER    ENTRY               (N,X,X2)
 J         EQU       1
 NSTEP     EQU       2
 N         EQU       3
 X         EQU       4
 JX        EQU       5
 JX2       EQU       6
 &REGLIM   SETA      6
 ENTRY     PROLOG
           LM        3,5,0(1)
 *                                       R3 = (N)
 *                                       R4 = (X)
 *                                       R5 = (X2)
           N         5,=X'7FFFFFFF'
           CLEAR     0                   F0 = 0.
           L         N,0(3)
           LA        NSTEP,2             NSTEP = 2
           C         N,=F'0'
           BC        LE,RETURN           IF(N .LE. 0) RETURN
           SR        5,4                 R5 = (X2) - (X) = JX
           LR        JX2,JX
           AR        JX2,JX              JX2 = 2 * JX
           VSUM      N,X,JX,JX2,J
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
