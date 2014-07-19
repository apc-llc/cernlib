*
* $Id: vset.inc,v 1.1.1.1 1996/02/15 17:48:19 mclareni Exp $
*
* $Log: vset.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:19  mclareni
* Kernlib
*
*
*
* vset.inc
*
#include "basic.inc"
#include "set.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      0
           SETYPE
           HEADER    ENTRY               (N,S,Z,Z2)
 J         EQU       1
 NSTEP     EQU       2
 N         EQU       3
 JZ2       EQU       4
 Z         EQU       5
 JZ        EQU       6
 &REGLIM   SETA      6
 ENTRY     PROLOG
           LM        3,6,0(1)
 *                                       R3 = (N)
 *                                       R4 = (S)
 *                                       R5 = (Z)
 *                                       R6 = (Z2)
           N         6,=X'7FFFFFFF'
           L         N,0(3)              R3 = N
           LA        NSTEP,2             NSTEP = 2
           LOAD      0,4                 F0 = S
           C         N,=F'0'
           BC        LE,RETURN           IF(N .LE. 0) RETURN
           SR        6,5                 R6 = (Z2) - (Z) = JZ
           LR        JZ2,JZ
           AR        JZ2,JZ              JZ2 = 2 * JZ
           VSET      N,Z,JZ,JZ2,J
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
