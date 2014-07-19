*
* $Id: mset.inc,v 1.1.1.1 1996/02/15 17:48:22 mclareni Exp $
*
* $Log: mset.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:22  mclareni
* Kernlib
*
*
*
* mset.inc
*
#include "basic.inc"
#include "set.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      2
           SETYPE
           HEADER    ENTRY               (M,N,S,Z,Z12,Z21)
 J         EQU       1
 NSTEP     EQU       2
 N         EQU       3
 JZ2       EQU       4
 Z         EQU       5
 JZ        EQU       6
 IZ        EQU       7
 MSTEP     EQU       8
 M         EQU       9
 I         EQU       10
 &REGLIM   SETA      10
 ENTRY     PROLOG
           LM        2,7,0(1)
 *                                       R2 = (M)
 *                                       R3 = (N)
 *                                       R4 = (S)
 *                                       R5 = (Z)
 *                                       R6 = (Z12)
 *                                       R7 = (Z21) + 8000 0000
           LA        7,0(7)              R7 = (Z21)
           L         M,0(2)              R9 = M
           L         N,0(3)              R3 = N
           LOAD      0,4                 F0 = S
           LTR       0,M                 TEST R0 = M
           BNH       RETURN              IF(M .LE. 0) RETURN
           LTR       0,N                 TEST R0 = N
           BNH       RETURN              IF(N .LE. 0) RETURN
           SR        6,5                 R6 = (Z12) - (Z) = JZ
           SR        7,5                 R7 = (Z21) - (Z) = IZ
           TMSET2    M,N,Z,IZ,JZ,JZ2,I,J
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
