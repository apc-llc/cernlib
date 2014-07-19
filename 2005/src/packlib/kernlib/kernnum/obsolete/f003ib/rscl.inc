*
* $Id: rscl.inc,v 1.1.1.1 1996/02/15 17:48:22 mclareni Exp $
*
* $Log: rscl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:22  mclareni
* Kernlib
*
*
*
* rscl.inc
*
#include "basic.inc"
#include "scl.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      5
           SETYPE
           HEADER    ENTRY               (M,N,D,D2,X,X12,X21,Z,Z12,Z21)
 NSTEP     EQU       2                   N-1 IS USED IN LOOPING
 N         EQU       3                   N MUST BE AN ODD REGISTER
 D         EQU       4
 ID        EQU       5
 X         EQU       6
 JX        EQU       7
 IX        EQU       8
 Z         EQU       9
 JZ        EQU       10
 IZ        EQU       11
 M         EQU       12                  TO CONTROL THE INNER LOOP
 &REGLIM   SETA      12
 ENTRY     PROLOG
           LM        2,11,0(1)
 *                                       R2 = (M)
 *                                       R3 = (N)
 *                                       R4 = (D)
 *                                       R5 = (D2)
 *                                       R6 = (X)
 *                                       R7 = (X12)
 *                                       R8 = (X21)
 *                                       R9 = (Z)
 *                                       R10= (Z12)
 *                                       R11= (Z21) + 8000 0000
           LA        11,0(11)            R11= (Z21)
           L         M,0(2)              R12= M
           L         N,0(3)              R3 = N
           LA        NSTEP,2             R2 = 2 = NSTEP
           LTR       0,M                 TEST R0 = M
           BNH       RETURN              IF(M .LE. 0) RETURN
           LTR       0,N                 TEST R0 = N
           BNH       RETURN              IF(N .LE. 0) RETURN
           SR        5,4                 R5 = (D2)  - (D) = ID
           SR        7,6                 R7 = (X12) - (X) = JX
           SR        8,6                 R8 = (X21) - (X) = IX
           SR        10,9                R10= (Z12) - (Z) = JZ
           SR        11,9                R11= (Z21) - (Z) = IZ
           RSCL      M,N,D,ID,X,IX,JX,Z,IZ,JZ
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
