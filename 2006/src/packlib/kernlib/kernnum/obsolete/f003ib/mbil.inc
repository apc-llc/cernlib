*
* $Id: mbil.inc,v 1.1.1.1 1996/02/15 17:48:22 mclareni Exp $
*
* $Log: mbil.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:22  mclareni
* Kernlib
*
*
*
* mbil.inc
*
#include "basic.inc"
#include "mpy.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      7
           SETYPE
           HEADER    ENTRY               (N,X,X2,Y,Y12,Y21,Z,Z2)
 JZ2       EQU       2
 N         EQU       3
 X         EQU       4
 IX        EQU       5
 Y         EQU       6
 JY        EQU       7
 IY        EQU       8
 Z         EQU       9
 JZ        EQU       10
 JY2       EQU       11
 &REGLIM   SETA      11
 ENTRY     PROLOG
           LM        3,10,0(1)
 *                                       R3 = (N)
 *                                       R4 = (X)
 *                                       R5 = (X2)
 *                                       R6 = (Y)
 *                                       R7 = (Y12)
 *                                       R8 = (Y21)
 *                                       R9 = (Z)
 *                                       R10= (Z2) + 8000 0000
           LA        10,0(10)            R10= (Z2)
           L         N,0(3)              R3 = N
           CLEAR     0                   F0 [,F2] = 0.
           LTR       0,N                 TEST R0 = N
           BNH       RETURN              IF(N .LE. 0) RETURN
           SR        5,4                 R5 = (X2) - (X) = IX
           SR        7,6                 R7 = (Y12)- (Y) = JY
           SR        8,6                 R8 = (Y21)- (Y) = IY
           SR        10,9                R10= (Z2) - (Z) = JZ
           TMBIL     N,X,IX,Y,IY,JY,Z,JZ,JY2,JZ2
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
