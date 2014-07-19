*
* $Id: mmpy.inc,v 1.1.1.1 1996/02/15 17:48:22 mclareni Exp $
*
* $Log: mmpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:22  mclareni
* Kernlib
*
*
*
* mmpy.inc
*
#include "basic.inc"
#include "mpy.inc"
           MACRO
           MAIN
#include "global.inc"
           LCLC      &OPC
 &OPC      SETC      '&NAME'(1,4)
           AIF       (&TYPE NE &CCTYPE).ALPHA
 &OPC      SETC      '&NAME'(2,4)
 .ALPHA    ANOP
 &TWORDS   SETA      5
           SETYPE
           HEADER    ENTRY               (M,N,X,X12,X21,Y,Y2,Z,Z2)
 M         EQU       2
 N         EQU       3
 X         EQU       4
 JX        EQU       5
 IX        EQU       6
 Y         EQU       7
 JY        EQU       8
 Z         EQU       9
 IZ        EQU       10
 &REGLIM   SETA      10
 ENTRY     PROLOG
           LM        2,10,0(1)
 *                                       R2 = (M)
 *                                       R3 = (N)
 *                                       R4 = (X)
 *                                       R5 = (X12)
 *                                       R6 = (X21)
 *                                       R7 = (Y)
 *                                       R8 = (Y2)
 *                                       R9 = (Z)
 *                                       R10= (Z2) + 8000 0000
           LA        10,0(10)            R10= (Z2)
           L         M,0(2)
           L         N,0(3)
           LTR       0,M                 TEST R0 = M
           BNH       RETURN              IF(M .LE. 0) RETURN
           LTR       0,N                 TEST R0 = N
           BNH       RETURN              IF(N .LE. 0) RETURN
           SR        6,4                 R6 = (X21)- (X) = IX
           SR        10,9                R10= (Z2) - (Z) = IZ
           SR        5,4                 R5 = (X12) - (X)
           SR        8,7                 R8 = (Y2)  - (Y)
           TXMPY     &OPC,M,N,X,IX,JX,Y,JY,Z,IZ
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
