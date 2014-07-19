*
* $Id: umpy.inc,v 1.1.1.1 1996/02/15 17:48:22 mclareni Exp $
*
* $Log: umpy.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:22  mclareni
* Kernlib
*
*
*
* umpy.inc
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
 &TWORDS   SETA      6
           SETYPE
           HEADER    ENTRY               (N,X,X12,X22,Y,Y2,Z,Z2)
 N         EQU       2
 X         EQU       3
 JX        EQU       4
 IX        EQU       5
 Y         EQU       6
 JY        EQU       7
 Z         EQU       8
 IZ        EQU       9
 &REGLIM   SETA      9
 ENTRY     PROLOG
           LM        2,9,0(1)
 *                                       R2 = (N)
 *                                       R3 = (X)
 *                                       R4 = (X12)
 *                                       R5 = (X22)
 *                                       R6 = (Y)
 *                                       R7 = (Y2)
 *                                       R8 = (Z)
 *                                       R9 = (Z2) + 8000 0000
           LA        9,0(9)              R9 = (Z2)
           L         N,0(2)
           LTR       0,N                 TEST R0 = N
           BNH       RETURN              IF(N .LE. 0) RETURN
           SR        4,3                 R4 = (X12) - (X) = JX
           SR        7,6                 R7 = (Y2)  - (Y) = JY
           SR        5,3                 R5 = (X22)- (X) = IX + JX
           SR        9,8                 R9 = (Z2) - (Z) = IZ
           SR        5,4                 R5 = IX
           TXMPY     &OPC,N,N,X,IX,JX,Y,JY,Z,IZ
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
