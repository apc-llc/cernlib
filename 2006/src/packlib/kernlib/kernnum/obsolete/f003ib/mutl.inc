*
* $Id: mutl.inc,v 1.1.1.1 1996/02/15 17:48:22 mclareni Exp $
*
* $Log: mutl.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:22  mclareni
* Kernlib
*
*
*
* mutl.inc
*
#include "basic.inc"
#include "cpy.inc"
#include "utl.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      3
           SETYPE
           HEADER    ENTRY               (N,X,X12,X21)
 Z         EQU       2
 N         EQU       3
 X         EQU       4
 JX        EQU       5
 IX        EQU       6
 &REGLIM   SETA      6
 ENTRY     PROLOG
           LM        3,6,0(1)
 *                                       R3 = (N)
 *                                       R4 = (X)
 *                                       R5 = (X12)
 *                                       R6 = (X21) + 8000 0000
           LA        6,0(6)              R6 = (X21)
           L         N,0(3)
           LTR       0,N                 TEST R0 = N
           BNH       RETURN              IF(N .LE. 0) RETURN
           SR        5,4                 R5 = (X12) - (X) = JX
           SR        6,4                 R6 = (X21) - (X) = IX
           TMUTL     N,X,IX,JX,Z
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
