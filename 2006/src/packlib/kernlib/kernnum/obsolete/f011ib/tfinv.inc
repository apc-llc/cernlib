*
* $Id: tfinv.inc,v 1.1.1.1 1996/02/15 17:48:29 mclareni Exp $
*
* $Log: tfinv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:29  mclareni
* Kernlib
*
*
*
* tfinv.inc
*
#include "basic.inc"
#include "xch.inc"
#include "mpy.inc"
#include "scl.inc"
#include "msubst.inc"
#include "finv.inc"
#include "tmprnt.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      7
           SETYPE
           HEADER    ENTRY
 JA2       EQU       2
 I         EQU       3
 N         EQU       4
 A         EQU       5
 JA        EQU       6
 R         EQU       7
 X         EQU       8
 Y         EQU       9
 IA        EQU       10
 Z         EQU       11
 IA2       EQU       12
 &REGLIM   SETA      12
 HFINV     DS        0F
           DC        C' &T.FINV  '
 ENTRY     PROLOG
           LM        4,7,0(1)
 *                                       R4 = (N)
 *                                       R5 = (A)
 *                                       R6 = (IDIM)
 *                                       R7 = (R) AND TAG IN BIT 0
           N         7,=X'7FFFFFFF'
           L         N,0(4)
           L         JA,0(6)             JA = IDIM
 *         IF(N .LE. 0  .OR.  IDIM .LT. N)  GOTO 99
           C         N,=F'0'
           BC        LE,FINV99           IF N .LE. 0
           CR        JA,N
           BC        LT,FINV99           IF IDIM .LT. N
           LA        IA,&LENGTH          IA = WORD LENGTH
           LR        1,JA                R1 = IDIM
           MR        0,IA                R1 = IDIM * IA
           LR        JA,1                JA = IDIM * IA
           TFINV     12,N,A,IA,JA,R,IA2,JA2,X,Y,Z,I
           B         RETURN
 FINV99    DS        0H
           TMPRNT    HFINV,N,JA,0
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
