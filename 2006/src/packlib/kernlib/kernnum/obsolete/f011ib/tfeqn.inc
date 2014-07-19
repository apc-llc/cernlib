*
* $Id: tfeqn.inc,v 1.1.1.1 1996/02/15 17:48:29 mclareni Exp $
*
* $Log: tfeqn.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:29  mclareni
* Kernlib
*
*
*
* tfeqn.inc
*
#include "basic.inc"
#include "xch.inc"
#include "mpy.inc"
#include "msubst.inc"
#include "feqn.inc"
#include "tmprnt.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      5
           SETYPE
           HEADER    ENTRY
 Z         EQU       2
 I         EQU       3
 N         EQU       4
 A         EQU       5
 JA        EQU       6
 R         EQU       7
 K         EQU       8
 B         EQU       9
 IA        EQU       10
 IA2       EQU       11
 JA2       EQU       12
 &REGLIM   SETA      12
 HFEQN     DS        0F
           DC        C' &T.FEQN  '
 ENTRY     PROLOG
           LM        4,9,0(1)
 *                                       R4 = (N)
 *                                       R5 = (A)
 *                                       R6 = (IDIM)
 *                                       R7 = (R)
 *                                       R8 = (K)
 *                                       R9 = (B)
           N         9,=X'7FFFFFFF'
           L         N,0(4)
           L         JA,0(6)             JA = IDIM
           L         K,0(8)
 *         IF(N.LE.0 .OR. IDIM.LT.N .OR. K.LE.0)  GOTO 99
           C         N,=F'0'
           BC        LE,FEQN99           IF N .LE. 0
           CR        JA,N
           BC        LT,FEQN99           IF IDIM .LT. N
           C         K,=F'0'
           BC        LE,FEQN99           IF K .LE. 0
           LA        IA,&LENGTH          IA = WORD LENGTH
           LR        1,JA                R1 = IDIM
           MR        0,IA                R1 = IDIM * IA
           LR        JA,1                JA = IDIM * IA
           TFEQN     12,N,A,IA,JA,R,K,B,IA2,JA2,Z,I
           B         RETURN
 FEQN99    DS        0H
           TMPRNT    HFEQN,N,JA,K
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
