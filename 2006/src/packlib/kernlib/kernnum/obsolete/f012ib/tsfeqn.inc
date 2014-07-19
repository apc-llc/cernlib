*
* $Id: tsfeqn.inc,v 1.1.1.1 1996/02/15 17:48:30 mclareni Exp $
*
* $Log: tsfeqn.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:30  mclareni
* Kernlib
*
*
*
* tsfeqn.inc
*
#include "basic.inc"
#include "mpy.inc"
#include "feqn.inc"
#include "tmprnt.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      5
           SETYPE
           HEADER    ENTRY               (N,A,IDIM,K,B)
 JA2       EQU       2
 I         EQU       3
 N         EQU       4
 A         EQU       5
 JA        EQU       6
 K         EQU       7
 B         EQU       8
 Z         EQU       9
 IA        EQU       10
 IA2       EQU       11
 &REGLIM   SETA      11
 HSFEQN    DS        0F
           DC        C'&T.SFEQN  '
 ENTRY     PROLOG
           LM        4,8,0(1)
 *                                       R4 = (N)
 *                                       R5 = (A)
 *                                       R6 = (IDIM)
 *                                       R7 = (K)
 *                                       R8 = (B)
           N         8,=X'7FFFFFFF'
           L         N,0(4)              R4 = N
           L         JA,0(6)             JA = IDIM
           L         K,0(7)              R7 = K
 *         IF(N.LE.0 .OR. IDIM.LT.N .OR. K.LE.0)  GOTO 99
           C         N,=F'0'
           BC        LE,SFEQN99          IF N .LE. 0
           CR        JA,N
           BC        LT,SFEQN99          IF IDIM .LT. N
           C         K,=F'0'
           BC        LE,SFEQN99          IF K .LE. 0
           LA        IA,&LENGTH          IA = WORD LENGTH
           LR        1,JA                R1 = IDIM
           MR        0,IA                R1 = IDIM * IA
           LR        JA,1                JA = IDIM * IA
           TFEQN1    N,A,IA,JA,K,B,IA2,JA2,Z,I
           B         RETURN
 SFEQN99   DS        0H
           TMPRNT    HSFEQN,N,JA,K
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
