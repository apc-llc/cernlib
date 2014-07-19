*
* $Id: tseqn.inc,v 1.1.1.1 1996/02/15 17:48:30 mclareni Exp $
*
* $Log: tseqn.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:30  mclareni
* Kernlib
*
*
*
* tseqn.inc
*
#include "basic.inc"
#include "mpy.inc"
#include "scl.inc"
#include "sfact.inc"
#include "feqn.inc"
#include "tmprnt.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      9
           SETYPE
           HEADER    ENTRY               (N,A,IDIM,IFAIL,K,B)
 IA        EQU       2
 I         EQU       3
 N         EQU       4
 A         EQU       5
 JA        EQU       6
 L         EQU       7
 K         EQU       8
 B         EQU       9
 JA2       EQU       10
 X         EQU       11
 Y         EQU       12
 &REGLIM   SETA      12
 HSEQN     DS        0F
           DC        C'&T.SEQN   '
 LOCOUT    DS        3F                  (IFAIL) (K) (B)
 ENTRY     PROLOG
           LM        4,9,0(1)
 *                                       R4 = (N)
 *                                       R5 = (A)
 *                                       R6 = (IDIM)
 *                                       R7 = (IFAIL)
 *                                       R8 = (K)
 *                                       R9 = (B)
           N         9,=X'7FFFFFFF'
           STM       7,9,LOCOUT
           L         N,0(4)              R4 = N
           L         JA,0(6)             JA = IDIM
           L         K,0(8)              R8 = K
 *         IF(N.LE.0 .OR. IDIM.LT.N .OR. K.LE.0)  GOTO 99
           C         N,=F'0'
           BC        LE,SEQN99           IF N .LE. 0
           CR        JA,N
           BC        LT,SEQN99           IF IDIM .LT. N
           C         K,=F'0'
           BC        LE,SEQN99           IF K .LE. 0
           LA        IA,&LENGTH          IA = WORD LENGTH
           LR        1,JA                R1 = IDIM
           MR        0,IA                R1 = IDIM * IA
           LR        JA,1                JA = IDIM * IA
           TSFACT    N,A,IA,JA,X,Y,I,B,L
           LM        7,9,LOCOUT
 *                                       R7 = (IFAIL)
 *                                       R8 = (K)
 *                                       R9 = (B)
           ST        I,0(7)              STORE IFAIL
           L         K,0(8)              R8 = K
           C         I,=F'0'
           BC        NE,RETURN           IF IFAIL .NE. 0
           TFEQN1    N,A,IA,JA,K,B,X,JA2,Y,I
           B         RETURN
 SEQN99    DS        0H
           TMPRNT    HSEQN,N,JA,K
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
