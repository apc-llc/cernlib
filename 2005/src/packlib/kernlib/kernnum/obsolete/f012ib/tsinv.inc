*
* $Id: tsinv.inc,v 1.1.1.1 1996/02/15 17:48:30 mclareni Exp $
*
* $Log: tsinv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:30  mclareni
* Kernlib
*
*
*
* tsinv.inc
*
#include "basic.inc"
#include "mpy.inc"
#include "scl.inc"
#include "cpy.inc"
#include "utl.inc"
#include "sfact.inc"
#include "sfinv.inc"
#include "tmprnt.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      9
           SETYPE
           HEADER    ENTRY               (N,A,IDIM,IFAIL)
 K         EQU       2
 I         EQU       3
 N         EQU       4
 A         EQU       5
 JA        EQU       6
 Z         EQU       7
 X         EQU       8
 Y         EQU       9
 IA        EQU       10
 IA2       EQU       11
 JA2       EQU       12
 &REGLIM   SETA      12
 HSINV     DS        0F
           DC        C'&T.SINV   '
 LOCOUT    DS        1F                  (IFAIL)
 ENTRY     PROLOG
           LM        4,7,0(1)
 *                                       R4 = (N)
 *                                       R5 = (A)
 *                                       R6 = (IDIM)
 *                                       R7 = (IFAIL)
           N         7,=X'7FFFFFFF'
           ST        7,LOCOUT
           L         N,0(4)              R4 = N
           L         JA,0(6)             JA = IDIM
 *         IF(N .LE. 0  .OR.  IDIM .LT. N)  GOTO 99
           CR        JA,N
           BC        LT,SINV99           IF IDIM .LT. N
           C         N,=F'1'
           BC        LT,SINV99           IF N .LT. 1
           LA        IA,&LENGTH          IA = WORD LENGTH
           LR        1,JA                R1 = IDIM
           MR        0,IA                R1 = IDIM * IA
           LR        JA,1                JA = IDIM * IA
           TSFACT    N,A,IA,JA,X,Y,I,Z,K
           L         K,LOCOUT            K = (IFAIL)
           ST        I,0(K)              STORE IFAIL
           C         I,=F'0'
           BC        NE,RETURN           IF IFAIL .NE. 0
 *         IF(N .EQ. 1)  RETURN
           C         N,=F'1'
           BC        EQ,RETURN           IF(N .EQ. 1)  RETURN
           TSFINV    N,A,IA,JA,IA2,JA2,X,Y,Z,I,K
           B         RETURN
 SINV99    DS        0H
           TMPRNT    HSINV,N,JA,0
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
