*
* $Id: tsfinv.inc,v 1.1.1.1 1996/02/15 17:48:30 mclareni Exp $
*
* $Log: tsfinv.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:30  mclareni
* Kernlib
*
*
*
* tsfinv.inc
*
#include "basic.inc"
#include "mpy.inc"
#include "scl.inc"
#include "cpy.inc"
#include "utl.inc"
#include "sfinv.inc"
#include "tmprnt.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      6
           SETYPE
           HEADER    ENTRY               (N,A,IDIM)
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
 HSFINV    DS        0F
           DC        C'&T.SFINV  '
 ENTRY     PROLOG
           LM        4,6,0(1)
 *                                       R4 = (N)
 *                                       R5 = (A)
 *                                       R6 = (IDIM)
           N         6,=X'7FFFFFFF'
           L         N,0(4)              R4 = N
           L         JA,0(6)             JA = IDIM
 *         IF(N .LE. 0  .OR.  IDIM .LT. N)  GOTO 99
           CR        JA,N
           BC        LT,SFINV99          IF IDIM .LT. N
           C         N,=F'1'
           BC        LT,SFINV99          IF N .LT. 1
 *         IF(N .EQ. 1)  RETURN
           BC        EQ,RETURN           IF(N .EQ. 1)  RETURN
           LA        IA,&LENGTH          IA = WORD LENGTH
           LR        1,JA                R1 = IDIM
           MR        0,IA                R1 = IDIM * IA
           LR        JA,1                JA = IDIM * IA
           TSFINV    N,A,IA,JA,IA2,JA2,X,Y,Z,I,K
           B         RETURN
 SFINV99   DS        0H
           TMPRNT    HSFINV,N,JA,0
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
