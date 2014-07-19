*
* $Id: tsfact.inc,v 1.1.1.1 1996/02/15 17:48:30 mclareni Exp $
*
* $Log: tsfact.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:30  mclareni
* Kernlib
*
*
*
* tsfact.inc
*
#include "basic.inc"
#include "mpy.inc"
#include "scl.inc"
#include "sfact.inc"
#include "det.inc"
#include "tmprnt.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      9
           SETYPE
           HEADER    ENTRY               (N,A,IDIM,IFAIL,DET,JFAIL)
 J         EQU       2                   (DIFFERS FROM 7 - 9)
 I         EQU       3                   (DIFFERS FROM 7 - 9)
 N         EQU       4
 A         EQU       5
 JA        EQU       6
 L         EQU       7
 IA        EQU       8
 X         EQU       9
 Y         EQU       10
 &REGLIM   SETA      10
 HSFACT    DS        0F
           DC        C'&T.SFACT  '
 LOCOUT    DS        3F                  (IFAIL), (DET), (JFAIL)
 ENTRY     PROLOG
           LM        4,9,0(1)
 *                                       R4 = (N)
 *                                       R5 = (A)
 *                                       R6 = (IDIM)
 *                                       R7 = (IFAIL)
 *                                       R8 = (DET)
 *                                       R9 = (JFAIL)
           N         9,=X'7FFFFFFF'
           STM       7,9,LOCOUT
           L         N,0(4)              R4 = N
           L         JA,0(6)             JA = IDIM
 *         IF(N.LE.0 .OR. IDIM.LT.N)  GOTO 99
           C         N,=F'0'
           BC        LE,SFACT99          IF N .LE. 0
           CR        JA,N
           BC        LT,SFACT99          IF IDIM .LT. N
           LA        IA,&LENGTH          IA = WORD LENGTH
           LR        1,JA                R1 = IDIM
           MR        0,IA                R1 = IDIM * IA
           LR        JA,1                JA = IDIM * IA
           TSFACTD   N,A,IA,JA,X,Y,I,J,L
 *                                       F0,F2  =  DET
 *                                       I      =  IFAIL
 *                                       J      =  JFAIL
           LM        7,9,LOCOUT
 *                                       R7 = (IFAIL)
 *                                       R8 = (DET)
 *                                       R9 = (JFAIL)
           ST        I,0(7)              STORE IFAIL
           STORE     0,8                 STORE DET
           ST        J,0(9)              STORE JFAIL
           B         RETURN
 SFACT99   DS        0H
           TMPRNT    HSFACT,N,JA,0
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
