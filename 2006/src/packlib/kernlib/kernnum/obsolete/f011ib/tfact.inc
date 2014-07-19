*
* $Id: tfact.inc,v 1.1.1.1 1996/02/15 17:48:29 mclareni Exp $
*
* $Log: tfact.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:29  mclareni
* Kernlib
*
*
*
* tfact.inc
*
#include "basic.inc"
#include "mxa.inc"
#include "xch.inc"
#include "mpy.inc"
#include "scl.inc"
#include "fact.inc"
#include "det.inc"
#include "tmprnt.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      11
           SETYPE
           HEADER    ENTRY
 ISTEP     EQU       2
 I         EQU       3                   (DIFFERS FROM 8 - 10)
 N         EQU       4
 A         EQU       5
 JA        EQU       6
 R         EQU       7
 IA        EQU       8
 X         EQU       9
 Y         EQU       10
 J         EQU       11                  (DIFFERS FROM 8 - 10)
 L         EQU       12
 &REGLIM   SETA      12
 HFACT     DS        0F
           DC        C' &T.FACT  '
 LOCOUT    DS        3F                  (IFAIL), (DET), (JFAIL)
 ENTRY     PROLOG
           LM        4,10,0(1)
 *                                       R4 = (N)
 *                                       R5 = (A)
 *                                       R6 = (IDIM)
 *                                       R7 = (R)
 *                                       R8 = (IFAIL)
 *                                       R9 = (DET)
 *                                       R10= (JFAIL)
           N         10,=X'7FFFFFFF'
           STM       8,10,LOCOUT
           L         N,0(4)
           L         JA,0(6)             JA = IDIM
 *         IF(N.LE.0 .OR. IDIM.LT.N)  GOTO 99
           C         N,=F'0'
           BC        LE,FACT99           IF N .LE. 0
           CR        JA,N
           BC        LT,FACT99           IF IDIM .LT. N
           LA        IA,&LENGTH          IA = WORD LENGTH
           LR        1,JA                R1 = IDIM
           MR        0,IA                R1 = IDIM * IA
           LR        JA,1                JA = IDIM * IA
           TFACT     12,N,A,IA,JA,R,X,Y,I,J,L
 *                                       F0,F2  =  DET
 *                                       I      =  IFAIL
 *                                       J      =  JFAIL
           LM        8,10,LOCOUT
           ST        I,0(8)              STORE IFAIL
           STORE     0,9                 STORE DET
           ST        J,0(10)             STORE JFAIL
           B         RETURN
 FACT99    DS        0H
           TMPRNT    HFACT,N,JA,0
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
