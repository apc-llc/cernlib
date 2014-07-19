*
* $Id: mran.inc,v 1.1.1.1 1996/02/15 17:48:22 mclareni Exp $
*
* $Log: mran.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:22  mclareni
* Kernlib
*
*
*
* mran.inc
*
#include "basic.inc"
#include "ran.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      23
           SETYPE
           HEADER    ENTRY               (M,N,A,B,Z,Z12,Z21)
           AIF       ('&T' EQ 'D').ALPHA
 A         DS        1F
 C         DS        1F
           AGO       .BETA
 .ALPHA    ANOP
           DS        0D
 A         DS        1D
 C         DS        1D
 .BETA     ANOP
 GET       DC        V(RANGET)
 SET       DC        V(RANSET)
 LIST      DC        A(SEED)
 SEED      DS        2F                  002BC68C 00FE166D
 DATA      RANDATA                       SEED IS FIRST DOUBLE WORD
 NSTEP     EQU       2
 N         EQU       3
 MSTEP     EQU       4
 M         EQU       5
 Z         EQU       6
 JZ        EQU       7
 IZ        EQU       8
 JZ2       EQU       9
 I         EQU       10
 J         EQU       11
 &REGLIM   SETA      11
 ENTRY     PROLOG
           LM        2,8,0(1)
 *                                       R2 = (M)
 *                                       R3 = (N)
 *                                       R4 = (A)
 *                                       R5 = (B)
 *                                       R6 = (Z)
 *                                       R7 = (Z12)
 *                                       R8 = (Z21) + 8000 0000
           LA        8,0(8)              R8 = (Z21)
           L&W       0,0(4)              F0 = A
           L&W       2,0(5)              F2 = B
           S&W.R     2,0                 F2 = B-A
           ST&W      0,A                 STORE A
           ST&W      2,C                 C = B-A
           L         M,0(2)              R5 = M
           L         N,0(3)              R3 = N
           LTR       0,M                 TEST R0 = M
           BNH       RETURN              IF(M .LE. 0) RETURN
           LTR       0,N                 TEST R0 = N
           BNH       RETURN              IF(N .LE. 0) RETURN
           SR        7,6                 R7 = (Z12) - (Z) = JZ
           SR        8,6                 R8 = (Z21) - (Z) = IZ
           ENTER     GET,LIST            CALL RANGET(SEED)
           LM        0,1,SEED            R0 = 002BC68C
 *                                       R1 = 00FE166D
           A         0,=X'40000000'      R0 = 402BC68C
           SLL       1,8                 R1 = FE166D00
           STM       0,1,DATA            DATA = 402BC68C FE166D00
           TMRAN1    A,C,M,N,Z,IZ,JZ,I,J,DATA
           LM        0,1,DATA            R0 = 402BC68C
 *                                       R1 = FE166D00
           N         0,=X'00FFFFFF'      R0 = 002BC68C
           SRL       1,8                 R1 = 00FE166D
           STM       0,1,SEED            SEED = 002BC68C 00FE166D
           ENTER     SET,LIST            CALL RANSET(SEED)
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
