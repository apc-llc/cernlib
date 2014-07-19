*
* $Id: vran.inc,v 1.1.1.1 1996/02/15 17:48:18 mclareni Exp $
*
* $Log: vran.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:18  mclareni
* Kernlib
*
*
*
* vran.inc
*
#include "basic.inc"
#include "ran.inc"
           MACRO
           MAIN
#include "global.inc"
 &TWORDS   SETA      22
           SETYPE
           HEADER    ENTRY               (N,A,B,Z,Z2)
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
 Z         EQU       6
 JZ        EQU       7
 J         EQU       8
 &REGLIM   SETA      8
 ENTRY     PROLOG
           LM        3,7,0(1)
 *                                       R3 = (N)
 *                                       R4 = (A)
 *                                       R5 = (B)
 *                                       R6 = (Z)
 *                                       R7 = (Z2)
           N         7,=X'7FFFFFFF'
           L&W       0,0(4)              F0 = A
           L&W       2,0(5)              F2 = B
           S&W.R     2,0                 F2 = B-A
           ST&W      0,A                 STORE A
           ST&W      2,C                 C = B-A
           L         N,0(3)
           LA        NSTEP,1             NSTEP=1
           C         N,=F'0'
           BC        LE,RETURN           IF(N .LE. 0) RETURN
           SR        7,6                 R7 = (Z2) - (Z) = JZ
           ENTER     GET,LIST            CALL RANGET(SEED)
           LM        0,1,SEED            R0 = 002BC68C
 *                                       R1 = 00FE166D
           A         0,=X'40000000'      R0 = 402BC68C
           SLL       1,8                 R1 = FE166D00
           STM       0,1,DATA            DATA = 402BC68C FE166D00
           VRAN      A,C,N,Z,JZ,J,DATA
           LM        0,1,DATA            R0 = 402BC68C
 *                                       R1 = FE166D00
           N         0,=X'00FFFFFF'      R0 = 002BC68C
           SRL       1,8                 R1 = 00FE166D
           STM       0,1,SEED            SEED = 002BC68C 00FE166D
           ENTER     SET,LIST            CALL RANSET(SEED)
 RETURN    EPILOG
           MEND
#include "tglobal.inc"
