*
* $Id: ran.inc,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: ran.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_RAN_INC
#define CERNLIB_KERNNUM_RAN_INC
*
*
* ran.inc
*
           MACRO
 &LABEL    RANDATA
           DS        0D
 &LABEL    DC        X'402BC68CFE166D00' SEED = 0.2BC68CFE166D00
           DC        X'4C2875A2E7B17500' MULT =    2875 A2E7 B175.00
           DC        X'4E10000000000000' NORM = 10 0000 0000 0000.
           MEND
           MACRO
 &LABEL    RANF      &DATA
 &LABEL    LD        0,&DATA             F0 = 0.2BC68CFE166D00
           MXD       0,&DATA+8           F0 =      123456789ABC.DE
 *                                    F2 = .(00)123456789A0000 (EXP=-2)
           LD        4,&DATA+16       F4 = 10 0000 0000 0000.  (EXP=14)
           LD        6,=D'0'             F6 = 0
           AXR       0,4            F0 = 10 1234 5678 9ABC.
 *                                  F2 =             .DE 1234 5678 9A00
           STD       2,&DATA             SEED = .DE 1234 5678 9A00
           LDR       0,2                 F0 = NEW SEED (NOT NORMALIZED)
           ADR       0,6                 F0 = .DE 1234 5678 9A00 = RANF
           MEND
           MACRO
 &LABEL    RAN       &A,&C,&Z,&DATA
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').RRAN
           AIF       ('&T' EQ 'D').DRAN
           AIF       ('&T' EQ 'C').CRAN
           MNOTE     13,'NO TYPE DECLARED FOR RAN MACRO'
           MEXIT
 .CRAN     ANOP
           RANF      &DATA               F0 = RANF
           M&W       0,&C                F0 = C*RANF
           A&W       0,&A                F0 = C*RANF+A
           ST&W      0,&D.(&Z)           STORE IMAGINARY PART
 .RRAN     ANOP
 .DRAN     ANOP
           RANF      &DATA               F0 = RANF
           M&W       0,&C                F0 = C*RANF
           A&W       0,&A                F0 = C*RANF+A
           ST&W      0,0(&Z)             STORE REAL PART
           MEND
           MACRO
 &LABEL    VRAN      &A,&C,&N,&Z,&JZ,&J,&DATA
 &LABEL    DS        0H
 L&SYSNDX  FORONE    &J
           RAN       &A,&C,&Z,&DATA
           AR        &Z,&JZ
           ENDFOR    &J,&N,LOOP=L&SYSNDX
           MEND
           MACRO
 &LABEL    TVRAN1    &A,&C,&N,&Z,&JZ,&J,&DATA
#include "kernnum/global.inc"
           LCLA      &OLD,&Z1
 &OLD      SETA      &STACK
 &Z1       SETA      &OLD
 &STACK    SETA      &Z1+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     14,'NO WORK SPACE FOR TVRAN1'
           MEXIT
 .ALPHA    ANOP
 &LABEL    ST        &Z,&Z1.(15)         Z1 = (Z)
           VRAN      &A,&C,&N,&Z,&JZ,&J,&DATA
           L         &Z,&Z1.(15)         &Z = (Z)
 &STACK    SETA      &OLD
           MEND
           MACRO
 &LABEL    MRAN      &A,&C,&M,&N,&Z,&IZ,&JZ,&I,&J,&DATA
 &LABEL    DS        0H
 L&SYSNDX  FORONE    &I
           TVRAN1    &A,&C,&N,&Z,&JZ,&J,&DATA
           AR        &Z,&IZ              &Z = (Z(I+1,1))
           ENDFOR    &I,&M,LOOP=L&SYSNDX
           MEND
           MACRO
 &LABEL    TMRAN1    &A,&C,&M,&N,&Z,&IZ,&JZ,&I,&J,&DATA
#include "kernnum/global.inc"
           LCLA      &OLD,&Z11
 &OLD      SETA      &STACK
 &Z11      SETA      &OLD
 &STACK    SETA      &Z11+1*4
           AIF       (&STACK LE &STKLIM).ALPHA
           MNOTE     14,'NO WORK SPACE FOR TMRAN1'
           MEXIT
 .ALPHA    ANOP
 &LABEL    LA        &M-1,1              MSTEP = 1
           LA        &N-1,1              NSTEP = 1
           ST        &Z,&Z11.(15)        Z11 = (Z)
           CR        &M,&N
           BC        GT,A&SYSNDX
           MRAN      &A,&C,&M,&N,&Z,&IZ,&JZ,&I,&J,&DATA
           B         B&SYSNDX
 A&SYSNDX  MRAN      &A,&C,&N,&M,&Z,&JZ,&IZ,&J,&I,&DATA
 B&SYSNDX  L         &Z,&Z11.(15)        &Z = (Z)
 &STACK    SETA      &OLD
           MEND

#endif
