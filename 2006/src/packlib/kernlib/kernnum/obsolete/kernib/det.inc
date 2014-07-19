*
* $Id: det.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: det.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_DET_INC
#define CERNLIB_KERNNUM_DET_INC
*
*
* det.inc
*
           MACRO
 &LABEL    DET1      &X,&I,&J
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').REAL
           AIF       ('&T' EQ 'D').REAL
           AIF       ('&T' EQ 'C').COMPLEX
           MEXIT
 .REAL     LA        0,0                 R0 = 0
           L         1,0(&X)             R1 = UPPER X
           SLDL      0,1                 R0 = SIGN(X)
           SLL       0,7                 R0 = SIGN(X) & ZERO CHAR
           A         0,=F'64'
           STC       0,0(&X)             STORE SCALED EXPONENT OF X
           SRL       1,1+24              R1 = CHAR(X)
           S         1,=F'64'            R1 = EXP(X)
           AR        &I,1                &I = &I+EXP(X)
           MEXIT
 .COMPLEX  L         1,0(&X)             R1 = UPPER X'
           SLL       1,1
           SRL       1,1+24              R1 = CHAR(X')
           LR        &J,1                &J = CHAR(X')
           L         1,&D.(&X)           R1 = UPPER X''
           SLL       1,1
           SRL       1,1+24              R1 = CHAR(X'')
           CR        &J,1
           BC        GE,A&SYSNDX         IF &J = MAX CHAR
           LR        &J,1
 A&SYSNDX  DS        0H                  &J = MAX CHAR
           S         &J,=F'64'           &J = MAX EXP
           AR        &I,&J               &I = &I + MAX EXP
           LA        0,0                 R0 = 0
           L         1,0(&X)             R1 = UPPER X'
           SLDL      0,1
           SLL       0,7                 R0 = SIGN(X') * 2**7
           SRL       1,1+24              R1 = CHAR(X')
           SR        1,&J                R1 = SCALED CHAR(X')
           BC        LT,C&SYSNDX         IF EXPONENT UNDERFLOW
           OR        1,0                 R1 = SIGN & SCALED CHAR
           STC       1,0(&X)             STORE EXPONENT BYTE
 B&SYSNDX  LA        0,0                 R0 = 0
           L         1,&D.(&X)           R1 = UPPER X''
           SLDL      0,1
           SLL       0,7                 R0 = SIGN(X'') * 2**7
           SRL       1,1+24              R1 = CHAR(X'')
           SR        1,&J                R1 = SCALED CHAR(X'')
           BC        LT,D&SYSNDX         IF EXPONENT UNDERFLOW
           OR        1,0                 R1 = SIGN & SCALED CHAR
           STC       1,&D.(&X)           STORE EXPONENT BYTE
           B         E&SYSNDX
 C&SYSNDX  SDR       0,0                 F0 = 0.
           ST&W      0,0(&X)             X' = 0.
           B         B&SYSNDX
 D&SYSNDX  SDR       0,0                 F0 = 0.
           ST&W      0,&D.(&X)           X'' = 0.
 E&SYSNDX  DS        0H
           MEND
           MACRO
 &LABEL    DET2      &X,&I,&J
#include "kernnum/global.inc"
 &LABEL    DS        0H
           AIF       ('&T' EQ 'R').REAL
           AIF       ('&T' EQ 'D').REAL
           AIF       ('&T' EQ 'C').COMPLEX
           MEXIT
 .REAL     IC        1,0(&X)             R1 = SIGN & CHAR (REAL PART)
           SLDL      0,24+1              R0 = SIGN
           SRL       1,24+1              R1 = CHAR
           AR        1,&I                R1 = CHAR + EXP(SCALE)
           BC        LT,A&SYSNDX         IF UNDERFLOW
           C         1,=F'127'
           BC        GT,B&SYSNDX         IF OVERFLOW
           SLL       1,24+1
           SRDL      0,24+1              R1 = SIGN & NEW CHAR
           STC       1,0(&X)
           LA        &I,0                JFAIL = 0
           B         C&SYSNDX
 A&SYSNDX  CLEAR     0                   F0 = 0.
           STORE     0,&X                DET = 0.
           L         &I,=F'-1'           JFAIL = -1
           B         C&SYSNDX
 B&SYSNDX  L         1,=X'7F800000'
           ST        1,0(&X)             DET = LARGE
           LA        &I,1                JFAIL = +1
 C&SYSNDX  DS        0H
           MEXIT
 .COMPLEX  LA        &J,0                &J = 0
           IC        1,&D.(&X)           R1 = SIGN & CHAR (IMAG PART)
           SLDL      0,24+1              R0 = SIGN
           SRL       1,24+1              R1 = CHAR
           AR        1,&I                R1 = CHAR + EXP(SCALE)
           BC        LT,A&SYSNDX         IF UNDERFLOW OF IMAG PART
           C         1,=F'127'
           BC        GT,D&SYSNDX         IF OVERFLOW OF IMAG PART
           SLL       1,24+1
           SRDL      0,24+1              R1 = SIGN & NEW CHAR
           STC       1,&D.(&X)
           B         B&SYSNDX
 A&SYSNDX  SDR       0,0                 F0 = 0.
           ST&W      0,&D.(&X)           AIMAG(DET) = 0.
           LA        &J,1                &J = 1
 B&SYSNDX  DS        0H
           IC        1,0(&X)             R1 = SIGN & CHAR (REAL PART)
           SLDL      0,24+1              R0 = SIGN
           SRL       1,24+1              R1 = CHAR
           AR        1,&I                R1 = CHAR + EXP(SCALE)
           BC        LT,C&SYSNDX         IF UNDERFLOW OF REAL PART
           C         1,=F'127'
           BC        GT,D&SYSNDX         IF OVERFLOW OF REAL PART
           SLL       1,24+1
           SRDL      0,24+1              R1 = SIGN & NEW CHAR
           STC       1,0(&X)
           LA        &I,0                JFAIL = 0
           B         E&SYSNDX
 C&SYSNDX  SDR       0,0                 F0 = 0.
           ST&W      0,0(&X)             REAL(DET) = 0.
           LA        &I,0                JFAIL = 0
           C         &J,=F'0'
           BC        EQ,E&SYSNDX
           L         &I,=F'-1'           JFAIL = -1
           B         E&SYSNDX
 D&SYSNDX  L         1,=X'7E800000'
           ST        1,0(&X)             DET = LARGE
           ST        1,&D.(&X)
           LA        &I,1                JFAIL = +1
 E&SYSNDX  DS        0H
           MEND

#endif
