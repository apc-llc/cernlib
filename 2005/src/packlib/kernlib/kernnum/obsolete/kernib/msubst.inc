*
* $Id: msubst.inc,v 1.1.1.1 1996/02/15 17:48:32 mclareni Exp $
*
* $Log: msubst.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:32  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_MSUBST_INC
#define CERNLIB_KERNNUM_MSUBST_INC
*
*
* msubst.inc
*
           MACRO
 &LABEL    MSUBST     &SIZE,&N,&X,&IX,&JX,&L,&R,&JR,&JX2,&XJ,&XK
 &LABEL    DS        0H
 A&SYSNDX  L         0,0(&R)             R0 = (R) = J * 2**12 + K
           LA        1,0                 R1 = 0
           L         &XJ,=F'-1'          &XJ= -1
           AR        &R,&JR              &R = (R(I+1))
           SRDL      0,&SIZE             R0 = J
           SRL       1,32-&SIZE          R1 = K
           AR        1,&XJ               R1 = K-1
           AR        &XJ,0               &XJ= J-1
           LR        &XK,&X              &XK= (X(1,1))
           MR        0,&IX               R1 = (K-1)*IX
           AR        &XK,1               &XK= (X(K,1))
           LR        1,&XJ               R1 = J-1
           LR        &XJ,&X              &XJ= (X(1,1))
           MR        0,&IX               R1 = (J-1)*IX
           AR        &XJ,1               &XJ= (X(J,1))
           VXCH2     &N,&XJ,&JX,&XK,&JX,&JX2,&JX2
           S         &L,=F'1'            &L = L-1
           BC        NE,A&SYSNDX
           MEND

#endif
