*
* $Id: tmprnt.inc,v 1.1.1.1 1996/02/15 17:48:33 mclareni Exp $
*
* $Log: tmprnt.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:33  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNNUM_TMPRNT_INC
#define CERNLIB_KERNNUM_TMPRNT_INC
*
*
* tmprnt.inc
*
           MACRO
 &LABEL    TMPRNT    &HVALUE,&N,&I,&K
 &LABEL    DS        0H
           ST        &N,N&SYSNDX         SAVE N
           ST        &I,I&SYSNDX         SAVE IDIM
           ST        &K,K&SYSNDX         SAVE K
           ST        12,R12&SYSNDX       SAVE R12
           ST        13,R13&SYSNDX       SAVE R13
           ST        14,R14&SYSNDX       SAVE R14
           LR        12,15               SAVE BASE REGISTER 15 IN 12
           LA        13,S&SYSNDX         R13= (SAVE AREA)
           LA        1,L&SYSNDX          R1 = (ARGUMENT LIST)
           L         15,P&SYSNDX         R15= (TMPRNT)
           BALR      14,15               CALL TMPRNT
           LR        15,12               RESTORE BASE REGISTER 15
           L         12,R12&SYSNDX       RESTORE R12
           L         13,R13&SYSNDX       RESTORE R13
           L         14,R14&SYSNDX       RESTORE R14
           B         Z&SYSNDX            RETURN
           DS        0F
 P&SYSNDX  DC        V(TMPRNT)
           DS        0F
           DC        C'BZ00'
           DC        A(LL&SYSNDX-L&SYSNDX)
 L&SYSNDX  DC        A(&HVALUE)
           DC        A(N&SYSNDX)
           DC        A(I&SYSNDX)
           DC        AL4(K&SYSNDX+X'80000000')
 LL&SYSNDX DC        A(E&SYSNDX)
           DC        A(F&SYSNDX)
           DC        A(F&SYSNDX)
           DC        AL4(F&SYSNDX+X'80000000')
 E&SYSNDX  DS        F'8'
 F&SYSNDX  DS        F'4'
 N&SYSNDX  DS        1F                  N
 I&SYSNDX  DS        1F                  IDIM
 K&SYSNDX  DS        1F                  K
 S&SYSNDX  DS        18F                 SAVE AREA
 R12&SYSNDX  DS      1F
 R13&SYSNDX  DS      1F
 R14&SYSNDX  DS      1F
 Z&SYSNDX  DS        0H
           MEND

#endif
