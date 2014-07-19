*
* $Id: hck.inc,v 1.1.1.1 1996/03/08 15:44:16 mclareni Exp $
*
* $Log: hck.inc,v $
* Revision 1.1.1.1  1996/03/08 15:44:16  mclareni
* Cspack
*
*
#ifndef CERNLIB_CSPACK_HCK_INC
#define CERNLIB_CSPACK_HCK_INC
*
*
* hck.inc
*
      INTEGER   KNCX  ,KXMIN  ,KXMAX  ,KBWIDX  ,KMIN  ,KMAX  ,KNORM  ,
     +          KTIT1   ,KNCY  ,KYMIN  ,KYMAX  ,KBWIDY   ,KSCAL2   ,
     +          KTIT2   ,KNBIT  ,KNOENT  ,KSTAT1  ,KNSDIR  ,KNRH  ,
     +          KCON1  ,KCON2  ,KBITS  ,KNTOT
      PARAMETER(KNCX=3,KXMIN=4,KXMAX=5,KBWIDX=6,KMIN=7,KMAX=8,KNORM=9,
     +          KTIT1=10,KNCY=7,KYMIN=8,KYMAX=9,KBWIDY=10,KSCAL2=11,
     +          KTIT2=12,KNBIT=1,KNOENT=2,KSTAT1=3,KNSDIR=5,KNRH=6,
     +          KCON1=9,KCON2=3,KBITS=1,KNTOT=2)
*

#endif
