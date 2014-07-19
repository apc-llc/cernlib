*
* $Id: kcgen.inc,v 1.1.1.1 1996/03/08 15:32:59 mclareni Exp $
*
* $Log: kcgen.inc,v $
* Revision 1.1.1.1  1996/03/08 15:32:59  mclareni
* Kuip
*
*
#ifndef CERNLIB_KUIP_KCGEN_INC
#define CERNLIB_KUIP_KCGEN_INC
*
*
* kcgen.inc
*
      COMMON /PAWC/ NWPAW,IXPAWC,IHBOOK,IXHIGZ,IXKUIP,IFENCE(5),
     +              LMAIN(1), WS(9989)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN(1))
      EQUIVALENCE (IQ(1),LQ(9))
      EQUIVALENCE ( Q(1),LQ(9))

      COMMON /KCLINK/
     +  LTOP,LVEC,LCMD,LACT,LMAC,LLIST,LSDUM1,
     +  LCEL,LNAME,LPAR,LPARV,LTRAV,LVECN,LCELGF,
     +  LROOT,LTOPW1,LTOPW2,LMACN,LCLIST,LLAST


#endif
