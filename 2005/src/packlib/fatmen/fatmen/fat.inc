*
* $Id: fat.inc,v 1.1.1.1 1996/03/07 15:17:54 mclareni Exp $
*
* $Log: fat.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:54  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FAT_INC
#define CERNLIB_FATMEN_FAT_INC
*
*
* fat.inc
*
      PARAMETER (LURCOR=200000)
      COMMON/FAT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(LURCOR)
      DIMENSION    LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)

#endif
