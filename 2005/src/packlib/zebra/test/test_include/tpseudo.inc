*
* $Id: tpseudo.inc,v 1.2 2004/07/29 13:57:31 mclareni Exp $
*
* $Log: tpseudo.inc,v $
* Revision 1.2  2004/07/29 13:57:31  mclareni
* For 64-bit pointer systems put local variables referenced by LOCF
* in a dummy named common block to keep addresses in the program region.
* Changed added by cpp define CERNLIB_QMLXIA64 but it should work in all
* systems.
*
* Revision 1.1.1.1  1996/03/06 10:47:02  mclareni
* Zebra
*
*
*
* tpseudo.inc
*
#if defined(CERNLIB_QMLXIA64)
      COMMON /TEST64/ PSEU
#endif
      DIMENSION    PSEU(6200)
