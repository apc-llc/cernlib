*
* $Id: fqnamrda.inc,v 1.1.1.1 1996/03/06 10:47:01 mclareni Exp $
*
* $Log: fqnamrda.inc,v $
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
*
* fqnamrda.inc
*
#if defined(CERNLIB_QMIBM)
      DATA  FQNAME / '/FZREAD DAT ' /
#elif defined(CERNLIB_QMIBMD)
      DATA  FQNAME / '/FZREAD DAT ' /
#elif 1
      DATA  FQNAME / 'fzread.dat  ' /
#endif
