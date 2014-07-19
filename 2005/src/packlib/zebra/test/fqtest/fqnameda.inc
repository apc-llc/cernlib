*
* $Id: fqnameda.inc,v 1.2 1996/04/24 17:19:50 mclareni Exp $
*
* $Log: fqnameda.inc,v $
* Revision 1.2  1996/04/24 17:19:50  mclareni
* Cleanup to allow tests to run together on all systems
*
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
*
* fqnameda.inc
*
#if defined(CERNLIB_QMIBM)
      DATA  FQNAME / '/ZXTEST DAT1', '/ZXTEST DAT2', '/ZXTEST DAT3' /
#elif defined(CERNLIB_QMIBMD)
      DATA  FQNAME / '/ZXTEST DAT1', '/ZXTEST DAT2', '/ZXTEST DAT3' /
#elif 1
      DATA  FQNAME / 'zxtest1.dat  ', 'zxtest2.dat ', 'zxtest3.dat ' /
#endif
