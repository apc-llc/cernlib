*
* $Id: tzmain1.inc,v 1.1.1.1 1996/03/06 10:47:05 mclareni Exp $
*
* $Log: tzmain1.inc,v $
* Revision 1.1.1.1  1996/03/06 10:47:05  mclareni
* Zebra
*
*
*
* tzmain1.inc
*
#if defined(CERNLIB_QMIBM)||defined(CERNLIB_QMIBMD)
      OPEN (16,FILE='/ZXTEST DAT',STATUS='OLD')
#endif
*
* tzmain1.inc
*
#if defined(CERNLIB_QMND3)
      OPEN (16,FILE='ZXTEST:DAT')
#endif
*
* tzmain1.inc
*
#if defined(CERNLIB_QMPDP)
#endif
