*
* $Id: fqtopmk.inc,v 1.1.1.1 1996/03/06 10:47:01 mclareni Exp $
*
* $Log: fqtopmk.inc,v $
* Revision 1.1.1.1  1996/03/06 10:47:01  mclareni
* Zebra
*
*
*
* fqtopmk.inc
*
#if defined(CERNLIB_QMIBM)
#if defined(CERNLIB_QFIASC)
      CALL FILEINF (IBMRC, 'RECFM','F', 'LRECL',80)
#endif
#endif
#if defined(CERNLIB_QMIBM)
#if defined(CERNLIB_QFIXDANY)
      CALL FILEINF (IBMRC, 'RECFM','F', 'LRECL',3600)
#endif
#endif
*
* fqtopmk.inc
*
#if defined(CERNLIB_QMIBM)
#if defined(CERNLIB_QFIDDANY)
      CALL FILEINF (IBMRC, 'MAXREC', 600)
#endif
#endif
*
* fqtopmk.inc
*
#if defined(CERNLIB_QMIBMD)
#if defined(CERNLIB_QFIASC)
      CALL FILEINF (IBMRC, 'RECFM','F', 'LRECL',80)
#endif
#endif
#if defined(CERNLIB_QMIBMD)
#if defined(CERNLIB_QFIXDANY)
      CALL FILEINF (IBMRC, 'RECFM','F', 'LRECL',3600)
#endif
#endif
*
* fqtopmk.inc
*
#if defined(CERNLIB_QMIBMD)
#if defined(CERNLIB_QFIDDANY)
      CALL FILEINF (IBMRC, 'MAXREC', 600)
#endif
#endif
