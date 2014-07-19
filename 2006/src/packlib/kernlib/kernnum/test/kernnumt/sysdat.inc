*
* $Id: sysdat.inc,v 1.1.1.1 1996/02/15 17:48:37 mclareni Exp $
*
* $Log: sysdat.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:37  mclareni
* Kernlib
*
*
*
* sysdat.inc
*
                    PARAMETER(LIMPK = 100)
                    PARAMETER(LMTIME = 20)
#if defined(CERNLIB_NUMHIPRE)
          REAL                SEEDIN
#endif
#if defined(CERNLIB_NUMLOPRE)
          DOUBLE PRECISION    SEEDIN
#endif
          COMMON    /SYSNUM/  SEEDIN
          COMMON    /SYSNUM/  LGFILE,   IOUNIT
          COMMON    /SYSNUM/  RELPRT(2)
          EQUIVALENCE        (RELPRT(1),RELPR)
          DOUBLE PRECISION DRANGE
          COMMON    /SYSNUM/  DRANGE, ERANGE, MXBINE,   MXBIND
          COMMON    /SYSNUM/  CHKTIM(0:4),        PKTIME(LIMPK)
          COMMON    /SYSNUM/  KNTIME
          LOGICAL             ERPRNT,   ERSTOP
          COMMON    /SYSNUM/  ERPRNT,   ERSTOP
          CHARACTER*4         NAMEPK,   PKNAME
          COMMON    /SYSCHR/  NAMEPK,   PKNAME(LIMPK)
          CHARACTER*30        SPTIME
          COMMON    /SYSCHR/  SPTIME(LMTIME)

