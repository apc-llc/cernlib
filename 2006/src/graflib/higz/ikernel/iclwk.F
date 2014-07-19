*
* $Id: iclwk.F,v 1.2 1996/09/25 14:58:42 couet Exp $
*
* $Log: iclwk.F,v $
* Revision 1.2  1996/09/25 14:58:42  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/01 08/05/95  11.41.29  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE ICLWK(IWKID)
      ENTRY      GCLWK(IWKID)
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#if defined(CERNLIB_PSCRIPT)
#include "higz/hipost.inc"
#endif
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/himeta.inc"
*
      IF(IGIWIN(IWKID).EQ.0)THEN
         CALL IGERR('This workstation is not open','ICLWK')
         RETURN
      ENDIF
#if !defined(CERNLIB_BATCH)
      IF(IGIWTY(IWKID).GT.0.AND.GFLAG)THEN
#endif
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
         CALL WINCLO(IWINID(IGIWIN(IWKID)))
#endif
#if defined(CERNLIB_MSDOS)
         IF(IGIWTY(IWKID).EQ.ISURF.AND.IWKID.EQ.IDMETA) THEN
            CALL GOLCLO
         ELSE
            CALL TEXT_MODE
         ENDIF
#endif
#if defined(CERNLIB_FALCO)
         ITY = IGIWTY(IWKID)
         IF(ITY.EQ.7878.OR.ITY.EQ.7879)THEN
            IWTYPE = 0
            GOTO 10
         ENDIF
#endif
#if defined(CERNLIB_X11)
         CALL IXSELWI(IWINID(IGIWIN(IWKID)))
         CALL IXCLSWI
#endif
#if defined(CERNLIB_MACMPW)
         CALL IMCLSWI
#endif
#if defined(CERNLIB_PHIGS)
         CALL PCLWK(IWKID)
#endif
#if !defined(CERNLIB_BATCH)
         GOTO 10
      ENDIF
#endif
#if defined(CERNLIB_PSCRIPT)
      IF(IGIWTY(IWKID).LT.0)THEN
         CALL IPEND
         IF(IWKID.EQ.IDMETA.AND.FILOPN)FILOPN=.FALSE.
         GOTO 10
      ENDIF
#endif
   10 CONTINUE
      DO 30 I=1,INOPWK
         IF(ILOPWK(I).EQ.IWKID)THEN
            INOPWK=INOPWK-1
            DO 20 J=I,INOPWK
               ILOPWK(J) = ILOPWK(J+1)
               IWTYL(J)  = IWTYL(J+1)
#if defined(CERNLIB_PSCRIPT)
               IF (IWTYL(J).LT.0) IPSI = J
#endif
               ACWKFL(J) = ACWKFL(J+1)
               XWKSIZ(J) = XWKSIZ(J+1)
               YWKSIZ(J) = YWKSIZ(J+1)
               WKMAX(J)  = WKMAX(J+1)
               IWINID(J) = IWINID(J+1)
               NODRFL(J) = NODRFL(J+1)
#if defined(CERNLIB_KERNEL)
               WKSC(J)   = WKSC(J+1)
               XWKR(J)   = XWKR(J+1)
               YWKR(J)   = YWKR(J+1)
               XWKW1(J)  = XWKW1(J+1)
               YWKW1(J)  = YWKW1(J+1)
               XWKV1(J)  = XWKV1(J+1)
               YWKV1(J)  = YWKV1(J+1)
               XWKW2(J)  = XWKW2(J+1)
               YWKW2(J)  = YWKW2(J+1)
               XWKV2(J)  = XWKV2(J+1)
               YWKV2(J)  = YWKV2(J+1)
#endif
   20       CONTINUE
            RETURN
         ENDIF
   30 CONTINUE
      END
