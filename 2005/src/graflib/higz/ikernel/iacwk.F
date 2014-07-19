*
* $Id: iacwk.F,v 1.2 1996/09/25 14:58:40 couet Exp $
*
* $Log: iacwk.F,v $
* Revision 1.2  1996/09/25 14:58:40  couet
* - GPR driver removed
*
* Revision 1.1.1.1  1996/02/14 13:10:55  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/09 22/08/94  18.27.58  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IACWK(IWKID)
      ENTRY      GACWK(IWKID)
#include "higz/hiflag.inc"
#if defined(CERNLIB_KERNEL)
#include "higz/hikern.inc"
#endif
#include "higz/hiatt.inc"
#if defined(CERNLIB_MSDOS)
#include "higz/himeta.inc"
#endif
#if defined(CERNLIB_X11)
      INTEGER DASHED(2),DOTTED(2),DSHDOT(4)
      DATA DASHED/5,5/
      DATA DOTTED/1,3/
      DATA DSHDOT/5,3,1,3/
#endif
#if !defined(CERNLIB_BATCH)
      IF(IWKID.EQ.0)THEN
         CALL IGERR('Workstation identifier equal 0','IACWK')
         RETURN
      ENDIF
      DO 10 I=1,INOPWK
         IF(ILOPWK(I).EQ.IWKID)THEN
            ACWKFL(I)=.TRUE.
            IF(IWTYL(I).LE.0)THEN
               NODRFL(I)=.TRUE.
            ELSE
               NODRFL(I)=.FALSE.
            ENDIF
            IWKIND=I
            GOTO 20
         ENDIF
   10 CONTINUE
      CALL IGERR('Workstation is not open','IACWK')
      RETURN
   20 CONTINUE
#endif
#if defined(CERNLIB_MSDOS)
      ISAVE = IWKNB
      IWKNB = IWKIND
#endif
#if defined(CERNLIB_PSCRIPT)
*
*              Active the PostScript metafile
*
      IF(IGIWTY(IWKID).LT.0)THEN
         PFLAG=.TRUE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         RETURN
      ENDIF
#endif
#if defined(CERNLIB_KERNEL)
*
*              Initialise the default attributes for the screen
*
      CALL IGSG(IWKIND)
#endif
#if (defined(CERNLIB_GL))&&(!defined(CERNLIB_MSDOS))
      CALL COLOR(IPLCI+IOFCOL)
      ICURCI(IWKIND)=IPLCI
      IF(IFAIS.LE.1)THEN
         CALL SETPAT(0)
      ELSE
         CALL SETPAT(IFASI)
      ENDIF
      CALL SETLIN(ILN-1)
      CALL LINEWI(INT(RLWSC))
*
#endif
#if defined(CERNLIB_MSDOS)
      CALL COLOR(IPLCI+IOFCOL)
      ICURCI(IWKIND)=IPLCI
      IF(IFAIS.LE.1)THEN
         CALL SETPAT(0)
      ELSE
         CALL SETPAT(IFASI)
      ENDIF
      CALL SETLIN(ILN)
      CALL LINEWI(INT(RLWSC))
      IF(IWKID.EQ.IDMETA)THEN
         IF(IWTYL(IWKNB).EQ.ISURF)THEN
            XMFACT = 1.0
            YMFACT = 1.0
         ENDIF
      ENDIF
      IWKNB = ISAVE
*
#endif
#if defined(CERNLIB_FALCO)
      IF(IGIWTY(IWKID).EQ.7878)RETURN
      IF(IGIWTY(IWKID).EQ.7879)THEN
         CALL IGSA(0)
         RETURN
      ENDIF
#endif
#if defined(CERNLIB_X11)
      CALL IXSELWI(IWINID(IWKIND))
      CALL IXSETFC(IFACI)
      CALL IXSETLC(IPLCI)
      CALL IXSETMC(IPMCI)
      CALL IXSETTC(ITXCI)
      CALL IXSETFS(MIN(1,IFAIS),0)
#endif
#if (defined(CERNLIB_X11))&&(!defined(CERNLIB_WINNT))
      IF(ILN.EQ.1)CALL IXSETLS(0,0)
      IF(ILN.EQ.2)CALL IXSETLS(2,DASHED)
      IF(ILN.EQ.3)CALL IXSETLS(2,DOTTED)
      IF(ILN.EQ.4)CALL IXSETLS(4,DSHDOT)
#endif
#if (defined(CERNLIB_X11))&&(defined(CERNLIB_WINNT))
      CALL IXSETLS(-ILN, 0)
#endif
#if defined(CERNLIB_X11)
      CALL IXSETLN(MAX(INT(RLWSC/2.2),1))
      CALL IXSETTA(ITXALH,ITXALV)
*
#endif
#if defined(CERNLIB_KERNEL)
      CALL IGSA(0)
#endif
      END
