*
* $Id: iclrwk.F,v 1.1.1.1 1996/02/14 13:10:48 mclareni Exp $
*
* $Log: iclrwk.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:48  mclareni
* Higz
*
*
#if defined(CERNLIB_MSDOS)
#include "higz/pilot.h"
*CMZ :  1.18/01 04/05/93  17.56.14  by  O.Couet
*-- Author : V.Fine
      SUBROUTINE ICLRWK(IWKID,ICOFL)
      ENTRY      GCLRWK(IWKID,ICOFL)
#include "higz/hikern.inc"
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/himeta.inc"
      CHARACTER CHPTNO*8
      DIMENSION RVAL(2)
*
      IF(GFLAG)THEN
         IF(IWKID.EQ.0)THEN
            DO 20 I=1,INOPWK
               IF(NODRFL(I))GOTO 20
               ISAVE = IWKNB
               IWKNB = I
               CALL IGQWK(ILOPWK(I),'MXDS',RVAL)
               IF((REDIT.NE.0).AND.(REDIT.NE.1.))THEN
                  CALL IGCLES
               ELSE
                  CALL IGSG(I)
                  CALL WINPOP
                  CALL SETPAT(0)
                  CALL COLOR(IOFCOL)
                  ICURCI(I)=0
                  CALL COLOR(IOFCOL)
                  call setlin(1)
C
                  IXSAVE(I) = -1.0
                  IYSAVE(I) = -1.0
C
                  IF (ILOPWK(I) .NE. IDMETA) then
                     IER = CLEAR()
                  ELSE
                     IF (IWTYL(I) .EQ. ISURF) THEN
*
* Automatic naming of the meta-file of the picture
*
                        WRITE (CHPTNO,10000) IPICNM
                        IPICNM=IPICNM+1
                        NLAST=LENOCC(CHPTNO)
                        NFIRST=1
   10                   IF(CHPTNO(NFIRST:NFIRST).EQ.' ')THEN
                           NFIRST=NFIRST+1
                           GOTO 10
                        ENDIF
                        CALL GOLCLO
                        CHMETA = ' '
#if defined(CERNLIB_ZEBRA)
                        IF (ZFLAG) CALL IZPICT(chmeta,'G')
#endif
                        IF (chmeta .EQ. ' ') THEN
                           chmeta='META'//CHPTNO(NFIRST:NLAST)//'.PLT'
                        ELSE
                           chmeta = chmeta(1:LENOCC(chmeta))//'.PLT'
                        END IF
                        LOCK = .FALSE.
                        SYMFIL = 'DEFAULT.SYM'
                        CALL GOLINI(LUNMET(I),chmeta,'BINARY')
                     END IF
                  END IF
                  IF(IFAIS.GT.1)CALL SETPAT(IFASI)
               ENDIF
   20       CONTINUE
         ELSE
            IWT=IGIWTY(IWKID)
            IF(IWT.LE.0)GOTO 40
            CALL IGQWK(IWKID,'MXDS',RVAL)
            IF((REDIT.NE.0).AND.(REDIT.NE.1.))THEN
               CALL IGCLES
            ELSE
               I=IGIWIN(IWKID)
               CALL IGSG(I)
               CALL WINPOP
               CALL SETPAT(0)
               CALL COLOR(IOFCOL)
               ICURCI(I)=0
               CALL COLOR(IOFCOL)
               call setlin(1)
               IXSAVE(I) = -1.0
               IYSAVE(I) = -1.0
               IF (ILOPWK(I) .NE. IDMETA) then
                  IER = CLEAR()
               ELSE
                  IF (IWTYL(I) .EQ. ISURF) THEN
*
* Automatic naming of the meta-file of the picture
*
                     WRITE (CHPTNO,10000) IPICNM
                     IPICNM=IPICNM+1
                     NLAST=LENOCC(CHPTNO)
                     NFIRST=1
   30                IF(CHPTNO(NFIRST:NFIRST).EQ.' ')THEN
                        NFIRST=NFIRST+1
                        GOTO 30
                     ENDIF
C===
                     CALL GOLCLO
                     CHMETA = ' '
#if defined(CERNLIB_ZEBRA)
                     IF (ZFLAG) CALL IZPICT(chmeta,'G')
#endif
                     IF (chmeta .EQ. ' ') THEN
                        chmeta='META'//CHPTNO(NFIRST:NLAST)//'.PLT'
                     ELSE
                        chmeta = chmeta(1:LENOCC(chmeta))//'.PLT'
                     END IF
C===
                     LOCK = .FALSE.
                     SYMFIL = 'DEFAULT.SYM'
                     CALL GOLINI(LUNMET(I),chmeta,'BINARY')
                  END IF
               END IF
               IF(IFAIS.GT.1)CALL SETPAT(IFASI)
            ENDIF
         ENDIF
      ENDIF
   40 CONTINUE
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZCLRD
#endif
10000 FORMAT(I5)
      END
#endif
