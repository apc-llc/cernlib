*
* $Id: papict.F,v 1.4 1998/11/16 11:18:51 couet Exp $
*
* $Log: papict.F,v $
* Revision 1.4  1998/11/16 11:18:51  couet
* - PI/PRINT can now scale output GIFs
*
* Revision 1.3  1997/06/20 14:44:36  couet
* - KUHOME added before calling IGGIF
*
* Revision 1.2  1996/08/14 07:12:51  couet
* - Code for the new command PICTURE/LOAD
*
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :          10/01/96  13.48.50  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAPICT
*
*          /PICTURE
*
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#include "paw/pcwk.inc"
      CHARACTER*8 CHOPT
*.______________________________________
*
      CALL KUPATL(CHPATL,NPAR)
*.______________________________________
*
*           FILE
*
      IF(CHPATL.EQ.'FILE')THEN
         CALL PAWROP('HIGZ')
         RETURN
      ENDIF
*.______________________________________
*
*           IZPICT
*
      IF(CHPATL.EQ.'IZPICT')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL KUGETC(CHOPT,NCH)
         CALL IZPICT(CHTITL,CHOPT)
         RETURN
      ENDIF
*.______________________________________
*
*           CREATE
*
      IF(CHPATL.EQ.'CREATE')THEN
         CALL KUGETC(CHTITL,NCH)
         CHOPT='M'
         CALL IZPICT(CHTITL,CHOPT)
         RETURN
      ENDIF
*.______________________________________
*
*           PLOT
*
      IF(CHPATL.EQ.'PLOT')THEN
         CALL KUGETC(CHTITL,NCH)
         IF(NCH.GT.0)THEN
            LP=IZRPIP(CHTITL)
            IF(LP.EQ.0)THEN
               CALL IZIN(CHTITL,9999)
            ENDIF
         ENDIF
         CALL IZPICT(CHTITL,'COD')
         RETURN
      ENDIF
*.______________________________________
*
*           MERGE
*
      IF(CHPATL.EQ.'MERGE')THEN
         CALL KUGETC(CHTITL,NCH)
         IF(NCH.GT.0)THEN
            LP=IZRPIP(CHTITL)
            IF(LP.EQ.0)THEN
               CALL IZIN(CHTITL,9999)
            ENDIF
         ENDIF
         CALL KUGETR(X)
         CALL KUGETR(Y)
         CALL KUGETR(SCALE)
         CALL KUGETC(CHOPT,NCH)
         CALL IZMERG(CHTITL,X,Y,SCALE,CHOPT)
         RETURN
      ENDIF
*.______________________________________
*
*           IZOUT
*
      IF(CHPATL.EQ.'IZOUT')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL IZOUT(CHTITL,ICYCLE)
         RETURN
      ENDIF
*.______________________________________
*
*           IZIN
*
      IF(CHPATL.EQ.'IZIN')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL KUGETI(ICYCLE)
         CALL IZIN(CHTITL,ICYCLE)
         RETURN
      ENDIF
*.______________________________________
*
*           SCRATCH
*
      IF(CHPATL.EQ.'SCRATCH')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL KUGETI(ICYCLE)
         CALL IZSCR(CHTITL,ICYCLE)
         RETURN
      ENDIF
*.______________________________________
*
*           SWITCH
*
      IF(CHPATL.EQ.'SWITCH')THEN
         CALL KUGETC(CHOPT,NCH)
         CALL IGZSET(CHOPT)
         RETURN
      ENDIF
*.______________________________________
*
*           COPY
*
      IF(CHPATL.EQ.'COPY')THEN
         CALL KUGETC(CHTITL ,NCH)
         CALL KUGETC(CHTEMP,NCH)
         CALL IZCOPY(CHTITL,CHTEMP,' ')
         RETURN
      ENDIF
*.______________________________________
*
*           RENAME
*
      IF(CHPATL.EQ.'RENAME')THEN
         CALL KUGETC(CHTITL ,NCH)
         CALL KUGETC(CHTEMP,NCH)
         CALL IZCOPY(CHTITL,CHTEMP,'R')
         RETURN
      ENDIF
*.______________________________________
*
*           DELETE
*
      IF(CHPATL.EQ.'DELETE')THEN
         CALL KUGETC(CHTITL,NCH)
         CALL IZPICT(CHTITL,'S')
         RETURN
      ENDIF
*.______________________________________
*
*           LIST
*
      IF(CHPATL.EQ.'LIST')THEN
         CALL IZPICT(' ','L')
         RETURN
      ENDIF
*.______________________________________
*
*           MODIFY
*
      IF(CHPATL.EQ.'MODIFY')THEN
         IF(IWK.EQ.999)THEN
            PRINT*,'*** Invalid command in Paw++'
            RETURN
         ENDIF
#if !defined(CERNLIB_IBM)
         CALL KUGETC(CHTITL,NCH)
         CALL KUGETC(CHOPT,NCH)
         CALL IZGED(CHTITL,CHOPT)
#endif
         RETURN
      ENDIF
*.______________________________________
*
*           IGSET
*
      IF(CHPATL.EQ.'IGSET')THEN
         CALL KUGETC(CHTITL,NCH)
         XT=0.
         IF(CHTITL.NE.'SHOW')CALL KUGETR(XT)
         CALL IGSET(CHTITL,XT)
         RETURN
      ENDIF
*.______________________________________
*
*           PRINT
*
      IF(CHPATL.EQ.'PRINT')THEN
         CALL KUGETF(CHFILE,NCH)
         CALL KUGETI(IW)
         CALL KUGETI(IH)
         IF(NCH.EQ.0)CHFILE='HIGZPRINTER'
         CALL PALUNF(20,3,IFREE)
         IF(IFREE.EQ.0)RETURN
         IF (INDEX(CHFILE,'.').NE.0) THEN
            ILEN   = LENOCC(CHFILE)
            CHTEMP = CHFILE(ILEN-3:ILEN)
            CALL CLTOU(CHTEMP)
            IF (CHTEMP.EQ.'.GIF') THEN
               CALL KUHOME(CHFILE,NCH)
               CALL IGGIF(1,FLOAT(IW),FLOAT(IH),CHFILE,'M')
               RETURN
            ENDIF
            CALL KUOPEN(IFREE,CHFILE,'UNKNOWN',ISTAT)
            IF(ISTAT.NE.0)RETURN
            IF (CHTEMP.EQ.'.TEX') THEN
               CALL IGMETA(-IFREE,-778)
            ELSEIF (CHTEMP.EQ.'.EPS') THEN
               CALL IGMETA(-IFREE,-113)
            ELSE
               CALL IGMETA(-IFREE,-111)
            ENDIF
         ELSE
            CALL CLTOU(CHFILE)
            IF (CHFILE.EQ.'HIGZPRINTER') THEN
               CALL GETENVF('HIGZPRINTER',CHTEMP)
               CALL KUOPEN(IFREE,'paw.ps','UNKNOWN',ISTAT)
               CALL IGMETA(-IFREE,-111)
            ELSE
               RETURN
            ENDIF
         ENDIF
         CALL IZPICT(' ','D')
         CALL IGMETA(0,0)
         CALL ICLWK(2)
         CLOSE (IFREE)
         IF (CHFILE.EQ.'HIGZPRINTER'.AND.CHTEMP.NE.' ')
     +      CALL KUEXEL('Shell '//CHTEMP)
         RETURN
      ENDIF
*.______________________________________
*
*           LOAD
*
      IF(CHPATL.EQ.'LOAD')THEN
         CALL KUGETR(X)
         CALL KUGETR(Y)
         CALL KUGETF(CHFILE,NCH)
         CALL KUGETI(IWKID)
         CALL KUHOME(CHFILE,NCH)
         CALL IGGIF(IWKID,X,Y,CHFILE,'L')
      ENDIF
*
      END
