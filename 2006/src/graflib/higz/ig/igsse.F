*
* $Id: igsse.F,v 1.1.1.1 1996/02/14 13:10:39 mclareni Exp $
*
* $Log: igsse.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:39  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/02 30/08/95  09.30.50  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGSSE(ERRFIL,WTYPE)
*.===========>
*.
*.   This routine initializes the standart graphic package environment .
*.
*. _Input parameters:
*.
*.  INTEGER ERRFIL : Error file LUN .
*.  INTEGER WTYPE : Workstation type .
*.
*. The screen size is returned in IQUEST(11) and IQUEST(12) .
*.
*..==========> (O.Couet)
#include "higz/hiques.inc"
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
#include "higz/himeta.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if defined(CERNLIB_MSDOS)
#include "higz/hikern.inc"
#endif
#if defined(CERNLIB_DI3000)
      INTEGER*4 MAXDEV
      PARAMETER (MAXDEV = 9)
      INTEGER*4 DEVICES(0:MAXDEV)
      EQUIVALENCE (ILOPWK,DEVICES)
      DIMENSION LIST(4)
      REAL MAXDIM(4)
      LOGICAL PHYS
#endif
      INTEGER ERRFIL,WKID,CONID,WTYPE
      LOGICAL ZFSAV
      DIMENSION RVAL(2)
*.______________________________________
*
#if defined(CERNLIB_MAIL)
*
*              Mail option
*
      IF((WTYPE.LT.0)   .AND.(WTYPE.NE.-111).AND.
     +   (WTYPE.NE.-112).AND.(WTYPE.NE.-113))THEN
         GFLAG=.FALSE.
         ZFLAG=.FALSE.
         PFLAG=.FALSE.
         MFLAG=.TRUE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         WRITE (CHMAIL,'(I3,I5,I10)') 100,ERRFIL,-WTYPE
         CALL IMWRIT(1)
         READ (5,'(2E16.7)') RQUEST(11),RQUEST(12)
         CALL IMWRIT(5)
         RETURN
      ENDIF
#endif
*
      GFLAG =.TRUE.
      ZFSAV =ZFLAG
      ZFLAG =.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      IQUEST(100)=-1
#if defined(CERNLIB_ATCGKS)
      CALL KNOMSG
#endif
      IWTYPE=IABS(WTYPE)
      IF(IWTYPE.LE.0)TEKACT=.FALSE.
      IDTY=IWTYPE
#if defined(CERNLIB_DI3000)
      IDID=IWTYPE
      WKID=IWTYPE
#endif
#if !defined(CERNLIB_DI3000)
      WKID=1
#endif
      CALL IOPKS(ERRFIL)
#if defined(CERNLIB_DECGKS)
      CONID=0
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_FALCO)||defined(CERNLIB_PHIGS)||defined(CERNLIB_GKSBX)
      CONID=1
#endif
#if (defined(CERNLIB_GKSGRAL)||defined(CERNLIB_UNIGKS)||defined(CERNLIB_PLOT10GKS)||defined(CERNLIB_MGKS)||defined(CERNLIB_ATCGKS))&&(!defined(CERNLIB_GKSIBM))
      CONID=1
#endif
#if defined(CERNLIB_GKSIBM)
      CONID=-1
#endif
#if (defined(CERNLIB_GKSGRAL))&&(defined(CERNLIB_IBM))
      IF(WTYPE.LT.0)CONID=101
#endif
#if (defined(CERNLIB_GKSGRAL))&&(defined(CERNLIB_ALLIANT))
      IF(WTYPE.GT.0)CONID=6
#endif
#if (defined(CERNLIB_GKSGRAL))&&(defined(CERNLIB_NEWLIB))
      IF (WTYPE.GT.470000) THEN
          CONID=MOD(WTYPE,100)
          WTYPE=WTYPE/100
      ENDIF
      IF (WTYPE.EQ.7878) CONID=11
      IF (WTYPE.EQ.13001.OR.WTYPE.EQ.13002) CONID=101
      IWTYPE=WTYPE
      IDTY=IWTYPE
#endif
#if defined(CERNLIB_SUNGKS)
      IF(IWTYPE.EQ.3.OR.IWTYPE.EQ.6.OR.IWTYPE.EQ.7.OR.
     +   IWTYPE.EQ.8.OR.IWTYPE.EQ.10)CONID=9
      IF(IWTYPE.EQ.4)CONID=0
#endif
#if !defined(CERNLIB_DI3000)
      IF(IWTYPE.GT.0)THEN
         CALL IOPWK(WKID,CONID,IWTYPE)
         CALL IACWK(WKID)
         CALL IGQWK(WKID,'MXDS',RVAL)
         IF((RMDSX.LT.0).OR.(RMDSY.LT.0))THEN
            CALL IGERR('Invalid workstation type','IGSSE')
            RETURN
         ENDIF
      ENDIF
      RDVXMI=0.
      RDVYMI=0.
      RDVXMA=RMDSX
      RDVYMA=RMDSY
      IF(IWTYPE.GT.0)THEN
         GRFLAG=.TRUE.
         RQUEST(11)=RMDSX
         RQUEST(12)=RMDSY
#endif
#if defined(CERNLIB_MSDOS)
         ISTEP  = 4
         IXLOCO = 0
         IYLOCO = 0
         MOUSE  = .FALSE.
         IF(INIT_MOUSE().GT.0)MOUSE=.TRUE.
         RDWXMI = 0.0
         RDWYMI = 0.0
         IF(WINSIZ(3).GE.WINSIZ(4))THEN
            RDWXMA = 1.0
            RDWYMA = RMDSY/RMDSX
         ELSE
            RDWYMA = 1.0
            RDWXMA = RMDSX/RMDSY
         ENDIF
#endif
#if !defined(CERNLIB_DI3000)
         CALL ISWKWN(1,RDWXMI,RDWXMA,RDWYMI,RDWYMA)
         CALL ISWKVP(1,RDVXMI,RDVXMA,RDVYMI,RDVYMA)
      ENDIF
#endif
#if defined(CERNLIB_DI3000)
      CONID=1
      CALL IOPWK(WKID,CONID,IWTYPE)
      CALL IACWK(WKID)
*
*              Determine if the device is capable of physical sizing and
*              the physical size of the viewport
*
      IF (WKID.NE.0) THEN
         CALL JIQDIM(WKID,PHYS,MAXDIM)
         RDVXMI=MAXDIM(1)
         RDVXMA=MAXDIM(2)
         RDVYMI=MAXDIM(3)
         RDVYMA=MAXDIM(4)
      ELSE
         CALL JIQDEV(WKID,20,LIST)
         RDVXMI=FLOAT(LIST(1))
         RDVXMA=FLOAT(LIST(2))
         RDVYMI=FLOAT(LIST(3))
         RDVYMA=FLOAT(LIST(4))
      ENDIF
*
*              Set the orientation of the maximums returned since not all
*              devices refer to the lower left as x0phy, y0phy and upper right
*              as x1phy, y1phy.
*
      IF (RDVXMI.GT.RDVXMA) THEN
         TEMP=RDVXMI
         RDVXMI=RDVXMA
         RDVXMA=TEMP
      ENDIF
      IF (RDVYMI .GT. RDVYMA) THEN
         TEMP=RDVYMI
         RDVYMI=RDVYMA
         RDVYMA=TEMP
      ENDIF
*
*              Maximum display surface size
*
      RMDSX=RDVXMA-RDVXMI
      RMDSY=RDVYMA-RDVYMI
      GRFLAG=.TRUE.
      RQUEST(11)=RDVXMA
      RQUEST(12)=RDVYMA
*
      CALL JASPEK(WKID, RATIO)
      IF (RATIO .LE. 1.0) THEN
         RDWXMI=0.0
         RDWXMA=1.0
         RDWYMI=0.0
         RDWYMA=RATIO
      ELSE
         RDWXMI=0.0
         RDWXMA=1.0/RATIO
         RDWYMI=0.0
         RDWYMA=1.0
      ENDIF
      CALL ISWKWN(WKID,RDWXMI,RDWXMA,RDWYMI,RDWYMA)
      IF(WKID.NE.0)CALL ISWKVP(WKID,RDVXMI,RDVXMA,RDVYMI,RDVYMA)
*
      IF((RDVXMA.LE.0).OR.(RDVYMA.LE.0))THEN
         CALL IGERR('Bad workstation type','IGSSE')
         RETURN
      ENDIF
*
      CALL JDCOLR(0)
      CALL JDINTE(16383)
      CALL JDLSTY(0)
      CALL JDLWID(16383)
      CALL JDPEN(0)
      CALL JDPEDG(0)
      CALL JDPINT(0)
      CALL JDPIDX(0,0)
      IF(WKID.NE.0)CALL JPFSIM(WKID,1)
      CALL JDMARK(IMK)
      CALL JWCLIP(ICLIP.EQ.1)
#endif
#if defined(CERNLIB_GKS)
*
      CALL GSCHH(RCHH)
      CALL GSCHUP(RCHUX,RCHUY)
      CALL GSTXAL(ITXALH,ITXALV)
      CALL GSFAIS(IFAIS)
      CALL GSMK(IMK)
      CALL GSLN(ILN)
      CALL GSLWSC(RLWSC)
      CALL GSPLCI(IPLCI)
      CALL GSPMCI(IPMCI)
      CALL GSFACI(IFACI)
      CALL GSTXCI(ITXCI)
      CALL GSCLIP(ICLIP)
#endif
#if (defined(CERNLIB_GKS))&&(!defined(CERNLIB_MGKS))
      CALL GSFASI(IFASI)
      CALL GSMKSC(RMKSC)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_SUNGKS)||defined(CERNLIB_GKSBX)
*
*              Initialize the begining of the color table
*
      GRFLAG=.FALSE.
      IF(IWTYPE.GT.0)THEN
         CALL IGSG(1)
         CALL ISCR(WKID,0,1.,1.,1.)
         CALL ISCR(WKID,1,0.,0.,0.)
         CALL ISCR(WKID,2,1.,0.,0.)
         CALL ISCR(WKID,3,0.,1.,0.)
         CALL ISCR(WKID,4,0.,0.,1.)
         CALL ISCR(WKID,5,1.,1.,0.)
         CALL ISCR(WKID,6,1.,0.,1.)
         CALL ISCR(WKID,7,0.,1.,1.)
#endif
#if defined(CERNLIB_MSDOS)
         CALL ISCR(WKID, 7,0.0,0.0,0.0)
         CALL ISCR(WKID, 8,0.5,0.5,0.5)
         CALL ISCR(WKID, 9,0.0,0.5,0.5)
         CALL ISCR(WKID,10,0.5,0.0,0.0)
         CALL ISCR(WKID,11,0.0,0.5,0.0)
         CALL ISCR(WKID,12,0.0,0.0,0.5)
         CALL ISCR(WKID,13,0.5,0.5,0.0)
         CALL ISCR(WKID,14,1.0,0.0,0.5)
         CALL ISCR(WKID,15,0.0,1.0,1.0)
#endif
#if defined(CERNLIB_KERNEL)||defined(CERNLIB_SUNGKS)||defined(CERNLIB_GKSBX)
      ENDIF
#endif
      ZFLAG =ZFSAV
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
