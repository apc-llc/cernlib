*
* $Id: iginme.F,v 1.1.1.1 1996/02/14 13:10:24 mclareni Exp $
*
* $Log: iginme.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:24  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.07/00 25/04/89  11.28.33  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGINME
*.===========>
*.
*.   This routine initialize the menu package and the
*. default primitives attributes of the editor.
*.
*..==========> (O.Couet)
#include "higz/higed.inc"
#include "higz/hiflag.inc"
      LOGICAL ZFS,GFS
*.______________________________________
*
      ZFS=ZFLAG
      GFS=GFLAG
      ZFLAG=.FALSE.
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=.TRUE.
*
*              Polyline
*
      ICPLCI=1
      ICLN=1
      RCLWSC=1.
      CALL ISPLCI(ICPLCI)
      CALL ISLWSC(RCLWSC)
      CALL ISLN(ICLN)
      CHPLC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
*
*              Fill area
*
      ICFACI=1
      ICFAIS=0
#if defined(CERNLIB_GKSGRAL)
      ICFASI=-3
#endif
#if !defined(CERNLIB_GKSGRAL)
      ICFASI=1
#endif
      CALL ISFACI(ICFACI)
      CALL ISFAIS(ICFAIS)
      CALL ISFASI(ICFASI)
      CHFAC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
      CHFAIS='Hollow,Solid,Pattern,Hatch'
*
*              Polymarker
*
      ICPMCI=1
      ICMK=3
      RCMKSC=1.
      CALL ISMKSC(RCMKSC)
      CALL ISPMCI(ICPMCI)
      CALL ISMK(ICMK)
      CHPMC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
*
*              Text
*
      ICTXCI=1
      RCTXAN=0.
      RCTXSI=.5
      ICTXFT=0
      ICTXPR=2
      ICTXCI=1
      ICTXAH=0
      ICTXAV=0
      CALL IGSET('TANG',0.)
      CALL ISCHH(RCTXSI)
      CALL ISTXFP(1,0)
      CALL ISTXCI(ICTXCI)
      CALL ISTXAL(ICTXAH,ICTXAV)
      CCTX=' '
      CHTXC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
*
*              Histogram
*
      CHOPHI='HF'
      RCBOF=0.250
      RCBWD=0.500
      ICHILN=1
      ICHIPC=1
      ICHIFC=1
      ICHIFI=0
#if defined(CERNLIB_GKSGRAL)
      ICHIFS=-3
#endif
#if !defined(CERNLIB_GKSGRAL)
      ICHIFS=1
#endif
      CHHIIS='Hollow,Solid,Pattern,Hatch'
      CHHIFC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
      CHHIPC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
      CALL IGSET('BARO',0.250)
      CALL IGSET('BARW',0.500)
*
*              Software text
*
      RCSTAN=0.
      RCSTSI=.5
      ICSTPA=1
      RCSTSH=0.
      CHOPST='L'
      ICSTCI=1
      CCST=' '
      CALL IGSET('CSHI',RCSTSH)
      CALL IGSET('PASS',FLOAT(ICSTPA))
      CHSTC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
*
*              Boxe
*
      ICBOCI=1
      ICBOIS=0
#if defined(CERNLIB_GKSGRAL)
      ICBOSI=-3
#endif
#if !defined(CERNLIB_GKSGRAL)
      ICBOSI=1
#endif
      ICBOBO=1
      CHBOC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
      CHBOIS='Hollow,Solid,Pattern,Hatch'
      CHBOBO='Yes,No'
      CALL IGSET('BORD',1.)
*
*              Frame boxe
*
      ICFBCI=1
      ICFBIS=0
#if defined(CERNLIB_GKSGRAL)
      ICFBSI=-3
#endif
#if !defined(CERNLIB_GKSGRAL)
      ICFBSI=1
#endif
      ICFBBO=1
      CHFBC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
      CHFBIS='Hollow,Solid,Pattern,Hatch'
      CHFBBO='Yes,No'
*
*              Arrow
*
      RCARSI=.4
      CHDOAR='No,Yes'
      CHAFL='No,Yes'
      ICARLC=1
      ICARFC=1
      ICARFI=0
#if defined(CERNLIB_GKSGRAL)
      ICARFS=-3
#endif
#if !defined(CERNLIB_GKSGRAL)
      ICARFS=1
#endif
      ICARLN=1
      CHARFC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
      CHARLC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
      CHARFI='Hollow,Solid,Pattern,Hatch'
*
*              Pave
*
      ICPBST=0
#if defined(CERNLIB_GKSGRAL)
      ICPFST=-3
#endif
#if !defined(CERNLIB_GKSGRAL)
      ICPFST=1
#endif
      RCDZPV=0.5
      CHOPPV='TR'
*
*              Axis
*
      RCAXTS=0.1
      RCAXLS=0.1
      RCAXLO=0.1
      RCAXW1=0.
      RCAXW2=1.
      ICAXND=10
      ICAXLN=1
      ICAXLC=1
      ICAXLF=1
      ICAXLP=0
      CHOPAX=' '
      CHAXLC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
*
*              Arc
*
      ICACFI=0
#if defined(CERNLIB_GKSGRAL)
      ICACFS=-3
#endif
#if !defined(CERNLIB_GKSGRAL)
      ICACFS=1
#endif
      ICACBO=1
      ICACLC=1
      ICACFC=1
      CHACIS='Hollow,Solid,Pattern,Hatch'
      CHACLC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
      CHACFC='White,Black,Red,Green,Blue,Magenta,Cyan,Yellow'
      CHACBO='Yes,No'
*
*              Grid
*
      IXSTEP=0
      IYSTEP=0
      IXFACT=1
      IYFACT=1
*
      DO 10 I=1,3
         ICUCHO(I)=0
         ICUMEN(I)=0
 10   CONTINUE
*
      ZFLAG=ZFS
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
      GFLAG=GFS
      END
