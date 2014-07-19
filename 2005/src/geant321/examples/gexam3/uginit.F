*
* $Id: uginit.F,v 1.3 1997/04/10 15:33:35 mclareni Exp $
*
* $Log: uginit.F,v $
* Revision 1.3  1997/04/10 15:33:35  mclareni
* Mods for WINNT
*
* Revision 1.2.2.1  1997/01/21 11:24:07  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.2  1996/03/13 17:30:22  ravndal
* Modifications for parallel version testing included
*
c Revision 1.1.1.1  95/10/24  10:22:12  cernlib
c Geant
c 
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE UGINIT
C
#include "geant321/gclist.inc"
#include "geant321/gcrz.inc"
C
      COMMON/DLSFLD/ISWFLD,FLDVAL
C
      DIMENSION AMYL(3),ZMYL(3),WMYL(3)
      DIMENSION APYR(4),ZPYR(4),WPYR(4)
      DIMENSION AFRE(3),ZFRE(3),WFRE(3)
      DIMENSION ACO2(2),ZCO2(2),WCO2(2)
      DIMENSION APOL(2),ZPOL(2),WPOL(2)
      DIMENSION ASCI(2),ZSCI(2),WSCI(2)
      DIMENSION AFDR(5),ZFDR(5),WFDR(5)
      DIMENSION ARDR(5),ZRDR(5),WRDR(5)
      CHARACTER*4 CHGET(20),CHSAVE(20)
C
      DATA AMYL,ZMYL,WMYL/12.01,1.01,16.00,6.,1.,8.,5.,4.,2./
      DATA APYR,ZPYR,WPYR/28.09,10.82,23.00,16.00,14.,5.,11.,8.,
     + .385,.039,.038,.538/
      DATA AFRE,ZFRE,WFRE/12.01,35.45,19.00,6.,17.,9.,1.,1.,3./
      DATA ACO2,ZCO2,WCO2/12.01,16.00,6.,8.,1.,2./
      DATA APOL,ZPOL,WPOL/12.01,1.01,6.,1.,1.,2./
      DATA ASCI,ZSCI,WSCI/12.01,1.01,6.,1.,1.,1./
      DATA AFDR,ZFDR,WFDR/63.54,183.85,39.95,12.01,1.01,
     + 29.,74.,18.,6.,1.,.457,.009,.313,.177,.045/
      DATA ARDR,ZRDR,WRDR/63.54,183.85,39.95,12.01,1.01,
     + 29.,74.,18.,6.,1.,.528,.002,.275,.156,.039/
*
      CALL GINIT

#if defined(MEIKO)
c     On the Meiko CS-2 reading from stdin-5 from multiple nodes
c       fails to give the same input: instead node 1 gets line 1, 
c       node 2 gets line 2, and so on
c
c     Reset the FFREAD Input unit to the one in ufiles ...
c
      call ffset('LINP', 4 )
c          -----------------
#endif
*
*             Open user files
*
      CALL UFILES
C
C                 Define here user data cards
C
      ISWFLD=1
      FLDVAL=3.9
      CALL FFKEY('FIEL',ISWFLD,2,'MIXED')
C
      CALL GFFGO
C
      CALL GZINIT
C
      CALL GDINIT
C
      IF(NRGET.NE.0)THEN
         CALL GLOOK('INIT',LRGET,NRGET,ILOOK)
         IF(ILOOK.NE.0)THEN
            CALL GRFILE(3,'TEST',' ')
            CALL GRGET('INIT',0,1,0,0)
            GO TO 10
         ENDIF
      ENDIF
C
      IF(NGET .GT.0) CALL GOPEN(1,'I',0,IER)
      IF(NSAVE.GT.0) CALL GOPEN(2,'O',0,IER)
C
      DO 5 I=1,20
         CALL UHTOC(LGET(I),4,CHGET(I),4)
         CALL UHTOC(LSAVE(I),4,CHSAVE(I),4)
   5  CONTINUE
      CALL GFIN(1,CHGET,NGET,IDENT,'I',IER)
      IF(IER.GE.0) GO TO 10
C
      CALL GPART
C
      CALL GSMATE(15,'AIR$',14.61,7.3,0.001205,30423.24,6750.,0,0)
      CALL GSMATE(16,'VACUUM$',1.E-16,1.E-16,1.E-16,1.E+16,1.E+16,0,0)
      CALL GSMATE(17,'CALCIUM$',40.08,20.,1.55,10.4,23.2,0,0)
      CALL GSMIXT(18,'MYLAR$',AMYL,ZMYL,1.39,-3,WMYL)
      CALL GSMIXT(19,'PYREX$',APYR,ZPYR,2.23,4,WPYR)
      CALL GSMIXT(20,'FREON13$',AFRE,ZFRE,4.26E-03,-3,WFRE)
      CALL GSMIXT(21,'CO2$',ACO2,ZCO2,1.79E-03,-2,WCO2)
      CALL GSMIXT(22,'POLYETHYLENE$',APOL,ZPOL,.935,-2,WPOL)
      CALL GSMIXT(23,'SCINT$',ASCI,ZSCI,1.032,-2,WSCI)
      CALL GSMIXT(24,'FRONTDRIFT$',AFDR,ZFDR,2.85E-03,5,WFDR)
      CALL GSMIXT(25,'REARDRIFT$',ARDR,ZRDR,3.24E-03,5,WRDR)
C
      CALL UGEOM
C
      CALL UDET
C
  10  CALL GPHYSI
C
      IF(NHSTA.GT.0) CALL GBHSTA
      CALL USETPL
C
      CALL GFOUT(2,CHSAVE,NSAVE,0,'I',IER)
C
      END
