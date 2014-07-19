*
* $Id: gxcs.F,v 1.2 2002/04/11 11:13:39 mclareni Exp $
*
* $Log: gxcs.F,v $
* Revision 1.2  2002/04/11 11:13:39  mclareni
* Make the CSEXT calls conform to the latest Comis
*
* Revision 1.1.1.1  1995/10/24 10:21:49  cernlib
* Geant
*
*
#include "geant321/pilot.h"
#if defined(CERNLIB_COMIS)
*CMZ :          20/06/95  09.32.44  by  S.Ravndal
*-- Author :
      SUBROUTINE GXCS
C.
C.    ******************************************************************
C.    *                                                                *
C.    *        To initialize the COMIS package                         *
C.    *        To declare addresses of FORTRAN routines and COMMONs    *
C.    *        which may be invoked from COMIS routines                *
C.    *        (one can call CSOMAP instead)                           *
C.    *                                                                *
C.    ******************************************************************
#include "geant321/gcbank.inc"
#include "geant321/gcmate.inc"
#include "geant321/gctmed.inc"
#include "geant321/gcvolu.inc"
#include "geant321/gcflag.inc"
#include "geant321/gctrak.inc"
#include "geant321/gckine.inc"
#include "geant321/gcking.inc"
#include "geant321/gccuts.inc"
#include "geant321/gclist.inc"
#include "geant321/gcnum.inc"
#include "geant321/gconst.inc"
#include "geant321/gcphys.inc"
#include "geant321/gcunit.inc"
#include "geant321/gcdraw.inc"
#include "geant321/gcmulo.inc"
#include "geant321/gcomis.inc"
      DIMENSION P(1)
*
      EXTERNAL GINIT,GZINIT,GDINIT,GPRINT,GPSETS,GXCLOS
      EXTERNAL GSVERT,GSKINE,GSKING,GOPEN,GFIN,GCLOSE
      EXTERNAL GFOUT
      EXTERNAL GMATE,GSMATE,GSMIXT,GSTMED,GSTPAR,GPART,GPHYSI
      EXTERNAL GFMATE,GPIONS
      EXTERNAL GTRIG,GTRIGI,GTRIGC,GTREVE,GIDROP
      EXTERNAL GSVOLU,GSPOS,GSPOSP,GSDVN,GSDVS,GGCLOS,GOPTIM
      EXTERNAL GSROTM,GSORD,GSDET,GSDETH,GSDETV,GSATT
      EXTERNAL GPLMAT,GSAHIT,GSCHIT,GSDIGI,GSXYZ,GDEBUG
      EXTERNAL GPCXYZ,GDCXYZ,GDXYZ,GDAHIT,GDCHIT,GDHITS,GDHEAD
      EXTERNAL GDOPEN,GDCLOS,GDRAW,GDRAWC,GDSCAL,GDMAN,GDCOL
      EXTERNAL GDELET,GDAXIS,GDRAWT
      EXTERNAL GSCANK,GSCANU,GSCANO
      EXTERNAL UGLAST
*
C.
C.    ------------------------------------------------------------------
C.
      CALL PAWCS
*
      CALL CSCOM('GCLINK,GCBANK,GCCUTS,GCFLAG,GCKINE,GCLIST#'
     +,           JDIGI ,NZEBRA,CUTGAM,IDEBUG,IKINE ,NHSTA
     +,           P,P,P,P)
      CALL CSCOM('GCMATE,GCNUM,GCONST,GCPHYS,GCTMED,GCTRAK#'
     +,           NMAT  ,NMATE,PI    ,IPAIR ,NUMED ,VECT
     +,           P,P,P,P)
      CALL CSCOM('GCUNIT,GCVOLU,GCDRAW,GCKING,GCMULO#'
     +,           LIN   ,NLEVEL,NUMNOD,KCASE ,SINMUL
     +,           P,P,P,P,P)
*
      CALL CSEXT(
     +  'GINIT.S,GZINIT.S,GDINIT.S,GPRINT.S,GPSETS.S,GXCLOS.S#'
     +,  GINIT  ,GZINIT  ,GDINIT  ,GPRINT  ,GPSETS  ,GXCLOS
     +,  P,P,P,P)
      CALL CSEXT(
     +  'GSVERT.S,GSKINE.S,GSKING.S,GFIN.S,GOPEN.S,GCLOSE.S,GFOUT.S#'
     +,  GSVERT  ,GSKINE  ,GSKING  ,GFIN  ,GOPEN  ,GCLOSE  ,GFOUT
     +,  P,P,P)
      CALL CSEXT(
     +  'GMATE.S,GSMATE.S,GFMATE.S,GSMIXT.S,GSTMED.S,GSTPAR.S#'
     +,  GMATE  ,GSMATE  ,GFMATE  ,GSMIXT  ,GSTMED  ,GSTPAR
     +,  P,P,P,P)
      CALL CSEXT(
     +  'GPART.S,GPIONS.S,GPHYSI.S#'
     +,  GPART  ,GPIONS  ,GPHYSI
     +,  P,P,P,P,P,P,P)
      CALL CSEXT(
     +  'GTRIG.S,GTRIGI.S,GTRIGC.S,GTREVE.S,GIDROP.S#'
     +,  GTRIG  ,GTRIGI  ,GTRIGC  ,GTREVE  ,GIDROP
     +,  P,P,P,P,P)
      CALL CSEXT(
     +  'GSVOLU.S,GSPOS.S,GSPOSP.S,GSDVN.S,GSDVS.S,GGCLOS.S,GOPTIM.S#'
     +,  GSVOLU  ,GSPOS  ,GSPOSP  ,GSDVN  ,GSDVS  ,GGCLOS  ,GOPTIM
     +,  P,P,P)
      CALL CSEXT(
     +  'GSROTM.S,GSORD.S,GSDET.S,GSDETH.S,GSDETV.S,GSATT.S#'
     +,  GSROTM  ,GSORD  ,GSDET  ,GSDETH  ,GSDETV  ,GSATT
     +,  P,P,P,P)
      CALL CSEXT(
     +  'GPLMAT.S,GSAHIT.S,GSCHIT.S,GSDIGI.S,GSXYZ.S,GDEBUG.S#'
     +,  GPLMAT  ,GSAHIT  ,GSCHIT  ,GSDIGI  ,GSXYZ  ,GDEBUG
     +,  P,P,P,P)
      CALL CSEXT(
     +  'GPCXYZ.S,GDCXYZ.S,GDXYZ.S,GDAHIT.S,GDCHIT.S,GDHITS.S,GDHEAD.S#'
     +,  GPCXYZ  ,GDCXYZ  ,GDXYZ  ,GDAHIT  ,GDCHIT  ,GDHITS  ,GDHEAD
     +,  P,P,P)
      CALL CSEXT(
     +  'GDOPEN.S,GDCLOS.S,GDELET.S,GDRAW.S,GDRAWC.S#'
     +,  GDOPEN  ,GDCLOS  ,GDELET  ,GDRAW  ,GDRAWC
     +,  P,P,P,P,P)
      CALL CSEXT(
     +  'GDAXIS.S,GDSCAL.S,GDMAN.S,GDCOL.S#'
     +,  GDAXIS  ,GDSCAL  ,GDMAN  ,GDCOL
     +,  P,P,P,P,P,P)
      CALL CSEXT(
     +  'GDRAWT.S#'
     +,  GDRAWT
     +,  P,P,P,P,P,P,P,P,P)
      CALL CSEXT(
     +  'GSCANK.S,GSCANU.S,GSCANO.S,GBRSGE.R#'
     +  ,GSCANK  ,GSCANU  ,GSCANO  ,GBRSGE
     +,  P,P,P,P,P,P)
      CALL CSEXT(
     +  'UGLAST.S#'
     +,  UGLAST
     +,  P,P,P,P,P,P,P,P,P)
*
      CALL GUXCS
*
      END
 
#endif
