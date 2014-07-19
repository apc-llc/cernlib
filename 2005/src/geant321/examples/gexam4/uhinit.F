*
* $Id: uhinit.F,v 1.1.1.1 1995/10/24 10:22:15 cernlib Exp $
*
* $Log: uhinit.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:15  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UHINIT
*
************************************************************************
*                                                                      *
*             To book the user's histograms                            *
*                                                                      *
************************************************************************
*
#include "geant321/gckine.inc"
#if defined(CERNLIB_NTUPLE)
      PARAMETER (NKEYS=13)
      CHARACTER*8 CHNAME(NKEYS)
      DATA CHNAME/'IEVENT  ','IPART   ','GEKIN   ','ISTOP   ','INWVOL  '
     +           ,'STEP    ','DESTEP  ','DXMIP   ','TOFG    ','IPL     '
     +           ,'PLMIP   ','SSTEP   ','THINLA  '/
#endif
*
*     ------------------------------------------------------------------
*
      XMIN=0.
      XMAX=100.*PKINE(1)
#if defined(CERNLIB_NTUPLE)
      CALL HBOOKN(1,'NTUPLE',NKEYS,'//HBOOK',9950,CHNAME)
#endif
      CALL HBOOK1(5,'PULSE HEIGHT DISTRIBUTION IN MIPS$',100,0.,100.,0.)
      CALL HBOOK1(6,'PULSE HEIGHT DISTRIBUTION IN MIPS$',100,0.,2.,0.)
      CALL HBOOK1(10,'NUMBER OF MIPS PER EVENT$',100,XMIN,XMAX,0.)
      CALL HBOOK1(101,'NUMBER OF PHOTONS PER PLANE$',60,1.,60.,0.)
      CALL HCOPY(101,102,'NUMBER OF ELECTRONS+POSITRONS PER PLANE$')
      CALL HCOPY(101,103,'NUMBER OF NEUTRAL PER PLANE$')
      CALL HCOPY(101,104,'NUMBER OF CHARGED HADRONS PER PLANE$')
      CALL HCOPY(101,105,'NUMBER OF MUONS PER PLANE$')
      CALL HBOOK1(500,'MUON AVERAGE ENERGY LOSS$',100,1.E-6,5.E-6,0.)
*
      END
