*
* $Id: ugeom.F,v 1.2 1996/02/01 15:03:16 ravndal Exp $
*
* $Log: ugeom.F,v $
* Revision 1.2  1996/02/01 15:03:16  ravndal
* Clean up repository
*
* Revision 1.1.1.1  1995/10/24 10:22:17  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE UGEOM
*
************************************************************************
*                                                                      *
*             Routine to define the geometry of the set-up.            *
*                                                                      *
************************************************************************
*
*
#include "nascom.inc"
*
      DIMENSION PAR(10)
      DIMENSION ANE213(4),ZNE213(4),WNE213(4)
*
      DATA ANE213/1.01,12.01,20.18,16.01/
      DATA ZNE213/1.,6.,10.,8./
      DATA WNE213/0.5493,0.4505,0.0001,0.0001/
*
*     -----------------------------------------------------------------
*
*
*             Defines materials
      CALL GSMATE( 1,'VACUUM$  ',1.E-16,1.E-16,1.E-16,1.E16,1.E16, 0,0)
      CALL GSMATE( 5,'LIQ NE213$',  6.26,3.25,0.874 , 43.0,   437.,0,0)
      CALL GSMIXT( 2,'LIQ NE213$',ANE213,ZNE213,0.874,4,WNE213)
      CALL GSMATE( 3,'NAI(TL)$    ', 74.95,32.,3.67,   2.6,   41.3,0,0)
*
*             Defines tracking media parameters.
      FIELDM =  0.
      IFIELD =  0
      TMAXFD =  0.
      DMAXMS =  0.5
      DEEMAX =  0.2
      EPSIL  =  0.01
      STMIN  =  0.8
*
      CALL GSTMED( 1,'VACUUM            $'    ,  1 , 0 , IFIELD,
     *                FIELDM,TMAXFD,0.,0., EPSIL, STMIN, 0 , 0 )
      CALL GSTMED( 2,'LIQ NE213$'    ,  2 , 0 , IFIELD,
     *                FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
      CALL GSTMED( 3,'NAI(TL)  $'    ,  3 , 0 , IFIELD,
     *                FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
*
*             Change CUTS for Ne213
*
      CALL GSTPAR(2,'CUTGAM',0.00005)
      CALL GSTPAR(2,'CUTELE',0.00005)
*
*             Defines geometry of the set-up
*
        PAR(1)=0.
        PAR(2)=15.
        PAR(3)=80.
      CALL GSVOLU('SETU', 'TUBE', 1, PAR, 3, IVOLU)
        PAR(1)=0.
        PAR(2)=14.
        PAR(3)=4.25
      CALL GSVOLU('DET1', 'TUBE', 2, PAR, 3, IVOLU)
        PAR(1)=0.
        PAR(2)=14.25
        PAR(3)=3.75
      CALL GSVOLU('DET2', 'TUBE', 3, PAR, 3, IVOLU)
C
      CALL GSPOS('DET1', 1, 'SETU', 0, 0,   -75., 0, 'ONLY')
      CALL GSPOS('DET2', 1, 'SETU', 0, 0, 76.25 , 0, 'ONLY')
*
      CALL GSORD('SETU',3)
*
*             Close geometry banks. Mandatory system routine.
*
      CALL GGCLOS
*
      END
