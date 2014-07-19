*
* $Id: gecdhs.F,v 1.2 1996/02/01 14:54:48 ravndal Exp $
*
* $Log: gecdhs.F,v $
* Revision 1.2  1996/02/01 14:54:48  ravndal
* Clean up the repository
*
* Revision 1.1.1.1  1995/10/24 10:22:11  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GECDHS
*
************************************************************************
*                                                                      *
*             Routine to define the geometry of the set-up.            *
*                                                                      *
************************************************************************
*
#include "cdhscom.inc"
*
      DIMENSION PAR(10)
*
*     -----------------------------------------------------------------
*
*
*             Defines materials
      CALL GSMATE( 1,'AIR$     ',  15.0,7.0,0.0012,30050.0,67500.0,0,0)
      CALL GSMATE( 2,'PLAST SC$',  6.25,3.4,1.032 ,   43.0,   437.,0,0)
      CALL GSMATE( 3,'IRON$    ', 55.85,26.,7.8   ,   1.76,   17.1,0,0)
*
*             Defines tracking media parameters.
      FIELDM =  0.
      IFIELD =  0
      TMAXFD =  0.
      DMAXMS = +0.50
      DEEMAX = +0.20
      EPSIL  =  0.01
      STMIN  =  0.80
*
      CALL GSTMED( 1,'AIR               $'    ,  1 , 0 , IFIELD,
     *                FIELDM,TMAXFD,0.,0., EPSIL, STMIN, 0 , 0 )
      CALL GSTMED( 2,'PLASTIC SCINTILLAT$'    ,  2 , 0 , IFIELD,
     *                FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
      CALL GSTMED( 3,'IRON              $'    ,  3 , 0 , IFIELD,
     *                FIELDM,TMAXFD,DMAXMS,DEEMAX, EPSIL, STMIN, 0 , 0 )
*
*
*
*             Defines geometry of the set-up
*
*             Define the overall calorimeter
*
      PAR(1)=0.
      PAR(2)=187.5
      PAR(3)=166.0
      CALL GSVOLU('CDHS', 'TUBE', 1, PAR, 3, IVOLU)
*
*             Now define one module,
*
      PAR(1)=0.0
      PAR(3)=31.
      CALL GSVOLU('MODU', 'TUBE', 1, PAR, 3, IVOLU)
*
*             position it
*
      ZC = -225.
      DO 1 J=1,4
        ZC=ZC+90.
        CALL GSPOS('MODU', J, 'CDHS', 0.0, 0.0, ZC, 0, 'ONLY')
   1  CONTINUE
*
*             and divide it.
*
      CALL GSDVN('PLAN', 'MODU',20, 3)
*
*             Define the iron block
*
      PAR(3)=1.225
      CALL GSVOLU('SHEE', 'TUBE', 3, PAR, 3, IVOLU)
*
*             and position it.
*
      CALL GSPOS('SHEE', 1, 'PLAN', 0.0, 0.0, -0.325, 0, 'ONLY')
*
*             Define the scintillator thickness
*
      PAR(3)=0.23
      CALL GSVOLU('SINT', 'TUBE', 2, PAR, 3, IVOLU)
*
*             and position it.
*
      CALL GSPOS('SINT', 1, 'PLAN', 0.0, 0.0, 1.13, 0, 'ONLY')
*
*
*             Optimisation of geometry definition
*
      CALL GSORD('CDHS',3)
      CALL GSORD('PLAN',3)
*
*             Close geometry banks. Mandatory system routine.
*
      CALL GGCLOS
*
      END
