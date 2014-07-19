*
* $Id: guout.F,v 1.2 1996/02/22 13:32:38 ravndal Exp $
*
* $Log: guout.F,v $
* Revision 1.2  1996/02/22 13:32:38  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:24  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUOUT
************************************************************************
*                                                                      *
*             GEANT3 user routine called at the end of each event.     *
*                                                                      *
************************************************************************
#include "geant321/gcunit.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcbank.inc"
#include "geant321/gclist.inc"
#include "geant321/gcsets.inc"
#include "geant321/gcnum.inc"
#include "hboot.inc"
      CHARACTER*4 LISTC(3),LIST(10)
      DATA LISTC/'KINE','VERT','HITS'/
*
      PARAMETER  (NDDIM=2, NHDIM=1, NHMAX=100, NVDIM=4)
      INTEGER*2  NTOT,KDIGI(NDDIM,NHMAX)
      INTEGER  LITRA(1), LLAYF(NHMAX), LLAYZ(NHMAX)
     +        ,LTOWF(NHMAX), LTOWZ(NHMAX), LTRAF(NHMAX), LTRAZ(NHMAX)
     +        ,NUMBF(NVDIM,NHMAX), NUMBZ(NVDIM,NHMAX), NUMVS(NVDIM)
      REAL  HITF(NHDIM,NHMAX), HITZ(NHDIM,NHMAX)
      CHARACTER*4  HBF, HBZ
*
      IF(IDEBUG.NE.0) CALL GPHITS('*   ','*   ')
 
      IF(NSAVE.GT.0) THEN
         NLIST=3
         DO 10 I=1,NLIST
   10    LIST(I)=LISTC(I)
         CALL GFOUT(2,LIST,NLIST,IDEVT,' ',IER)
      ENDIF
*
      IF (LHBOOT(1) .EQ. 1) THEN
         NSET = IQ(JSET - 1)
         CALL GLOOK('HTST',IQ(JSET+1),NSET,ISET)
         IF(ISET.LE.0) RETURN
         JS = LQ(JSET-ISET)
*
         CALL VZERO (NUMVS, NVDIM)
         HBF = 'HBFC'
         HBZ = 'HBZC'
*
* *** Extract hits for each of the 3 relevant rings in turn
*
         CALL GFHITS ('HTST', HBF, NVDIM,NHDIM,NHMAX, 0, NUMVS, LTRAF,
     +   NUMBF, HITF, NHITF)
         CALL GFHITS ('HTST', HBZ, NVDIM,NHDIM,NHMAX, 0, NUMVS, LTRAZ,
     +   NUMBZ, HITZ, NHITZ)
         NTOT=NHITF+NHITZ
*
         ETOT = 0.0
         CALL HFILL(201,1.*NTOT,0.,1.)
         DO 20 I=1,NHITF
            IL=(NUMBF(3,I)+1)/2
            IT=NUMBF(4,I)
            CALL HFILL(202,HITF(1,I),0.,1.)
            RL = IL * 2. -1
            CALL HFILL(204,RL, 0., HITF(1,I))
            MML = MIN0(IL,26)
            RIT = IT-17-0.5*(MML-1)
            CALL HFILL(205,RIT, 0., HITF(1,I))
            ETOT = ETOT + HITF(1,I)
   20    CONTINUE
         DO 30 I=1,NHITZ
            IL=NUMBZ(3,I)/2
            IT=NUMBZ(4,I)
            CALL HFILL(202,HITZ(1,I),0.,1.)
            RL = IL * 2.
            CALL HFILL(204,RL, 0., HITZ(1,I))
            CALL HFILL(205,(IT-23)*1., 0., HITZ(1,I))
            ETOT = ETOT + HITZ(1,I)
   30    CONTINUE
*
         CALL HFILL(203,ETOT,0.0,1.)
*
      ELSE
*
         CALL BTSTOR
*
      END IF
*
      END
