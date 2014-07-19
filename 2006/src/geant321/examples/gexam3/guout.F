*
* $Id: guout.F,v 1.1.1.1 1995/10/24 10:22:13 cernlib Exp $
*
* $Log: guout.F,v $
* Revision 1.1.1.1  1995/10/24 10:22:13  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.35  by  S.Giani
*-- Author :
      SUBROUTINE GUOUT
C.
C.    ******************************************************************
C.    *                                                                *
C.    *                                                                *
C.    *       User routine called at the end of each event             *
C.    *                                                                *
C.    *                                                                *
C.    ******************************************************************
C.
#include "geant321/gclist.inc"
#include "geant321/gcflag.inc"
      DIMENSION NUMVS(2),NUMBV(2,50),HITS(9,50),ITRA(50)
      CHARACTER*4 CHSAVE(20)
      SAVE NUMVS
      DATA NUMVS/0,1/
C.
C.    ------------------------------------------------------------------
C.
      IF(IDEBUG.NE.0)THEN
        IF(ISWIT(1).EQ.1)THEN
           CALL GPVERT(0)
           CALL GPKINE(0)
        ENDIF
        IF(ISWIT(2).EQ.1)THEN
           CALL GPJXYZ(0)
        ENDIF
        IF(ISWIT(3).EQ.10)THEN
           CALL GPHITS('*','*')
        ENDIF
      ENDIF
C
      CALL GFHITS('DRFT','FSP ',2,9,50,0,NUMVS,ITRA,NUMBV,HITS,NHITS)
C
      IF(NHITS.LE.0)GO TO 20
      DO 10 IHIT=1,NHITS
      ID=0
      IF(NUMBV(1,IHIT).EQ.1)ID=100
      IF(NUMBV(1,IHIT).EQ.2)ID=110
      X=HITS(4,IHIT)
      Y=HITS(5,IHIT)
      Z=HITS(6,IHIT)
      IF(ID.EQ.0)GO TO 10
      CALL HFILL(ID  ,X,0.,1.)
      CALL HFILL(ID+1,Y,0.,1.)
      CALL HFILL(ID+2,Z,0.,1.)
   10 CONTINUE
C
C                    SAVE EVENT
C
  20  IF(NSAVE.LE.0)GO TO 30
      DO 25 I=1,NSAVE
         CALL UHTOC(LSAVE(I),4,CHSAVE(I),4)
  25  CONTINUE
      CALL GFOUT(2,CHSAVE,NSAVE,1,'T',IER)
C
  30  CALL DLSOUT
C
   99 RETURN
      END
