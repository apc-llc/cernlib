*
* $Id: guout.F,v 1.2 1996/02/22 13:16:31 ravndal Exp $
*
* $Log: guout.F,v $
* Revision 1.2  1996/02/22 13:16:31  ravndal
* Cleaning up CARTOCVS conversion
*
* Revision 1.1.1.1  1995/10/24 10:22:19  cernlib
* Geant
*
*
#include "geant321/pilot.h"
*CMZ :  3.21/02 29/03/94  15.41.36  by  S.Giani
*-- Author :
      SUBROUTINE GUOUT
C.    *
C.    *       User routine called at the end of each event
C.    *
C
#include "geant321/gcbank.inc"
#include "geant321/gcflag.inc"
#include "geant321/gcunit.inc"
#include "shobuf.inc"
C
      PARAMETER (NV=3,NHMX=1000)
      DIMENSION  NVL(NV),ISLC(NV,NHMX),ECELL(NHMX),LTRA(NHMX)
C
      PARAMETER (IWS1=NV*NHMX+1,IWS2=IWS1+NHMX,IWS3=IWS2+NHMX)
      EQUIVALENCE  ( WS(   1), ISLC(1,1) )
     *            ,( WS(IWS1), ECELL(1)  )
     *            ,( WS(IWS2), LTRA (1)  )
C
      SAVE NVL
      DATA NVL / NV*0 /
C
C *** Fetch the cells fired
C
      IF(IDEBUG*ISWIT(3).EQ.1) CALL GPHITS('CELL','SLIZ')
      CALL MZWORK(IXSTOR,WS(1),WS(IWS3),0)
      CALL GFHITS('CELL','SLIZ',NV,1,NHMX,0,NVL,LTRA,ISLC,ECELL,NHITS)
      IF(NHITS.LE.0) RETURN
C
C *** Writout shower
C
      IENBUF(2+ISHOW)=IRECOR
      IWORD = 0
      ECHECK = 0.
      DO 10 IHIT =1,NHITS
      CALL HFILL( 1, ECELL(IHIT)*1000.,0.,1.)
      IF(ECELL(IHIT).LT.CUTLOW) GO TO 10
      ECHECK = ECHECK+ECELL(IHIT)
      NFIRE(IENERG) = NFIRE(IENERG) + 1
      IWORD=IWORD+2
      IX=ISLC(1,IHIT)
      IY=ISLC(2,IHIT)
      IZ=ISLC(3,IHIT)
      IRI=NXBIN*NYBIN*(IZ-1)+NXBIN*(IY-1)+IX+1
       BUF(IWORD-1)=ECELL(IHIT)*1000.
      IBUF(IWORD)  =IRI
C
      IF(IWORD.EQ.NWORD) THEN
CCCC     PRINT *,'IRECOR,IBUF  ',IRECOR,(BUF(J),IBUF(J+1),J=1,NWORD,2)
         WRITE(LUNITS(1),REC=IRECOR) IBUF
         CALL VZERO(IBUF,NWORD)
         IWORD=0
         INQUIRE(LUNITS(1),NEXTREC=IRECOR)
      ENDIF
   10 CONTINUE
 
      IF(IWORD.GT.0) THEN
CCCC     PRINT *,'IRECOR,IBUF  ',IRECOR,(BUF(J),IBUF(J+1),J=1,NWORD,2)
         WRITE(LUNITS(1),REC=IRECOR) IBUF
         CALL VZERO(IBUF,NWORD)
         IWORD=0
         INQUIRE(LUNITS(1),NEXTREC=IRECOR)
      ENDIF
 
      IENBUF(2+ISHOW+1)=IRECOR
C
      ECHECK = 100.*ECHECK/ESHOW(IENERG)
      ID = 1000*(ESHOW(IENERG)+0.0005)
      CALL HFILL( ID ,ECHECK,0.,1.)
C
C *** Write directory block if necessary
C
      IF(ISHOW.EQ.NSHOW(IENERG)) THEN
         NRCOR(IENERG)=IENBUF(3+ISHOW)-IENBUF(3)
CCCC     PRINT *,' DIR. BLOCK  ',ENBUF(1),(IENBUF(J),J=2,NWORD)
         WRITE(LUNITS(1),REC=IENERG+1) IENBUF
      ENDIF
      END
