*
* $Id: ipmid.F,v 1.2 1996/04/24 09:28:14 dinofm Exp $
*
* $Log: ipmid.F,v $
* Revision 1.2  1996/04/24 09:28:14  dinofm
* IPM4ID command handling added.
*
* Revision 1.1.1.1  1996/03/01 11:39:26  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.04/02 16/11/93  20.23.23  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   02/05/93
      SUBROUTINE IPMID(N,X,Y,LEVEL,ID)
      DIMENSION X(*),Y(*),Z(*),ID(*),U(*)
*
* Piaf server stub: tell client to receive points and call IPMID
*
#include "paw/pchost.inc"
#include "hbook/hcbook.inc"

      CHARACTER CHMAIL*80, NAME*6

      NAME='IPMID'
      NDIM=2
      GOTO 1

      ENTRY IPM4ID(N,X,Y,Z,U,V,W,LEVEL,ID)
*---- Old PAW client can't cope with HPLFR3 command
      IF (ISPROTO.LT.2) RETURN
      NAME='IPM4ID'
      NDIM=4
      GOTO 1

      ENTRY IPM3ID(N,X,Y,Z,LEVEL,ID)
      NAME='IPM3ID'
      NDIM=3
      GOTO 1

      ENTRY IPM(N,X,Y)
      NAME='IPM'
      NDIM=2
      LEVEL=0
      GOTO 1

      ENTRY IPL(N,X,Y)
      NAME='IPL'
      NDIM=2
      LEVEL=0
      GOTO 1

      ENTRY IPL3(N,X,Y,Z)
      NAME='IPL3'
      NDIM=3
      LEVEL=0
      GOTO 1

 1    CONTINUE
      IF (NDIM.LT.4) THEN
         WRITE(CHMAIL,'(A,2I12)') NAME,N,LEVEL
      ELSE
         WRITE(CHMAIL,'(A,2I12,2E15.9)') NAME,N,LEVEL,V,W
      ENDIF
      CALL CZPUTA(CHMAIL,ISTAT)
*
* pack point into a stand-alone bank
*
      CALL MZBOOK(IHWORK,LPIAF,0,2,'PIAF',0,0,N*NDIM,3,-1)
      CALL UCOPY(X(1),Q(LPIAF+1),N)
      CALL UCOPY(Y(1),Q(LPIAF+1+N),N)
      IF(NDIM.GE.3) CALL UCOPY(Z(1),Q(LPIAF+1+N*2),N)
      IF(NDIM.EQ.4) THEN 
         CALL UCOPY(U(1),Q(LPIAF+1+N*3),N)
      ENDIF
      CALL FZOUT(998,IHWORK,LPIAF,1,' ',0,0,0)
      CALL MZDROP(IHWORK,LPIAF,' ')

      IF(NAME.EQ.'IPM' .OR. NAME.EQ.'IPL' .OR. NAME.EQ.'IPL3') RETURN
*
* same for IDs
*
      CALL MZBOOK(IHWORK,LPIAF,0,2,'PIAF',0,0,N,2,-1)
      CALL UCOPY(ID(1),IQ(LPIAF+1),N)
      CALL FZOUT(998,IHWORK,LPIAF,1,' ',0,0,0)
      CALL MZDROP(IHWORK,LPIAF,' ')

      END


