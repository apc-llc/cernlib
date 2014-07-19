*
* $Id: cspawv.F,v 1.1.1.1 1996/02/26 17:16:22 mclareni Exp $
*
* $Log: cspawv.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:22  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_PAW)
*CMZ :  1.17/01 26/10/93  16.50.51  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      INTEGER FUNCTION CSPAWV(NUMGB,ITYPE,KDA,NDIM,INFVEC,IPCE)
***--------------------------------------------
*       treatments current vect from VECTOR list
***--------------------------------------------
      INTEGER INFVEC(11), LENGTH(3),INF(11)
      INTEGER CSLTLI,CSITLI,CSKUIX
      COMMON/QUEST/IQUEST(100)
      COMMON/MDPOOL/IQ(99)
      CHARACTER *32 VNAME
#include "comis/cspar.inc"
*un+SEQ,COMIS.
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
      CSPAWV=1
      ITYPE=ABS(ITYPE)
      VNAME=' '
      CALL CCOPYS(JID,MJSCHA(VNAME),NCIDEN)
      CALL KUVECT(VNAME,LLOW,LHIGH)
      IL=CSLTLI(IPVSL)
      IF(IL.NE.0)THEN
        IF(NUMGI.NE.0.OR.MODEGI.GT.1)THEN
*         not local   or  array
          CSPAWV=-19
          RETURN
        ELSEIF(ITYPE.GT.2)THEN
          PRINT 77,' CS:KUIP vector ',VNAME, ' not integer or real'
          CSPAWV=-1
          RETURN
        ENDIF
      ENDIF
      IF(NDIM.EQ.0)THEN
*       vector must be existing
        IF(LLOW.EQ.0)THEN
          CSPAWV=-1
          PRINT 77,' CS:KUIP vector ',VNAME, ' is not existing'
 77   FORMAT(1X,3A)
          RETURN
        ENDIF
* fill infvec
        LENGTH(1)=IQUEST(31)
        LENGTH(2)=IQUEST(32)
        LENGTH(3)=IQUEST(33)
        CALL CKTOIV(LENGTH,INFVEC,NDIM)
      ELSEIF(NDIM.GT.3)THEN
          CSPAWV=-1
          PRINT 77,' CS:KUIP vector ',VNAME, ' has more then 3 dims'
          RETURN
      ELSE
*     ndim>0  --> vect(i...)
        IF(LLOW.EQ.0)THEN
*       create kuip vector
          IF(NDIM.EQ.1)THEN
            LENGTH(1)=INFVEC(5)
            LENGTH(2)=0
            LENGTH(3)=0
          ELSEIF(NDIM.EQ.2)THEN
            LENGTH(1)=INFVEC(5)
            LENGTH(2)=INFVEC(6)/INFVEC(5)
            LENGTH(3)=0
          ELSE
            LENGTH(1)=INFVEC(5)
            LENGTH(2)=INFVEC(6)/INFVEC(5)
            LENGTH(3)=INFVEC(7)/INFVEC(6)
          ENDIF
          IF(ITYPE.EQ.1)THEN
            CALL KUVCRE(VNAME,'I',LENGTH,LLOW,LHIGH)
          ELSE
            CALL KUVCRE(VNAME,'R',LENGTH,LLOW,LHIGH)
          ENDIF
          IF(LLOW.EQ.0)THEN
            IGSST=-1
            PRINT 77,' CS:KUIP vector ',VNAME, ' not created'
            RETURN
          ENDIF
        ELSE
*       check consistency
           LENGTH(1)=IQUEST(31)
           LENGTH(2)=IQUEST(32)
           LENGTH(3)=IQUEST(33)
           CALL CKTOIV(LENGTH,INF,ND)
           IF(ND.NE.NDIM)CSPAWV=-1
           IF(NDIM.EQ.1)THEN
             IF(INFVEC(5).GT.INF(5))INFVEC(5)=INF(5)
           ELSEIF(ND.EQ.2)THEN
             IF(INFVEC(5).NE.INF(5).OR.INFVEC(6).NE.INF(6))CSPAWV=-1
           ELSEIF(ND.EQ.3)THEN
             IF(INFVEC(5).NE.INF(5).OR.INFVEC(6).NE.INF(6))CSPAWV=-1
             IF(INFVEC(7).NE.INF(7))CSPAWV=-1
           ENDIF
           IF(CSPAWV.NE.1)THEN
         PRINT 77,' CS:KUIP vector ',VNAME, ' not match declaration'
             RETURN
           ENDIF
        ENDIF
      ENDIF
      IF(IQUEST(14).EQ.1)ITYPGI=2
      IF(IQUEST(14).EQ.2)ITYPGI=1
      LENEGI=1
      ISHGI=CSKUIX(VNAME,LLOW)
      IF(ISHGI.EQ.0)THEN
        CSPAWV=-1
        RETURN
      ENDIF
      NUMGI=NUMGB
      INFVEC(1)=1+KON2
      L=NDIM+4
      MODEGI=MHLOC(L+1)+1
      CALL CCOPYA(INFVEC(1),IQ(MODEGI),L)
      IF(IL.NE.0)THEN
        CALL CSRTLI(IL)
      ELSE
        I=CSITLI(IPVSL)
      ENDIF
      END
#endif
