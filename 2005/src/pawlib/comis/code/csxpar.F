*
* $Id: csxpar.F,v 1.1.1.1 1996/02/26 17:16:25 mclareni Exp $
*
* $Log: csxpar.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:25  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.18/00 18/02/94  12.35.15  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSXPAR(N)
***-----------------------
#include "comis/cswpar.inc"
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/comis.inc"
*un+SEQ,CSBUF.
#include "comis/csrec.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/cssysd.inc"
#include "comis/csdpvs.inc"
      PARAMETER (KCALL=92,KCALLS=87)
**      DATA K2HCS,K3HPAR/4HCS  ,4HPAR /
      DATA NOENTR/0/
**      DATA K1HN  ,K1HK  ,K1HC  ,K1HI  ,K1HR  ,K1HL  ,K1HT  ,K1HS/
**     +      4HN   ,4HK   ,4HC   ,4HI   ,4HR   ,4HL   ,4HT   ,4HS   /
      IF(NOENTR.EQ.0)THEN
        K1HN=ICHAR('N')
        K1HK=ICHAR('K')
        K1HC=ICHAR('C')
        K1HI=ICHAR('I')
        K1HR=ICHAR('R')
        K1HL=ICHAR('L')
        K1HT=ICHAR('T')
        K1HS=ICHAR('S')
        NOENTR=1
      ENDIF
      J=MJCHAR(IDGP(1))
      IFPP=IFP
      NSADR=NBYTPW
      IF(MLEQS(MJSCHA('CS  '),J,2).EQ.1
     + .AND.MLEQS(MJSCHA('PAR '),J+3,3).EQ.1)THEN
         K=MKCHAR(J+2)
         IF(K.EQ.K1HN.OR.K.EQ.K1HK.OR.K.EQ.K1HC.OR.K.EQ.K1HI
     +  .OR.K.EQ.K1HR.OR.K.EQ.K1HL.OR.K.EQ.K1HT.OR.K.EQ.K1HS)THEN
            IFPP=IA(IFPP)
            NSADR=1
         ENDIF
      ENDIF
      IBASEP=IA(IFPP+2)
      IPCALL=IA(IFPP+4)
      KCALLT=KD(IPCALL)
      NPAR=KD2(IPCALL)
      IREP=0
      IF(N.EQ.0)RETURN
      IF(NPAR.GT.0)THEN
        IF(N.GT.0.AND.N.LE.NPAR)THEN
          IREP=1
          I=IFPP-NPAR-1+N
#if defined(CERNLIB_VAX)
          JRESP=IA(I)/NSADR
#endif
#if !defined(CERNLIB_VAX)
          JRESP=IA(I)
#endif
          IF(KCALLT.EQ.KCALL)THEN
            I=IPCALL+N*3
            IPCB=KD(I)+IBASEP
            IPCE=KD1(I)+IBASEP
            ITB=KD2(I)
            JCHP=MJCHAR(KD(IPCE+2))
            NCHP=KD(IPCE+1)*NBYTPW
          ELSE
            I=IPCALL+N
            IPCB=0
            IPCE=0
            ITB=KD2(I)
            JCHP=0
            NCHP=0
          ENDIF
          ITP=MOD(ITB,100)
          IBP=ITB/100
        ENDIF
      ENDIF
      END
