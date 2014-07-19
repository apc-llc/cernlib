*
* $Id: igsa.F,v 1.3 1998/01/30 15:22:28 couet Exp $
*
* $Log: igsa.F,v $
* Revision 1.3  1998/01/30 15:22:28  couet
* - APOLLO version removed
*
* Revision 1.2  1996/06/05 10:20:33  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.21/01 12/04/94  09.21.01  by  O.Couet
*-- Author :
      SUBROUTINE IGSA(IIWK)
*.===========>
*.
*.   IGSA switches a raster terminal from graphics to alpha mode.
*. The terminal must be an activated HIGZ workstation.This is done by
*. prompting the user who should respond by typing RETURN or any
*. character string followed by return.
*.
*..==========>
#if (defined(CERNLIB_GKSGRAL)||defined(CERNLIB_ATCGKS))&&(defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_SUN)||defined(CERNLIB_ALLIANT))
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/himeta.inc"
#endif
#if (defined(CERNLIB_GKSGRAL))&&(defined(CERNLIB_NEWLIB))
      INTEGER      ATTYPE,ATTVAL
      CHARACTER*80 ISTR
      INTEGER      IB(1)
      LOGICAL      FIRST/.TRUE./
      SAVE         FIRST
      REAL         RX(1)
#endif
#if (defined(CERNLIB_GKSGRAL)||defined(CERNLIB_ATCGKS))&&(defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_SUN)||defined(CERNLIB_ALLIANT))
      COMMON/QUEST/IQUEST(100)
      LOGICAL INTRAC
*.______________________________________
*
      IF(GRFLAG)THEN
         GRFLAG=.FALSE.
         IWK=IIWK
         IF(IWK.EQ.0)IWK=1
         IF(IQUEST(100).EQ.-1)THEN
            IQUEST(100)=0
         ENDIF
         IF(INTRAC(DUMMY))THEN
#endif
#if (defined(CERNLIB_GKSGRAL))&&(defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_ALLIANT))&&(!defined(CERNLIB_ATCGKS))&&(!defined(CERNLIB_NEWLIB))
            IF(TEKACT)CALL GCGTOA(IWK)
#endif
#if (defined(CERNLIB_GKSGRAL))&&(defined(CERNLIB_NEWLIB))
            IF(TEKACT)THEN
*
*          hold graphic to look at
*          wait for enter
*          ignore 1. entry (in alpha window)
*
               IF (IWTYPE.EQ.5003.OR.IWTYPE.EQ.5005) THEN
                  IF(.NOT.FIRST) THEN
                     CALL ASREAD(ATTYPE,ATTVAL,ICOUNT)
                     IB(1) = 2
                     CALL GK5XAO (200, 1,IB, 1,RX,RX,IB   ,IB   )
                  END IF
                  FIRST = .FALSE.
               END IF
*
*     send window switch sequence for FALCO (7878)
*
               IF (IWTYPE.EQ.7878) THEN
                  CALL GKC1AS(105,1,IB)
                  CALL GKC1AW
                  CALL ERASC1
               END IF
*
*     CLEAR ALPHA WINDOW FOR IPS-ATARI
*
               IF (IWTYPE.EQ.4703.OR.IWTYPE.EQ.4713) THEN
                  CALL ERASD3
               END IF
            ENDIF
#endif
#if (defined(CERNLIB_ATCGKS))&&(defined(CERNLIB_VAX)||defined(CERNLIB_SUN))
            IF(TEKACT)CALL GUESC001(IWK,0)
#endif
#if (defined(CERNLIB_GKSGRAL)||defined(CERNLIB_ATCGKS))&&(defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_SUN)||defined(CERNLIB_ALLIANT))
         ENDIF
      ENDIF
#endif
#if (defined(CERNLIB_PLOT10GKS))&&(defined(CERNLIB_IBM))
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      COMMON/QUEST/IQUEST(100)
      CHARACTER*80 REPLY
      INTEGER WKID,LSTRI
      INTEGER T4107
      PARAMETER (IPG=301400,IMAC=301401,T4107=410700)
      LOGICAL INTRAC
      DATA PGSW/Z18/
*.______________________________________
*
*
*
      IF(GRFLAG)THEN
         GRFLAG=.FALSE.
         IWK=IIWK
         IF(IWK.EQ.0)IWK=1
         CALL GUWK(IWK,0)
         IF(IQUEST(100).EQ.-1)THEN
            IQUEST(100)=0
         ELSE
            CALL GRQST(IWK,1,IST,LSTRI,REPLY)
         ENDIF
         IF(INTRAC(DUMMY))THEN
            IF(IWTYPE.EQ.IMAC)THEN
               CALL HWRASC(1,PGSW)
               CALL HWRASC(1,PGSW)
            ELSEIF(IWTYPE.EQ.IPG)THEN
               CALL HWRASC(1,PGSW)
            ELSEIF(IWTYPE.EQ.T4107)THEN
               CALL HWRASC(1,27)
               CALL HWRASC(1,37)
               CALL HWRASC(1,33)
               CALL HWRASC(1,49)
            ENDIF
            CALL HTIMEO(500)
         ENDIF
      ENDIF
#endif
#if (defined(CERNLIB_UNIGKS))&&(defined(CERNLIB_IBM))
*
*              W.Wojcik, CCIN2P3/Lyon 19/06/87 (WOJCIK at FRCPN11)
*
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
*
      CHARACTER*20 REPLY
      PARAMETER (LIFN=2,LRFN=1,LCFN=4,LDTR=4)
      DIMENSION IFN(LIFN)
      DIMENSION RFN(LRFN)
      CHARACTER*(LCFN) CFN
      CHARACTER*80 DTR(LDTR)
*
*
      IF (GRFLAG) THEN
         GRFLAG=.FALSE.
         IWK=IIWK
         IF(IWK.EQ.0)IWK=1
*
*              Test if GKS is open...
*
         CALL GQOPS(IFLG)
         IF (IFLG.EQ.0) GO TO 99
*
         CALL GQWKC(IWK,IERR,ICONID,IWTYPE)
*
*              GKS not in proper state
*
         IF (IERR.EQ. 7) GOTO 99
*
*              Specified workstation id is not valid
*
         IF (IERR.EQ.20) GOTO 99
*
*              Specified workstation is not open
*
         IF (IERR.EQ.25) GOTO 99
*
         CALL GUWK(IDID,0)
*
         IFN(1)=IDID
         IFN(2)=3
         RFN(1)=3.
         CFN='BELL'
*
         CALL GPREC(LIFN,IFN,LRFN,RFN,LCFN,CFN,LDTR,DTR)
         CALL GESC(11,LDTR,DTR)
*
         CALL GRQST(IWK,1,IERR,LREPLY,REPLY)
*
         IFN(1)=IDID
         IFN(2)=1
         RFN(1)=1.
         CFN='IGSA'
*
         CALL GPREC(LIFN,IFN,LRFN,RFN,LCFN,CFN,LDTR,DTR)
         CALL GESC(11,LDTR,DTR)
*
      ENDIF
*
#endif
#if (defined(CERNLIB_DECGKS))&&(defined(CERNLIB_VAX))
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/himeta.inc"
#include "higz/hilun.inc"
      COMMON/QUEST/IQUEST(100)
      LOGICAL INTRAC
      CHARACTER*1 BSLASH
*.______________________________________
*
      IF(GRFLAG)THEN
         GRFLAG=.FALSE.
         IWK=IIWK
         IF(IWK.EQ.0)IWK=1
         IF(IQUEST(100).EQ.-1)THEN
            IQUEST(100)=0
         ENDIF
         IF(INTRAC(DUMMY))THEN
           IWK=IIWK
           IF(IWK.EQ.0)IWK=IDID
           IF(IQUEST(100).EQ.-1)THEN
              IQUEST(100)=0
           ENDIF
           IF(IWK.NE.0) THEN
             IF((IWK.EQ.13).OR.(IWK.EQ.14).OR.
     +          (IWK.EQ.16).OR.(IWK.EQ.17)) THEN
#endif
#if (!defined(CERNLIB_BSLASH))&&(defined(CERNLIB_DECGKS))&&(defined(CERNLIB_VAX))
                BSLASH = '\'
#endif
#if (defined(CERNLIB_BSLASH))&&(defined(CERNLIB_DECGKS))&&(defined(CERNLIB_VAX))
                BSLASH = '\\'
#endif
#if (defined(CERNLIB_DECGKS))&&(defined(CERNLIB_VAX))
                WRITE(LUNOUT,'(1X,A2)') CHAR(27)//BSLASH
             ELSEIF ((IWK.EQ.72).OR.(IWK.EQ.82)) THEN
                WRITE(LUNOUT,'(1X,A1)') CHAR(24)
             ENDIF
           ENDIF
*
         ENDIF
      ENDIF
#endif
*
   99 END
#endif
