*
* $Id: igsg.F,v 1.4 1998/01/30 15:22:29 couet Exp $
*
* $Log: igsg.F,v $
* Revision 1.4  1998/01/30 15:22:29  couet
* - APOLLO version removed
*
* Revision 1.3  1998/01/28 14:34:31  couet
* - GGDM driver removed
*
* Revision 1.2  1996/06/05 10:20:34  cernlib
* Move pilot.h before the ifdef for GKS
*
* Revision 1.1.1.1  1996/02/14 13:10:44  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_GKS)
*CMZ :  1.18/03 25/05/93  16.58.09  by  O.Couet
*-- Author :
      SUBROUTINE IGSG(IIWK)
*.===========>
*.
*.   IGSG switches a raster terminal from alpha to graphics mode .
*. The terminal must be an activated HIGZ workstation .
*.
*..==========> (O.Couet)
#if (defined(CERNLIB_GKSGRAL)||defined(CERNLIB_ATCGKS))&&(defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_SUN)||defined(CERNLIB_ALLIANT))
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/himeta.inc"
#endif
#if (defined(CERNLIB_GKSGRAL))&&(defined(CERNLIB_NEWLIB))
      INTEGER      WKID
      INTEGER      ATTYPE,ATTVAL
      CHARACTER*80 ISTR
      INTEGER      IB(1)
      REAL         RX(1)
#endif
#if (defined(CERNLIB_GKSGRAL)||defined(CERNLIB_ATCGKS))&&(defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_SUN)||defined(CERNLIB_ALLIANT))
*.______________________________________
*
      IF(.NOT.GRFLAG)THEN
         GRFLAG=.TRUE.
         IWK=IIWK
         IF(IWK.EQ.0)IWK=1
#endif
#if (defined(CERNLIB_GKSGRAL))&&(defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_ALLIANT))&&(!defined(CERNLIB_ATCGKS))&&(!defined(CERNLIB_NEWLIB))
         IF(TEKACT)CALL GCATOG(IWK)
#endif
#if (defined(CERNLIB_GKSGRAL))&&(defined(CERNLIB_NEWLIB))
         IF(TEKACT)THEN
*
*           switch from alpha to graphic window
*
            IF (IWTYPE.EQ.5003.OR.IWTYPE.EQ.5005) THEN
               IB(1) = 1
               CALL GK5XAO (200, 1,IB, 1,RX,RX,IB   ,IB   )
            END IF
*
*           treatment for FALCO (7878)
*
            IF (IWTYPE.EQ.7878) THEN
               CALL GKC1AS(106,1,IB)
               CALL GKC1AW
            ENDIF
         ENDIF
#endif
#if (defined(CERNLIB_ATCGKS))&&(defined(CERNLIB_VAX)||defined(CERNLIB_SUN))
         IF(TEKACT)CALL GUESC001(IWK,1)
#endif
#if (defined(CERNLIB_GKSGRAL)||defined(CERNLIB_ATCGKS))&&(defined(CERNLIB_IBM)||defined(CERNLIB_VAX)||defined(CERNLIB_SUN)||defined(CERNLIB_ALLIANT))
      ENDIF
#endif
#if (defined(CERNLIB_PLOT10GKS))&&(defined(CERNLIB_IBM))
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      INTEGER T4107
      PARAMETER (IPG=301400,IMAC=301401,T4107=410700)
      DATA ESC/Z1B/,GS/Z1D/
*.______________________________________
*
      IF(.NOT.GRFLAG)THEN
         GRFLAG=.TRUE.
         IF(IWTYPE.EQ.IMAC)THEN
            CALL HWRAS(1,ESC)
            CALL HWRAS(1,GS)
         ELSEIF(IWTYPE.EQ.IPG)THEN
            CALL HWRAS(1,ESC)
            CALL HWRAS(1,GS)
         ELSEIF(IWTYPE.EQ.T4107)THEN
            CALL HWRASC(1,27)
            CALL HWRASC(1,37)
            CALL HWRASC(1,33)
            CALL HWRASC(1,48)
         ENDIF
      ENDIF
#endif
#if (defined(CERNLIB_UNIGKS))&&(defined(CERNLIB_IBM))
* W.WOJCIK, CCIN2P3/LYON 19/06/87 (WOJCIK AT FRCPN11)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
*
      PARAMETER (LIFN=2,LRFN=1,LCFN=4,LDTR=4)
      DIMENSION IFN(LIFN)
      DIMENSION RFN(LRFN)
      CHARACTER*(LCFN) CFN
      CHARACTER*80 DTR(LDTR)
*
      CHARACTER*80 MSG
      INTEGER NTLUN,NSEQ
*
      DATA NTLUN,NSEQ/88,1/
*
      IF (.NOT.GRFLAG) THEN
         GRFLAG=.TRUE.
*
* TEST IF GKS IS OPEN...
         IWK=IIWK
         IF(IWK.EQ.0)IWK=1
         CALL GQOPS(IFLG)
         IF (IFLG.EQ.0) GO TO 99
*
         CALL GQWKC(IWK,IERR,ICONID,IWTYPE)
* GKS NOT IN PROPER STATE
         IF (IERR.EQ. 7) GO TO 99
* SPECIFIED WORKSTATION ID IS NOT VALID
         IF (IERR.EQ.20) GO TO 99
* SPECIFIED WORKSTATION IS NOT OPEN
         IF (IERR.EQ.25) GO TO 99
*
         IF (NSEQ .EQ. 1) THEN
            MSG='FI FT00F001 TERM'
            IF (NTLUN .GT. 9) THEN
               WRITE(MSG(6:7),FMT='(I2)') NTLUN
            ELSE
               WRITE(MSG(7:7),FMT='(I1)') NTLUN
            ENDIF
            CALL VMCMS(MSG,IRC)
         ENDIF
         READ(UNIT=NTLUN,FMT='(A)',END=11) MSG
         GO TO 22
   11    NSEQ = NSEQ + 1
         MSG='FI FT00F001 TERM'
         IF (NTLUN .GT. 9) THEN
            WRITE(MSG(6:7),FMT='(I2)') NTLUN
         ELSE
            WRITE(MSG(7:7),FMT='(I1)') NTLUN
         ENDIF
         IF (NSEQ .GT. 99) THEN
            WRITE(MSG(9:11),FMT='(I3)') NSEQ
         ELSE IF (NSEQ .GT. 9) THEN
            WRITE(MSG(10:11),FMT='(I2)') NSEQ
         ELSE
            WRITE(MSG(11:11),FMT='(I1)') NSEQ
         ENDIF
         CALL VMCMS(MSG,IRC)
   22    IRC=IOSCLR()
*
         IFN(1)=IWK
         IFN(2)=2
         RFN(1)=2.
         CFN='IGSG'
*
         CALL GPREC(LIFN,IFN,LRFN,RFN,LCFN,CFN,LDTR,DTR)
         CALL GESC(11,LDTR,DTR)
*
      ENDIF
#endif
#if (defined(CERNLIB_DECGKS))&&(defined(CERNLIB_VAX))
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#include "higz/himeta.inc"
#include "higz/hilun.inc"
*.______________________________________
*
      IF(.NOT.GRFLAG)THEN
         GRFLAG=.TRUE.
         IWK=IIWK
         IF(IWK.EQ.0)IWK=1
         IF(IWK.NE.0) THEN
           IF((IWK.EQ.13).OR.(IWK.EQ.14).OR.
     +        (IWK.EQ.16).OR.(IWK.EQ.17)) THEN
              WRITE(LUNOUT,'(1X,A4)') CHAR(27)//'[2J'
           ELSEIF ((IWK.EQ.72).OR.(IWK.EQ.82)) THEN
              WRITE(LUNOUT,'(1X,A4)') CHAR(27)//'[2J'
           ENDIF
         ENDIF
*
      ENDIF
#endif
*
      END
#endif
