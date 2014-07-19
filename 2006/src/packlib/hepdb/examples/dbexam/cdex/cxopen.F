*
* $Id: cxopen.F,v 1.1.1.1 1996/02/28 16:23:39 mclareni Exp $
*
* $Log: cxopen.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CXOPEN(LINE,JPAK,IFERR)
*     -----------------------****-====-
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
      CHARACTER LINE*(*)
      INTEGER   JPAK
*
*  (ICDXDIV)
#include "cdxdiv.inc"
*
#include "cdxluns.inc"
*
#include "cdxfils.inc"
*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC+KEEP,CDXFILS.
CC*
CC      INTEGER MCDXFIL
CC      PARAMETER (MCDXFIL = 10)
CC      COMMON /CDXFILS/ NCDXFIL,KCDXFIL,
CC     +                 ICXFSTA(10),CDXFPRF(10),CDXFTOP(10),CDXFNAM(10)
CC      INTEGER   NCDXFIL, KCDXFIL, ICXFSTA
CC      CHARACTER CDXFPRF*2, CDXFTOP*8, CDXFNAM*40
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*
*  -- externals --
      INTEGER JBYT
*
*  -- initialization flag --
      INTEGER   IFINIT
*  -- default/set parameters
*                 for CDOPEN --
      INTEGER   LVOPT
      CHARACTER CDOPT(2)*8
#if !defined(CERNLIB_IBMVM)
      STATIC    IFINIT
      STATIC    LVOPT,CDOPT
#endif
*
*  -- other parameters for CDOPEN --
      CHARACTER CHTOP*8 , CHFILE*40 , CHOPT*6
      INTEGER   LUNDB , LUNFZ , LRECL , IDIV , IRC
*  - - - - - - - - - - - - - -
      INTEGER   LVNEW , LVOLD
      INTEGER   IVNEW , IVOLD
      REAL      RVNEW , RVOLD
      CHARACTER*6 CVNEW , CVOLD
*  ----------------------------
*
*  -- Command string analyse variables --
      INTEGER IFTX , IFNX , LPRF
      CHARACTER PRFX*8 , PRF*8
      INTEGER NUMB
      REAL    FNUM
*
*  -- Local variables --
*
      INTEGER IFERR
      CHARACTER XX*2
      CHARACTER MESL*78
      INTEGER   LMES
      INTEGER   J1 , J2 , LR , LFIL , LEN , JLASNC
      INTEGER   KK , JJ , JE , K , J
*
      DATA IFINIT/0/
*
      IF(IFINIT.EQ.0) THEN
        IFINIT=1
        LVOPT=1
        CDOPT(1)='P'
      ENDIF
*
      IFERR=0
   12 CONTINUE
      CALL TEXINS(LINE,JPAK,IFTX,PRFX,NUMB,FNUM)
      IFNX=JBYT(IFTX,1,2)
      CALL CLTOU(PRFX)
      LPRF=JLASNC(PRFX,' ')
      IF((LPRF.GT.0.AND.LPRF.NE.2).OR.IFNX.EQ.3) THEN
        CALL TEXINS(LINE,JPAK,IFTX,PRF,NUMB,FNUM)
        CALL CLTOU(PRF)
        IF(PRFX(1:1).EQ.'C'.OR.PRFX(1:1).EQ.'O'.OR.PRFX.EQ.' ') THEN
          PRFX='CHOPT'
          LVOLD=LVOPT
          CVOLD=CDOPT(LVOLD)
          IF(PRF.EQ.' '.OR.PRF.EQ.'=') THEN
            LVNEW=1
            CVNEW=CDOPT(1)
          ELSE
            IF(PRF.EQ.CDOPT(1)) THEN
              LVNEW=1
            ELSE
              LVNEW=2
              CVNEW=PRF
            ENDIF
          ENDIF
          LVOPT=LVNEW
          IF(LVNEW.GT.1) CDOPT(LVNEW)=CVNEW
          CALL UXCPARM('CDOPEN',PRFX,LVNEW,CVNEW,LVOLD,CVOLD,CDOPT(1))
        ELSE
*       -- signal an invalid set parameter --
          CALL UXCPARM('CDOPEN',PRFX,0,' ',0,' ',' ')
        ENDIF
      ELSE
*     -- it is a database ID (prefix)
        XX=PRFX
*       -- Locate the XX in the CDXFILS table
        KK=0
        DO 111 K=1,NCDXFIL
          IF(KK.EQ.0.AND.CDXFPRF(K).EQ.XX ) KK=K
  111   CONTINUE
        IF(KK.GT.0) THEN
          IF(ICXFSTA(KK).EQ.-1) THEN
*         -- OK, get file info from CDXFILS table
            CHTOP =CDXFTOP(KK)
            CHFILE=CDXFNAM(KK)
*           -- other call parameters --
            LUNDB=LUNAMES+KK
            LUNFZ=LUNDB+10
            LRECL=0
            IDIV=ICDXDIV
            CHOPT=CDOPT(LVOPT)
*
            CALL CDX_MESS('>>CDOPEN call<<')
            CALL CDX_MESS('             chtop : '//CHTOP)
            CALL CDX_MESS('             chfile: '//CHFILE)
            CALL CDX_MESS('             ChOpt : '//CHOPT)
            CALL CDX_MESS('             lrecl : 0')
            MESL='>>CDOPEN('
            WRITE(MESL(10:),1111) LUNDB,LUNFZ,IDIV
 1111       FORMAT(2(I3,','),'CHTOP,CHFILE,LRECL,',I4,',','CHOPT,IRC)')
            LMES=INDEX(MESL,')')
            CALL CDX_MESS(MESL(:LMES))
*
            CALL CDOPEN(LUNDB,LUNFZ,CHTOP,CHFILE,LRECL,IDIV,CHOPT,IRC)
            CALL CDI_MESS('==CDOPEN>>                LRECL:0',LRECL)
            IF(IRC.NE.0) CALL UXIRCM('CDOPEN',IRC)
*
            ICXFSTA(KK)=IABS(IRC)+1
          ENDIF
        ELSE
        ENDIF
      ENDIF
      IF(IFNX.GT.0.AND.IFERR.EQ.0) GOTO 12
*
      END
