*
* $Id: fzpkey.F,v 1.1.1.1 1996/03/06 10:47:21 mclareni Exp $
*
* $Log: fzpkey.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
* ----------------------------------------------------------
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE FZPKEY(IXSTOR,LBANK,KEYI,NELEM,MODE,IERR)
#include "dzc1.inc"
#include "mqsys.inc"
#include "qequ.inc"
#include "mzcn.inc"
#include "zmach.inc"
#include "bankparq.inc"
#include "bkfoparq.inc"

      COMMON/LZPKEY/NZPKEY,NZPSYS,LEPK,LKEY,LNID,LNEL,LCOD

      INTEGER LBANK(*)
      CHARACTER CHROUT*(*)
      PARAMETER (CHROUT = 'FZPKEY')
      DATA IFIRST/0/

#if (defined(CERNLIB_DEBUGON))&&(defined(CERNLIB_VFORT))
#include "debugvf2.inc"
#endif
      IERR   = 0
      KEY    = KEYI

      CALL MZSDIV(IXSTOR,0)

      CALL MZLINT(IXSTOR,'/LZPKEY/',NZPKEY,LEPK,LCOD)

      IF (IFIRST.NE.0)         THEN
          LEPK   = LQSYSS(KQT+MSYSKQ)
          CALL MZCHLS(NCHEKQ,LEPK)
          IF (IQFOUL.NE.0)                   THEN
              IERR = 1000*IQFOUL
                                                           GO TO 990
          ENDIF
          LKEY   = LQ(KQS+LEPK-1)
          LNID   = LQ(KQS+LEPK-2)
          LNEL   = LQ(KQS+LEPK-3)
          LCOD   = LQ(KQS+LEPK-4)
          LOC    = 0
      ELSE
          CALL MZBOOK(IXSTOR+JQDVSY,LEPK,LQSYSS(KQT+MSYSKQ),1,
     +                '*EPK',4,4,0,0,0                        )
          CALL MZBOOK(IXSTOR+JQDVSY,LKEY,LEPK,-1,'*KEY',0,0,1,IFOINQ,0)
          CALL MZBOOK(IXSTOR+JQDVSY,LNID,LEPK,-2,'*NID',0,0,1,IFOINQ,0)
          CALL MZBOOK(IXSTOR+JQDVSY,LNEL,LEPK,-3,'*NEL',0,0,1,IFOINQ,0)
          CALL MZBOOK(IXSTOR+JQDVSY,LCOD,LEPK,-4,'*COD',0,0,1,IFOINQ,0)
          IFIRST = 1
          LOC    = 1
      ENDIF
      NKEY   = IQWND(KQS+LKEY)

      IF (NKEY.EQ.0)                                       GO TO 20
      IF (LOC.NE.0)                                        GO TO 20


      IUP    = NKEY + 1
      IDOWN  = 0

  11      IF (IUP-IDOWN.LE.1)                              GO TO 14
          IMEAN  = (IUP+IDOWN) / 2
          LOC    = IMEAN
          IF (KEY-IQ(KQS+LKEY+LOC)) 12,20,13
  12      IUP    = IMEAN
                                                           GO TO 11
  13      IDOWN  = IMEAN
          LOC    = LOC + 1
                                                           GO TO 11
  14      LOC    = -LOC


  20  IF (NELEM.LT.0)                                      GO TO 50
      IF (LOC.GT.0)                                        GO TO 40


      LOC           = -LOC
      CALL MZPUSH(IXSTOR,LKEY,0,1,'I')
      CALL MZPUSH(IXSTOR,LNID,0,1,'I')
      CALL MZPUSH(IXSTOR,LNEL,0,1,'I')
      CALL MZPUSH(IXSTOR,LCOD,0,1,'I')
      NCOPY         = NKEY - LOC + 1
      IF (NCOPY.LE.0)                                      GO TO 40


      DO 30 I=1,NCOPY
          J                = LOC+NCOPY-I
          IQ(KQS+LKEY+J+1) = IQ(KQS+LKEY+J)
          IQ(KQS+LNID+J+1) = IQ(KQS+LNID+J)
          IQ(KQS+LNEL+J+1) = IQ(KQS+LNEL+J)
          IQ(KQS+LCOD+J+1) = IQ(KQS+LCOD+J)
  30  CONTINUE


  40  IQ(KQS+LKEY+LOC) = KEY
      IQ(KQS+LNID+LOC) = LOCF(LBANK(1)) - LQSTOR
      IQ(KQS+LNEL+LOC) = NELEM
      IQ(KQS+LCOD+LOC) = MODE
                                                           GO TO 990


  50  IF (NKEY.EQ.0)                                       GO TO 990
      IF (LOC.LE.0)                                        GO TO 990

      NCOPY  = NKEY - LOC
      IF (NCOPY.LE.0)                                      GO TO 60

      DO 55 I=1,NCOPY
          J                = LOC+I
          IQ(KQS+LKEY+J-1) = IQ(KQS+LKEY+J)
          IQ(KQS+LNID+J-1) = IQ(KQS+LNID+J)
          IQ(KQS+LNEL+J-1) = IQ(KQS+LNEL+J)
          IQ(KQS+LCOD+J-1) = IQ(KQS+LCOD+J)
  55  CONTINUE

  60  CALL MZPUSH(IXSTOR,LKEY,0,-1,'I')
      CALL MZPUSH(IXSTOR,LNID,0,-1,'I')
      CALL MZPUSH(IXSTOR,LNEL,0,-1,'I')
      CALL MZPUSH(IXSTOR,LCOD,0,-1,'I')

  990 NZPKEY = 0
  999 RETURN
      END
