*
* $Id: fzpin.F,v 1.1.1.1 1996/03/06 10:47:21 mclareni Exp $
*
* $Log: fzpin.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
* -----------------------------------------------------------
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE FZPIN (IXDIV,LUN,KEYS,NKEY,IDCAT,IERR)
#include "dzc1.inc"
#include "mqsys.inc"
#include "qequ.inc"
#include "mzcn.inc"
#include "zmach.inc"
#include "bankparq.inc"

      COMMON/ZCFORM/NLFORM,NLSYST,LD(10),L

      CHARACTER CHROUT*(*)
      PARAMETER (CHROUT = 'FZPIN ')
      CHARACTER CNUM*40,CNAME*4,CNAMEL*4,CMISS*4,CBLANK*4

      PARAMETER (MXFORQ = 16)
      INTEGER  KEYS(*),ICFORM(MXFORQ),IFORM(50)
      INTEGER  IH(60) , IL(10) , IM(10) , MODSEP(8)

      DATA MODSEP/11,12,13,13,13,12,13,13/
      DATA CMISS  /'****'/,CBLANK/'    '/
      DATA CNUM/' 1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ*+-'/

      IERR   = 0
      CNAMEL = CBLANK
      NWFORL = 0

      CALL MZSDIV(IXDIV,1)
      JDIVI  = JQDIVI

      CALL MZLINT(IXDIV,'/ZCFORM/',NLFORM,LD(1),L)


      LBUF   = LQSYSS(KQT+MSYSPQ)

      CALL MZCHLS(NCHEKQ,LBUF)
      IF (IQFOUL.NE.0)                                     GO TO 10

      NUNIT  = IQWND(KQS+LBUF)
      IBUF   = IUCOMP (LUN,IQ(KQS+LBUF+1),NUNIT)

      IF (IBUF.EQ.0)  THEN
          IERR   = 900
                                                           GO TO 180
      ENDIF


      LBUF   = LQ(KQS+LBUF-IBUF) + 3
      LASOPD = IQ(KQS+LBUF-2)

      IF (LASOPD.EQ.0)                                     GO TO 10
      IF (LASOPD.EQ.3)                                     GO TO 10

      IDCAT  = IQ(KQS+LBUF-1)
                                                           GO TO 20


   10 CALL FZPNXT (IXDIV,LUN,IDCAT,IERR)
      IF (IERR.NE.0.AND.IERR.NE.6)                         GO TO 180


   20 IF(JDIVI.NE.0) THEN
          CALL MZGARB(IXDIV,0)
      ELSE
          CALL MZGARB(IXDIV+2,0)
      ENDIF


   50 LBUF   = LQSYSS(KQT+MSYSPQ)
      LBUF   = LQ(KQS+LBUF-IBUF) + 3

      CALL EPFHDR (LUN,7,IH,IQ(KQS+LBUF),IERR)
      IF (IERR.NE.0.AND.IERR.NE.6)                         GO TO 180

   60 IF (IH(2).EQ.4)                                      GO TO 170
      IF (IH(2).NE.2)                                      GO TO 50
      IF (NKEY.LE.0)                                       GO TO 50


      CALL EPFRD (LUN,13,NW,IH,IQ(KQS+LBUF),IERR)
      IF (IERR.NE.0)                                       GO TO 180
      CALL ZFRIBM(IH,NW,2)

      KEY    = IH(1)
      IKEY   = IUCOMP (KEY,KEYS,NKEY)
      IF(IKEY.EQ.0)                                        GO TO 50

      CALL FZPLOC (IXDIV,KEY,NID,NMEMB,ICODE)
      IF (NID.EQ.0)                                        GO TO 50
      IF (NMEMB.EQ.0)                                      GO TO 50

      ICODE  = MOD (IH(2),10)
      MODEP  = MODSEP(ICODE + 1)
      NMB    = 0

   70 NST    = 1
      LD(1)  = NID + NMB
      IL(1)  = 2
      IM(1)  = 1


   80 LBUF   = LQSYSS(KQT+MSYSPQ)
      LBUF   = LQ(KQS+LBUF-IBUF) + 3

      CALL EPFHDR (LUN,60,IH,IQ(KQS+LBUF),IERR)
      IF (IERR.NE.0.AND.IERR.NE.6)                         GO TO 180
      IF (IH(2).NE.5)                                      GO TO 83


      NMSS   = IH(4)
      IF (NST.NE.1)                                        GO TO 82
      NMB    = NMB + NMSS
      IF (NMB.GE.NMEMB)                                    GO TO 50
                                                           GO TO 70
   82 IL(NST) = IL(NST) - NMSS
      IF (IL(NST).GT.0)                                    GO TO 80
      NST    = NST - 1
                                                           GO TO 80
   83 IF (IH(2).NE.3)                                      GO TO 60


      NWH    = IH(3)
      NDATA  = IH(1) - IH(3)

      NWFOR  = 0
      IF (NWH.GT.7)             THEN
          NWFOR  = IH(NWH)
          NWH    = NWH-NWFOR-1

      ENDIF

      ISIT   = NWH - 2
      IF (ISIT.GT.2) ISIT = ISIT - 1

      GO TO (84,85,86,87), ISIT

   84 NLINKS = 0
      CNAME  = CBLANK
      NUMB   = 0
                                                           GO TO 89

   85 NLINKS = IH(4)
      CNAME  = CBLANK
      NUMB   = 0
                                                           GO TO 89

   86 NLINKS = 0
      NUMB   = IH(6)
      INDEX  = 4
                                                           GO TO 88

   87 NLINKS = IH(4)
      NUMB   = IH(7)
      INDEX  = 5
   88 J1     = MOD(IH(INDEX),256)
      J2     = MOD(IH(INDEX)/256,256)
      J3     = MOD(IH(INDEX+1),256)
      J4     = MOD(IH(INDEX+1)/256,256)
      CNAME  = CNUM(J1:J1)//CNUM(J2:J2)//CNUM(J3:J3)//CNUM(J4:J4)

      IF (CNAME.EQ.CMISS)                                  GO TO 130

   89 IF(NWFOR.NE.0)  THEN
          NCFORM = MXFORQ
          CALL UCOPY(IH(NWH+1),IFORM,NWFOR)
          CALL FZPFOR(IFORM,NWFOR,ICFORM,NCFORM)
          NWFORL = NWFOR
      ELSEIF(CNAME.NE.CNAMEL)  THEN
          NWFORL = 0
      ENDIF

      IF (ICODE.EQ.1)                                      GO TO 100
      IF (ICODE.EQ.5)                                      GO TO 100
      IF (ICODE.EQ.0)                                      GO TO 90

      NDATA  = NDATA / 2
                                                           GO TO 100

   90 NDATA  = (NDATA*16-1) / NQBITW + 1

  100 JB     = - IM(NST) + IL(NST) - 1
      IF (JB.EQ.0)     THEN
          IF (NST.NE.1) CALL ZFATAM('FZPIN tree error. ')
          CALL MZBOOK
     X    (IXDIV,L,LQ(LD(1)+KQS),1,CNAME,NLINKS,NLINKS,NDATA,ICFORM,0)
          LD(1)  = L
      ELSE
          CALL MZBOOK
     X    (IXDIV,L,LD(NST),JB,CNAME,NLINKS,NLINKS,NDATA,ICFORM,0)
      ENDIF
      LBUF   = LQSYSS(KQT+MSYSPQ)
      LBUF   = LQ(KQS+LBUF-IBUF) + 3
      CALL EPFRD (LUN,MODEP,NW,IQ(KQS+L+1),IQ(KQS+LBUF),IERR)
      IF (IERR.NE.0)                                       GO TO 180
      IF (ICODE.EQ.0)                                      GO TO 130

      IF (NDATA.GT.0) THEN
          IF(NWFORL.EQ.0) THEN
              IF (ICODE.NE.5) CALL ZFRIBM (IQ(KQS+L+1),NDATA,ICODE)
          ELSE
              CALL ZFCONV(IFORM,NWFOR,IQ(KQS+L+1),NDATA,0)
          ENDIF
      ENDIF

  130 CNAMEL  = CNAME
      IL(NST) = IL(NST) - 1
      IF (IL(NST).NE.0)                                    GO TO 140

      NST     = NST - 1

  140 IF (NLINKS.NE.0)                                     GO TO 150
      IF (NST.NE.1)                                        GO TO 80
                                                           GO TO 160

  150 NST     = NST + 1
      LD(NST) = L
      IL(NST) = NLINKS
      IM(NST) = NLINKS
                                                           GO TO 80

  160 NMB     = NMB + 1
      IF (NMB.GE.NMEMB)                                    GO TO 50
                                                           GO TO 70
  170 JF         = LBUF - 2
      IQ(KQS+JF) = 3

  180 NLFORM     = 0

  998 CONTINUE
  999 RETURN
      END
