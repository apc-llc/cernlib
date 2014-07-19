*
* $Id: cdls.F,v 1.2 1999/08/17 12:42:39 mclareni Exp $
*
* $Log: cdls.F,v $
* Revision 1.2  1999/08/17 12:42:39  mclareni
* Y2k mods submitted by Rob Komar
*
* Revision 1.1.1.1  1996/02/28 16:24:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLS(CHPATH,IRC)
 
      CHARACTER*(*) CHPATH
      CHARACTER*80  CHLINE
#include "hepdb/nwpaw.inc"
      COMMON /PAWC/  NWPAWC,IXPAWC,IHBOOK,IXHIGZ,IXKU,IFENCE(5)
     +,              LMAIN, IPAW(NWPAW)
#include "zebra/zunit.inc"
#include "zebra/rzcl.inc"
#include "zebra/rzdir.inc"
#include "zebra/rzch.inc"
#include "zebra/rzk.inc"
      DIMENSION     IHTAG(2)
      CHARACTER*8   CHTAG(KNMAX)
      DIMENSION     KEYS(KNMAX),KEYT(KNMAX)
      CHARACTER*36  CHOPT
      CHARACTER*4   CHID
      INTEGER       IDATE,ITIME
      INTEGER       MIDATE,MIDAT,MJDAT
      PARAMETER     (JBIAS=2)
      DIMENSION     MINVAL(10),MAXVAL(10)
#include "hepdb/cdtime.inc"
#include "hepdb/cdkeys.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/cdunit.inc"
#include "hepdb/cdulnk.inc"
#include "hepdb/hdbkeys.inc"
#include "hepdb/cdlscm.inc"
#include "hepdb/hdbopt.inc"
#include "hepdb/hdboptd.inc"
#include "zebra/q_jbit.inc"
* Ignoring t=pass
      CHOPT = CHOPTT
#include "hepdb/hdboptc.inc"
*
*     C - display object count
*     D - display key definitions
*     E - display the experiment keys
*     G - display keys using generic routine (RZPRNK)
*     K - display all keys
*     L - list only lowest level (end node) directories (D)
*     M - show maxima and minima of validity ranges
*     N - display number of data words
*     P - display pathname (D)
*     S - display the system keys
*     T - display insertion date and time (RZ value)
*     U - display user keys
*     V - display validity range pairs
*     Z - dump ZEBRA bank with DZSHOW.
*
      IRC   = 0
 
      LPATH = LENOCC(CHPATH)
      IDATE = 0
      ITIME = 0
      NSHOW = 0
 
*
*     Use modified Window dates such that post-millenium dates are
*     larger than pre-millenium dates.
*
      IF(IDATCD.GT.350000) THEN
         MIDAT = IDATCD
      ELSE IF(IDATCD.EQ.0) THEN
         MIDAT = 0
      ELSE
         MIDAT = IDATCD + 1000000
      ENDIF
      IF(JDATCD.GT.350000) THEN
         MJDAT = JDATCD
      ELSE IF(JDATCD.EQ.0) THEN
         MJDAT = 0
      ELSE
         MJDAT = JDATCD + 1000000
      ENDIF

      DO 10 I=1,NPAICD
         MINVAL(I) =  999999999
         MAXVAL(I) = -999999999
   10 CONTINUE
*
*     Bump directory & object count
*
      NOBJ  = IQ(LCDIR+KQSP+KNKEYS)
      NWKEY = IQ(LCDIR+KQSP+KNWKEY)
      NDIRT = NDIRT + 1
      NOBJT = NOBJT + NOBJ
*
*     Lowest level directory?
*
      IF(IOPTL.NE.0.AND.IQ(KQSP+LCDIR+KNSD).NE.0) RETURN
*
*     Keys
*
      IF(IOPTK.NE.0) THEN
         IOPTE = 1
         IOPTS = 1
         IOPTU = 1
         IOPTV = 1
      ENDIF
 
      WRITE(LOUTCD,*)
*
*     Pathname
*
      IF(IOPTP.NE.0) WRITE(LOUTCD,*) 'Directory: ',CHPATH(1:LPATH)
*
*     Get tags
*
      NWKEY=IQ(KQSP+LCDIR+KNWKEY)
      KTAGS=KKDES+(NWKEY-1)/10+1
      DO 20 J=1,NWKEY
         CALL ZITOH(IQ(KQSP+LCDIR+KTAGS+2*J-2),IHTAG,2)
         CALL UHTOC(IHTAG,4,CHTAG(J),8)
   20 CONTINUE
*
*     Select and display specified objects
*
      IF(IOPTC.NE.0) WRITE(LOUTCD,*) 'Objects  : ',NOBJ
*
*     Key definitions
*
      IF(IOPTD.NE.0) THEN
         WRITE(LOUTCD,*) ' Tags    :'
         WRITE(LOUTCD,9001) (CHTAG(J),J=1,NWKEY)
 9001    FORMAT(8(2X,A8))
      ENDIF
 
      LK   = IQ(KQSP+LCDIR+KLK)
*
*     Loop over objects
*
      DO 60 I=1,NOBJ
*
*     Extract KEYS vector for this object
*
         K = LK+(NWKEY+1)*(I-1)
*
*     Selection on key serial number
*
         IF(IOPTM.EQ.0.AND.
     +   NKEYSN.NE.0.AND.IQ(KQSP+LCDIR+K+1).NE.NKEYSN) GOTO 60
         DO 30 J=1,NWKEY
            IKDES=(J-1)/10
            IKBIT1=3*J-30*IKDES-2
            IKTYPE = JBYT(IQ(KQSP+LCDIR+KKDES+IKDES),IKBIT1,3)
            KEYT(J) = IKTYPE
            IF(IKTYPE.LT.3)THEN
               KEYS(J)=IQ(KQSP+LCDIR+K+J)
            ELSE
               CALL ZITOH(IQ(KQSP+LCDIR+K+J),KEYS(J),1)
            ENDIF
   30    CONTINUE
*
*     Selection on validity ranges
*
         J = 0
         DO 40 K=KOFVAL,NSYSCK,2
            J = J + 1
            IF(KEYS(K)  .LT.MINVAL(J)) MINVAL(J) = KEYS(K)
            IF(KEYS(K+1).GT.MAXVAL(J)) MAXVAL(J) = KEYS(K+1)
   40    CONTINUE
*
*     Selection on key serial number
*
         IF(NKEYSN.NE.0.AND.KEYS(1).NE.NKEYSN) GOTO 60
*
*     Selection on insertion date and time
*
         CALL CDUPTM(IDATE,ITIME,KEYS(KOFINS),IRC)
         IF(IDATE.GT.350000) THEN
            MIDATE = IDATE
         ELSE IF(IDATE.EQ.0) THEN
	    MIDATE = 0
         ELSE
            MIDATE = IDATE + 1000000
         ENDIF
*
*     Insertion date outside range
*     (fixed to include post-millenium dates)
*
         IF(MIDATE.LT.MIDAT.OR.
     +     (MIDATE.GT.MJDAT.AND.MJDAT.NE.0)) GOTO 60
*
*     Insertion date matches - check insertion time
*
         IF(IDATE.EQ.IDATCD) THEN
            IF(ITIME.LT.ITIMCD) GOTO 60
            IF(ITIME.GT.JTIMCD.AND.JTIMCD.NE.0) GOTO 60
         ENDIF
 
         IF(IOPTZ.NE.0) THEN
            CALL RZIN(IXPAWC,LINK01,JBIAS,KEYS,ICYCLE,' ')
            CALL UHTOC(IQ(KQSP+LINK01-4),4,CHID,4)
*
*     Check bank name
*
            LNAME = LENOCC(CHBANK)
            IF(LNAME.EQ.0.OR.CHBANK.EQ.CHID) THEN
               CALL DZSHOW('HEPDB',IXPAWC,LINK01,'LV',ILNK1,ILNK2,
     +                      IDAT1,IDAT2)
               NOBJM = NOBJM + 1
            ENDIF
            CALL MZDROP(IXPAWC,LINK01,'L')
            LINK01 = 0
         ELSE
            NOBJM = NOBJM + 1
         ENDIF
*
*     Pointer to cycles
*
         LKC=LK+(NWKEY+1)*(I-1)
         LCYC=IQ(KQSP+LCDIR+LKC)
*
*     Generic
*
         IF(IOPTG.NE.0) THEN
            IQPRSV = IQPRNT
            IQPRNT = LOUTCD
            ICYC = JBYT(IQ(KQSP+LCDIR+LCYC+3),21,12)
            CALL RZPRNK(CHPATH,I,ICYC,' ')
            IQPRNT = IQPRSV
         ENDIF
*
*     System keys
*
         IF(IOPTS.NE.0) THEN
*
*     Unpack insertion time
*
            WRITE(LOUTCD,9002) (CHTAG(K),KEYS(K),K=KOFSYS,KOFPTR),
     +      CHTAG(KOFFLG),KEYS(KOFFLG), CHTAG(KOFINS),IDATE,ITIME,
     +      CHTAG(KOFSY1),KEYS(KOFSY1)
 9002    FORMAT(' System keys:     ',2(A8,1X,I11,1X),A8,1X,Z8/
     +          18X,A8,1X,I6,1X,I4,1X,A8,1X,I11)
         ENDIF
*
*     Experiment keys
*
         IF(IOPTE.NE.0) THEN
            WRITE(LOUTCD,9003) (CHTAG(K),KEYS(K),K=KOFEXP, KOFEXP+
     +      NOFEXP-1)
 9003    FORMAT(' Experiment keys: ',3(A8,1X,I11,1X)/
     +          '                  ',2(A8,1X,I11,1X))
         ENDIF
*
*     Validity range pairs
*
         IF(IOPTV.NE.0) THEN
            WRITE(LOUTCD,9004) (CHTAG(K),KEYS(K),K=KOFVAL,NSYSCK)
 9004    FORMAT(' Validity pairs : ',2(A8,1X,I11,1X)/
     +         ('                  ',2(A8,1X,I11,1X)))
         ENDIF
*
*     User keys
*
         KOFUSR = NSYSCK + 1
         IF(IOPTU.NE.0) THEN
            IF(KOFUSR.LE.NWKEY) THEN
*              WRITE(LOUTCD,9005) (CHTAG(K),KEYS(K),K=KOFUSR,NWKEY)
*9005    FORMAT(' User keys      : ',2(A8,1X,I11,1X)/
*    +         ('                  ',2(A8,1X,I11,1X)))
               CHLINE = ' User keys      : '
               IOFF   =  19
               DO 50 K=KOFUSR,NWKEY
                  IF(KEYT(K).LT.3) THEN
                     WRITE(CHLINE(IOFF:),'(A8,1X,I11)') CHTAG(K),
     +               KEYS(K)
                  ELSE
                     WRITE(CHLINE(IOFF:),'(A8)') CHTAG(K)
                     CALL UHTOC(KEYS(K),4,CHLINE(IOFF+10:),4)
                  ENDIF
                  IOFF = IOFF + 21
                  IF(IOFF.GT.60) THEN
                     WRITE(LOUTCD,'(A)') CHLINE(1:IOFF-1)
                     CHLINE = ' '
                     IOFF = 19
                  ENDIF
   50          CONTINUE
               IF(IOFF.GT.19) WRITE(LOUTCD,'(A)') CHLINE(1:IOFF-1)
            ELSE
               IF(NSHOW.EQ.0) THEN
                  WRITE(LOUTCD,9005) CHPATH(1:LPATH)
 9005    FORMAT(' No user keys in directory : ',A)
                  NSHOW = 1
               ENDIF
            ENDIF
         ENDIF
*
*     Number of data words
*
         IF(IOPTN.NE.0) THEN
            NW = JBYT(IQ(KQSP+LCDIR+LCYC+3),1,20)
            WRITE(LOUTCD,*) '# words  : ',NW
         ENDIF
*
*     Insertion date & time
*
         IF(IOPTT.NE.0) THEN
            CALL RZDATE(IQ(KQSP+LCDIR+LCYC+1),IDATE,ITIME,1)
            WRITE(LOUTCD,*) 'Inserted : ',IDATE,ITIME
         ENDIF
 
   60 CONTINUE
 
      IF(IOPTM.NE.0) THEN
         DO 70 I=1,NPAICD
            WRITE(LOUTCD,9006) I,MINVAL(I),MAXVAL(I)
 9006  FORMAT(' Minimum/maximum values for validity pair # ',I3,
     +        ' = ',I10,' / ',I10)
   70    CONTINUE
 
      ENDIF
 
      END
 
