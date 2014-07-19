*
* $Id: izseln.F,v 1.2 1998/12/01 15:48:42 couet Exp $
*
* $Log: izseln.F,v $
* Revision 1.2  1998/12/01 15:48:42  couet
* - Clean up: commented lines of code removed
*
* Revision 1.1.1.1  1996/02/14 13:11:14  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
*CMZ :  1.18/00 02/04/93  10.57.30  by  O.Couet
*-- Author :
      SUBROUTINE IZSELN(NT)
*.===========>
*.
*.   This routine create in the current picture a bank for the
*. normalisation transformation .
*.
*. _Input parameters:
*.
*.  INTEGER NT : Normalization transformation number.
*.
*..==========> (O.Couet)
#if defined(CERNLIB_ZEBRA)
#include "higz/hiatnb.inc"
#endif
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_PSCRIPT)
#include "higz/hipost.inc"
#endif
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if defined(CERNLIB_ZEBRA)
#include "higz/hiflat.inc"
#endif
#if defined(CERNLIB_ZEBRA)
#include "higz/hiaadr.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if defined(CERNLIB_ZEBRA)
      DIMENSION IRFLAT(NBRATT),IIFLAT(NBIATT)
      EQUIVALENCE (IRFLAT(1),KMKSC),(IIFLAT(1),KPLCI)
      LOGICAL LOPSAV
*.______________________________________
*
#if defined(CERNLIB_PSCRIPT)
#include "higz/icmtop.inc"
#endif
#endif
#if defined(CERNLIB_PSCRIPT)
      IF(PFLAG)THEN
         X1W=RWXMIN
         X2W=RWXMAX
         Y1W=RWYMIN
         Y2W=RWYMAX
         IF(LOPRNG)THEN
            X1V=((XVP2-XVP1)/(XVS2-XVS1))*(RVXMIN-XVS1)+XVP1
            X2V=((XVP2-XVP1)/(XVS2-XVS1))*(RVXMAX-XVS1)+XVP1
            Y1V=((YVP2-YVP1)/(YVS2-YVS1))*(RVYMIN-YVS1)+YVP1
            Y2V=((YVP2-YVP1)/(YVS2-YVS1))*(RVYMAX-YVS1)+YVP1
         ELSE
            X1V=RVXMIN
            X2V=RVXMAX
            Y1V=RVYMIN
            Y2V=RVYMAX
         ENDIF
         FX=(X2V-X1V)/(X2W-X1W)
         FY=(Y2V-Y1V)/(Y2W-Y1W)
         IF(IPSWTY.EQ.-113.AND.NT.EQ.1.AND..NOT.LOBB)THEN
            LOPSAV=LOPRT
            IF(TFLAG) THEN
               CALL IPTMAC
            ELSE
               CALL IPPSTR('@%%BoundingBox: ')
               CALL IPIOUT(ICMTOP(X1W))
               CALL IPIOUT(ICMTOP(Y1W))
               CALL IPIOUT(ICMTOP(X2W))
               CALL IPIOUT(ICMTOP(Y2W))
               CALL IPPSTR('@')
            ENDIF
            CALL IPDEF
            LOBB=.TRUE.
            LOPRT=LOPSAV
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,I5)') 200,NT
         CALL IMWRIT(1)
         WRITE (CHMAIL,'(4E16.7)') RWXMIN,RWXMAX,RWYMIN,RWYMAX
         CALL IMWRIT(2)
         WRITE (CHMAIL,'(4E16.7)') RVXMIN,RVXMAX,RVYMIN,RVYMAX
         CALL IMWRIT(3)
      ENDIF
#endif
#if defined(CERNLIB_ZEBRA)
*
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         DO 10 I=1,NBIATT
            IATADR(30+I)=-1
   10    CONTINUE
         DO 20 I=1,NBRATT
            IATADR(I)=-1
   20    CONTINUE
*
*              The normalization transformation is the transformation 0
*
         IF(NT.EQ.0)THEN
            LHNT=LHNT0
            IQ(LHNT+3)=IZGHNP(LPICT)+1
            ITBPTR=IQ(LHNT+4)
            INBLEV=IQ(LHNT+5)
            IRNSIZ=IQ(LHNT-1)
            IIAPTR=IZGADR(LHNT,8)
            IRAPTR=IZGADR(LHNT,9)
            IF(NT.NE.INTR)THEN
               DO 30 I=1,NBIATT
                  IIFLAT(I)=JBIT(IQ(LHNT+6),I)
   30          CONTINUE
               DO 40 I=1,NBRATT
                  IRFLAT(I)=JBIT(IQ(LHNT+7),I)
   40          CONTINUE
               CALL IZSENV
            ENDIF
            RETURN
         ENDIF
*
         LN=IZGNTP(NT)
*
*              The normalization transformation NT is not yet in
*              the current IZ picture
*
         IF(LN.EQ.0)THEN
*
* Create the Normalisation Tranformation Bank number NT
*
            CALL MZBOOK(IXHIGZ,LHNTP,LHNTLA,0,'HZNT',0,0,INSIZ,2,0)
            LHNT=LHNTP
            LHNTLA=LHNT
*
* Initialisation of the Normalisation Transformation bank NT
*
            IQ(LHNT+1)=NT
            ITBPTR=10
            IQ(LHNT+3)=IZGHNP(LPICT)+1
            IQ(LHNT+4)=ITBPTR
            IRNSIZ=INSIZ
            IQ(LHNT+5)=0
            INBLEV=IQ(LHNT+5)
            IIAPTR=INTPTR
            IRAPTR=IFLPTR
            IQ(LHNT+8)=1000000*NBIATT+IIAPTR
            IQ(LHNT+9)=1000000*NBRATT+IRAPTR
*
* Store the default attributes in the new NT bank
*
            IF(IZPUSH(NBIATT,NBRATT,0,'ISELNT').NE.0)RETURN
            CALL IZCDA(0)
            CALL IZINCI(NBIATT)
            CALL IZINCF(NBRATT)
*
* Attributes flags initialization
*
            DO 50 I=1,NBRATT
               IRFLAT(I)=0
   50       CONTINUE
            DO 60 I=1,NBIATT
               IIFLAT(I)=0
   60       CONTINUE
*
* Increment the NT bank counter
*
            INBNT=INBNT+1
            IQ(LPICT+8)=INBNT
*
* Store the window and viewport parameters in the REAL bank
*
            IQ(LHNT+2)=IFLPTR
            IF(IZPUSH(0,8,0,'ISELNT').NE.0)RETURN
            Q(LHF+IFLPTR)=RWXMIN
            Q(LHF+IFLPTR+1)=RWXMAX
            Q(LHF+IFLPTR+2)=RWYMIN
            Q(LHF+IFLPTR+3)=RWYMAX
            Q(LHF+IFLPTR+4)=RVXMIN
            Q(LHF+IFLPTR+5)=RVXMAX
            Q(LHF+IFLPTR+6)=RVYMIN
            Q(LHF+IFLPTR+7)=RVYMAX
            CALL IZINCF(8)
            RETURN
         ELSE
*
*              The normalisation transformation bank NT exits in the
*              current picture . The transformation NT becomes the
*              current transformation .
*
            LHNT=LN
            Q(LHF+IQ(LHNT+2))=RWXMIN
            Q(LHF+IQ(LHNT+2)+1)=RWXMAX
            Q(LHF+IQ(LHNT+2)+2)=RWYMIN
            Q(LHF+IQ(LHNT+2)+3)=RWYMAX
            Q(LHF+IQ(LHNT+2)+4)=RVXMIN
            Q(LHF+IQ(LHNT+2)+5)=RVXMAX
            Q(LHF+IQ(LHNT+2)+6)=RVYMIN
            Q(LHF+IQ(LHNT+2)+7)=RVYMAX
            IQ(LHNT+3)=IZGHNP(LPICT)+1
            ITBPTR=IQ(LHNT+4)
            IRNSIZ=IQ(LHNT-1)
            INBLEV=IQ(LHNT+5)
            IIAPTR=IZGADR(LHNT,8)
            IRAPTR=IZGADR(LHNT,9)
            IF(NT.NE.INTR)THEN
*
* Set the Flags Attributes .
*
               DO 70 I=1,NBIATT
                  IIFLAT(I)=JBIT(IQ(LHNT+6),I)
   70          CONTINUE
               DO 80 I=1,NBRATT
                  IRFLAT(I)=JBIT(IQ(LHNT+7),I)
   80          CONTINUE
               CALL IZSENV
            ENDIF
         ENDIF
      ENDIF
#endif
*
      END
#endif
