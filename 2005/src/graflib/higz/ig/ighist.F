*
* $Id: ighist.F,v 1.2 2001/10/08 16:12:45 couet Exp $
*
* $Log: ighist.F,v $
* Revision 1.2  2001/10/08 16:12:45  couet
* - When a HIST/PLOT command (in PAW) is performed with the option S with
*   markers or lines, if the 2nd bin of the histogram is outside the plot
*   frame, the 1st bin is clipped too even if it is inside. The following
*   PAW macro illustrate the problem:
*
*   v/cr a(5) r 1 0 1 0.5 1
*   set mtyp 20
*   1d 100 ' ' 5 0. 5.
*   put/cont 100 a
*   null 0. 5. .1 1.5
*   h/pl 100 s
*   h/pl 100 sp
*   h/pl 100 sl
*
*   Some "clipping code" in IGHIST was causing this problem. This code was
*   not necessary since the clipping is correctly handled elsewhere in HIGZ
*   for all the basic primitives. So this code has been commented and it now
*   works. For the time being we leave the commented code in the routine in
*   case some unwanted side effects would appear. We do not have find any for
*   the time being.
*
* Revision 1.1.1.1  1996/02/14 13:10:35  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/04 09/06/94  14.25.04  by  O.Couet
*-- Author :
      SUBROUTINE IGHIST(N,X,Y,CHOPT)
*.===========>
*.
*.   Draws all sorts of 1 dimensional graphs :
*. - statistical graphs (histogram, bars or columns charts,
*. - smoothed curved obtained with spline functions.
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of channels in X or in Y.
*.  REAL X(N) or X(2) : X coordinates or (XMIN,XMAX) (WC space).
*.  REAL Y(N) or Y(2) : Y coordinates or (YMIN,YMAX) (WC space).
*.  CHARACTER*(*) CHOPT : Option.
*.
*.  CHOPT='R' :  Graph is drawn horizontaly, parallel to X axis.
*.               (default is vertically, parallel to Y axis)
*.               If option R is selected the user must give:
*.                 2 values for Y (Y(1)=YMIN and Y(2)=YMAX)
*.                 N values for X, one for each channel.
*.               Otherwise the user must give:
*.                 N values for Y, one for each channel.
*.                 2 values for X (X(1)=XMIN and X(2)=XMAX)
*.
*.  CHOPT='L' :  A simple polyline beetwen every points is drawn
*.
*.  CHOPT='H' :  An Histogram with equidistant bins is drawn
*.               as a polyline.
*.
*.  CHOPT='F' :  An histogram with equidistant bins is drawn
*.               as a fill area. Contour is not drawn unless
*.               CHOPT='H' is also selected..
*.
*.  CHOPT='N' :  Non equidistant bins (default is equidistant)
*.               If N is the number of channels array X and Y
*.               must be dimensionned as follow:
*.               If option R is not selected (default) then
*.               the user must give:
*.                 (N+1) values for X (limits of channels).
*.                  N values for Y, one for each channel.
*.               Otherwise the user must give:
*.                 (N+1) values for Y (limits of channels).
*.                  N values for X, one for each channel.
*.
*.  CHOPT='F1':  Idem as 'F' except that fill area is no more
*.               reparted arround axis X=0 or Y=0 .
*.
*.  CHOPT='C' :  A smooth Curve is drawn.
*.
*.  CHOPT='*' :  A Star is plotted at the center of each bin.
*.
*.  CHOPT='P' :  Idem with the current marker
*.
*.  CHOPT='B' :  A Bar chart with equidistant bins is drawn as fill
*.               areas (Contours are drawn).
*.
*.  CHOPT='GX':  LOGX
*.
*.  CHOPT='GY':  LOGY
*.
*.  CHOPT='Z' :  The vectors X and Y are taken into PAWC at the
*.               adresses IQUEST(81) and IQUEST(82)
*.
*.  CHOPT='K' :  Data are pacKed like in HBOOK. In this case, Y is
*.               at the adress IQUEST(82) in PAWC. Note that with
*.               this option exclude option N and option R. This
*.               option is usefull with very big histograms (>100000
*.               bins). Such histo cannot be stored in pictures.
*.
*..==========> (O.Couet N.Cremel-Somon)
#include "higz/hipaw.inc"
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#include "higz/hihist.inc"
#if !defined(CERNLIB_MGKS)
      PARAMETER (NPMAX=204,NPMXFA=99)
#endif
#if defined(CERNLIB_MGKS)
      PARAMETER (NPMAX=204,NPMXFA=204)
#endif
      DIMENSION X(*),Y(*)
      CHARACTER*(*) CHOPT
      CHARACTER*8 CHOPA
      REAL IGHCX
      EQUIVALENCE (LXADR,L2NTLA),(LYADR,LDNTLA)
      LOGICAL ZFSAV,FRSTFA
      DIMENSION IOPT(14)
      EQUIVALENCE (IOPTH ,IOPT(1)) , (IOPTF ,IOPT(2))
      EQUIVALENCE (IOPTC ,IOPT(3)) , (IOPTST,IOPT(4))
      EQUIVALENCE (IOPTR ,IOPT(5)) , (IOPT1,IOPT(6))
      EQUIVALENCE (IOPTB ,IOPT(7)) , (IOPTN,IOPT(8))
      EQUIVALENCE (IOPTL ,IOPT(9)) , (IOPTP,IOPT(10))
      EQUIVALENCE (IOPTA ,IOPT(11)), (IOPTG ,IOPT(12))
      EQUIVALENCE (IOPTX ,IOPT(13)), (IOPTY ,IOPT(14))
*.______________________________________
*
      IQUEST(1)=0
      IF(N.LE.0)THEN
         CALL IGERR(' Number of points is invalid','IGHIST')
         RETURN
      ENDIF
*
      CALL UOPTC(CHOPT,'HFC*R1BNLPAGXY',IOPT)
*
*              If necessary the adresses of the vectors are saved in
*              a link area.
*
      IFIRST = 1
      ILAST  = N
      IOPTZ  = 0
      IOPTK  = 0
      IF(INDEX(CHOPT,'Z').NE.0)THEN
         LXADR  = IQUEST(81)
         LYADR  = IQUEST(82)
         IOPTZ  = 1
      ENDIF
      IF(INDEX(CHOPT,'K').NE.0)THEN
         LYADR  = IQUEST(82)
         IFIRST = IQUEST(83)
         ILAST  = IQUEST(84)
         IOPTK  = 1
      ENDIF
      NBINS     = ILAST-IFIRST+1
*
#if defined(CERNLIB_MAIL)
      CHOPTM = CHOPT
#endif
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG.AND.IOPTK.EQ.0)CALL IZHIST(N,X,Y,IOPT,IOPTZ)
#endif
      IF((.NOT.GFLAG).AND.(.NOT.PFLAG))RETURN
      ZFSAV  = ZFLAG
      ZFLAG  = .FALSE.
      GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
*
*              Draw the Axis with a fixed number of division: 510
*
      IF(IOPTA.NE.0)THEN
         ILNOLD = ILN
         IFAOLD = IFAIS
         CALL ISLN(1)
         CALL ISFAIS(0)
         CALL IGBOX(RWXMIN,RWXMAX,RWYMIN,RWYMAX)
         CALL ISLN(ILNOLD)
         CALL ISFAIS(IFAOLD)
         AXFLAG = .FALSE.
         CHOPA  = ' '
         RWMIN  = RWXMIN
         RWMAX  = RWXMAX
         IF(IOPTG.NE.0.AND.IOPTX.NE.0)THEN
            RWMIN = 10**RWXMIN
            RWMAX = 10**RWXMAX
            CHOPA = 'G'
         ENDIF
         CALL IGAXIS(RWXMIN,RWXMAX,RWYMIN,RWYMIN,RWMIN,RWMAX,510,CHOPA)
         AXFLAG = .TRUE.
         CHOPA  = ' '
         RWMIN  = RWYMIN
         RWMAX  = RWYMAX
         IF(IOPTG.NE.0.AND.IOPTY.NE.0)THEN
            RWMIN = 10**RWYMIN
            RWMAX = 10**RWYMAX
            CHOPA = 'G'
         ENDIF
         CALL IGAXIS(RWXMIN,RWXMIN,RWYMIN,RWYMAX,RWMIN,RWMAX,510,CHOPA)
         AXFLAG = .FALSE.
      ENDIF
*
*              Min-Max scope
*
      IF(IOPTR.EQ.0)THEN
         IF(IOPTZ.NE.0)THEN
            WMIN = Q(LXADR)
            WMAX = Q(LXADR+1)
         ELSE
            WMIN = X(1)
            WMAX = X(2)
         ENDIF
      ELSE
         IF(IOPTZ.NE.0)THEN
            WMIN = Q(LYADR)
            WMAX = Q(LYADR+1)
         ELSE
            WMIN = Y(1)
            WMAX = Y(2)
         ENDIF
      ENDIF
      IF(IOPTN.EQ.0)DELTA = (WMAX-WMIN)/NBINS
*
*              Draw the Histogram with a Fill Area
*
      IF((IOPTF.NE.0).AND.(IOPTC.EQ.0))THEN
         FRSTFA = .TRUE.
         IF(IOPTR.EQ.0)THEN
            XWORK(1) = WMIN
            IF(IOPT1.EQ.0)THEN
               YWORK(1) = MAX(0.,RWYMIN)
            ELSE
               YWORK(1) = RWYMIN
            ENDIF
            NPT = 2
            DO 10 J=IFIRST,ILAST
               IF(IOPTN.EQ.0)THEN
                  XWORK(NPT)   = XWORK(NPT-1)
                  XWORK(NPT+1) = WMIN+((J-IFIRST+1)*DELTA)
               ELSE
                  IF(IOPTZ.NE.0)THEN
                     XJ1 = Q(LXADR+J)
                     XJ  = Q(LXADR+J-1)
                  ELSE
                     XJ1 = X(J+1)
                     XJ  = X(J)
                  ENDIF
                  IF(XJ1.LT.XJ)THEN
                     IF(J.NE.ILAST)THEN
                        CALL IGERR('X must be in increasing order'
     +                  ,'IGHIST')
                     ELSE
                        CALL IGERR('X must have N+1 values with '
     +                  //'option N' ,'IGHIST')
                     ENDIF
                     GOTO 110
                  ENDIF
                  IF(IOPTZ.NE.0)THEN
                     XWORK(NPT)   = Q(LXADR+J-1)
                     XWORK(NPT+1) = Q(LXADR+J)
                  ELSE
                     XWORK(NPT)   = X(J)
                     XWORK(NPT+1) = X(J+1)
                  ENDIF
               ENDIF
               IF(IOPTZ.NE.0)THEN
                  YWORK(NPT)   = Q(LYADR+J-1)
                  YWORK(NPT+1) = Q(LYADR+J-1)
               ELSEIF(IOPTK.NE.0)THEN
                  YWORK(NPT)   = IGHCX(LYADR,J)
                  YWORK(NPT+1) = IGHCX(LYADR,J)
               ELSE
                  YWORK(NPT)   = Y(J)
                  YWORK(NPT+1) = Y(J)
               ENDIF
               NPT = NPT+2
               IF(J.EQ.ILAST)THEN
                  XWORK(NPT) = XWORK(NPT-1)
                  YWORK(NPT) = YWORK(1)
                  CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
                  CALL IFA(NPT,XWORKL,YWORKL)
                  IF(IBORD.NE.0)THEN
                     IF(.NOT.FRSTFA)YWORKL(1) = YLAST
                     CALL IPL(NPT-1,XWORKL,YWORKL)
                  ENDIF
                  GOTO 10
               ENDIF
               IF(NPT.GE.NPMXFA)THEN
                  XWORK(NPT) = XWORK(NPT-1)
                  YWORK(NPT) = YWORK(1)
                  CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
                  CALL IFA(NPT,XWORKL,YWORKL)
                  IF(IBORD.NE.0)THEN
                     IF(.NOT.FRSTFA)YWORKL(1) = YLAST
                     CALL IPL(NPT-1,XWORKL,YWORKL)
                     FRSTFA = .FALSE.
                  ENDIF
                  YLAST    = YWORKL(NPT-1)
                  XWORK(1) = XWORK(NPT)
                  NPT      = 2
               ENDIF
   10       CONTINUE
         ELSE
            YWORK(1) = WMIN
            IF(IOPT1.EQ.0)THEN
               XWORK(1) = MAX(0.,RWXMIN)
            ELSE
               XWORK(1) = RWXMIN
            ENDIF
            NPT = 2
            DO 20  J=IFIRST,ILAST
               IF(IOPTN.EQ.0)THEN
                  YWORK(NPT)   = YWORK(NPT-1)
                  YWORK(NPT+1) = WMIN+((J-IFIRST+1)*DELTA)
               ELSE
                  IF(IOPTZ.NE.0)THEN
                     YJ1 = Q(LYADR+J)
                     YJ  = Q(LYADR+J-1)
                  ELSE
                     YJ1 = Y(J+1)
                     YJ  = Y(J)
                  ENDIF
                  IF(YJ1.LT.YJ)THEN
                     IF(J.NE.ILAST)THEN
                        CALL IGERR('Y must be in increasing order'
     +                  ,'IGHIST')
                     ELSE
                        CALL IGERR('Y must have N+1 values with '
     +                  //'option N' ,'IGHIST')
                     ENDIF
                     GOTO 110
                  ENDIF
                  IF(IOPTZ.NE.0)THEN
                     YWORK(NPT)   = Q(LYADR+J-1)
                     YWORK(NPT+1) = Q(LYADR+J)
                  ELSE
                     YWORK(NPT)   = Y(J)
                     YWORK(NPT+1) = Y(J+1)
                  ENDIF
               ENDIF
               IF(IOPTZ.NE.0)THEN
                  XWORK(NPT)   = Q(LXADR+J-1)
                  XWORK(NPT+1) = Q(LXADR+J-1)
               ELSE
                  XWORK(NPT)   = X(J)
                  XWORK(NPT+1) = X(J)
               ENDIF
               NPT = NPT+2
               IF(J.EQ.ILAST)THEN
                  YWORK(NPT) = YWORK(NPT-1)
                  XWORK(NPT) = XWORK(1)
                  CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
                  CALL IFA(NPT,XWORKL,YWORKL)
                  IF(IBORD.NE.0)THEN
                     IF(.NOT.FRSTFA)YWORKL(1) = YLAST
                     CALL IPL(NPT-1,XWORKL,YWORKL)
                  ENDIF
                  GOTO 20
               ENDIF
               IF(NPT.GE.NPMXFA)THEN
                  YWORK(NPT) = YWORK(NPT-1)
                  XWORK(NPT) = XWORK(1)
                  CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
                  CALL IFA(NPT,XWORKL,YWORKL)
                  IF(IBORD.NE.0)THEN
                     IF(.NOT.FRSTFA)YWORKL(1) = YLAST
                     CALL IPL(NPT-1,XWORKL,YWORKL)
                     FRSTFA = .FALSE.
                  ENDIF
                  YLAST    = YWORKL(NPT-1)
                  YWORK(1) = YWORK(NPT)
                  NPT      = 2
               ENDIF
   20       CONTINUE
         ENDIF
      ENDIF
*
*              Draw a standard Histogram (default)
*
      IF((IOPTH.NE.0).OR.(CHOPT.EQ.' '))THEN
         IF(IOPTR.EQ.0)THEN
            XWORK(1) = WMIN
            YWORK(1) = MAX(0.,RWYMIN)
            YWMIN    = YWORK(1)
            NPT      = 2
            DO 30  I=IFIRST,ILAST
               IF(IOPTN.EQ.0)THEN
                  XWORK(NPT)   = XWORK(NPT-1)
                  XWORK(NPT+1) = WMIN+((I-IFIRST+1)*DELTA)
               ELSE
                  IF(IOPTZ.NE.0)THEN
                     XI1 = Q(LXADR+I)
                     XI  = Q(LXADR+I-1)
                  ELSE
                     XI1 = X(I+1)
                     XI  = X(I)
                  ENDIF
                  IF(XI1.LT.XI)THEN
                     IF(I.NE.ILAST)THEN
                        CALL IGERR('X must be in increasing order'
     +                  ,'IGHIST')
                     ELSE
                        CALL IGERR('X must have N+1 values with '
     +                  //'option N' ,'IGHIST')
                     ENDIF
                     GOTO 110
                  ENDIF
                  IF(IOPTZ.NE.0)THEN
                     XWORK(NPT)   = Q(LXADR+I-1)
                     XWORK(NPT+1) = Q(LXADR+I)
                  ELSE
                     XWORK(NPT)   = X(I)
                     XWORK(NPT+1) = X(I+1)
                  ENDIF
               ENDIF
               IF(IOPTZ.NE.0)THEN
                  YWORK(NPT)   = Q(LYADR+I-1)
                  YWORK(NPT+1) = Q(LYADR+I-1)
               ELSEIF(IOPTK.NE.0)THEN
                  YWORK(NPT)   = IGHCX(LYADR,I)
                  YWORK(NPT+1) = IGHCX(LYADR,I)
               ELSE
                  YWORK(NPT)   = Y(I)
                  YWORK(NPT+1) = Y(I)
               ENDIF
               NPT = NPT+2
               IF(I.EQ.ILAST)THEN
                  XWORK(NPT) = XWORK(NPT-1)
                  YWORK(NPT) = YWMIN
                  CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
                  CALL IPL(NPT,XWORKL,YWORKL)
                  GOTO 30
               ENDIF
               IF(NPT.GE.NPMAX)THEN
                  CALL IGHIS1(NPT-1,IOPTG,IOPTX,IOPTY)
                  CALL IPL(NPT-1,XWORKL,YWORKL)
                  XWORK(1) = XWORK(NPT-1)
                  YWORK(1) = YWORK(NPT-1)
                  NPT      = 2
               ENDIF
   30       CONTINUE
         ELSE
            YWORK(1) = WMIN
            XWORK(1) = MAX(0.,RWXMIN)
            XWMIN    = XWORK(1)
            NPT      = 2
            DO 40  I=IFIRST,ILAST
               IF(IOPTN.EQ.0)THEN
                  YWORK(NPT)   = YWORK(NPT-1)
                  YWORK(NPT+1) = WMIN+((I-IFIRST+1)*DELTA)
               ELSE
                  IF(IOPTZ.NE.0)THEN
                     YI1 = Q(LYADR+I)
                     YI  = Q(LYADR+I-1)
                  ELSE
                     YI1 = Y(I+1)
                     YI  = Y(I)
                  ENDIF
                  IF(YI1.LT.YI)THEN
                     IF(I.NE.ILAST)THEN
                        CALL IGERR('Y must be in increasing order'
     +                  ,'IGHIST')
                     ELSE
                        CALL IGERR('Y must have N+1 values with '
     +                  //'option N' ,'IGHIST')
                     ENDIF
                     GOTO 110
                  ENDIF
                  IF(IOPTZ.NE.0)THEN
                     YWORK(NPT)   = Q(LYADR+I-1)
                     YWORK(NPT+1) = Q(LYADR+I)
                  ELSE
                     YWORK(NPT)   = Y(I)
                     YWORK(NPT+1) = Y(I+1)
                  ENDIF
               ENDIF
               IF(IOPTZ.NE.0)THEN
                  XWORK(NPT)   = Q(LXADR+I-1)
                  XWORK(NPT+1) = Q(LXADR+I-1)
               ELSE
                  XWORK(NPT)   = X(I)
                  XWORK(NPT+1) = X(I)
               ENDIF
               NPT = NPT+2
               IF(I.EQ.ILAST)THEN
                  YWORK(NPT) = YWORK(NPT-1)
                  XWORK(NPT) = XWMIN
                  CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
                  CALL IPL(NPT,XWORKL,YWORKL)
                  GOTO 40
               ENDIF
               IF(NPT.GE.NPMAX)THEN
                  CALL IGHIS1(NPT-1,IOPTG,IOPTX,IOPTY)
                  CALL IPL(NPT-1,XWORKL,YWORKL)
                  XWORK(1) = XWORK(NPT-1)
                  YWORK(1) = YWORK(NPT-1)
                  NPT      = 2
               ENDIF
   40       CONTINUE
         ENDIF
      ENDIF
*
*              Draw the histogram with a smooth Curve. The computing
*              of the smoothing is done by the routine IGRAP1
*
      IF(IOPTC.NE.0)THEN
         IF(IOPTF.EQ.0)THEN
            ITYPS = 1
         ELSE
            IF(IOPT1.EQ.0)THEN
               ITYPS = 2
            ELSE
               ITYPS = 3
            ENDIF
         ENDIF
         IF(IOPTR.EQ.0)THEN
            NPT = 0
            DO 50  I=IFIRST,ILAST
               NPT = NPT+1
               IF(IOPTN.EQ.0)THEN
                  XWORK(NPT) = WMIN+(I-IFIRST)*DELTA+0.5*DELTA
               ELSE
                  IF(IOPTZ.NE.0)THEN
                     XI1 = Q(LXADR+I)
                     XI  = Q(LXADR+I-1)
                  ELSE
                     XI1 = X(I+1)
                     XI  = X(I)
                  ENDIF
                  IF(XI1.LT.XI)THEN
                     IF(I.NE.ILAST)THEN
                        CALL IGERR('X must be in increasing order'
     +                  ,'IGHIST')
                     ELSE
                        CALL IGERR('X must have N+1 values with '
     +                  //'option N' ,'IGHIST')
                     ENDIF
                     GOTO 110
                  ENDIF
                  IF(IOPTZ.NE.0)THEN
                     XWORK(NPT) = Q(LXADR+I-1)+
     +                           (Q(LXADR+I)-Q(LXADR+I-1))/2.
                  ELSE
                     XWORK(NPT) = X(I)+(X(I+1)-X(I))/2.
                  ENDIF
               ENDIF
               IF(IOPTZ.NE.0)THEN
                  YWORK(NPT) = Q(LYADR+I-1)
               ELSEIF(IOPTK.NE.0)THEN
                  YWORK(NPT) = IGHCX(LYADR,I)
               ELSE
                  YWORK(NPT) = Y(I)
               ENDIF
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               IF((YWORKL(NPT).LT.RWYMIN).OR.(YWORKL(NPT).GT.RWYMAX))
     +         THEN
                  IF(NPT.GT.2)THEN
                     CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
                     CALL IGRAP1(XWORKL,YWORKL,NPT,ITYPS)
                  ENDIF
                  XWORK(1) = XWORK(NPT)
                  YWORK(1) = YWORK(NPT)
                  NPT      = 1
                  GOTO 50
               ENDIF
               IF(NPT.GE.50)THEN
                  CALL IGHIS1(50,IOPTG,IOPTX,IOPTY)
                  CALL IGRAP1(XWORKL,YWORKL,50,ITYPS)
                  XWORK(1) = XWORK(NPT)
                  YWORK(1) = YWORK(NPT)
                  NPT      = 1
               ENDIF
   50       CONTINUE
            IF(NPT.GT.1)THEN
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               CALL IGRAP1(XWORKL,YWORKL,NPT,ITYPS)
            ENDIF
         ELSE
            ITYPS = ITYPS+10
            NPT   = 0
            DO 60  I=IFIRST,ILAST
               NPT = NPT+1
               IF(IOPTN.EQ.0)THEN
                  YWORK(NPT) = WMIN+(I-IFIRST)*DELTA+0.5*DELTA
               ELSE
                  IF(IOPTZ.NE.0)THEN
                     YI1 = Q(LYADR+I)
                     YI  = Q(LYADR+I-1)
                  ELSE
                     YI1 = Y(I+1)
                     YI  = Y(I)
                  ENDIF
                  IF(YI1.LT.YI)THEN
                     IF(I.NE.ILAST)THEN
                        CALL IGERR('Y must be in increasing order'
     +                  ,'IGHIST')
                     ELSE
                        CALL IGERR('Y must have N+1 values with '
     +                  //'option N' ,'IGHIST')
                     ENDIF
                     GOTO 110
                  ENDIF
                  IF(IOPTZ.NE.0)THEN
                     YWORK(NPT) = Q(LYADR+I-1)+
     +                           (Q(LYADR+I)-Q(LYADR+I-1))/2.
                  ELSE
                     YWORK(NPT) = Y(I)+(Y(I+1)-Y(I))/2.
                  ENDIF
               ENDIF
               IF(IOPTZ.NE.0)THEN
                  XWORK(NPT) = Q(LXADR+I-1)
               ELSE
                  XWORK(NPT) = X(I)
               ENDIF
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               IF((XWORKL(NPT).LT.RWXMIN).OR.(XWORKL(NPT).GT.RWXMAX))
     +         THEN
                  IF(NPT.GT.2)THEN
                     CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
                     CALL IGRAP1(XWORKL,YWORKL,NPT,ITYPS)
                  ENDIF
                  XWORK(1) = XWORK(NPT)
                  YWORK(1) = YWORK(NPT)
                  NPT      = 1
                  GOTO 60
               ENDIF
               IF(NPT.GE.50)THEN
                  CALL IGHIS1(50,IOPTG,IOPTX,IOPTY)
                  CALL IGRAP1(XWORKL,YWORKL,50,ITYPS)
                  XWORK(1) = XWORK(NPT)
                  YWORK(1) = YWORK(NPT)
                  NPT      = 1
               ENDIF
   60       CONTINUE
            IF(NPT.GT.1)THEN
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               CALL IGRAP1(XWORKL,YWORKL,NPT,ITYPS)
            ENDIF
         ENDIF
      ENDIF
*
*              Draw the histogram with a simple line
*
      IOPTPK = 0
      IF((IOPTST.NE.0).OR.(IOPTP.NE.0))IOPTPK=1
      IF((IOPTPK.NE.0).OR.(IOPTL.NE.0))THEN
         IMKOLD = IMK
         IF(IOPTST.NE.0)CALL ISMK(3)
         WMINST = WMIN+DELTA/2.
         IF(IOPTR.EQ.0)THEN
            NPT = 0
            DO 70  I=IFIRST,ILAST
               NPT = NPT+1
               IF(IOPTN.EQ.0)THEN
                  XWORK(NPT) = WMINST+(I-IFIRST)*DELTA
               ELSE
                  IF(IOPTZ.NE.0)THEN
                     XI1 = Q(LXADR+I)
                     XI  = Q(LXADR+I-1)
                  ELSE
                     XI1 = X(I+1)
                     XI  = X(I)
                  ENDIF
                  IF(XI1.LT.XI)THEN
                     IF(I.NE.ILAST)THEN
                        CALL IGERR('X must be in increasing order'
     +                  ,'IGHIST')
                     ELSE
                        CALL IGERR('X must have N+1 values with '
     +                  //'option N' ,'IGHIST')
                     ENDIF
                     GOTO 110
                  ENDIF
                  IF(IOPTZ.NE.0)THEN
                     XWORK(NPT) = Q(LXADR+I-1)+
     +                           (Q(LXADR+I)-Q(LXADR+I-1))/2.
                  ELSE
                     XWORK(NPT) = X(I)+(X(I+1)-X(I))/2.
                  ENDIF
               ENDIF
               IF(IOPTZ.NE.0)THEN
                  YWORK(NPT) = Q(LYADR+I-1)
               ELSEIF(IOPTK.NE.0)THEN
                  YWORK(NPT) = IGHCX(LYADR,I)
               ELSE
                  YWORK(NPT) = Y(I)
               ENDIF
CCC               IF((YWORK(NPT).LT.RWYMIN).OR.(YWORK(NPT).GT.RWYMAX))
CCC     +         THEN
CCC                  IF(NPT.GT.2)THEN
CCC                     IF(IOPTPK.NE.0)THEN
CCC                        CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
CCC                        CALL IPM(NPT,XWORKL,YWORKL)
CCC                     ENDIF
CCC                     IF(IOPTL.NE.0)THEN
CCC                        CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
CCC                        CALL IPL(NPT,XWORKL,YWORKL)
CCC                     ENDIF
CCC                  ENDIF
CCC                  XWORK(1) = XWORK(NPT)
CCC                  YWORK(1) = YWORK(NPT)
CCC                  NPT      = 1
CCC                  GOTO 70
CCC               ENDIF
               IF(NPT.GE.50)THEN
                  IF(IOPTPK.NE.0)THEN
                     CALL IGHIS1(50,IOPTG,IOPTX,IOPTY)
                     CALL IPM(50,XWORKL,YWORKL)
                  ENDIF
                  IF(IOPTL.NE.0)THEN
                     CALL IGHIS1(50,IOPTG,IOPTX,IOPTY)
                     CALL IPL(50,XWORKL,YWORKL)
                  ENDIF
                  XWORK(1) = XWORK(NPT)
                  YWORK(1) = YWORK(NPT)
                  NPT      = 1
               ENDIF
   70       CONTINUE
            IF(IOPTPK.NE.0.AND.NPT.GT.0)THEN
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               CALL IPM(NPT,XWORKL,YWORKL)
            ENDIF
            IF(IOPTL.NE.0.AND.NPT.GT.1)THEN
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               CALL IPL(NPT,XWORKL,YWORKL)
            ENDIF
         ELSE
            NPT = 0
            DO 80  I=IFIRST,ILAST
               NPT = NPT+1
               IF(IOPTN.EQ.0)THEN
                  YWORK(NPT) = WMINST+(I-IFIRST)*DELTA
               ELSE
                  IF(IOPTZ.NE.0)THEN
                     YI1 = Q(LYADR+I)
                     YI  = Q(LYADR+I-1)
                  ELSE
                     YI1 = Y(I+1)
                     YI  = Y(I)
                  ENDIF
                  IF(YI1.LT.YI)THEN
                     IF(I.NE.ILAST)THEN
                        CALL IGERR('Y must be in increasing order'
     +                  ,'IGHIST')
                     ELSE
                        CALL IGERR('Y must have N+1 values with '
     +                  //'option N' ,'IGHIST')
                     ENDIF
                     GOTO 110
                  ENDIF
                  IF(IOPTZ.NE.0)THEN
                     YWORK(NPT) = Q(LYADR+I-1)+
     +                           (Q(LYADR+I)-Q(LYADR+I-1))/2.
                  ELSE
                     YWORK(NPT) = Y(I)+(Y(I+1)-Y(I))/2.
                  ENDIF
               ENDIF
               IF(IOPTZ.NE.0)THEN
                  XWORK(NPT) = Q(LXADR+I-1)
               ELSE
                  XWORK(NPT) = X(I)
               ENDIF
CCC               IF((XWORK(NPT).LT.RWXMIN).OR.(XWORK(NPT).GT.RWXMAX))
CCC     +         THEN
CCC                  IF(NPT.GT.2)THEN
CCC                     IF(IOPTPK.NE.0)THEN
CCC                        CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
CCC                        CALL IPM(NPT,XWORKL,YWORKL)
CCC                     ENDIF
CCC                     IF(IOPTL.NE.0)THEN
CCC                        CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
CCC                        CALL IPL(NPT,XWORKL,YWORKL)
CCC                     ENDIF
CCC                  ENDIF
CCC                  XWORK(1) = XWORK(NPT)
CCC                  YWORK(1) = YWORK(NPT)
CCC                  NPT      = 1
CCC                  GOTO 80
CCC               ENDIF
               IF(NPT.GE.50)THEN
                  IF(IOPTPK.NE.0)THEN
                     CALL IGHIS1(50,IOPTG,IOPTX,IOPTY)
                     CALL IPM(50,XWORKL,YWORKL)
                  ENDIF
                  IF(IOPTL.NE.0)THEN
                     CALL IGHIS1(50,IOPTG,IOPTX,IOPTY)
                     CALL IPL(50,XWORKL,YWORKL)
                  ENDIF
                  XWORK(1) = XWORK(NPT)
                  YWORK(1) = YWORK(NPT)
                  NPT      = 1
               ENDIF
   80       CONTINUE
            IF(IOPTPK.NE.0.AND.NPT.GT.0)THEN
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               CALL IPM(NPT,XWORKL,YWORKL)
            ENDIF
            IF(IOPTL.NE.0.AND.NPT.GT.1)THEN
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               CALL IPL(NPT,XWORKL,YWORKL)
            ENDIF
         ENDIF
         CALL ISMK(IMKOLD)
      ENDIF
*
*              Draw the histogram as a bar chart
*
      IF(IOPTB.NE.0)THEN
         IF(IOPTN.EQ.0)THEN
            OFFS = DELTA*RBOF
            DBAR = DELTA*RBWD
         ELSE
            IF(IOPTR.EQ.0)THEN
               IF(IOPTZ.NE.0)THEN
                  OFFS = (Q(LXADR+1)-Q(LXADR))*RBOF
                  DBAR = (Q(LXADR+1)-Q(LXADR))*RBWD
               ELSE
                  OFFS = (X(2)-X(1))*RBOF
                  DBAR = (X(2)-X(1))*RBWD
               ENDIF
            ELSE
               IF(IOPTZ.NE.0)THEN
                  OFFS = (Q(LYADR+1)-Q(LYADR))*RBOF
                  DBAR = (Q(LYADR+1)-Q(LYADR))*RBWD
               ELSE
                  OFFS = (Y(2)-Y(1))*RBOF
                  DBAR = (Y(2)-Y(1))*RBWD
               ENDIF
            ENDIF
         ENDIF
         IBOLD = IBORD
         CALL IGSET('BORD',1.)
         IF(IOPTR.EQ.0)THEN
            XLOW  = WMIN+OFFS
            XHIGH = WMIN+OFFS+DBAR
            IF(IOPT1.EQ.0)THEN
               YLOW = MAX(0.,RWYMIN)
            ELSE
               YLOW = RWYMIN
            ENDIF
            DO 90  I=IFIRST,ILAST
               IF(IOPTZ.NE.0)THEN
                  YHIGH    = Q(LYADR+I-1)
               ELSEIF(IOPTK.NE.0)THEN
                  YHIGH    = IGHCX(LYADR,I)
               ELSE
                  YHIGH    = Y(I)
               ENDIF
               XWORK(1) = XLOW
               YWORK(1) = YLOW
               XWORK(2) = XHIGH
               YWORK(2) = YHIGH
               CALL IGHIS1(2,IOPTG,IOPTX,IOPTY)
               CALL IGBOX(XWORKL(1),XWORKL(2),YWORKL(1),YWORKL(2))
               IF(IOPTN.EQ.0)THEN
                  XLOW  = XLOW+DELTA
                  XHIGH = XHIGH+DELTA
               ELSE
                  IF(I.LT.ILAST)THEN
                     IF(IOPTZ.NE.0)THEN
                        XI1 = Q(LXADR+I)
                        XI  = Q(LXADR+I-1)
                     ELSE
                        XI1 = X(I+1)
                        XI  = X(I)
                     ENDIF
                     IF(XI1.LT.XI)THEN
                        CALL IGERR('X must be in increasing order'
     +                  ,'IGHIST')
                        GOTO 110
                     ENDIF
                     IF(IOPTZ.NE.0)THEN
                        OFFS  = (Q(LXADR+I+1)-Q(LXADR+I))*RBOF
                        DBAR  = (Q(LXADR+I+1)-Q(LXADR+I))*RBWD
                        XLOW  = Q(LXADR+I)+OFFS
                        XHIGH = Q(LXADR+I)+OFFS+DBAR
                     ELSE
                        OFFS  = (X(I+2)-X(I+1))*RBOF
                        DBAR  = (X(I+2)-X(I+1))*RBWD
                        XLOW  = X(I+1)+OFFS
                        XHIGH = X(I+1)+OFFS+DBAR
                     ENDIF
                  ENDIF
               ENDIF
   90       CONTINUE
         ELSE
            YLOW  = WMIN+OFFS
            YHIGH = WMIN+OFFS+DBAR
            IF(IOPT1.EQ.0)THEN
               XLOW = MAX(0.,RWXMIN)
            ELSE
               XLOW = RWXMIN
            ENDIF
            DO 100 I=IFIRST,ILAST
               IF(IOPTZ.NE.0)THEN
                  XHIGH = Q(LXADR+I-1)
               ELSE
                  XHIGH = X(I)
               ENDIF
               XWORK(1) = XLOW
               YWORK(1) = YLOW
               XWORK(2) = XHIGH
               YWORK(2) = YHIGH
               CALL IGHIS1(2,IOPTG,IOPTX,IOPTY)
               CALL IGBOX(XWORKL(1),XWORKL(2),YWORKL(1),YWORKL(2))
               CALL IGBOX(XLOW,XHIGH,YLOW,YHIGH)
               IF(IOPTN.EQ.0)THEN
                  YLOW  = YLOW+DELTA
                  YHIGH = YHIGH+DELTA
               ELSE
                  IF(I.LT.ILAST)THEN
                     IF(IOPTZ.NE.0)THEN
                        YI1 = Q(LYADR+I)
                        YI  = Q(LYADR+I-1)
                     ELSE
                        YI1 = Y(I+1)
                        YI  = Y(I)
                     ENDIF
                     IF(YI1.LT.YI)THEN
                        CALL IGERR('Y must be in increasing order'
     +                  ,'IGHIST')
                        GOTO 110
                     ENDIF
                     IF(IOPTZ.NE.0)THEN
                        OFFS  = (Q(LYADR+I+1)-Q(LYADR+I))*RBOF
                        DBAR  = (Q(LYADR+I+1)-Q(LYADR+I))*RBWD
                        YLOW  = Q(LYADR+I)+OFFS
                        YHIGH = Q(LYADR+I)+OFFS+DBAR
                     ELSE
                        OFFS  = (Y(I+2)-Y(I+1))*RBOF
                        DBAR  = (Y(I+2)-Y(I+1))*RBWD
                        YLOW  = Y(I+1)+OFFS
                        YHIGH = Y(I+1)+OFFS+DBAR
                     ENDIF
                  ENDIF
               ENDIF
  100       CONTINUE
         ENDIF
         CALL IGSET('BORD',FLOAT(IBOLD))
      ENDIF
*
  110 ZFLAG  = ZFSAV
      GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
