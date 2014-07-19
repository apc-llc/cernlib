*
* $Id: igraph.F,v 1.2 2001/10/17 14:36:57 couet Exp $
*
* $Log: igraph.F,v $
* Revision 1.2  2001/10/17 14:36:57  couet
* - The WORK buffers were too small and the follwoing macro didn't not
*   work for more that 101 points:
*
*   vec/del *
*   n = [1]
*   vec/cr time([n])
*   vec/cr etime([n]) r [n]*0.0
*   vec/cr y([n]) r
*   vec/cr ey([n]) r [n]*0.7
*   do i = 1, [n]
*     vec/input time([i]) [i]
*     vec/input y([i]) $eval(3.0+0.01*[i])
*   enddo
*   null -10 160 0 10
*   hplot/errors time y etime ey [n] 20 0.1 3
*
* Revision 1.1.1.1  1996/02/14 13:10:38  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/09 19/08/94  10.47.03  by  O.Couet
*-- Author :
      SUBROUTINE IGRAPH(N,X,Y,CHOPT)
*.===========>
*.
*.   Draws one dimensional graphs. The aspect of the graph is done
*. according to the value of the CHOPT.
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of channels in X or in Y.
*.  REAL X(N) or X(2) : X coordinates or (XMIN,XMAX) (WC space).
*.  REAL Y(N) or Y(2) : Y coordinates or (YMIN,YMAX) (WC space).
*.  CHARACTER*(*) CHOPT : Option.
*.
*.  CHOPT='L' :  A simple polyline beetwen every points is drawn
*.
*.  CHOPT='F' :  A fill area is drawn ('CF' draw a smooth fill area)
*.
*.  CHOPT='A' :  Axis are drawn around the graph
*.
*.  CHOPT='C' :  A smooth Curve is drawn
*.
*.  CHOPT='*' :  A Star is plotted at each point
*.
*.  CHOPT='P' :  Idem with the current marker
*.
*.  CHOPT='B' :  A Bar chart is drawn at each point
*.
*.  CHOPT='1' :  YLOW=RWYMIN
*.
*.  CHOPT='GX':  LOGX
*.
*.  CHOPT='GY':  LOGY
*.
*.  CHOPT='Z' :  The vectors X and Y are taken into PAWC at the
*.               adresses IQUEST(81) and IQUEST(82)
*.
*..==========> (O.Couet N.Cremel-Somon)
#include "higz/hipaw.inc"
#include "higz/hiflag.inc"
#include "higz/hiatt.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#include "higz/hihist.inc"
      PARAMETER (NPMAX=1004)
      DIMENSION X(*),Y(*)
      CHARACTER*(*) CHOPT
      CHARACTER*8 CHOPA
      EQUIVALENCE (LXADR,L2NTLA),(LYADR,LDNTLA)
      LOGICAL ZFSAV
      DIMENSION IOPT(12)
      EQUIVALENCE (IOPTL ,IOPT(1)) , (IOPTA ,IOPT(2))
      EQUIVALENCE (IOPTC ,IOPT(3)) , (IOPTST,IOPT(4))
      EQUIVALENCE (IOPTP ,IOPT(5)) , (IOPTB ,IOPT(6))
      EQUIVALENCE (IOPTR ,IOPT(7)) , (IOPT1 ,IOPT(8))
      EQUIVALENCE (IOPTG ,IOPT(9)) , (IOPTX ,IOPT(10))
      EQUIVALENCE (IOPTY ,IOPT(11)), (IOPTF ,IOPT(12))
*.______________________________________
*
      IQUEST(1)=0
      IF(N.LE.0)THEN
         CALL IGERR(' Number of points is invalid','IGRAPH')
         RETURN
      ENDIF
*
      CALL UOPTC(CHOPT,'LAC*PBR1GXYF',IOPT)
*
*              If no "drawing" option is selected and if CHOPT<>' '
*              nothing is done.
*
      IF(IOPTL+IOPTF+IOPTC+IOPTST+IOPTP+IOPTB.EQ.0)THEN
         IF(CHOPT.EQ.' ')THEN
            IOPTL=1
         ELSE
            RETURN
         ENDIF
      ENDIF
*
*              If necessary the adresses of the vectors are saved in
*              a link area.
*
      IF(INDEX(CHOPT,'Z').NE.0)THEN
         LXADR = IQUEST(81)
         LYADR = IQUEST(82)
         IOPTZ = 1
      ELSE
         IOPTZ = 0
      ENDIF
*
#if defined(CERNLIB_MAIL)
      CHOPTM=CHOPT
#endif
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
      IF(GLFLAG)CALL IZGRAP(N,X,Y,IOPT,IOPTZ)
#endif
      IF((.NOT.GFLAG).AND.(.NOT.PFLAG))RETURN
      ZFSAV  = ZFLAG
      ZFLAG  = .FALSE.
      GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
*
      IOPTCF = 0
      IF((IOPTC.NE.0).AND.(IOPTF.NE.0))THEN
         IOPTCF = 1
         IOPTF  = 0
      ENDIF
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
*              Draw the graph with a polyline or a fill area
*              beetwen every point
*
      IF((IOPTL.NE.0).OR.(IOPTF.NE.0))THEN
         IF(IOPTZ.NE.0)THEN
            XWORK(1) = Q(LXADR)
            YWORK(1) = Q(LYADR)
            X1       = Q(LXADR)
            XN       = Q(LXADR+N-1)
            Y1       = Q(LYADR)
            YN       = Q(LYADR+N-1)
         ELSE
            XWORK(1) = X(1)
            YWORK(1) = Y(1)
            X1       = X(1)
            XN       = X(N)
            Y1       = Y(1)
            YN       = Y(N)
         ENDIF
         NLOOP = N
         IF(IOPTF.NE.0.AND.(XN.NE.X1.OR.YN.NE.Y1))NLOOP = NLOOP+1
         NPT = 1
         DO 10 I=1,NLOOP
            NPT = NPT+1
            IF(I.GT.N)THEN
               XWORK(NPT) = XWORK(1)
               YWORK(NPT) = YWORK(1)
            ELSE
               IF(IOPTZ.NE.0)THEN
                  XWORK(NPT) = Q(LXADR+I-1)
                  YWORK(NPT) = Q(LYADR+I-1)
               ELSE
                  XWORK(NPT) = X(I)
                  YWORK(NPT) = Y(I)
               ENDIF
            ENDIF
            IF(NPT.EQ.NPMAX.OR.I.EQ.NLOOP)THEN
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               IF(IOPTR.NE.0)THEN
                  IF(IOPTF.NE.0)THEN
                     CALL IFA(NPT,YWORKL,XWORKL)
                     IF(IBORD.NE.0)CALL IPL(NPT,YWORKL,XWORKL)
                  ELSE
                     CALL IPL(NPT,YWORKL,XWORKL)
                  ENDIF
               ELSE
                  IF(IOPTF.NE.0)THEN
                     CALL IFA(NPT,XWORKL,YWORKL)
                     IF(IBORD.NE.0)CALL IPL(NPT,XWORKL,YWORKL)
                  ELSE
                     CALL IPL(NPT,XWORKL,YWORKL)
                  ENDIF
               ENDIF
               XWORK(1) = XWORK(NPT)
               YWORK(1) = YWORK(NPT)
               NPT      = 1
            ENDIF
   10    CONTINUE
      ENDIF
*
*              Draw the graph with a smooth Curve. The computing
*              of the smoothing is done by the routine IGRAP1
*
      IF(IOPTC.NE.0)THEN
         IF(IOPTZ.NE.0)THEN
            X1 = Q(LXADR)
            XN = Q(LXADR+N-1)
            Y1 = Q(LYADR)
            YN = Q(LYADR+N-1)
         ELSE
            X1 = X(1)
            XN = X(N)
            Y1 = Y(1)
            YN = Y(N)
         ENDIF
         ITYPS = 1
         NLOOP = N
         IF(IOPTCF.NE.0)THEN
            ITYPS = ITYPS+1000
            IF(XN.NE.X1.OR.YN.NE.Y1)NLOOP = NLOOP+1
         ENDIF
         IF(IOPTR.EQ.0)THEN
            NPT = 0
            DO 20 I=1,NLOOP
               NPT = NPT+1
               IF(I.GT.N)THEN
                  XWORK(NPT) = XWORK(1)
                  YWORK(NPT) = YWORK(1)
               ELSE
                  IF(IOPTZ.NE.0)THEN
                     XWORK(NPT) = Q(LXADR+I-1)
                     YWORK(NPT) = Q(LYADR+I-1)
                  ELSE
                     XWORK(NPT) = X(I)
                     YWORK(NPT) = Y(I)
                  ENDIF
               ENDIF
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               IF(YWORKL(NPT).LT.RWYMIN.OR.YWORKL(NPT).GT.RWYMAX)THEN
                  IF(NPT.GT.2)THEN
                     CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
                     CALL IGRAP1(XWORKL,YWORKL,NPT,ITYPS)
                  ENDIF
                  XWORK(1) = XWORK(NPT)
                  YWORK(1) = YWORK(NPT)
                  NPT=1
                  GOTO 20
               ENDIF
               IF(NPT.GE.NPMAX)THEN
                  CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
                  CALL IGRAP1(XWORKL,YWORKL,NPT,ITYPS)
                  XWORK(1) = XWORK(NPT)
                  YWORK(1) = YWORK(NPT)
                  NPT      = 1
               ENDIF
   20       CONTINUE
            IF(NPT.GT.1)THEN
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               CALL IGRAP1(XWORKL,YWORKL,NPT,ITYPS)
            ENDIF
         ELSE
            ITYPS = ITYPS+10
            NPT   = 0
            DO 30 I=1,NLOOP
               NPT=NPT+1
               IF(I.GT.N)THEN
                  XWORK(NPT) = XWORK(1)
                  YWORK(NPT) = YWORK(1)
               ELSE
                  IF(IOPTZ.NE.0)THEN
                     XWORK(NPT) = Q(LXADR+I-1)
                     YWORK(NPT) = Q(LYADR+I-1)
                  ELSE
                     XWORK(NPT) = X(I)
                     YWORK(NPT) = Y(I)
                  ENDIF
               ENDIF
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               IF(XWORKL(NPT).LT.RWXMIN.OR.XWORKL(NPT).GT.RWXMAX)THEN
                  IF(NPT.GT.2)THEN
                     CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
                     CALL IGRAP1(XWORKL,YWORKL,NPT,ITYPS)
                  ENDIF
                  XWORK(1) = XWORK(NPT)
                  YWORK(1) = YWORK(NPT)
                  NPT=1
                  GOTO 30
               ENDIF
               IF(NPT.GE.NPMAX)THEN
                  CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
                  CALL IGRAP1(XWORKL,YWORKL,NPT,ITYPS)
                  XWORK(1) = XWORK(NPT)
                  YWORK(1) = YWORK(NPT)
                  NPT      = 1
               ENDIF
   30       CONTINUE
            IF(NPT.GT.1)THEN
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               CALL IGRAP1(XWORKL,YWORKL,NPT,ITYPS)
            ENDIF
         ENDIF
      ENDIF
*
*              Draw the graph with a '*' on every points
*
      IF(IOPTST.NE.0)THEN
         IMKOLD = IMK
         CALL ISMK(3)
         NPT = 0
         DO 40 I=1,N
            NPT = NPT+1
            IF(IOPTZ.NE.0)THEN
               XWORK(NPT) = Q(LXADR+I-1)
               YWORK(NPT) = Q(LYADR+I-1)
            ELSE
               XWORK(NPT) = X(I)
               YWORK(NPT) = Y(I)
            ENDIF
            IF(NPT.EQ.NPMAX.OR.I.EQ.N)THEN
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               IF(IOPTR.NE.0)THEN
                  CALL IPM(NPT,YWORKL,XWORKL)
               ELSE
                  CALL IPM(NPT,XWORKL,YWORKL)
               ENDIF
               NPT = 0
            ENDIF
   40    CONTINUE
         CALL ISMK(IMKOLD)
      ENDIF
*
*              Draw the graph with the current polymarker on
*              every points
*
      IF(IOPTP.NE.0)THEN
         NPT = 0
         DO 50 I=1,N
            NPT = NPT+1
            IF(IOPTZ.NE.0)THEN
               XWORK(NPT) = Q(LXADR+I-1)
               YWORK(NPT) = Q(LYADR+I-1)
            ELSE
               XWORK(NPT) = X(I)
               YWORK(NPT) = Y(I)
            ENDIF
            IF(NPT.EQ.NPMAX.OR.I.EQ.N)THEN
               CALL IGHIS1(NPT,IOPTG,IOPTX,IOPTY)
               IF(IOPTR.NE.0)THEN
                  CALL IPM(NPT,YWORKL,XWORKL)
               ELSE
                  CALL IPM(NPT,XWORKL,YWORKL)
               ENDIF
               NPT = 0
            ENDIF
   50    CONTINUE
      ENDIF
*
*              Draw the graph as a bar chart
*
      IF(IOPTB.NE.0)THEN
         IF(IOPTR.EQ.0)THEN
            IF(IOPTZ.NE.0)THEN
               BXMIN = VMIN(Q(LXADR),N)
               BXMAX = VMAX(Q(LXADR),N)
            ELSE
               BXMIN = VMIN(X,N)
               BXMAX = VMAX(X,N)
            ENDIF
            BDELTA = (BXMAX-BXMIN)/N
         ELSE
            IF(IOPTZ.NE.0)THEN
               BYMIN = VMIN(Q(LYADR),N)
               BYMAX = VMAX(Q(LYADR),N)
            ELSE
               BYMIN = VMIN(Y,N)
               BYMAX = VMAX(Y,N)
            ENDIF
            BDELTA = (BYMAX-BYMIN)/N
         ENDIF
         DBAR  = BDELTA*RBWD*0.5
         IBOLD = IBORD
         CALL IGSET('BORD',1.)
         IF(IOPTR.EQ.0)THEN
            DO 60 I=1,N
               IF(IOPTZ.NE.0)THEN
                  XLOW  = Q(LXADR+I-1)-DBAR
                  XHIGH = Q(LXADR+I-1)+DBAR
                  YHIGH = Q(LYADR+I-1)
               ELSE
                  XLOW  = X(I)-DBAR
                  XHIGH = X(I)+DBAR
                  YHIGH = Y(I)
               ENDIF
               IF(IOPT1.EQ.0)THEN
                  YLOW = MAX(0.,RWYMIN)
               ELSE
                  YLOW = RWYMIN
               ENDIF
               XWORK(1) = XLOW
               YWORK(1) = YLOW
               XWORK(2) = XHIGH
               YWORK(2) = YHIGH
               CALL IGHIS1(2,IOPTG,IOPTX,IOPTY)
               CALL IGBOX(XWORKL(1),XWORKL(2),YWORKL(1),YWORKL(2))
   60       CONTINUE
         ELSE
            DO 70 I=1,N
               IF(IOPTZ.NE.0)THEN
                  XHIGH = Q(LXADR+I-1)
                  YLOW  = Q(LYADR+I-1)-DBAR
                  YHIGH = Q(LYADR+I-1)+DBAR
               ELSE
                  XHIGH = X(I)
                  YLOW  = Y(I)-DBAR
                  YHIGH = Y(I)+DBAR
               ENDIF
               XLOW     = MAX(0.,RWXMIN)
               XWORK(1) = XLOW
               YWORK(1) = YLOW
               XWORK(2) = XHIGH
               YWORK(2) = YHIGH
               CALL IGHIS1(2,IOPTG,IOPTX,IOPTY)
               CALL IGBOX(XWORKL(1),XWORKL(2),YWORKL(1),YWORKL(2))
   70       CONTINUE
         ENDIF
         CALL IGSET('BORD',FLOAT(IBOLD))
      ENDIF
*
      ZFLAG=ZFSAV
      GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
