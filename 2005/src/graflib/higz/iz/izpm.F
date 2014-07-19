*
* $Id: izpm.F,v 1.4 1998/05/26 09:19:30 couet Exp $
*
* $Log: izpm.F,v $
* Revision 1.4  1998/05/26 09:19:30  couet
* - For filling patterns 1 to 25 the PS ouput was wrong in some cases. For those
* the pattern we now close the polygon (move to X1 Y1).
*
* Revision 1.3  1997/01/30 10:49:15  couet
* - Initialisation of NP was missing (in case all the points are outside the
* world coordinates).
*
* Revision 1.2  1997/01/30 10:04:18  couet
* - Bug fixed in the marker PS drawing: when the markers drawn were too for from
* the world coordinates, unvalid PS coordinates were generated.
*
* Revision 1.1.1.1  1996/02/14 13:11:13  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_PSCRIPT)||defined(CERNLIB_MAIL)
*CMZ :  1.23/03 20/09/95  13.58.45  by  O.Couet
*-- Author :
      SUBROUTINE IZPM(N,X,Y)
*.===========>
*.
*.   This routine stores in the current picture the necessary data to
*. to draw polymarkers .
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of points .
*.  REAL X(N) : X coordinates .
*.  REAL Y(N) : Y coordinates .
*.
*..==========> (O.Couet)
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
#include "higz/hicode.inc"
#endif
#include "higz/hiflag.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if defined(CERNLIB_PSCRIPT)
#include "higz/hiatt.inc"
#include "higz/hipost.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
      DIMENSION X(*),Y(*)
#if defined(CERNLIB_PSCRIPT)
      CHARACTER*4 CHTEMP
      CHARACTER TEXMRK(5)*7
      LOGICAL ZFSAV
      PARAMETER (ISTS = 100)
      DATA TEXMRK/'dot','plus','star','circle','cross'/
#endif
*.______________________________________
*
#if defined(CERNLIB_PSCRIPT)
#include "higz/hiwcps.inc"
      IF(PFLAG)THEN
         CALL IPZONE
         IF(TFLAG)THEN
            LM=ABS(IMK)
            IF(LM.GE.2 .AND. LM.LE.5) THEN
               IM=NINT(RMKSC*16)
            ELSE
               LM=1
               IM=NINT(RMKSC)
            ENDIF
            CALL IPPSTR(BSLASH//'let'//BSLASH//'higzmarker='//
     +         BSLASH//'higz'//TEXMRK(LM)(:INDEX(TEXMRK(LM),' ')-1)//
     +         BSLASH//'higzmsize=')
            CALL IPIOUT(IM)
            DO 10  I=1,N
               IF(X(I).GE.X1W .AND. X(I).LE.X2W .AND.
     +            Y(I).GE.Y1W .AND. Y(I).LE.Y2W) THEN
                  IXD=IXWCPS(X(I))
                  IYD=IYWCPS(Y(I))
                  CALL IPTLIN('p',IXD,IYD)
               ENDIF
   10       CONTINUE
         ELSE
            CALL IPLWID(INT(RLWSC))
            CALL IPLTYP(1)
            CALL IPSCOL(IPMCI)
            LM = ABS(IMK)
            IF(LM.LE.0) CHTEMP =' m20'
            IF(LM.EQ.1) CHTEMP =' m20'
            IF(LM.EQ.2) CHTEMP =' m2'
            IF(LM.EQ.3) CHTEMP =' m31'
            IF(LM.EQ.4) CHTEMP =' m24'
            IF(LM.EQ.5) CHTEMP =' m5'
            IF(LM.GE.6.AND.LM.LE.19) CHTEMP = ' m20'
            IF(LM.GE.20.AND.LM.LE.31)THEN
               CHTEMP=' m  '
               WRITE (CHTEMP(3:4),'(I2)') LM
            ENDIF
            IF(LM.GE.32) CHTEMP = ' m20'
*
*              Normalized space
*
            ZFSAV  = ZFLAG
            ZFLAG  = .FALSE.
            GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
            INTSAV = INTR
            SVXMAX = RVXMAX
            SVXMIN = RVXMIN
            SWXMAX = RWXMAX
            SWXMIN = RWXMIN
            SVYMAX = RVYMAX
            SVYMIN = RVYMIN
            SWYMAX = RWYMAX
            SWYMIN = RWYMIN
            LOSCLI = .TRUE.
            CALL ISELNT(0)
*
*              Set the PostScript marker size
*
            RMKSIZ = 0.009*RMKSC
            RNSIZ  = 0.
            CALL IGTEXT(0.,0.,'"0 ',RMKSIZ,RNSIZ,'S')
            IMSIZ  = IYWCPS(RNSIZ)-IYWCPS(0.)
            IF(LM.EQ.1)IMSIZ=INT(2.*RMKSC)
            IF(LM.EQ.6.OR.LM.EQ.7)IMSIZ=INT(4.*RMKSC)
            IF(IPSMSC.NE.IMSIZ)THEN
               IPSMSC = IMSIZ
               CALL IPPSTF(3,' /w')
               CALL IPIOUT(IMSIZ)
               CALL IPPSTF(40,
     +                     ' def /w2 {w 2 div} def /w3 {w 3 div} def')
            ENDIF
*
            XV=(((SVXMAX-SVXMIN)*(X(1)-SWXMIN))/(SWXMAX-SWXMIN))+SVXMIN
            YV=(((SVYMAX-SVYMIN)*(Y(1)-SWYMIN))/(SWYMAX-SWYMIN))+SVYMIN
            IXPS = IXWCPS(XV)
            IYPS = IYWCPS(YV)
            NP = 0
            IF (IXPS.GE.0.AND.IYPS.GE.0) THEN
               CALL IPIOUT(IXPS)
               CALL IPIOUT(IYPS)
               IF(N.EQ.1)THEN
                  CALL IPPSTR(CHTEMP)
                  GOTO 40
               ENDIF
               NP = 1
            ENDIF
            DO 20 I=2,N
               XV = (((SVXMAX-SVXMIN)*(X(I)-SWXMIN))/
     +                (SWXMAX-SWXMIN))+SVXMIN
               YV = (((SVYMAX-SVYMIN)*(Y(I)-SWYMIN))/
     +                (SWYMAX-SWYMIN))+SVYMIN
               IXPS = IXWCPS(XV)
               IYPS = IYWCPS(YV)
               IF (IXPS.GE.0.AND.IYPS.GE.0) THEN
                  CALL IPIOUT(IXWCPS(XV))
                  CALL IPIOUT(IYWCPS(YV))
                  NP = NP+1
               ENDIF
               IF(NP.EQ.ISTS.OR.I.EQ.N)THEN
                  IF (NP.GT.0) THEN
                     CALL IPIOUT(NP)
                     CALL IPPSTF(2,' {')
                     CALL IPPSTR(CHTEMP)
                     CALL IPPSTF(3,'} R')
                     NP = 0
                  ENDIF
               ENDIF
   20       CONTINUE
   40       CALL ISELNT(INTSAV)
            LOSCLI = .FALSE.
            ZFLAG  = ZFSAV
            GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         IF(N.EQ.1)THEN
            WRITE (CHMAIL,'(I3,4E16.7)') IPM1CO,X(1),Y(1)
            CALL IMWRIT(4)
         ELSE
            WRITE (CHMAIL,'(I3,I5)') IPMCO,N
            CALL IMWRIT(1)
            CALL IMFOUT(N,X)
            CALL IMFOUT(N,Y)
            CALL IMWRIT(5)
         ENDIF
      ENDIF
#endif
#if defined(CERNLIB_ZEBRA)
*
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(IZPUSH(2,2*N,0,'IPM').NE.0)RETURN
         CALL IZCFA(IMKCO,1)
         CALL IZCFA(IMKSCO,1)
         CALL IZCFA(IPMCCO,1)
         IF(N.NE.1)THEN
            CALL IZSTCC(IPMCO,INTPTR)
            IQ(LHI+INTPTR)   = IFLPTR
            IQ(LHI+INTPTR+1) = N
            CALL IZINCI(2)
            IADR  = LHF+IFLPTR-1
            IADRN = IADR+N
            DO 50 I=1,N
               Q(IADR+I)  = X(I)
               Q(IADRN+I) = Y(I)
   50       CONTINUE
            CALL IZINCF(2*N)
         ELSE
            CALL IZSTCC(IPM1CO,IFLPTR)
            Q(LHF+IFLPTR)   = X(1)
            Q(LHF+IFLPTR+1) = Y(1)
            CALL IZINCF(2)
         ENDIF
      ENDIF
#endif
*
      END
#endif
