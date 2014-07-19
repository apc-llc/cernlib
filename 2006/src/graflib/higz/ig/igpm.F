*
* $Id: igpm.F,v 1.1.1.1 1996/02/14 13:10:37 mclareni Exp $
*
* $Log: igpm.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:37  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.18/10 21/06/93  11.05.42  by  O.Couet
*-- Author :
      SUBROUTINE IGPM(N,X,Y,ITYPE)
*.===========>
*.
*.   This routines draws markers 20 to 31.
*.
*. _Input parameters:
*.
*.  INTEGER N : Number of markers .
*.  REAL X(N) : Markers X coordinates .
*.  REAL Y(N) : Markers Y coordinates .
*.  INTEGER ITYPE : Markers type.
*.
*..==========> (O.Couet)
#include "higz/hiatt.inc"
#include "higz/hiflag.inc"
      DIMENSION X(*),Y(*)
      LOGICAL ZFSAV,PFSAV
      CHARACTER*3 CHARS(12)
      REAL MKSIZE
      DATA CHARS/'"0 ','"1 ','"2 ','"3 ','"<0','"<1','"<2','"<3',
     +           '"C ','"4 ','"<4','"<*'/
*.______________________________________
*
      IF(ITYPE.LT.20.OR.ITYPE.GT.31)RETURN
      ZFSAV = ZFLAG
      ZFLAG = .FALSE.
      PFSAV = PFLAG
      IF(.NOT.TFLAG)PFLAG=.FALSE.
      GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
      NTSAV  = INTR
      IPS    = INPASS
      INPASS = 1
      ITXCIS = ITXCI
      CALL ISTXCI(IPMCI)
      MKSIZE = RMKSC*0.009
      SF     = MKSIZE/2.
      DO 10 I=1,N
         IF(X(I).GT.RWXMAX.OR.X(I).LT.RWXMIN.OR.
     +      Y(I).GT.RWYMAX.OR.Y(I).LT.RWYMIN)GOTO 10
         XV = (((RVXMAX-RVXMIN)*(X(I)-RWXMIN))/(RWXMAX-RWXMIN))+RVXMIN
         YV = (((RVYMAX-RVYMIN)*(Y(I)-RWYMIN))/(RWYMAX-RWYMIN))+RVYMIN
         YV = YV-SF
         CALL ISELNT(0)
         CALL IGTEXT(XV,YV,CHARS(ITYPE-19),MKSIZE,0.,'C')
         CALL ISELNT(NTSAV)
   10 CONTINUE
      CALL ISTXCI(ITXCIS)
      INPASS = IPS
      ZFLAG  = ZFSAV
      PFLAG  = PFSAV
      GLFLAG = (ZFLAG.OR.PFLAG.OR.MFLAG)
*
      END
