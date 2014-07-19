*
* $Id: hintrfc_.s,v 1.1.1.1 1996/02/15 17:54:07 mclareni Exp $
*
* $Log: hintrfc_.s,v $
* Revision 1.1.1.1  1996/02/15 17:54:07  mclareni
* Kernlib
*
*
. **************************************
.
.     INTERFACE ROUTINE USED FOR FORTRAN ROUTINES WITH A VARIABLE NUMBER
.     OF ARGUMENTS (SEE HBOOK, GD3...)
. ROUTINE NOARG WAS DROPPED BECAUSE IT COULD NOT BE USED WITH ALL COMPILERS
. .FOR EACH ROUTINE CONCERNED YOU MUST PROVIDE A 2 WORDS INTERFACE:
. . ASSUMING XXX IS THE NAME OF THE ROUTINE WITH A VAR. NUMB. OF ARGS.
.  @ASM,I   ,INTERFXXX
.  $(1),XXX*   LMJ   14,HINTRFC$
.              +     YYY
.              END
. . AND YOU CHANGE THE NAME (XXX) OF THE ROUTINE TO YYY IN THE FUNCTION OR
. . SUBROUTINE STATEMENT
. . YOU MAY THEN MERGE THE ROUTINE YYY AND THE INTERFACE INTO ONE SINGLE
. . RELOCATABLE ELEMEMT
.  @MAP,IR   ,XXX
.  IN YYY,INTERFXXX
.  DEF XXX
.
. **************************************
 
          AXR$
          FORT$
HNOARG    INFO  2  2
$(2),NARG +         0
 
$(1)
HINTRFC$*
#if defined(CERNLIB_GUYFORT)||defined(CERNLIB_GUYFORE)
          L         A3,X11
          LXI,U     A3,1
          TNZ,S1    0,*A3
          J         $-1
          AN,U      A3,1,X11          . 'RETURN INCREMENT'
          L,U       A1,06
          L         A2,0,A2           . LOC( TARGET )
 DO FORT , S,H2     A3,NARG
 DO FORE , ANU,U    A3,1
 DO FORE , S,H2     A4,NARG
          TE,S1     A1,0,A2
          J         HIN4
          S,S2      A3,*0,A2
          J         0,A2
HIN4
          LSSL      A3,6
          A,U       A3,013
          LXI       X11,A3
          J         1,A2
#endif
#if defined(CERNLIB_GUYFTN)
          L         A4,A0
          SSL       A4,18
          S,S6      A4,NARG
          J         *0,A2
#endif
          END
