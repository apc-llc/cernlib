*
* $Id: datime.s,v 1.1.1.1 1996/02/15 17:53:56 mclareni Exp $
*
* $Log: datime.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:56  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  01/09/76
          AXR$
 .    CALL DATIME (ID,IT)    RETURN INTEGER DATE + TIME
 .                           ID = YYMMDD,  IT = HHMM
 .                           STRASSBURG, AUG 1976
$(1)
DATIME*   ER        TDATE$
          S         A0,WORD
 .
 .    PROCESS DATE
          L,S3      A1,WORD
          A,U       A1,64    . A1= YY
          MSI,U     A1,100   . A1= 100*YY
          L,S1      A2,WORD
          A         A1,A2    . A1= 100*YY + MM
          MSI,U     A1,100   . A1= 100 * (100*YY+MM)
          L,S2      A2,WORD
          A         A1,A2    . A1= 100 * (100*YY+MM)  + DD
          S         A1,*0,X11
 .
 .    PROCESS TIME
          LA,U      A0,0
          L,H2      A1,WORD  . A1= 60 * (60*HH+MM)  + SS
          DI,U      A0,60    . A0= 60*HH + MM
          L         A1,A0
          LA,U      A0,0
          DI,U      A0,60    . A0= HH,   A1= MM
          MSI,U     A0,100
          AA        A1,A0    . A1= 100*HH + MM
          S         A1,*1,X11
          J         3,X11
$(0)
WORD      +         0
          END
#ifdef CERNLIB_TCGEN_DATIME
#undef CERNLIB_TCGEN_DATIME
#endif
