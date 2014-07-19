*
* $Id: dzehpe.F,v 1.1.1.1 1996/03/04 16:13:24 mclareni Exp $
*
* $Log: dzehpe.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:24  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEHPE(LUN)
      WRITE(LUN,'(9(A/),A)')
     & '%!PS-Adobe-2.0'
     &,'%! DZEDIT/PostScript Metafile'
     &,'/d {lineto} def /m {moveto} def /t {translate} def'
     &,'/sw {stringwidth} def /r {rotate} def /rl {roll} def'
     &,'/rd {rlineto} def /rm {rmoveto} def /gr {grestore} def'
     &,'/lw {setlinewidth} def /sd {setdash} def'
     &,'/s {stroke} def /rgb {setrgbcolor} def'
     &,'/cl {closepath} def /sf {scalefont setfont} def'
     &,'/oshow {gsave [] 0 sd true charpath stroke gr} def'
     &,'/cs {gsave dup sw pop 2 div neg 5 -1 rl 5 -1 rl t 3 -1'
     &,' rl r 0 m show gr} def'
      WRITE(LUN,'(8(A/),A)')
     & '/rs {gsave dup sw pop neg 5 -1 rl 5 -1 rl t 3 -1 rl r'
     &,' 0 m show gr} def'
     &,'/ocs {gsave dup sw pop 2 div neg 5 -1 rl 5 -1 rl t 3 -1'
     &,' rl r 0 m oshow gr} def'
     &,'/ors {gsave dup sw pop neg 5 -1 rl 5 -1 rl t 3 -1 rl r'
     &,' 0 m oshow gr} def'
     &,'/H /Helvetica-Bold findfont def'
     &,'/C /Courier findfont def'
     &,'/n {newpath} def'
     &,'/sh {show} def'
     &,'/sg {setgray} def'
      END
****************************************************************
