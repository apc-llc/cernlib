*
* $Id: margin.s,v 1.1.1.1 1996/02/15 17:53:57 mclareni Exp $
*
* $Log: margin.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:57  mclareni
* Kernlib
*
*
 .                                      LAST MODIF.  01/09/76
          AXR$
 .    CALL MARGIN (TEXT,NWTX)      PRINTER MARGIN CONTROL
 .                                 STRASSBURG, AUG 1976
$(1)
MARGIN*   L         A0,*1,X11           . LENGTH OF TEXT
          LSSL      A0,18               . SHIFT TO H1
          A         A0,0,X11            . ADDRESS OF TEXT
          ER        PRTCN$              . CALL PRINTER CONTROL
          J         3,X11               . RETURN
          END
