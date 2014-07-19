*
* $Id: ipmark.F,v 1.3 2005/10/28 12:38:14 couet Exp $
*
* $Log: ipmark.F,v $
* Revision 1.3  2005/10/28 12:38:14  couet
* - SVG driver, workstation type -779
*
* Revision 1.1.1.1  1996/02/14 13:11:07  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.21/03 19/05/94  14.20.47  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IPMARK
*.===========>
*.
*.  This routine define the markers
*.
*.==========> (O.Couet)
#include "higz/hiflag.inc"
      IF (VFLAG) THEN
         CALL IPPSTR(' <marker id="M01"')
         CALL IPPSTR(' viewBox="0 0 10 10" refX="5" refY="5"')
         CALL IPPSTR(' markerUnits="strokeWidth"')
         CALL IPPSTR(' markerWidth="1" markerHeight="10"')
         CALL IPPSTR(' orient="auto">')
         CALL IPPSTR(' <circle cx="5" cy="5" r="5"/>')
         CALL IPPSTR(' </marker>')
      ELSE
      CALL IPPSTR('/mp {newpath /y exch def /x exch def} def@')
      CALL IPPSTR('/side {[w .77 mul w .23 mul] .385 w mul sd w 0 l curr
     +entpoint t -144 r} def@')
      CALL IPPSTR('/mr {mp x y w2 0 360 arc} def /m24 {mr s} def /m20 {m
     +r f} def@')
      CALL IPPSTR('/mb {mp x y w2 add m w2 neg 0 d 0 w neg d w 0 d 0 w d
     + cl} def@')
      CALL IPPSTR('/mt {mp x y w2 add m w2 neg w neg d w 0 d cl} def@')
      CALL IPPSTR('/m21 {mb f} def /m25 {mb s} def /m22 {mt f} def /m26
     +{mt s} def@')
      CALL IPPSTR('/m23 {mp x y w2 sub m w2 w d w neg 0 d cl f} def@')
      CALL IPPSTR('/m27 {mp x y w2 add m w3 neg w2 neg d w3 w2 neg d w3
     + w2 d cl s} def@')
      CALL IPPSTR('/m28 {mp x w2 sub y w2 sub w3 add m w3 0 d ')
      CALL IPPSTR(' 0 w3 neg d w3 0 d 0 w3 d w3 0 d ')
      CALL IPPSTR(' 0 w3 d w3 neg 0 d 0 w3 d w3 neg 0 d')
      CALL IPPSTR(' 0 w3 neg d w3 neg 0 d cl s } def@')
      CALL IPPSTR('/m29 {mp gsave x w2 sub y w2 add w3 sub m currentpoin
     +t t')
      CALL IPPSTR(' 4 {side} repeat cl fill gr} def@')
      CALL IPPSTR('/m30 {mp gsave x w2 sub y w2 add w3 sub m currentpoin
     +t t')
      CALL IPPSTR(' 5 {side} repeat s gr} def@')
      CALL IPPSTR('/m31 {mp x y w2 sub m 0 w d x w2 sub y m w 0 d')
      CALL IPPSTR(' x w2 sub y w2 add m w w neg d x w2 sub y w2')
      CALL IPPSTR(' sub m w w d s} def@')
      CALL IPPSTR('/m2 {mp x y w2 sub m 0 w d x w2 sub y m w 0 d s} def@
     +')
      CALL IPPSTR('/m5 {mp x w2 sub y w2 sub m w w d x w2 sub y w2 add m
     + w w neg d s} def@')
      ENDIF
*
      END
