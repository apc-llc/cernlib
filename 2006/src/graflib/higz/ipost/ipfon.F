*
* $Id: ipfon.F,v 1.3 2005/10/28 12:38:14 couet Exp $
*
* $Log: ipfon.F,v $
* Revision 1.3  2005/10/28 12:38:14  couet
* - SVG driver, workstation type -779
*
* Revision 1.1.1.1  1996/02/14 13:11:06  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.16/01 13/08/92  18.40.25  by  O.Couet
*-- Author :    P.Juillot   13/08/92
      SUBROUTINE IPFON
*.===========>
*.
*. Font reencoding
*.
*..==========> (O.Couet)
      CALL IPPSTR('@/reencdict 24 dict def')
      CALL IPPSTR(' /ReEncode')
      CALL IPPSTR(' {reencdict begin')
      CALL IPPSTR(' /nco&na exch def')
      CALL IPPSTR('@/nfnam exch def /basefontname exch')
      CALL IPPSTR(' def /basefontdict basefontname')
      CALL IPPSTR(' findfont def')
      CALL IPPSTR('@/newfont basefontdict maxlength dict def')
      CALL IPPSTR(' basefontdict')
      CALL IPPSTR(' {exch dup /FID ne')
      CALL IPPSTR('@{dup /Encoding eq')
      CALL IPPSTR(' {exch dup length array copy')
      CALL IPPSTR(' newfont 3 1 roll put} {exch ')
      CALL IPPSTR('@newfont 3 1 roll put}')
      CALL IPPSTR(' ifelse}')
      CALL IPPSTR(' {pop pop}')
      CALL IPPSTR(' ifelse')
      CALL IPPSTR(' } forall newfont')
      CALL IPPSTR('@/FontName nfnam put')
      CALL IPPSTR(' nco&na aload pop')
      CALL IPPSTR(' nco&na length 2 idiv {newfont')
      CALL IPPSTR('@/Encoding get 3 1 roll put}')
      CALL IPPSTR(' repeat')
      CALL IPPSTR(' nfnam newfont definefont pop')
      CALL IPPSTR(' end } def')
      CALL IPPSTR('@/accvec [')
      CALL IPPSTR(' 176 /agrave     ')
      CALL IPPSTR(' 181 /Agrave     ')
      CALL IPPSTR(' 190 /acircumflex')
      CALL IPPSTR(' 192 /Acircumflex')
      CALL IPPSTR('@201 /adieresis  ')
      CALL IPPSTR(' 204 /Adieresis  ')
      CALL IPPSTR(' 209 /ccedilla   ')
      CALL IPPSTR(' 210 /Ccedilla   ')
      CALL IPPSTR(' 211 /eacute     ')
      CALL IPPSTR('@212 /Eacute     ')
      CALL IPPSTR(' 213 /egrave     ')
      CALL IPPSTR(' 214 /Egrave     ')
      CALL IPPSTR(' 215 /ecircumflex')
      CALL IPPSTR(' 216 /Ecircumflex')
      CALL IPPSTR('@217 /edieresis  ')
      CALL IPPSTR(' 218 /Edieresis  ')
      CALL IPPSTR(' 219 /icircumflex')
      CALL IPPSTR(' 220 /Icircumflex')
      CALL IPPSTR('@221 /idieresis  ')
      CALL IPPSTR(' 222 /Idieresis  ')
      CALL IPPSTR(' 223 /ntilde     ')
      CALL IPPSTR(' 224 /Ntilde     ')
      CALL IPPSTR(' 226 /ocircumflex')
      CALL IPPSTR('@228 /Ocircumflex')
      CALL IPPSTR(' 229 /odieresis  ')
      CALL IPPSTR(' 230 /Odieresis  ')
      CALL IPPSTR(' 231 /ucircumflex')
      CALL IPPSTR(' 236 /Ucircumflex')
      CALL IPPSTR('@237 /udieresis  ')
      CALL IPPSTR(' 238 /Udieresis  ')
      CALL IPPSTR(' 239 /aring      ')
      CALL IPPSTR(' 242 /Aring      ')
      CALL IPPSTR(' 243 /ydieresis  ')
      CALL IPPSTR('@244 /Ydieresis  ')
      CALL IPPSTR(' 246 /aacute     ')
      CALL IPPSTR(' 247 /Aacute     ')
      CALL IPPSTR(' 252 /ugrave     ')
      CALL IPPSTR(' 253 /Ugrave     ')
      CALL IPPSTR('] def')
      CALL IPPSTR('/Times-Roman /Times-Roman accvec ReEncode@')
      CALL IPPSTR('/Times-Italic /Times-Italic accvec ReEncode@')
      CALL IPPSTR('/Times-Bold /Times-Bold accvec ReEncode@')
      CALL IPPSTR('/Times-BoldItalic /Times-BoldItalic accvec ReEncode@'
     +)
      CALL IPPSTR('/Helvetica /Helvetica accvec ReEncode@')
      CALL IPPSTR('/Helvetica-Oblique /Helvetica-Oblique accvec ReEncode
     +@')
      CALL IPPSTR('/Helvetica-Bold /Helvetica-Bold accvec ReEncode@
     +')
      CALL IPPSTR('/Helvetica-BoldOblique /Helvetica-BoldOblique  accvec
     + ReEncode@')
      CALL IPPSTR('/Courier /Courier accvec ReEncode@')
      CALL IPPSTR('/Courier-Oblique /Courier-Oblique accvec ReEncode')
      CALL IPPSTR('/Courier-Bold /Courier-Bold accvec ReEncode@')
      CALL IPPSTR('/Courier-BoldOblique /Courier-BoldOblique accvec ReEn
     +code@')
*
*              Initialization of text PostScript procedures
*
      CALL IPPSTR('/oshow {gsave [] 0 sd true charpath stroke gr} def@')
      CALL IPPSTR('/stwn { /fs exch def /fn exch def /text exch def fn f
     +indfont fs sf')
      CALL IPPSTR(' text sw pop xs add /xs exch def} def@')
      CALL IPPSTR('/stwb { /fs exch def /fn exch def /nbas exch def /tex
     +tf exch def')
      CALL IPPSTR('textf length /tlen exch def nbas tlen gt {/nbas tlen
     +def} if')
      CALL IPPSTR('fn findfont fs sf textf dup length nbas sub nbas geti
     +nterval sw')
      CALL IPPSTR('pop neg xs add /xs exch def} def@')
*
      END
