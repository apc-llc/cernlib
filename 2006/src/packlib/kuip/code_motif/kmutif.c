/*
 * $Id: kmutif.c,v 1.3 1996/10/24 12:13:39 cremel Exp $
 *
 * $Log: kmutif.c,v $
 * Revision 1.3  1996/10/24 12:13:39  cremel
 * Version 2.07/10
 * Add resource higzMessGeometry to have control on the size and position of
 * the "HigzOutput" window (e.g. for the locator). To change the size
 * and position of this window one must add in his .Xdefaults (or .Xresources)
 * file the 2 following lines:
 *
 * Paw++*kuipScroll1.defaultPosition: False
 * Paw++*higzMessGeometry: 200x250+650+650
 *
 * Revision 1.2  1996/08/27 12:08:57  cremel
 * Put the HizOutput window always on top of the others.
 *
 * Revision 1.1.1.1  1996/03/08 15:33:09  mclareni
 * Kuip
 *
 */
/*CMZ :  2.05/00 14/03/94  13.45.14  by  N.Cremel*/
/*-- Author :    N.Cremel   03/04/92*/
/******************************************************************************/
/*                                                                            */
/*           MOTIF-based library for KUIP - deck KMUTIF                       */
/*                                                                            */
/*           General Motif utilitity routines which are FORTRAN callable      */
/*                                                                            */
/*           - F77_ENTRY_I4CCC(Kmxmes, ...) : Motif version of HIGX/X Igxmess */
/*                                                                            */
/******************************************************************************/

#include "kuip/kuip.h"
#include "kuip/kfor.h"
#include "kuip/kmenu.h"

#include <Xm/Xm.h>
#if defined(__hpux) && XtSpecificationRelease == 4
#include <X11/Protocols.h>
#else
#include <Xm/Protocols.h>
#endif

#include "mkincl.h"
#include "mksres.h"
#include "mksreg.h"
#include "mkutdm.h"
#include "mkutfm.h"
#define CONTEXT_MACRO_ACCESS 1
#include "mkuxs1.h"
#undef CONTEXT_MACRO_ACCESS

#define  Kmxmes          F77_NAME(kmxmes,KMXMES)

/* extern data */
extern XmFontList help_font;
extern XFontStruct *help_font_struct;

/* extern functions */
extern C_PROTO_1(Widget km_kuipScroll1,
                         int help_flag);


/***********************************************************************
 *                                                                     *
 *   Motif version of HIGX/X Igxmess                                   *
 *                                                                     *
 ***********************************************************************/
void F77_ENTRY_I4CCC(Kmxmes,ix,iy,nc,nl,chmess,chtit,chopt)
/* { */
   int i, pos;
   Dimension     dw, dh;

   char *mtext = NULL;
   char *tit = fstrdup( chtit, len_chtit );
   char *opt = fstrdup( chopt, len_chopt );
   int mlen = 0;
   int do_popup = 0;
   Boolean dogeom = True;
   int gh, gw, gx, gy;


   if (*nl == 0) return;

   sscanf (srec.higzmess_geometry, "%dx%d+%d+%d", &gw, &gh, &gx, &gy);
   if (gw == 0 && gh == 0 && gx == 0 && gy == 0) dogeom = False;


   /* Create window if not done */
   if (km_HigzOutput == NULL ) {
       XmString      xmstr;
       Position xr = (Position) *ix;
       Position yr = (Position) *iy;
       int flag = 0;

       km_HigzOutput = km_kuipScroll1(flag);
       km_store_appShell (km_HigzOutput, tit);
       xmstr = XmStringCreateLtoR(tit, XmSTRING_DEFAULT_CHARSET);
       if (!dogeom) {
       XtVaSetValues(km_HigzOutput,
                     XmNdialogTitle, xmstr, XmNx, xr, XmNy, yr, NULL);
       } else {
       XtVaSetValues(km_HigzOutput, XmNdialogTitle, xmstr, 
                     XmNx, (Position)gx, XmNy, (Position)gy, NULL);
       }
       XmStringFree(xmstr);
       do_popup = 1;
   }


   /* Write text */
   UxKuipScroll1Context = (_UxCkuipScroll1 *)UxGetContext(km_HigzOutput);
   /* set help_font (proportional) if defined */
   if (help_font != NULL )
       XtVaSetValues(scrollText1, XmNfontList, help_font, NULL);
   pos = XmTextGetLastPosition(scrollText1);
   for( i = 0; i < *nl; i++) {
           char *mess = fstrdup( &chmess[len_chmess*i], len_chmess );
           int len = strlen(mess);

           if (len > mlen) {
               mlen = len;
               mtext = strdup ( mess );
           }
           mess = mstrcat (mess, "\n");
           XmTextInsert(scrollText1, pos, mess);
           pos = XmTextGetLastPosition(scrollText1);
           free (mess);
   }
   if (!dogeom) {
   if (help_font_struct != NULL) {
   /* calculate width according to maximum text line length */
       if (mtext != NULL)
           dw = (Dimension) XTextWidth(help_font_struct, mtext, strlen(mtext));
       dw += 50;
       XtVaGetValues(km_HigzOutput, XmNheight, &dh, NULL);
   } else
       XtVaGetValues(km_HigzOutput, XmNheight, &dh, XmNwidth, &dw, NULL);
   if (dw != 0 && dh != 0)
       XtVaSetValues(km_HigzOutput, XmNwidth, dw, XmNheight, dh, NULL);
   } else {   /* if dogeom */
       XtVaSetValues(km_HigzOutput, XmNwidth, (Dimension)gw, XmNheight, 
                     (Dimension)gh, NULL);
   }

   XmTextInsert(scrollText1, pos, "--->\n");
   pos = XmTextGetLastPosition(scrollText1);
   XmTextShowPosition(scrollText1, pos);

   /* if (do_popup) UxPopupInterface(km_HigzOutput, XtGrabNone); */
   /* otherwise window can be obscured and not visible anymore */
   UxPopupInterface(km_HigzOutput, XtGrabNone);

   free(tit);
   free(opt);
}
