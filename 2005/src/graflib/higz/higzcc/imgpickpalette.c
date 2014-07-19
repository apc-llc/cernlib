/*
 * $Id: imgpickpalette.c,v 1.5 1998/11/13 13:19:53 couet Exp $
 *
 * $Log: imgpickpalette.c,v $
 * Revision 1.5  1998/11/13 13:19:53  couet
 * - removed unused variables.
 *
 * Revision 1.4  1998/11/13 10:49:05  couet
 * - New version of the routine imgpickpalette coming from a C++ version
 *   from Fons Rademakers. This version allows to work on TrueColor
 *   devices. ixdogif in x11int.c has been modified in order to use this
 *   new version of imgpickpalette. Protections have been added in
 *   ixdogif to prevent some crashes when one try to create a gif after
 *   a window resizing or when the program (for example PAW) started
 *   with a workstation type equal to 0. All the static arrays in ixdogif
 *   have been converted into dynamic one.
 *
 * Revision 1.3  1996/03/19 16:10:28  couet
 * The variable "window" is now compared with 0 instead of NULL.
 *
 * Revision 1.2  1996/03/19 15:27:36  couet
 * The variable "window" is now compared with (Window)NULL instead of NULL.
 * Some compilers need this cast.
 *
 * Revision 1.1.1.1  1996/02/14 13:10:26  mclareni
 * Higz
 *
 */
#include "higz/pilot.h"
#if defined(CERNLIB_GIF)
#include <X11/Xlib.h>
#include <stdio.h>

void CollectImageColors(unsigned long, unsigned long **, int *, int *);
int FindColor(unsigned long, unsigned long *, int);

/******************************************************************************
 *                                                                            *
 * Name: ImgPickPalette                                     Date:    12.11.98 *
 * Author: F.Rademakers                                     Revised:          *
 *         Converted in C by O.Couet                                          *
 *                                                                            *
 * Returns in R G B the ncol colors of the palette used by the image.         *
 * The image pixels are changed to index values in these R G B arrays.        *
 * This produces a colormap with only the used colors (so even on displays    *
 * with more than 8 planes we will be able to create GIF's when the image     *
 * contains no more than 256 different colors). If it does contain more       *
 * colors we will have to use GIFquantize to reduce the number of colors.     *
 * The R G B arrays must be deleted by the caller.                            *
 *                                                                            *
 ******************************************************************************/
void ImgPickPalette(display, image, ncol, R, G, B)
Display *display;
XImage  *image;
int     *ncol, **R, **G, **B;
{
   unsigned long *orgcolors, pixel;
   int      maxcolors, ncolors;
   int      x, y, i, theScr, idx;
   XColor   *xcol;
   Colormap theMap;

/* 
 * collect different image colors
 */
   maxcolors = 0;
   ncolors = 0;
   for (x = 0; x < image->width; x++) {
      for (y = 0; y < image->height; y++) {
         pixel = XGetPixel(image, x, y);
         CollectImageColors(pixel, &orgcolors, &ncolors, &maxcolors);
      }
   }

/*
 *  get RGB values belonging to pixels
 */
   xcol = (XColor *) malloc(ncolors*sizeof(XColor));

   for (i = 0; i < ncolors; i++) {
      xcol[i].pixel = orgcolors[i];
      xcol[i].red   = xcol[i].green = xcol[i].blue = 0;
      xcol[i].flags = DoRed || DoGreen || DoBlue;
   }

   theScr  = DefaultScreen(display);
   theMap  = DefaultColormap(display, theScr);

   XQueryColors(display, theMap, xcol, ncolors);

/*
 * create RGB arrays and store RGB's for each color and set number of colors
 * (space must be delete by caller)
 */
   *R = (int *) malloc(ncolors*sizeof(int));
   *G = (int *) malloc(ncolors*sizeof(int));
   *B = (int *) malloc(ncolors*sizeof(int));

   for (i = 0; i < ncolors; i++) {
      (*R)[i] = xcol[i].red;
      (*G)[i] = xcol[i].green;
      (*B)[i] = xcol[i].blue;
   }
   *ncol = ncolors;

/*
 * update image with indices (pixels) into the new RGB colormap
 */
   for (x = 0; x < (int) image->width; x++) {
      for (y = 0; y < (int) image->height; y++) {
         pixel = XGetPixel(image, x, y);
         idx = FindColor(pixel, orgcolors, ncolors);
         XPutPixel(image, x, y, idx);
      }
   }

/*
 *  cleanup
 */
   free(xcol);
   free(orgcolors);
}

/*
 * Collects in orgcolors all different original image colors.
 */
void CollectImageColors(pixel, orgcolors, ncolors, maxcolors)
unsigned long pixel, **orgcolors;
int *ncolors, *maxcolors;
{
  int i;

   if (*maxcolors == 0) {
      *ncolors   = 0;
      *maxcolors = 100;
      *orgcolors  = (unsigned long *) malloc ((*maxcolors)*sizeof(unsigned long));
   }

   for (i = 0; i < *ncolors; i++) {
      if (pixel == (*orgcolors)[i]) return;
   }

   if ((*ncolors) >= (*maxcolors)) {
      *orgcolors = (unsigned long *) realloc(*orgcolors,
                                          (*maxcolors)*2*sizeof(unsigned long));
      *maxcolors *= 2;
   }

   (*orgcolors)[(*ncolors)] = pixel;
   (*ncolors)++;
}

/*
 * Returns index in orgcolors (and new_colors) for pixel.
 */
int FindColor(pixel, orgcolors, ncolors)
unsigned long pixel, *orgcolors;
int ncolors;
{
   int i;
   for (i = 0; i < ncolors; i++)
      if (pixel == orgcolors[i]) return i;

   printf("**** Error: did not find color, should never happen! \n");

   return 0;
}
#endif
