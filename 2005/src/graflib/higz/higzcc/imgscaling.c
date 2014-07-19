/*
 * $Id: imgscaling.c,v 
 *
 * $Log: imgscaling.c,v $
 * Revision 1.1  1998/11/16 09:36:55  couet
 * - new procedure to scale bitmap images.
 *
 *
 */
#include "higz/pilot.h"
#if defined(CERNLIB_GIF)
#include <X11/Xlib.h>
#include <stdio.h>

XImage * ImgScaling(Display *, XImage *, int, int);

/******************************************************************************
 *                                                                            *
 * Name: ImgScaling                                         Date:    13.11.98 *
 * Author: O.Couet                                          Revised:          *
 *                                                                            *
 * Transforms an XImage into a other one in a other scale.                    *
 *                                                                            *
 ******************************************************************************/
XImage * ImgScaling(display, image, new_width, new_height)
Display *display;
XImage  *image;
int new_width, new_height;
{
XImage  *new_image;
int old_width, old_height;
float xratio, yratio;
int x, y, xo, yo;
unsigned long pixel;

  new_image = XGetImage( display, RootWindow( display, DefaultScreen(display)), 
                         0, 0, new_width, new_height, AllPlanes, ZPixmap);

  old_width  = image->width;
  old_height = image->height;

  xratio = (float)(old_width)/(float)(new_width);
  yratio = (float)(old_height)/(float)(new_height);

  for (x = 0; x < new_width; x++) {
     for (y = 0; y < new_height; y++) {
	xo = (int)(xratio*(float)x);
	yo = (int)(yratio*(float)y);
        pixel = XGetPixel(image, xo, yo);  
	XPutPixel(new_image, x, y, pixel);
     }
  }

  return new_image;
}
#endif
